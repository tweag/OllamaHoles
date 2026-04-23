{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Options.Spec (tests) where

import Data.Aeson (Value(..))
import Data.Aeson.KeyMap qualified as KM
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles
  ( mkTemplateSpec
  )
import GHC.Plugin.OllamaHoles.Options
  ( Flags(..)
  , defaultFlags
  , parseCommandLineOptions
  , OptError(..)
  , Token(..)
  )
import GHC.Plugin.OllamaHoles.Logger
  ( LogMode(..)
  )
import GHC.Plugin.OllamaHoles.Template
  ( TemplateSpec(..)
  , TemplateSource(..)
  , TemplateError(..)
  , unsafeCreateRawTemplateName
  )

tests :: TestTree
tests =
  testGroup "Options"
    [ parserDefaultsTests
    , parserSimpleTests
    , parserPrecedenceTests
    , parserFailureTests
    , parserLogOptionTests
    , templateParserTests
    , mkTemplateSpecTests
    ]

expectParseOk :: [String] -> IO (Flags, [Token])
expectParseOk opts =
  case parseCommandLineOptions defaultFlags opts of
    Left err ->
      assertFailure ("unexpected parse error: " <> show err) >> fail "unreachable"
    Right ok ->
      pure ok

parserDefaultsTests :: TestTree
parserDefaultsTests =
  testGroup "parseCommandLineOptions defaults"
    [ testCase "empty options yields defaultFlags and no unknowns" $ do
        (flags, unknowns) <- expectParseOk []
        flags @?= defaultFlags
        unknowns @?= []
    ]

parserSimpleTests :: TestTree
parserSimpleTests =
  testGroup "parseCommandLineOptions simple options"
    [ testCase "model= sets model_name" $ do
        (flags, unknowns) <- expectParseOk ["model=phi4"]
        model_name flags @?= "phi4"
        unknowns @?= []

    , testCase "backend= sets backend_name" $ do
        (flags, unknowns) <- expectParseOk ["backend=openai"]
        backend_name flags @?= "openai"
        unknowns @?= []

    , testCase "openai_base_url= sets openai_base_url" $ do
        (flags, unknowns) <- expectParseOk ["openai_base_url=https://example.com/v1"]
        openai_base_url flags @?= "https://example.com/v1"
        unknowns @?= []

    , testCase "openai_key_name= sets openai_key_name" $ do
        (flags, unknowns) <- expectParseOk ["openai_key_name=MY_API_KEY"]
        openai_key_name flags @?= "MY_API_KEY"
        unknowns @?= []

    , testCase "debug enables debug" $ do
        (flags, unknowns) <- expectParseOk ["debug"]
        debug flags @?= True
        unknowns @?= []

    , testCase "include-docs enables include_docs" $ do
        (flags, unknowns) <- expectParseOk ["include-docs"]
        include_docs flags @?= True
        unknowns @?= []

    , testCase "n= sets num_expr" $ do
        (flags, unknowns) <- expectParseOk ["n=17"]
        num_expr flags @?= 17
        unknowns @?= []

    , testCase "model-options= parses valid JSON object" $ do
        (flags, unknowns) <- expectParseOk ["model-options={\"temperature\":1.0,\"num_ctx\":32000}"]
        unknowns @?= []
        case model_options flags of
          Nothing ->
            assertFailure "expected model_options to be set"
          Just (Object obj) -> do
            assertBool "expected temperature key" ("temperature" `KM.member` obj)
            assertBool "expected num_ctx key" ("num_ctx" `KM.member` obj)
          Just other ->
            assertFailure ("expected JSON object, got: " <> show other)

    , testCase "unknown options are reported and do not change flags" $ do
        (flags, unknowns) <- expectParseOk ["bogus-option", "another=thing"]
        flags @?= defaultFlags
        unknowns @?= [BooleanToken "bogus-option", ValueToken "another" "thing"]

    , testCase "unknown options are reported but do not block recognized options" $ do
        (flags, unknowns) <- expectParseOk ["bogus-option", "model=qwen3", "debug"]
        model_name flags @?= "qwen3"
        debug flags @?= True
        unknowns @?= [BooleanToken "bogus-option"]
    ]

parserPrecedenceTests :: TestTree
parserPrecedenceTests =
  testGroup "parseCommandLineOptions precedence"
    [ testCase "leftmost model= wins" $ do
        (flags, unknowns) <- expectParseOk ["model=first", "model=second"]
        model_name flags @?= "first"
        unknowns @?= []

    , testCase "leftmost backend= wins" $ do
        (flags, unknowns) <- expectParseOk ["backend=ollama", "backend=openai"]
        backend_name flags @?= "ollama"
        unknowns @?= []

    , testCase "leftmost openai_base_url= wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "openai_base_url=https://first.example"
          , "openai_base_url=https://second.example"
          ]
        openai_base_url flags @?= "https://first.example"
        unknowns @?= []

    , testCase "leftmost openai_key_name= wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "openai_key_name=FIRST_KEY"
          , "openai_key_name=SECOND_KEY"
          ]
        openai_key_name flags @?= "FIRST_KEY"
        unknowns @?= []

    , testCase "leftmost n= wins" $ do
        (flags, unknowns) <- expectParseOk ["n=3", "n=9"]
        num_expr flags @?= 3
        unknowns @?= []

    , testCase "boolean flags are sticky" $ do
        (flags, unknowns) <- expectParseOk ["debug", "debug", "include-docs", "include-docs"]
        debug flags @?= True
        include_docs flags @?= True
        unknowns @?= []

    , testCase "leftmost model-options= wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "model-options={\"temperature\":0.1}"
          , "model-options={\"temperature\":0.9}"
          ]
        unknowns @?= []
        case model_options flags of
          Nothing ->
            assertFailure "expected model_options to be set"
          Just (Object obj) ->
            assertBool "expected temperature key from leftmost option"
              ("temperature" `KM.member` obj)
          Just other ->
            assertFailure ("expected JSON object, got: " <> show other)

    , testCase "leftmost wins even when interleaved with other options" $ do
        (flags, unknowns) <- expectParseOk
          [ "model=first"
          , "debug"
          , "backend=ollama"
          , "model=second"
          , "include-docs"
          , "backend=openai"
          ]
        model_name flags @?= "first"
        backend_name flags @?= "ollama"
        debug flags @?= True
        include_docs flags @?= True
        unknowns @?= []
    ]

parserFailureTests :: TestTree
parserFailureTests =
  testGroup "parseCommandLineOptions failures"
    [ testCase "invalid n returns structured error" $ do
        parseCommandLineOptions defaultFlags ["n=not-an-int"]
          @?= Left (InvalidInt "n" "not-an-int")

    , testCase "invalid model-options returns structured error" $ do
        case parseCommandLineOptions defaultFlags ["model-options={not json}"] of
          Left (InvalidJson "model-options" "{not json}" _) ->
            pure ()
          other ->
            assertFailure ("expected InvalidJson, got: " <> show other)

    , testCase "unknown options are reported and do not block recognized options" $ do
        (flags, unknowns) <- expectParseOk ["bogus-option", "model=qwen3", "debug"]
        model_name flags @?= "qwen3"
        debug flags @?= True
        unknowns @?= [BooleanToken "bogus-option"]

    , testCase "value-like unknown options are reported and do not block recognized options" $ do
        (flags, unknowns) <- expectParseOk ["bogus=thing", "backend=openai"]
        backend_name flags @?= "openai"
        unknowns @?= [ValueToken "bogus" "thing"]
    ]

parserLogOptionTests :: TestTree
parserLogOptionTests =
  testGroup "parseCommandLineOptions logging options"
    [ testCase "log=off sets log_mode" $ do
        (flags, unknowns) <- expectParseOk ["log=off"]
        log_mode flags @?= Just LogOff
        unknowns @?= []

    , testCase "log=basic sets log_mode" $ do
        (flags, unknowns) <- expectParseOk ["log=basic"]
        log_mode flags @?= Just LogBasic
        unknowns @?= []

    , testCase "log=full sets log_mode" $ do
        (flags, unknowns) <- expectParseOk ["log=full"]
        log_mode flags @?= Just LogFull
        unknowns @?= []

    , testCase "log-dir sets log_dir" $ do
        (flags, unknowns) <- expectParseOk ["log-dir=/tmp/ollama-holes-logs"]
        log_dir flags @?= Just "/tmp/ollama-holes-logs"
        unknowns @?= []

    , testCase "leftmost log= wins" $ do
        (flags, unknowns) <- expectParseOk ["log=basic", "log=off"]
        log_mode flags @?= Just LogBasic
        unknowns @?= []

    , testCase "leftmost log-dir= wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "log-dir=/tmp/one"
          , "log-dir=/tmp/two"
          ]
        log_dir flags @?= Just "/tmp/one"
        unknowns @?= []

    , testCase "log option interleaves with other options" $ do
        (flags, unknowns) <- expectParseOk
          [ "model=qwen3"
          , "log=basic"
          , "debug"
          , "log-dir=/tmp/logs"
          ]
        model_name flags @?= "qwen3"
        debug flags @?= True
        log_mode flags @?= Just LogBasic
        log_dir flags @?= Just "/tmp/logs"
        unknowns @?= []

    , testCase "empty log value is an error" $ do
        parseCommandLineOptions defaultFlags ["log="]
          @?= Left (EmptyValue "log")

    , testCase "empty log-dir value is an error" $ do
        parseCommandLineOptions defaultFlags ["log-dir="]
          @?= Left (EmptyValue "log-dir")

    , testCase "invalid log mode is a structured error" $ do
        parseCommandLineOptions defaultFlags ["log=weird"]
          @?= Left (InvalidEnum "log" "weird" ["off", "basic", "full"])
    ]

templateParserTests :: TestTree
templateParserTests =
  testGroup "parseCommandLineOptions template options"
    [ testCase "defaults contain no template path or name" $ do
        (flags, unknowns) <- expectParseOk []
        template_path flags @?= Nothing
        template_name flags @?= Nothing
        template_search_dir flags @?= "."
        unknowns @?= []

    , testCase "template= sets path and clears name" $ do
        (flags, unknowns) <- expectParseOk ["template=/tmp/prompt.txt"]
        template_path flags @?= Just "/tmp/prompt.txt"
        template_name flags @?= Nothing
        unknowns @?= []

    , testCase "template-name= sets name and clears path" $ do
        (flags, unknowns) <- expectParseOk ["template-name=qwen"]
        template_path flags @?= Nothing
        template_name flags @?= Just "qwen"
        unknowns @?= []

    , testCase "template-dir= sets search dir" $ do
        (flags, unknowns) <- expectParseOk ["template-dir=/tmp/templates"]
        template_search_dir flags @?= "/tmp/templates"
        unknowns @?= []

    , testCase "leftmost template selector wins: path then name keeps path" $ do
        (flags, unknowns) <- expectParseOk
          [ "template=/tmp/a.txt"
          , "template-name=qwen"
          ]
        template_path flags @?= Just "/tmp/a.txt"
        template_name flags @?= Nothing
        unknowns @?= []

    , testCase "leftmost template selector wins: name then path keeps name" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-name=qwen"
          , "template=/tmp/a.txt"
          ]
        template_path flags @?= Nothing
        template_name flags @?= Just "qwen"
        unknowns @?= []

    , testCase "leftmost template-dir wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/one"
          , "template-dir=/tmp/two"
          ]
        template_search_dir flags @?= "/tmp/one"
        unknowns @?= []

    , testCase "template-dir combines with template-name" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template-name=qwen"
          ]
        template_search_dir flags @?= "/tmp/templates"
        template_name flags @?= Just "qwen"
        template_path flags @?= Nothing
        unknowns @?= []

    , testCase "template-dir combines with template path" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template=/tmp/prompt.txt"
          ]
        template_search_dir flags @?= "/tmp/templates"
        template_path flags @?= Just "/tmp/prompt.txt"
        template_name flags @?= Nothing
        unknowns @?= []

    , testCase "leftmost template selector still wins when interleaved with dir flags" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-name=alpha"
          , "template-dir=/tmp/one"
          , "template=/tmp/prompt.txt"
          , "template-dir=/tmp/two"
          ]
        template_name flags @?= Just "alpha"
        template_path flags @?= Nothing
        template_search_dir flags @?= "/tmp/one"
        unknowns @?= []
    ]

mkTemplateSpecTests :: TestTree
mkTemplateSpecTests =
  testGroup "mkTemplateSpec"
    [ testCase "default flags choose DefaultTemplate" $ do
        mkTemplateSpec defaultFlags
          @?= Right (TemplateSpec
                { tsSearchDir = "."
                , tsSource = DefaultTemplate
                })

    , testCase "path chooses TemplateFile" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template=/tmp/prompt.txt"
          ]
        unknowns @?= []
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = TemplateFile "/tmp/prompt.txt"
                })

    , testCase "name chooses NamedTemplate" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template-name=qwen"
          ]
        unknowns @?= []
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = NamedTemplate (unsafeCreateRawTemplateName "qwen")
                })

    , testCase "search dir is preserved with default template" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates" ]
        unknowns @?= []
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = DefaultTemplate
                })

    , testCase "path beats name in mkTemplateSpec if both are present" $ do
        let flags = defaultFlags
              { template_path = Just "/tmp/prompt.txt"
              , template_name = Just "qwen"
              , template_search_dir = "/tmp/templates"
              }
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = TemplateFile "/tmp/prompt.txt"
                })

    , testCase "invalid name is rejected" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template-name=../secrets"
          ]
        unknowns @?= []
        mkTemplateSpec flags
          @?= Left (InvalidTemplateName "../secrets")
    ]