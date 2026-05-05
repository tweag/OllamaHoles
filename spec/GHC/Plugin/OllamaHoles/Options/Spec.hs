{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Options.Spec (tests) where

import Data.Aeson (Value(..))
import Data.Aeson.KeyMap qualified as KM
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Flags
  ( mkTemplateSpec
  )
import GHC.Plugin.OllamaHoles.Data.Flags
  (Flags(..), parseFlags, FlagError(..), FlagToken(..))
import GHC.Plugin.OllamaHoles.Logger
  (LogMode(..))
import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Error
import GHC.Plugin.OllamaHoles.Template
  ( TemplateSpec(..)
  , TemplateSource(..)
  , TemplateError(..)
  , unsafeCreateRawTemplateName
  )
import GHC.Plugin.OllamaHoles.Backend
  ( BackendSlug(..)
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
    , triggerParserTests
    , mkTemplateSpecTests
    ]

expectParseOk :: [String] -> IO (Flags, [FlagToken])
expectParseOk opts =
  case parseFlags opts of
    Left err ->
      assertFailure ("unexpected parse error: " <> show err) >> fail "unreachable"
    Right ok ->
      pure ok

parserDefaultsTests :: TestTree
parserDefaultsTests =
  testGroup "parseFlags defaults"
    [ testCase "empty options yields default flags and no unknowns" $ do
        (flags, unknowns) <- expectParseOk []
        flags @?= mempty
        unknowns @?= []
    ]

parserSimpleTests :: TestTree
parserSimpleTests =
  testGroup "parseFlags simple options"
    [ testCase "model= sets model_name" $ do
        (flags, unknowns) <- expectParseOk ["model=phi4"]
        model_name flags @?= Just "phi4"
        unknowns @?= []

    , testCase "backend= sets backend_name" $ do
        (flags, unknowns) <- expectParseOk ["backend=openai"]
        backend_name flags @?= Just OpenAI
        unknowns @?= []

    , testCase "openai_base_url= sets openai_base_url" $ do
        (flags, unknowns) <- expectParseOk ["openai_base_url=https://example.com/v1"]
        openai_base_url flags @?= Just "https://example.com/v1"
        unknowns @?= []

    , testCase "openai_key_name= sets openai_key_name" $ do
        (flags, unknowns) <- expectParseOk ["openai_key_name=MY_API_KEY"]
        openai_key_name flags @?= Just "MY_API_KEY"
        unknowns @?= []

    , testCase "debug enables debug" $ do
        (flags, unknowns) <- expectParseOk ["debug"]
        debug flags @?= Just True
        unknowns @?= []

    , testCase "include-docs enables include_docs" $ do
        (flags, unknowns) <- expectParseOk ["include-docs"]
        include_docs flags @?= Just True
        unknowns @?= []

    , testCase "n= sets num_expr" $ do
        (flags, unknowns) <- expectParseOk ["n=17"]
        num_expr flags @?= Just 17
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
        flags @?= mempty
        unknowns @?= [BooleanToken "bogus-option", ValueToken "another" "thing"]

    , testCase "unknown options are reported but do not block recognized options" $ do
        (flags, unknowns) <- expectParseOk ["bogus-option", "model=qwen3", "debug"]
        model_name flags @?= Just "qwen3"
        debug flags @?= Just True
        unknowns @?= [BooleanToken "bogus-option"]
    ]

parserPrecedenceTests :: TestTree
parserPrecedenceTests =
  testGroup "parseFlags precedence"
    [ testCase "leftmost model= wins" $ do
        (flags, unknowns) <- expectParseOk ["model=first", "model=second"]
        model_name flags @?= Just "first"
        unknowns @?= []

    , testCase "leftmost backend= wins" $ do
        (flags, unknowns) <- expectParseOk ["backend=ollama", "backend=openai"]
        backend_name flags @?= Just Ollama
        unknowns @?= []

    , testCase "leftmost openai_base_url= wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "openai_base_url=https://first.example"
          , "openai_base_url=https://second.example"
          ]
        openai_base_url flags @?= Just "https://first.example"
        unknowns @?= []

    , testCase "leftmost openai_key_name= wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "openai_key_name=FIRST_KEY"
          , "openai_key_name=SECOND_KEY"
          ]
        openai_key_name flags @?= Just "FIRST_KEY"
        unknowns @?= []

    , testCase "leftmost n= wins" $ do
        (flags, unknowns) <- expectParseOk ["n=3", "n=9"]
        num_expr flags @?= Just 3
        unknowns @?= []

    , testCase "boolean flags are sticky" $ do
        (flags, unknowns) <- expectParseOk ["debug", "debug", "include-docs", "include-docs"]
        debug flags @?= Just True
        include_docs flags @?= Just True
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
        model_name flags @?= Just "first"
        backend_name flags @?= Just Ollama
        debug flags @?= Just True
        include_docs flags @?= Just True
        unknowns @?= []
    ]

parserFailureTests :: TestTree
parserFailureTests =
  testGroup "parseFlags failures"
    [ testCase "invalid n returns structured error" $ do
        parseFlags ["n=not-an-int"]
          @?= Left (InvalidInt "n" "not-an-int")

    , testCase "invalid model-options returns structured error" $ do
        case parseFlags ["model-options={not json}"] of
          Left (InvalidJson "model-options" "{not json}" _) ->
            pure ()
          other ->
            assertFailure ("expected InvalidJson, got: " <> show other)

    , testCase "unknown options are reported and do not block recognized options" $ do
        (flags, unknowns) <- expectParseOk ["bogus-option", "model=qwen3", "debug"]
        model_name flags @?= Just "qwen3"
        debug flags @?= Just True
        unknowns @?= [BooleanToken "bogus-option"]

    , testCase "value-like unknown options are reported and do not block recognized options" $ do
        (flags, unknowns) <- expectParseOk ["bogus=thing", "backend=openai"]
        backend_name flags @?= Just OpenAI
        unknowns @?= [ValueToken "bogus" "thing"]

    , testCase "missing value for model is structured error" $ do
        parseFlags ["model"]
          @?= Left (MissingValue "model")

    , testCase "missing value for backend is structured error" $ do
        parseFlags ["backend"]
          @?= Left (MissingValue "backend")

    , testCase "unexpected value for debug is structured error" $ do
        parseFlags ["debug=true"]
          @?= Left (UnexpectedValue "debug" "true")

    , testCase "unexpected value for include-docs is structured error" $ do
        parseFlags ["include-docs=yes"]
          @?= Left (UnexpectedValue "include-docs" "yes")

    , testCase "empty option is structured error" $ do
        parseFlags [""]
          @?= Left EmptyFlag
    ]

parserLogOptionTests :: TestTree
parserLogOptionTests =
  testGroup "parseFlags logging options"
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
        model_name flags @?= Just "qwen3"
        debug flags @?= Just True
        log_mode flags @?= Just LogBasic
        log_dir flags @?= Just "/tmp/logs"
        unknowns @?= []

    , testCase "empty log value is an error" $ do
        parseFlags ["log="]
          @?= Left (EmptyValue "log")

    , testCase "empty log-dir value is an error" $ do
        parseFlags ["log-dir="]
          @?= Left (EmptyValue "log-dir")

    , testCase "invalid log mode is a structured error" $ do
        parseFlags ["log=weird"]
          @?= Left (InvalidEnum "log" "weird" ["off", "basic", "full"])
    ]

templateParserTests :: TestTree
templateParserTests =
  testGroup "parseFlags template options"
    [ testCase "defaults contain no template path or name" $ do
        (flags, unknowns) <- expectParseOk []
        template_path flags @?= Nothing
        template_name flags @?= Nothing
        template_search_dir flags @?= Just "."
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
        template_search_dir flags @?= Just "/tmp/templates"
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
        template_search_dir flags @?= Just "/tmp/one"
        unknowns @?= []

    , testCase "template-dir combines with template-name" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template-name=qwen"
          ]
        template_search_dir flags @?= Just "/tmp/templates"
        template_name flags @?= Just "qwen"
        template_path flags @?= Nothing
        unknowns @?= []

    , testCase "template-dir combines with template path" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template=/tmp/prompt.txt"
          ]
        template_search_dir flags @?= Just "/tmp/templates"
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
        template_search_dir flags @?= Just "/tmp/one"
        unknowns @?= []
    ]

mkTemplateSpecTests :: TestTree
mkTemplateSpecTests =
  testGroup "mkTemplateSpec"
    [ testCase "default flags choose DefaultTemplate" $ do
        mkTemplateSpec mempty
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
        let flags = mempty
              { template_path = Just "/tmp/prompt.txt"
              , template_name = Just "qwen"
              , template_search_dir = Just "/tmp/templates"
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

triggerParserTests :: TestTree
triggerParserTests =
  testGroup "parseFlags trigger options"
    [ testCase "default trigger policy is the module default" $ do
        (flags, unknowns) <- expectParseOk []
        trigger_policy flags @?= Just defaultTriggerPolicy
        unknowns @?= []

    , testCase "trigger=all sets TriggerAll" $ do
        (flags, unknowns) <- expectParseOk ["trigger=all"]
        trigger_policy flags @?= Just TriggerAll
        unknowns @?= []

    , testCase "trigger=none sets TriggerNone" $ do
        (flags, unknowns) <- expectParseOk ["trigger=none"]
        trigger_policy flags @?= Just TriggerNone
        unknowns @?= []

    , testCase "trigger=prefix:foo sets TriggerPrefix foo" $ do
        (flags, unknowns) <- expectParseOk ["trigger=prefix:foo"]
        trigger_policy flags @?= Just (TriggerPrefix "foo")
        unknowns @?= []

    , testCase "leftmost trigger wins" $ do
        (flags, unknowns) <- expectParseOk
          [ "trigger=prefix:foo"
          , "trigger=none"
          ]
        trigger_policy flags @?= Just (TriggerPrefix "foo")
        unknowns @?= []

    , testCase "trigger interleaves with other options" $ do
        (flags, unknowns) <- expectParseOk
          [ "model=qwen3"
          , "trigger=prefix:foo"
          , "debug"
          ]
        model_name flags @?= Just "qwen3"
        debug flags @?= Just True
        trigger_policy flags @?= Just (TriggerPrefix "foo")
        unknowns @?= []

    , testCase "missing trigger value is structured error" $ do
        parseFlags ["trigger"]
          @?= Left (MissingValue "trigger")

    , testCase "empty trigger value is structured error" $ do
        parseFlags ["trigger="]
          @?= Left (EmptyValue "trigger")

    , testCase "invalid trigger policy is structured error" $ do
        parseFlags ["trigger=prefix:_foo"]
          @?= Left (InvalidTriggerPolicy "prefix:_foo" (InvalidTriggerPrefix "_foo"))
    ]