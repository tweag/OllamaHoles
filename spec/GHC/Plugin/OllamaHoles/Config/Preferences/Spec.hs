{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Config.Preferences.Spec (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Config.Preferences
  ( Preferences(..)
  , parsePreferencesToml
  , TomlParseResult(..)
  )
import GHC.Plugin.OllamaHoles.Config.Types
  ( FanoutProf(..)
  , GeminiConfig(..)
  , ModelName(..)
  , OllamaConfig(..)
  , OpenAIConfig(..)
  , Profile(..)
  , ProfileKind(..)
  , ProfileName(..)
  , Service(..)
  , ServiceConfig(..)
  , ServiceName(..)
  , ServiceProf(..)
  )
import GHC.Plugin.OllamaHoles.Template
  ( TemplateSource(..)
  , unsafeCreateRawTemplateName
  )
import GHC.Plugin.OllamaHoles.Trigger
  ( TriggerPolicy(..)
  )

tests :: TestTree
tests =
  testGroup "Config.Preferences"
    [ serviceDecodeTests
    , profileDecodeTests
    , preferencesDecodeTests
    , propertyTests
    ]


-- Helpers
-----------

expectParseOk :: Text -> IO Preferences
expectParseOk txt =
  case parsePreferencesToml txt of
    TomlParseFailure err ->
      assertFailure ("unexpected parse failure: " <> show err) >> fail "unreachable"
    TomlParseSuccess _ prefs ->
      pure prefs

expectParseFail :: Text -> Assertion
expectParseFail txt =
  case parsePreferencesToml txt of
    TomlParseFailure _  -> pure ()
    TomlParseSuccess _ x -> assertFailure ("expected parse failure, got: " <> show x)

singleService :: Preferences -> Service
singleService prefs =
  case prefServices prefs of
    [svc] -> svc
    xs    -> error ("expected exactly one service, got " <> show (length xs))

singleProfile :: Preferences -> Profile
singleProfile prefs =
  case prefProfiles prefs of
    [prof] -> prof
    xs     -> error ("expected exactly one profile, got " <> show (length xs))


-- Unit tests: services
-----------------------

serviceDecodeTests :: TestTree
serviceDecodeTests =
  testGroup "service decoding"
    [ testCase "decodes ollama service with host" $ do
        prefs <- expectParseOk $ T.unlines
          [ "profiles = []"
          , ""
          , "[[services]]"
          , "name = \"local\""
          , "protocol = \"ollama\""
          , "host = \"http://127.0.0.1:11434\""
          ]
        singleService prefs
          @?= Service
                { svcName = ServiceName "local"
                , svcConfig = SvcOllama (OllamaConfig (Just "http://127.0.0.1:11434"))
                }

    , testCase "decodes ollama service without host" $ do
        prefs <- expectParseOk $ T.unlines
          [ "profiles = []"
          , ""
          , "[[services]]"
          , "name = \"local\""
          , "protocol = \"ollama\""
          ]
        singleService prefs
          @?= Service
                { svcName = ServiceName "local"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }

    , testCase "decodes openai service" $ do
        prefs <- expectParseOk $ T.unlines
          [ "profiles = []"
          , ""
          , "[[services]]"
          , "name = \"groq\""
          , "protocol = \"openai\""
          , "base_url = \"https://api.groq.com/openai/v1\""
          , "key_name = \"GROQ_API_KEY\""
          ]
        singleService prefs
          @?= Service
                { svcName = ServiceName "groq"
                , svcConfig = SvcOpenAI (OpenAIConfig
                    { svcOpenAIBaseUrl = "https://api.groq.com/openai/v1"
                    , svcOpenAIKeyName = "GROQ_API_KEY"
                    })
                }

    , testCase "decodes openai-compatible service alias" $ do
        prefs <- expectParseOk $ T.unlines
          [ "profiles = []"
          , ""
          , "[[services]]"
          , "name = \"router\""
          , "protocol = \"openai-compatible\""
          , "base_url = \"https://openrouter.ai/api/v1\""
          , "key_name = \"OPENROUTER_API_KEY\""
          ]
        singleService prefs
          @?= Service
                { svcName = ServiceName "router"
                , svcConfig = SvcOpenAI (OpenAIConfig
                    { svcOpenAIBaseUrl = "https://openrouter.ai/api/v1"
                    , svcOpenAIKeyName = "OPENROUTER_API_KEY"
                    })
                }

    , testCase "decodes gemini service" $ do
        prefs <- expectParseOk $ T.unlines
          [ "profiles = []"
          , ""
          , "[[services]]"
          , "name = \"gemini\""
          , "protocol = \"gemini\""
          , "key_name = \"GEMINI_API_KEY\""
          ]
        singleService prefs
          @?= Service
                { svcName = ServiceName "gemini"
                , svcConfig = SvcGemini (GeminiConfig "GEMINI_API_KEY")
                }

    , testCase "rejects invalid service protocol" $
        expectParseFail $ T.unlines
          [ "profiles = []"
          , ""
          , "[[services]]"
          , "name = \"bad\""
          , "protocol = \"wat\""
          ]

    , testCase "rejects openai service missing base_url" $
        expectParseFail $ T.unlines
          [ "profiles = []"
          , ""
          , "[[services]]"
          , "name = \"groq\""
          , "protocol = \"openai\""
          , "key_name = \"GROQ_API_KEY\""
          ]
    ]


-- Unit tests: profiles
-----------------------

profileDecodeTests :: TestTree
profileDecodeTests =
  testGroup "profile decoding"
    [ testCase "decodes service profile minimal" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          ]
        singleProfile prefs
          @?= Profile
                { profName = ProfileName "local"
                , profKind = ProfService ServiceProf
                    { profService      = ServiceName "ollama"
                    , profModel        = ModelName "qwen3:4b"
                    , profTemplate     = Nothing
                    , profModelOptions = Nothing
                    , profNumExpr      = Nothing
                    , profIncludeDocs  = Nothing
                    , profTrigger      = Nothing
                    }
                }

    , testCase "decodes service profile with default template" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          , "template = \"default\""
          ]
        singleProfile prefs
          @?= Profile
                { profName = ProfileName "local"
                , profKind = ProfService ServiceProf
                    { profService      = ServiceName "ollama"
                    , profModel        = ModelName "qwen3:4b"
                    , profTemplate     = Just DefaultTemplate
                    , profModelOptions = Nothing
                    , profNumExpr      = Nothing
                    , profIncludeDocs  = Nothing
                    , profTrigger      = Nothing
                    }
                }

    , testCase "decodes service profile with named template" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          , "template = \"small\""
          ]
        singleProfile prefs
          @?= Profile
                { profName = ProfileName "local"
                , profKind = ProfService ServiceProf
                    { profService      = ServiceName "ollama"
                    , profModel        = ModelName "qwen3:4b"
                    , profTemplate     = Just (NamedTemplate (unsafeCreateRawTemplateName "small"))
                    , profModelOptions = Nothing
                    , profNumExpr      = Nothing
                    , profIncludeDocs  = Nothing
                    , profTrigger      = Nothing
                    }
                }

    , testCase "decodes service profile with template_file" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          , "template_file = \"/tmp/prompt.txt\""
          ]
        singleProfile prefs
          @?= Profile
                { profName = ProfileName "local"
                , profKind = ProfService ServiceProf
                    { profService      = ServiceName "ollama"
                    , profModel        = ModelName "qwen3:4b"
                    , profTemplate     = Just (TemplateFile "/tmp/prompt.txt")
                    , profModelOptions = Nothing
                    , profNumExpr      = Nothing
                    , profIncludeDocs  = Nothing
                    , profTrigger      = Nothing
                    }
                }

    , testCase "rejects service profile with both template and template_file" $
        expectParseFail $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          , "template = \"default\""
          , "template_file = \"/tmp/prompt.txt\""
          ]

    , testCase "decodes service profile trigger policy" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          , "trigger = \"prefix:llm\""
          ]
        case singleProfile prefs of
          Profile _ (ProfService sp) ->
            profTrigger sp @?= Just (TriggerPrefix "llm")
          other ->
            assertFailure ("expected service profile, got: " <> show other)

    , testCase "rejects invalid trigger policy" $
        expectParseFail $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          , "trigger = \"prefix:_llm\""
          ]

    , testCase "decodes model_options object as aeson value" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"ollama\""
          , "model = \"qwen3:4b\""
          , ""
          , "[profiles.model_options]"
          , "temperature = 0.3"
          , "num_ctx = 32768"
          ]
        case singleProfile prefs of
          Profile _ (ProfService sp) ->
            case profModelOptions sp of
              Just (Aeson.Object obj) -> do
                assertBool "temperature present" ("temperature" `KM.member` obj)
                assertBool "num_ctx present" ("num_ctx" `KM.member` obj)
              other ->
                assertFailure ("expected object model_options, got: " <> show other)
          other ->
            assertFailure ("expected service profile, got: " <> show other)

    , testCase "decodes fanout profile" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"pair\""
          , "type = \"profiles\""
          , "profiles = [\"local\", \"remote\"]"
          ]
        singleProfile prefs
          @?= Profile
                { profName = ProfileName "pair"
                , profKind = ProfFanout (FanoutProf
                    { profProfiles =
                        ProfileName "local" NE.:| [ProfileName "remote"]
                    })
                }

    , testCase "fanout alias parses too" $ do
        prefs <- expectParseOk $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"pair\""
          , "type = \"fanout\""
          , "profiles = [\"local\"]"
          ]
        case singleProfile prefs of
          Profile _ (ProfFanout _) -> pure ()
          other ->
            assertFailure ("expected fanout profile, got: " <> show other)

    , testCase "rejects empty fanout profile list" $
        expectParseFail $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"pair\""
          , "type = \"profiles\""
          , "profiles = []"
          ]

    , testCase "rejects invalid profile type" $
        expectParseFail $ T.unlines
          [ "services = []"
          , ""
          , "[[profiles]]"
          , "name = \"bad\""
          , "type = \"wat\""
          ]
    ]


-- Unit tests: end to end preferences
-------------------------------------

preferencesDecodeTests :: TestTree
preferencesDecodeTests =
  testGroup "preferences decoding"
    [ testCase "decodes full preferences document" $ do
        prefs <- expectParseOk $ T.unlines
          [ "[[services]]"
          , "name = \"local\""
          , "protocol = \"ollama\""
          , "host = \"http://127.0.0.1:11434\""
          , ""
          , "[[services]]"
          , "name = \"groq\""
          , "protocol = \"openai-compatible\""
          , "base_url = \"https://api.groq.com/openai/v1\""
          , "key_name = \"GROQ_API_KEY\""
          , ""
          , "[[profiles]]"
          , "name = \"local\""
          , "type = \"service\""
          , "service = \"local\""
          , "model = \"qwen3:4b\""
          , "trigger = \"prefix:llm\""
          , ""
          , "[[profiles]]"
          , "name = \"pair\""
          , "type = \"profiles\""
          , "profiles = [\"local\"]"
          ]
        length (prefServices prefs) @?= 2
        length (prefProfiles prefs) @?= 2
    ]


-- Properties
--------------

propertyTests :: TestTree
propertyTests =
  testGroup "properties"
    [ QC.testProperty "openai and openai-compatible protocols decode the same way" $
        \(QC.NonEmpty base) (QC.NonEmpty key) ->
          let mkDoc protocol = T.unlines
                [ "[[services]]"
                , "name = \"svc\""
                , "protocol = \"" <> protocol <> "\""
                , "base_url = \"" <> T.pack base <> "\""
                , "key_name = \"" <> T.pack key <> "\""
                , ""
                , "profiles = []"
                ]
              p1 = parsePreferencesToml (mkDoc "openai")
              p2 = parsePreferencesToml (mkDoc "openai-compatible")
          in p1 QC.=== p2

    , QC.testProperty "template and template_file together are always rejected" $
        \(QC.NonEmpty tmpl) (QC.NonEmpty fp) ->
          let doc = T.unlines
                [ "services = []"
                , ""
                , "[[profiles]]"
                , "name = \"p\""
                , "type = \"service\""
                , "service = \"svc\""
                , "model = \"m\""
                , "template = \"" <> T.pack tmpl <> "\""
                , "template_file = \"" <> T.pack fp <> "\""
                ]
          in case parsePreferencesToml doc of
               TomlParseFailure _  -> QC.property True
               TomlParseSuccess _ _ -> QC.counterexample "expected parse failure" False

    , QC.testProperty "valid trigger policies inside service profiles decode" $
        QC.forAll genValidTriggerPolicyText $ \trig ->
          let doc = T.unlines
                [ "services = []"
                , ""
                , "[[profiles]]"
                , "name = \"p\""
                , "type = \"service\""
                , "service = \"svc\""
                , "model = \"m\""
                , "trigger = \"" <> trig <> "\""
                ]
          in case parsePreferencesToml doc of
               TomlParseSuccess _ (Preferences _ [Profile _ (ProfService sp)]) ->
                 QC.property (isJustTrigger (profTrigger sp))
               other ->
                 QC.counterexample ("unexpected parse result: " <> show other) False
    ]

genValidTriggerPolicyText :: QC.Gen Text
genValidTriggerPolicyText =
  QC.oneof
    [ pure "all"
    , pure "none"
    , do
        c0 <- QC.elements ['a' .. 'z']
        rest <- QC.listOf (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
        pure ("prefix:" <> T.pack (c0 : rest))
    ]

isJustTrigger :: Maybe a -> Bool
isJustTrigger = maybe False (const True)