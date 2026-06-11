module GHC.Plugin.OllamaHoles.Data.Prefs.Parse.Spec (tests) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck qualified as QC

import Toml.TestHelper

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Prefs

import GHC.Plugin.OllamaHoles.Data.Service.Types.Gen
import GHC.Plugin.OllamaHoles.Data.Prefs.Types.Gen



tests :: TestTree
tests = testGroup "Prefs.Parse"
  [ tests_tomlPreferences_unit
  , tests_tomlPreferences_prop
  ]

tests_tomlPreferences_unit :: TestTree
tests_tomlPreferences_unit = testGroup "tomlPreferences unit"
  [ testGroup "success" $
      tests_tomlPreferences_unit_success <&> \(name, input, expected) ->
        testCase name $ assertTomlParsesAs tomlPreferences input expected

  , testGroup "failure" $
      tests_tomlPreferences_unit_failure <&> \(name, input) ->
        testCase name $ assertTomlParseFails tomlPreferences input
  ]

tests_tomlPreferences_prop :: TestTree
tests_tomlPreferences_prop = testGroup "tomlPreferences prop"
  [ QC.testProperty "openai and openai-compatible protocols decode the same way" $
    QC.forAll genHostText $ \baseUrl ->
    QC.forAll genEnvVarText $ \keyName ->
      let
        mkDoc protocol =
          "templates = []\n\
          \profiles = []\n\
          \\n\
          \[[services]]\n\
          \name = 'svc'\n\
          \protocol = '" <> protocol <> "'\n\
          \base_url = '" <> baseUrl <> "'\n\
          \key_name = '" <> keyName <> "'"
        p1 = parseTomlWith tomlPreferences (mkDoc "openai")
        p2 = parseTomlWith tomlPreferences (mkDoc "openai-compatible")
      in p1 QC.=== p2

  , QC.testProperty "template and template_file together are always rejected" $
    QC.forAll genTemplateValueText $ \tmpl ->
    QC.forAll genTemplateFilePathText $ \fp ->
      let
        doc =
          "services = []\n\
          \templates = []\n\
          \\n\
          \[[profiles]]\n\
          \name = 'p'\n\
          \type = 'service'\n\
          \service = 'svc'\n\
          \model = 'm'\n\
          \template = '" <> tmpl <> "'\n\
          \template_file = '" <> fp <> "'"
      in case parseTomlWith tomlPreferences doc of
          Left _  -> QC.property True
          Right _ -> QC.counterexample "expected parse failure" False

  , QC.testProperty "valid trigger policies inside service profiles decode" $
    QC.forAll genValidTriggerPolicyCase $ \(trigText, expectedTrigger) ->
      let
        doc =
          "services = []\n\
          \templates = []\n\
          \\n\
          \[[profiles]]\n\
          \name = 'p'\n\
          \type = 'service'\n\
          \service = 'svc'\n\
          \model = 'm'\n\
          \trigger = '" <> trigText <> "'"
      in
        case parseTomlWith tomlPreferences doc of
          Right prefs -> case prefProfiles prefs of
            [profile] -> QC.counterexample
              ("decoded profile: " <> show profile) $
              profTrigger profile QC.=== expectedTrigger

            profiles -> QC.counterexample
              ("expected exactly one profile, got: " <> show profiles)
              False

          other -> QC.counterexample
            ("unexpected parse result: " <> show other)
            False
    ]




tests_tomlPreferences_unit_success
  :: [(String, Text, Preferences)]
tests_tomlPreferences_unit_success =
  [ ( "decodes ollama service with host"
    , "profiles = []\n\
      \templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'local'\n\
      \protocol = 'ollama'\n\
      \host = 'http://127.0.0.1:11434'"
    , Preferences
      { prefServices =
        [ Service
          { svcName   = ServiceName "local"
          , svcConfig = SvcOllama (OllamaConfig (Just "http://127.0.0.1:11434"))
          }
        ]
      , prefProfiles = []
      , prefTemplates = []
      }
    )

  , ( "decodes ollama service without host"
    , "profiles = []\n\
      \templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'local'\n\
      \protocol = 'ollama'"
    , Preferences
      { prefServices =
        [ Service
          { svcName   = ServiceName "local"
          , svcConfig = SvcOllama (OllamaConfig Nothing)
          }
        ]
      , prefProfiles = []
      , prefTemplates = []
      }
    )

  , ( "decodes openai service"
    , "profiles = []\n\
      \templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'groq'\n\
      \protocol = 'openai'\n\
      \base_url = 'https://api.groq.com/openai/v1'\n\
      \key_name = 'GROQ_API_KEY'"
    , Preferences
      { prefServices =
        [ Service
          { svcName   = ServiceName "groq"
          , svcConfig = SvcOpenAI $ OpenAIConfig
            { svcOpenAIBaseUrl = "https://api.groq.com/openai/v1"
            , svcOpenAIKeyName = "GROQ_API_KEY"
            }
          }
        ]
      , prefProfiles = []
      , prefTemplates = []
      }
    )

  , ( "decodes openai-compatible service alias"
    , "profiles = []\n\
      \templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'router'\n\
      \protocol = 'openai-compatible'\n\
      \base_url = 'https://openrouter.ai/api/v1'\n\
      \key_name = 'OPENROUTER_API_KEY'"
    , Preferences
      { prefServices =
        [ Service
          { svcName   = ServiceName "router"
          , svcConfig = SvcOpenAI $ OpenAIConfig
            { svcOpenAIBaseUrl = "https://openrouter.ai/api/v1"
            , svcOpenAIKeyName = "OPENROUTER_API_KEY"
            }
          }
        ]
      , prefProfiles = []
      , prefTemplates = []
      }
    )

  , ( "decodes gemini service"
    , "profiles = []\n\
      \templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'gemini'\n\
      \protocol = 'gemini'\n\
      \key_name = 'GEMINI_API_KEY'"
    , Preferences
      { prefServices =
        [ Service
          { svcName   = ServiceName "gemini"
          , svcConfig = SvcGemini (GeminiConfig "GEMINI_API_KEY")
          }
        ]
      , prefProfiles = []
      , prefTemplates = []
      }
    )

  , ( "decodes service profile minimal"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "local"
          , profTrigger = TriggerNone
          , profKind = ProfService ServiceProf
            { profService      = ServiceName "ollama"
            , profModel        = ModelName "qwen3:4b"
            , profTemplate     = Nothing
            , profModelOptions = Nothing
            , profNumExpr      = Nothing
            , profIncludeDocs  = Nothing
            }
          }
        ]
      }
    )

  , ( "decodes service profile with default template"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'\n\
      \template = 'default'"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "local"
          , profTrigger = TriggerNone
          , profKind = ProfService ServiceProf
            { profService      = ServiceName "ollama"
            , profModel        = ModelName "qwen3:4b"
            , profTemplate     = Just DefaultTemplate
            , profModelOptions = Nothing
            , profNumExpr      = Nothing
            , profIncludeDocs  = Nothing
            }
          }
        ]
      }
    )

  , ( "decodes service profile with named template"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'\n\
      \template = 'small'"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "local"
          , profTrigger = TriggerNone
          , profKind = ProfService $ ServiceProf
            { profService      = ServiceName "ollama"
            , profModel        = ModelName "qwen3:4b"
            , profTemplate     = Just (NamedTemplate (unsafeCreateRawTemplateName "small"))
            , profModelOptions = Nothing
            , profNumExpr      = Nothing
            , profIncludeDocs  = Nothing
            }
          }
        ]
      }
    )

  , ( "decodes service profile with template_file"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'\n\
      \template_file = '/tmp/prompt.txt'"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "local"
          , profTrigger = TriggerNone
          , profKind = ProfService ServiceProf
            { profService      = ServiceName "ollama"
            , profModel        = ModelName "qwen3:4b"
            , profTemplate     = Just (TemplateFile "/tmp/prompt.txt")
            , profModelOptions = Nothing
            , profNumExpr      = Nothing
            , profIncludeDocs  = Nothing
            }
          }
        ]
      }
    )

  , ( "decodes service profile trigger policy"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'trigger'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'\n\
      \trigger = 'prefix:llm'"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "trigger"
          , profTrigger = TriggerPrefix "llm"
          , profKind = ProfService ServiceProf
            { profService      = ServiceName "ollama"
            , profModel        = ModelName "qwen3:4b"
            , profTemplate     = Nothing
            , profModelOptions = Nothing
            , profNumExpr      = Nothing
            , profIncludeDocs  = Nothing
            }
          }
        ]
      }
    )

  , ( "decodes fanout profile"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'pair'\n\
      \type = 'profiles'\n\
      \profiles = ['local', 'remote']"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "pair"
          , profTrigger = TriggerNone
          , profKind = ProfFanout $ FanoutProf
            { profProfiles =
                ProfileName "local" :| [ProfileName "remote"]
            }
          }
        ]
      }
    )

  , ( "fanout alias parses too"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'pair'\n\
      \type = 'fanout'\n\
      \profiles = ['local']"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "pair"
          , profTrigger = TriggerNone
          , profKind = ProfFanout $ FanoutProf
            { profProfiles = ProfileName "local" :| [] }
          }
        ]
      }
    )

  , ( "decodes model_options object as aeson value"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'\n\
      \\n\
      \[profiles.model_options]\n\
      \temperature = 0.3\n\
      \num_ctx = 32768"
    , Preferences
      { prefServices = []
      , prefTemplates = []
      , prefProfiles =
        [ Profile
          { profName = ProfileName "local"
          , profTrigger = TriggerNone
          , profKind = ProfService ServiceProf
            { profService      = ServiceName "ollama"
            , profModel        = ModelName "qwen3:4b"
            , profTemplate     = Nothing
            , profModelOptions = Just $ Aeson.object
              [ "temperature" .= (0.3 :: Double)
              , "num_ctx"     .= (32768 :: Int) 
              ]
            , profNumExpr      = Nothing
            , profIncludeDocs  = Nothing
            }
          }
        ]
      }
    )

  , ( "decodes preferences document with services and profiles"
    , "templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'local'\n\
      \protocol = 'ollama'\n\
      \host = 'http://127.0.0.1:11434'\n\
      \\n\
      \[[services]]\n\
      \name = 'groq'\n\
      \protocol = 'openai-compatible'\n\
      \base_url = 'https://api.groq.com/openai/v1'\n\
      \key_name = 'GROQ_API_KEY'\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'local'\n\
      \model = 'qwen3:4b'\n\
      \trigger = 'prefix:llm'\n\
      \\n\
      \[[profiles]]\n\
      \name = 'pair'\n\
      \type = 'profiles'\n\
      \profiles = ['local']"
    , Preferences
      { prefServices =
        [ Service
          { svcName = ServiceName "local"
          , svcConfig = SvcOllama $ OllamaConfig
            { svcOllamaHost = Just "http://127.0.0.1:11434"}
          }
        , Service
          { svcName = ServiceName "groq"
          , svcConfig = SvcOpenAI $ OpenAIConfig
            { svcOpenAIBaseUrl = "https://api.groq.com/openai/v1"
            , svcOpenAIKeyName = "GROQ_API_KEY"
            }
          }
        ]
      , prefProfiles =
        [ Profile
          { profName = ProfileName "local"
          , profKind = ProfService $ ServiceProf
            { profService = ServiceName "local"
            , profModel = ModelName "qwen3:4b"
            , profTemplate = Nothing
            , profModelOptions = Nothing
            , profNumExpr = Nothing
            , profIncludeDocs = Nothing
            }
          , profTrigger = TriggerPrefix "llm"
          }
        , Profile
          { profName = ProfileName {unProfileName = "pair"}
          , profKind = ProfFanout $ FanoutProf
            { profProfiles = ProfileName "local" :| []
            }
          , profTrigger = TriggerNone
          }
        ]
      , prefTemplates = []
      }
    )

  , ( "decodes empty templates list"
    , "services = []\n\
      \profiles = []\n\
      \templates = []"
    , Preferences
      { prefServices = []
      , prefProfiles = []
      , prefTemplates = []
      }
    )

  , ( "accepts missing top-level templates key"
    , "services = []\n\
      \profiles = []"
    , Preferences mempty mempty mempty
    )

  , ( "decodes single template"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = 'brief'\n\
      \body = 'Return only expressions, one per line.'"
    , Preferences
      { prefServices = []
      , prefProfiles = []
      , prefTemplates =
        [ ( expectTemplateName "brief"
          , expectTemplate "Return only expressions, one per line."
          )
        ]
      }
    )

  , ( "decodes multiple templates"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = 'brief'\n\
      \body = 'Return only expressions.'\n\
      \\n\
      \[[templates]]\n\
      \name = 'verbose'\n\
      \body = 'Explain why each expression fits.'"
    , Preferences
      { prefServices = []
      , prefProfiles = []
      , prefTemplates =
        [ ( expectTemplateName "brief"
          , expectTemplate "Return only expressions."
          )
        , ( expectTemplateName "verbose"
          , expectTemplate "Explain why each expression fits."
          )
        ]
      }
    )

  , ( "decodes multiline template body"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = 'typed-hole'\n\
      \body = '''\n\
      \You are filling a Haskell typed hole.\n\
      \\n\
      \Return candidate expressions only.\n\
      \'''"
    , Preferences
      { prefServices = []
      , prefProfiles = []
      , prefTemplates =
        [ ( expectTemplateName "typed-hole"
          , expectTemplate
              "You are filling a Haskell typed hole.\n\
              \\n\
              \Return candidate expressions only.\n"
          )
        ]
      }
    )
  ]


tests_tomlPreferences_unit_failure
  :: [(String, Text)]
tests_tomlPreferences_unit_failure =
  [ ( "rejects invalid service protocol"
    , "profiles = []\n\
      \templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'bad'\n\
      \protocol = 'wat'"
    )

  , ( "rejects openai service missing base_url"
    , "profiles = []\n\
      \templates = []\n\
      \\n\
      \[[services]]\n\
      \name = 'groq'\n\
      \protocol = 'openai'\n\
      \key_name = 'GROQ_API_KEY'"
    )

  , ( "rejects service profile with both template and template_file"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'\n\
      \template = 'default'\n\
      \template_file = '/tmp/prompt.txt'"
    )

  , ( "rejects invalid trigger policy"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:4b'\n\
      \trigger = 'prefix:_llm'"
    )

  , ( "rejects empty fanout profile list"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'pair'\n\
      \type = 'profiles'\n\
      \profiles = []"
    )

  , ( "rejects invalid profile type"
    , "services = []\n\
      \templates = []\n\
      \\n\
      \[[profiles]]\n\
      \name = 'bad'\n\
      \type = 'wat'"
    )

  , ( "rejects template missing name"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \body = 'Return only expressions.'"
    )

  , ( "rejects template missing body"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = 'brief'"
    )

  , ( "rejects non-string template name"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = 123\n\
      \body = 'Return only expressions.'"
    )

  , ( "rejects non-string template body"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = 'brief'\n\
      \body = 123"
    )

  , ( "rejects invalid template name"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = '../secrets'\n\
      \body = 'Return only expressions.'"
    )

  , ( "rejects malformed template body"
    , "services = []\n\
      \profiles = []\n\
      \\n\
      \[[templates]]\n\
      \name = 'bad'\n\
      \body = 'This has a malformed {{ placeholder.'"
    )
  ]



expectTemplateName :: Text -> TemplateName
expectTemplateName raw =
  case parseTemplateName raw of
    Right name ->
      name
    Left err ->
      error $ "invalid test template name: " <> show err

expectTemplate :: Text -> Template
expectTemplate raw =
  case parseTemplate raw of
    Right template ->
      template
    Left err ->
      error $ "invalid test template body: " <> show err
