module GHC.Plugin.OllamaHoles.Data.Profile.Parse.Spec (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Profile.Types.Gen
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Service.Types.Gen
import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Template

import Toml.TestHelper


tests :: TestTree
tests = testGroup "Profile.Parse"
  [ tests_tomlProfile_unit
  , tests_tomlProfile_prop
  ]

tests_tomlProfile_unit :: TestTree
tests_tomlProfile_unit = testGroup "tomlProfile unit"
  [ testGroup "success" $
      tests_tomlProfile_unit_success <&> \(name, input, expected) ->
        testCase name $ assertTomlParsesAs tomlProfile input expected

  , testGroup "failure" $
      tests_tomlProfile_unit_failure <&> \(name, input) ->
        testCase name $ assertTomlParseFails tomlProfile input
  ]

tests_tomlProfile_prop :: TestTree
tests_tomlProfile_prop = testGroup "tomlProfile properties"
  [ QC.testProperty "generated service profiles parse" $
    QC.forAll genProfileNameText $ \profileName ->
    QC.forAll genServiceNameText $ \serviceName ->
    QC.forAll genModelNameText $ \modelName ->
      propTomlParseSuccess tomlProfile
        ( "name = '" <> profileName <> "'\n\
          \type = 'service'\n\
          \service = '" <> serviceName <> "'\n\
          \model = '" <> modelName <> "'"
        , Profile
            { profName = ProfileName profileName
            , profKind = ProfService ServiceProf
              { profService = ServiceName serviceName
              , profModel = ModelName modelName
              , profTemplate = Nothing
              , profModelOptions = Nothing
              , profNumExpr = Nothing
              , profIncludeDocs = Nothing
              }
            , profTrigger = TriggerNone
            }
        )

  , QC.testProperty "generated service profiles with prefix triggers parse" $
    QC.forAll genProfileNameText $ \profileName ->
    QC.forAll genServiceNameText $ \serviceName ->
    QC.forAll genModelNameText $ \modelName ->
    QC.forAll genTriggerPrefixText $ \prefix ->
      propTomlParseSuccess tomlProfile
        ( "name = '" <> profileName <> "'\n\
          \type = 'service'\n\
          \trigger = 'prefix:" <> prefix <> "'\n\
          \service = '" <> serviceName <> "'\n\
          \model = '" <> modelName <> "'"
        , Profile
            { profName = ProfileName profileName
            , profKind = ProfService ServiceProf
              { profService = ServiceName serviceName
              , profModel = ModelName modelName
              , profTemplate = Nothing
              , profModelOptions = Nothing
              , profNumExpr = Nothing
              , profIncludeDocs = Nothing
              }
            , profTrigger = TriggerPrefix prefix
            }
        )

  , QC.testProperty "generated fanout profiles parse" $
    QC.forAll genProfileNameText $ \profileName ->
    QC.forAll genProfileNameText $ \childA ->
    QC.forAll genProfileNameText $ \childB ->
    QC.forAll genTriggerPrefixText $ \prefix ->
      propTomlParseSuccess tomlProfile
        ( "name = '" <> profileName <> "'\n\
          \type = 'fanout'\n\
          \trigger = 'prefix:" <> prefix <> "'\n\
          \profiles = ['" <> childA <> "', '" <> childB <> "']"
        , Profile
            { profName = ProfileName profileName
            , profKind = ProfFanout FanoutProf
              { profProfiles = ProfileName childA :| [ProfileName childB]
              }
            , profTrigger = TriggerPrefix prefix
            }
        )
  ]



tests_tomlProfile_unit_success
  :: [(String, Text, Profile)]
tests_tomlProfile_unit_success =
  [ ( "minimal service profile defaults trigger to none"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    , Profile
        { profName = ProfileName "local"
        , profKind = ProfService ServiceProf
          { profService = ServiceName "ollama"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Nothing
          , profIncludeDocs = Nothing
          }
        , profTrigger = TriggerNone
        }
    )

  , ( "service profile with prefix trigger"
    , "name = 'local'\n\
      \type = 'service'\n\
      \trigger = 'prefix:llm'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    , Profile
        { profName = ProfileName "local"
        , profKind = ProfService ServiceProf
          { profService = ServiceName "ollama"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Nothing
          , profIncludeDocs = Nothing
          }
        , profTrigger = TriggerPrefix "llm"
        }
    )

  , ( "service profile with all trigger"
    , "name = 'local'\n\
      \type = 'service'\n\
      \trigger = 'all'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    , Profile
        { profName = ProfileName "local"
        , profKind = ProfService ServiceProf
          { profService = ServiceName "ollama"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Nothing
          , profIncludeDocs = Nothing
          }
        , profTrigger = TriggerAll
        }
    )

  , ( "service profile with explicit none trigger"
    , "name = 'local'\n\
      \type = 'service'\n\
      \trigger = 'none'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    , Profile
        { profName = ProfileName "local"
        , profKind = ProfService ServiceProf
          { profService = ServiceName "ollama"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Nothing
          , profIncludeDocs = Nothing
          }
        , profTrigger = TriggerNone
        }
    )

  , ( "service profile with default template"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'\n\
      \template = 'default'"
    , Profile
      { profName = ProfileName "local"
      , profKind = ProfService ServiceProf
        { profService = ServiceName "ollama"
        , profModel = ModelName "qwen3:latest"
        , profTemplate = Just DefaultTemplate
        , profModelOptions = Nothing
        , profNumExpr = Nothing
        , profIncludeDocs = Nothing
        }
      , profTrigger = TriggerNone
      }
    )

  , ( "service profile with named template"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'\n\
      \template = 'compact'"
    , Profile
      { profName = ProfileName "local"
      , profKind = ProfService ServiceProf
        { profService = ServiceName "ollama"
        , profModel = ModelName "qwen3:latest"
        , profTemplate = Just $ NamedTemplate $
            unsafeCreateRawTemplateName "compact"
        , profModelOptions = Nothing
        , profNumExpr = Nothing
        , profIncludeDocs = Nothing
        }
      , profTrigger = TriggerNone
      }
    )

  , ( "service profile with template file"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'\n\
      \template_file = 'templates/compact.prompt'"
    , Profile
      { profName = ProfileName "local"
      , profKind = ProfService ServiceProf
        { profService = ServiceName "ollama"
        , profModel = ModelName "qwen3:latest"
        , profTemplate = Just $
            TemplateFile "templates/compact.prompt"
        , profModelOptions = Nothing
        , profNumExpr = Nothing
        , profIncludeDocs = Nothing
        }
      , profTrigger = TriggerNone
      }
    )

  , ( "service profile with options"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'\n\
      \num_expr = 7\n\
      \include_docs = true"
    , Profile
        { profName = ProfileName "local"
        , profKind = ProfService ServiceProf
          { profService = ServiceName "ollama"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Just 7
          , profIncludeDocs = Just True
          }
        , profTrigger = TriggerNone
        }
    )

  , ( "service profile with model_options"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'\n\
      \model_options = { temperature = 1, enabled = true, label = 'fast' }"
    , Profile
        { profName = ProfileName "local"
        , profKind = ProfService ServiceProf
          { profService = ServiceName "ollama"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Just $ Aeson.object
            [ "temperature" .= (1 :: Int)
            , "enabled" .= True
            , "label" .= ("fast" :: Text)
            ]
          , profNumExpr = Nothing
          , profIncludeDocs = Nothing
          }
        , profTrigger = TriggerNone
        }
    )

  , ( "fanout profile parses type fanout"
    , "name = 'both'\n\
      \type = 'fanout'\n\
      \trigger = 'prefix:llm'\n\
      \profiles = ['fast', 'careful']"
    , Profile
        { profName = ProfileName "both"
        , profKind = ProfFanout FanoutProf
          { profProfiles =  ProfileName "fast" :| [ProfileName "careful"]
          }
        , profTrigger = TriggerPrefix "llm"
        }
    )

  , ( "fanout profile parses legacy type profiles"
    , "name = 'both'\n\
      \type = 'profiles'\n\
      \trigger = 'prefix:llm'\n\
      \profiles = ['fast', 'careful']"
    , Profile
        { profName = ProfileName "both"
        , profKind = ProfFanout FanoutProf
          { profProfiles = ProfileName "fast" :| [ProfileName "careful"]
          }
        , profTrigger = TriggerPrefix "llm"
        }
    )
  ]


tests_tomlProfile_unit_failure :: [(String, Text)]
tests_tomlProfile_unit_failure =
  [ ( "missing name"
    , "type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    )

  , ( "missing type"
    , "name = 'local'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    )

  , ( "invalid type"
    , "name = 'local'\n\
      \type = 'wat'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    )

  , ( "service profile missing service"
    , "name = 'local'\n\
      \type = 'service'\n\
      \model = 'qwen3:latest'"
    )

  , ( "service profile missing model"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'"
    )

  , ( "invalid trigger"
    , "name = 'local'\n\
      \type = 'service'\n\
      \trigger = 'prefix:'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'"
    )

  , ( "invalid template name"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'\n\
      \template = '../secrets'"
    )

  , ( "template and template_file are mutually exclusive"
    , "name = 'local'\n\
      \type = 'service'\n\
      \service = 'ollama'\n\
      \model = 'qwen3:latest'\n\
      \template = 'compact'\n\
      \template_file = 'templates/compact.prompt'"
    )

  , ( "fanout profile requires non-empty profiles list"
    , "name = 'both'\n\
      \type = 'fanout'\n\
      \profiles = []"
    )

  , ( "fanout profile requires profiles field"
    , "name = 'both'\n\
      \type = 'fanout'"
    )
  ]
