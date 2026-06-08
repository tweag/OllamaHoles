module GHC.Plugin.OllamaHoles.Data.Config.Build.Spec
  ( tests
  ) where

import Control.Monad.Except ( runExceptT )
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson ( Value(..) )
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), takeDirectory, takeFileName )
import System.IO.Temp ( withSystemTempDirectory )

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Config.Build
import GHC.Plugin.OllamaHoles.Data.Config.Error
import GHC.Plugin.OllamaHoles.Data.Config.Types
import GHC.Plugin.OllamaHoles.Data.Flags.Types
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal (unsafeCreateRawTemplateName)

import GHC.Plugin.OllamaHoles.Data.Config.Types.Gen
import GHC.Plugin.OllamaHoles.Data.Config.Error.Predicate



tests :: TestTree
tests = testGroup "Config.Build"
  [ test_buildConfig_unit_basic
  , test_buildConfig_unit_validate
  , test_buildConfig_prop
  ]

test_buildConfig_unit_basic :: TestTree
test_buildConfig_unit_basic = testGroup "buildConfig (unit)"
  [ testGroup "success" $
      tests_buildConfig_unit_basic_success <&>
        \(name, flags, mConfig, expected) ->
          testCase name $ do
            result <- run_buildConfig flags mConfig
            case result of
              Left err -> assertFailure $
                "expected successful build but got this error: " <> show err
              Right actual -> actual @?= expected

  , testGroup "failure" $
      tests_buildConfig_unit_basic_failure <&>
        \(name, flags, mConfig) ->
          testCase name $ do
            result <- run_buildConfig flags mConfig
            case result of
              Left _ -> pure ()
              Right ok -> assertFailure $
                "expected failed build but got this result: " <> show ok
  ]

test_buildConfig_unit_validate :: TestTree
test_buildConfig_unit_validate = testGroup "buildConfig (validation)"
  [ testGroup "success" $
      tests_buildConfig_unit_validate_success <&>
        \(name, flags, mConfig, expected) ->
          testCase name $ do
            result <- run_buildConfig flags mConfig
            case result of
              Left err -> assertFailure $
                "expected successful build but got this error: " <> show err
              Right actual -> actual @?= expected

  , testGroup "failure" $
      tests_buildConfig_unit_validate_failure <&>
        \(name, flags, mConfig, checkFailure) ->
          testCase name $ do
            result <- run_buildConfig flags mConfig
            case result of
              Left err -> checkFailure err
              Right ok -> assertFailure $
                "expected failed build but got this result: " <> show ok
  ]

run_buildConfig
  :: (Maybe FilePath -> Flags) -> Maybe (String, Text)
  -> IO (Either ConfigError Config)
run_buildConfig flags mConfig =
  withSystemTempDirectory "config-build" $ \dir -> do
    path <- writeConfig dir mConfig
    runExceptT $ buildConfig $ flags path

writeConfig
  :: FilePath -> Maybe (String, Text) -> IO (Maybe FilePath)
writeConfig dir mConfig = case mConfig of
  Nothing -> pure Nothing
  Just (configTitle, configContent) -> do
    let cpath = dir </> configTitle
    createDirectoryIfMissing True dir
    T.writeFile cpath configContent
    pure $ Just cpath

test_buildConfig_prop :: TestTree
test_buildConfig_prop = testGroup "buildConfig (prop)"
  [ QC.testProperty "config=disabled always builds simple config" $
    QC.forAll genSimpleFlags $ \flags0 -> QC.ioProperty $ do
      result <- run_buildConfig
        (\_path -> flags0
          { config_path = Just ConfigDisabled
          })
        Nothing

      pure $ case fmap configMode result of
        Right ConfigSimple{} -> QC.property True
        other -> QC.counterexample
          ("expected ConfigSimple, got: " <> show other)
          False

  , QC.testProperty "config=disabled ignores config files" $
    QC.forAll genSimpleFlags $ \flags0 -> QC.ioProperty $ do
      result <- run_buildConfig
        (\_path -> flags0
          { config_path = Just ConfigDisabled
          })
        (Just ("config.toml", basicFancyToml))

      pure $ case fmap configMode result of
        Right ConfigSimple{} -> QC.property True
        other -> QC.counterexample
          ("expected ConfigSimple, got: " <> show other)
          False

  , QC.testProperty "explicit missing config paths are rejected" $
    QC.forAll genSafeFileName $ \fileName -> QC.ioProperty $ do
      result <- run_buildConfig
        (\path -> mempty
          { config_path = Just $ ConfigExplicit $
              case path of
                Nothing -> fileName
                Just anchorPath -> takeDirectory anchorPath </> fileName
          })
          (Just ("anchor.toml", ""))

      pure $ case result of
        Left (ConfigFileNotFound path) ->
          takeFileName path QC.=== fileName
        other -> QC.counterexample
          ("expected ConfigFileNotFound for " <> show fileName <> ", got: " <> show other)
          False

    , QC.testProperty "buildConfig succeeds for valid service-only config files" $
      QC.forAll genValidServiceOnlyConfigToml $
        \(toml, expectedServiceCount, expectedProfileCount) ->
          QC.ioProperty $ do
            result <- run_buildConfig
              (\path -> mempty
                { config_path = ConfigExplicit <$> path
                })
              (Just ("config.toml", toml))

            pure $ case fmap configMode result of
              Left err -> QC.counterexample
                ("unexpected error: " <> show err <> "\n\nTOML:\n" <> T.unpack toml)
                False
              Right (ConfigFancy cfg) ->
                  M.size (cfgServices cfg) QC.=== expectedServiceCount
                    QC..&&.
                  M.size (cfgProfiles cfg) QC.=== expectedProfileCount
              Right other -> QC.counterexample
                ("expected ConfigFancy, got: " <> show other)
                False

    , QC.testProperty "buildConfig rejects missing service references" $
      QC.forAll genMissingServiceConfigToml $
        \(toml, missingProfile, missingService) ->
          QC.ioProperty $ do
            result <- run_buildConfig
              (\path -> mempty
                { config_path = ConfigExplicit <$> path
                })
              (Just ("config.toml", toml))

            pure $ case result of
              Left (UnknownServiceReference prof service) ->
                prof QC.=== missingProfile
                  QC..&&.
                service QC.=== missingService
              other -> QC.counterexample
                ("expected UnknownServiceReference, got: " <> show other <> "\n\nTOML:\n" <> T.unpack toml)
                False

    , QC.testProperty "buildConfig rejects self-cycles" $
      QC.forAll genSelfCycleConfigToml $ \(toml, cyclicProfile) ->
        QC.ioProperty $ do
          result <- run_buildConfig
            (\path -> mempty
              { config_path = ConfigExplicit <$> path
              })
            (Just ("config.toml", toml))

          pure $ case result of
            Left (CyclicProfileReference cyclePath) -> QC.counterexample
              ("cycle path did not mention expected profile: " <> show cyclePath)
              (cyclicProfile `elem` cyclePath)
            other -> QC.counterexample
              ("expected CyclicProfileReference, got: " <> show other <> "\n\nTOML:\n" <> T.unpack toml)
              False
  ]



tests_buildConfig_unit_basic_success
  :: [(TestName, Maybe FilePath -> Flags, Maybe (String, Text), Config)]
tests_buildConfig_unit_basic_success =
  [ ( "config=disabled builds simple config with defaults"
    , \_path -> mempty
        { config_path = Just ConfigDisabled
        }
    , Nothing
    , defaultConfigOfMode $ ConfigSimple $ SimpleConfig
        { simpleTrigger = defaultTriggerPolicy
        , simpleService = Service
          { svcName = ServiceName "__simple__"
          , svcConfig = SvcOllama (OllamaConfig Nothing)
          }
        , simpleProfile = ServiceProf
          { profService = ServiceName "__simple__"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Just 5
          , profIncludeDocs = Just False
          }
        }
    )

  , ( "simple config honors command-line service/profile flags"
    , \_path -> mempty
        { config_path = Just ConfigDisabled
        , backend_name = Just OpenAI
        , openai_base_url = Just "https://example.invalid/v1"
        , openai_key_name = Just "TEST_API_KEY"
        , model_name = Just "gpt-test"
        , trigger_policy = Just TriggerAll
        , num_expr = Just 12
        , include_docs = Just True
        , model_options = Just (String "opts")
        , template_path = Just "prompt.txt"
        }
    , Nothing
    , defaultConfigOfMode $ ConfigSimple SimpleConfig
        { simpleTrigger = TriggerAll
        , simpleService = Service
          { svcName = ServiceName "__simple__"
          , svcConfig = SvcOpenAI $ OpenAIConfig
              "https://example.invalid/v1"
              "TEST_API_KEY"
          }
        , simpleProfile = ServiceProf
          { profService = ServiceName "__simple__"
          , profModel = ModelName "gpt-test"
          , profTemplate = Just (TemplateFile "prompt.txt")
          , profModelOptions = Just (String "opts")
          , profNumExpr = Just 12
          , profIncludeDocs = Just True
          }
        }
    )

  , ( "explicit config file builds fancy config with override extras by default"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
              { profName = ProfileName "p"
              , profTrigger = TriggerPrefix "llm"
              , profKind = ProfService ServiceProf
                { profService = ServiceName "ollama"
                , profModel = ModelName "qwen3:latest"
                , profTemplate = Nothing
                , profModelOptions = Nothing
                , profNumExpr = Nothing
                , profIncludeDocs = Nothing
                }
              }
            )
          ]
        , cfgExtras =
            Just (ConfigOverride emptyOverrideConfig)
        }
    )

  , ( "fancy config uses ConfigOverride for non-overlay command-line flags"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , model_name = Just "override-model"
        , num_expr = Just 3
        , include_docs = Just True
        , model_options = Just (String "override-options")
        , template_name = Just $ unsafeCreateRawTemplateName "compact"
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
          ( ConfigOverride OverrideConfig
              { overrideModelName = Just (ModelName "override-model")
              , overrideNumExpr = Just 3
              , overrideIncludeDocs = Just True
              , overrideModelOptions = Just (String "override-options")
              , overrideTemplate = Just (NamedTemplate $ unsafeCreateRawTemplateName "compact")
              }
          )
        }
    )

  , ( "fancy config creates overlay when backend-ish flags are set"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , backend_name = Just OpenAI
        , openai_base_url = Just "https://example.invalid/v1"
        , openai_key_name = Just "TEST_API_KEY"
        , model_name = Just "overlay-model"
        , trigger_policy = Just (TriggerPrefix "ask")
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          , ( ServiceName "__simple__"
            , Service
                { svcName = ServiceName "__simple__"
                , svcConfig = SvcOpenAI $ OpenAIConfig
                    "https://example.invalid/v1"
                    "TEST_API_KEY"
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
            ( ConfigOverlay SimpleConfig
                { simpleTrigger = TriggerPrefix "ask"
                , simpleService = Service
                  { svcName = ServiceName "__simple__"
                  , svcConfig = SvcOpenAI $ OpenAIConfig
                      "https://example.invalid/v1"
                      "TEST_API_KEY"
                  }
                , simpleProfile = ServiceProf
                  { profService = ServiceName "__simple__"
                  , profModel = ModelName "overlay-model"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Just 5
                  , profIncludeDocs = Just False
                  }
                }
            )
        }
    )

  , ( "config=disabled ignores available config file"
    , \_path -> mempty
        { config_path = Just ConfigDisabled
        }
    , Just ("config.toml", basicFancyToml)
    , defaultConfigOfMode $ ConfigSimple SimpleConfig
        { simpleTrigger = defaultTriggerPolicy
        , simpleService = Service
          { svcName = ServiceName "__simple__"
          , svcConfig = SvcOllama (OllamaConfig Nothing)
          }
        , simpleProfile = ServiceProf
          { profService = ServiceName "__simple__"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Just 5
          , profIncludeDocs = Just False
          }
        }
    )

  , ( "missing default config path builds simple config"
    , \_path -> mempty
        { config_path = Nothing
        }
    , Nothing
    , defaultConfigOfMode $ ConfigSimple SimpleConfig
        { simpleTrigger = defaultTriggerPolicy
        , simpleService = Service
          { svcName = ServiceName "__simple__"
          , svcConfig = SvcOllama (OllamaConfig Nothing)
          }
        , simpleProfile = ServiceProf
          { profService = ServiceName "__simple__"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Just 5
          , profIncludeDocs = Just False
          }
        }
    )

  , ( "default config path uses config file when present"
    , \path -> mempty
        { config_path =
            case path of
              Nothing -> Nothing
              Just p  -> Just (ConfigExplicit p)
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just (ConfigOverride emptyOverrideConfig)
        }
    )

  , ( "simple config uses template path over template name"
    , \_path -> mempty
        { config_path = Just ConfigDisabled
        , template_path = Just "prompt.txt"
        , template_name = Just $ unsafeCreateRawTemplateName "compact"
        }
    , Nothing
    , defaultConfigOfMode $ ConfigSimple SimpleConfig
        { simpleTrigger = defaultTriggerPolicy
        , simpleService = Service
          { svcName = ServiceName "__simple__"
          , svcConfig = SvcOllama (OllamaConfig Nothing)
          }
        , simpleProfile = ServiceProf
          { profService = ServiceName "__simple__"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Just (TemplateFile "prompt.txt")
          , profModelOptions = Nothing
          , profNumExpr = Just 5
          , profIncludeDocs = Just False
          }
        }
    )

  , ( "simple config uses template name when template path is absent"
    , \_path -> mempty
        { config_path = Just ConfigDisabled
        , template_name = Just $ unsafeCreateRawTemplateName "compact"
        }
    , Nothing
    , defaultConfigOfMode $ ConfigSimple SimpleConfig
        { simpleTrigger = defaultTriggerPolicy
        , simpleService = Service
          { svcName = ServiceName "__simple__"
          , svcConfig = SvcOllama (OllamaConfig Nothing)
          }
        , simpleProfile = ServiceProf
          { profService = ServiceName "__simple__"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Just (NamedTemplate $ unsafeCreateRawTemplateName "compact")
          , profModelOptions = Nothing
          , profNumExpr = Just 5
          , profIncludeDocs = Just False
          }
        }
    )

  , ( "fancy config override uses template path over template name"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , template_path = Just "prompt.txt"
        , template_name = Just $ unsafeCreateRawTemplateName "compact"
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
            ( ConfigOverride OverrideConfig
                { overrideModelName = Nothing
                , overrideNumExpr = Nothing
                , overrideIncludeDocs = Nothing
                , overrideModelOptions = Nothing
                , overrideTemplate = Just (TemplateFile "prompt.txt")
                }
            )
        }
    )

  , ( "fancy config override uses template name when template path is absent"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , template_name = Just $ unsafeCreateRawTemplateName "compact"
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
            ( ConfigOverride OverrideConfig
                { overrideModelName = Nothing
                , overrideNumExpr = Nothing
                , overrideIncludeDocs = Nothing
                , overrideModelOptions = Nothing
                , overrideTemplate = Just (NamedTemplate $ unsafeCreateRawTemplateName "compact")
                }
            )
        }
    )

  , ( "fancy config overlay uses template path over template name"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , backend_name = Just OpenAI
        , openai_base_url = Just "https://example.invalid/v1"
        , openai_key_name = Just "TEST_API_KEY"
        , template_path = Just "prompt.txt"
        , template_name = Just $ unsafeCreateRawTemplateName "compact"
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          , ( ServiceName "__simple__"
            , Service
                { svcName = ServiceName "__simple__"
                , svcConfig = SvcOpenAI $ OpenAIConfig
                    "https://example.invalid/v1"
                    "TEST_API_KEY"
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
            ( ConfigOverlay SimpleConfig
                { simpleTrigger = defaultTriggerPolicy
                , simpleService = Service
                  { svcName = ServiceName "__simple__"
                  , svcConfig = SvcOpenAI $ OpenAIConfig
                      "https://example.invalid/v1"
                      "TEST_API_KEY"
                  }
                , simpleProfile = ServiceProf
                  { profService = ServiceName "__simple__"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Just (TemplateFile "prompt.txt")
                  , profModelOptions = Nothing
                  , profNumExpr = Just 5
                  , profIncludeDocs = Just False
                  }
                }
            )
        }
    )

  , ( "fancy config overlay uses template name when template path is absent"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , backend_name = Just OpenAI
        , openai_base_url = Just "https://example.invalid/v1"
        , openai_key_name = Just "TEST_API_KEY"
        , template_name = Just $ unsafeCreateRawTemplateName "compact"
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          , ( ServiceName "__simple__"
            , Service
                { svcName = ServiceName "__simple__"
                , svcConfig = SvcOpenAI $ OpenAIConfig
                    "https://example.invalid/v1"
                    "TEST_API_KEY"
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
            ( ConfigOverlay SimpleConfig
                { simpleTrigger = defaultTriggerPolicy
                , simpleService = Service
                  { svcName = ServiceName "__simple__"
                  , svcConfig = SvcOpenAI $ OpenAIConfig
                      "https://example.invalid/v1"
                      "TEST_API_KEY"
                  }
                , simpleProfile = ServiceProf
                  { profService = ServiceName "__simple__"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Just (NamedTemplate $ unsafeCreateRawTemplateName "compact")
                  , profModelOptions = Nothing
                  , profNumExpr = Just 5
                  , profIncludeDocs = Just False
                  }
                }
            )
        }
    )

  , ( "openai overlay uses default base url"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , backend_name = Just OpenAI
        , openai_key_name = Just "TEST_API_KEY"
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          , ( ServiceName "__simple__"
            , Service
                { svcName = ServiceName "__simple__"
                , svcConfig = SvcOpenAI $ OpenAIConfig
                    "https://api.openai.com"
                    "TEST_API_KEY"
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
            ( ConfigOverlay SimpleConfig
                { simpleTrigger = defaultTriggerPolicy
                , simpleService = Service
                  { svcName = ServiceName "__simple__"
                  , svcConfig = SvcOpenAI $ OpenAIConfig
                      "https://api.openai.com"
                      "TEST_API_KEY"
                  }
                , simpleProfile = ServiceProf
                  { profService = ServiceName "__simple__"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Just 5
                  , profIncludeDocs = Just False
                  }
                }
            )
        }
    )

  , ( "openai overlay uses default key name"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        , backend_name = Just OpenAI
        , openai_base_url = Just "https://example.invalid/v1"
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          , ( ServiceName "__simple__"
            , Service
                { svcName = ServiceName "__simple__"
                , svcConfig = SvcOpenAI $ OpenAIConfig
                    "https://example.invalid/v1"
                    "OPENAI_API_KEY"
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerPrefix "llm"
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just
            ( ConfigOverlay SimpleConfig
                { simpleTrigger = defaultTriggerPolicy
                , simpleService = Service
                  { svcName = ServiceName "__simple__"
                  , svcConfig = SvcOpenAI $ OpenAIConfig
                      "https://example.invalid/v1"
                      "OPENAI_API_KEY"
                  }
                , simpleProfile = ServiceProf
                  { profService = ServiceName "__simple__"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Just 5
                  , profIncludeDocs = Just False
                  }
                }
            )
        }
    )

  , ( "valid toml with empty config semantics builds empty fancy config"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = []\n\
        \profiles = []"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList []
        , cfgProfiles = M.fromList []
        , cfgExtras = Just
            ( ConfigOverride OverrideConfig
                { overrideModelName = Nothing
                , overrideNumExpr = Nothing
                , overrideIncludeDocs = Nothing
                , overrideModelOptions = Nothing
                , overrideTemplate = Nothing
                }
            )
        }
    )

  , ( "debug flag false becomes configDebug false"
    , \_path -> mempty
        { debug = Just False
        , config_path = Just ConfigDisabled
        }
    , Nothing
    , Config
        { configDebug = False
        , configTemplateSearchDir = "."
        , configMode = ConfigSimple SimpleConfig
          { simpleTrigger = defaultTriggerPolicy
          , simpleService = Service
            { svcName = ServiceName "__simple__"
            , svcConfig = SvcOllama (OllamaConfig Nothing)
            }
          , simpleProfile = ServiceProf
            { profService = ServiceName "__simple__"
            , profModel = ModelName "qwen3:latest"
            , profTemplate = Nothing
            , profModelOptions = Nothing
            , profNumExpr = Just 5
            , profIncludeDocs = Just False
            }
          }
        }
    )

  , ( "debug flag true becomes configDebug true"
    , \_path -> mempty
        { debug = Just True
        , config_path = Just ConfigDisabled
        }
    , Nothing
    , Config
        { configDebug = True
        , configTemplateSearchDir = "."
        , configMode = ConfigSimple SimpleConfig
          { simpleTrigger = defaultTriggerPolicy
          , simpleService = Service
            { svcName = ServiceName "__simple__"
            , svcConfig = SvcOllama (OllamaConfig Nothing)
            }
          , simpleProfile = ServiceProf
            { profService = ServiceName "__simple__"
            , profModel = ModelName "qwen3:latest"
            , profTemplate = Nothing
            , profModelOptions = Nothing
            , profNumExpr = Just 5
            , profIncludeDocs = Just False
            }
          }
        }
    )

  , ( "missing template search dir flag defaults to current directory"
    , \_path -> mempty
        { config_path = Just ConfigDisabled
        }
    , Nothing
    , Config
        { configDebug = False
        , configTemplateSearchDir = "."
        , configMode = ConfigSimple SimpleConfig
          { simpleTrigger = defaultTriggerPolicy
          , simpleService = Service
            { svcName = ServiceName "__simple__"
            , svcConfig = SvcOllama (OllamaConfig Nothing)
            }
          , simpleProfile = ServiceProf
            { profService = ServiceName "__simple__"
            , profModel = ModelName "qwen3:latest"
            , profTemplate = Nothing
            , profModelOptions = Nothing
            , profNumExpr = Just 5
            , profIncludeDocs = Just False
            }
          }
        }
    )

  , ( "template search dir flag becomes configTemplateSearchDir"
    , \_path -> mempty
        { config_path = Just ConfigDisabled
        , template_search_dir = Just "templates"
        }
    , Nothing
    , Config
        { configDebug = False
        , configTemplateSearchDir = "templates"
        , configMode = ConfigSimple SimpleConfig
          { simpleTrigger = defaultTriggerPolicy
          , simpleService = Service
            { svcName = ServiceName "__simple__"
            , svcConfig = SvcOllama (OllamaConfig Nothing)
            }
          , simpleProfile = ServiceProf
            { profService = ServiceName "__simple__"
            , profModel = ModelName "qwen3:latest"
            , profTemplate = Nothing
            , profModelOptions = Nothing
            , profNumExpr = Just 5
            , profIncludeDocs = Just False
            }
          }
        }
    )
  ]

tests_buildConfig_unit_basic_failure
  :: [(TestName, Maybe FilePath -> Flags, Maybe (String, Text))]
tests_buildConfig_unit_basic_failure =
  [ ( "explicit missing config file is an error"
    , \path -> mempty
        { config_path = Just $ ConfigExplicit $
            case path of
              Nothing -> "missing.toml"
              Just anchorPath -> takeDirectory anchorPath </> "missing.toml"
        }
    , Just ("anchor.toml", "")
    )

  , ( "fancy config reports parse errors"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
        ( "bad.toml"
        , "not valid toml = [\n"
        )
    )

  , ( "empty toml is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , ""
      )
    )

  , ( "valid toml with profile referencing missing service is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'missing', model = 'qwen3:latest' }\n\
        \]"
      )
    )

  , ( "valid toml with unknown service protocol is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'bad', protocol = 'not-a-backend' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'bad', model = 'qwen3:latest' }\n\
        \]"
      )
    )

  , ( "valid toml with invalid trigger policy is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    )

  , ( "valid toml with invalid template name is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \templates = [\n\
        \  { name = '../bad', path = 'prompt.txt' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', trigger = 'prefix:llm', service = 'ollama', model = 'qwen3:latest', template = '../bad' }\n\
        \]"
      )
    )
  ]

tests_buildConfig_unit_validate_success
  :: [(TestName, Maybe FilePath -> Flags, Maybe (String, Text), Config)]
tests_buildConfig_unit_validate_success =
  [ ( "fanout profiles are flattened to service leaves"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , nestedFanoutToml
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "a"
            , Profile
                { profName = ProfileName "a"
                , profTrigger = TriggerNone
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "ma"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          , ( ProfileName "b"
            , Profile
                { profName = ProfileName "b"
                , profTrigger = TriggerNone
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "mb"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          , ( ProfileName "mid"
            , Profile
                { profName = ProfileName "mid"
                , profTrigger = TriggerPrefix "mid"
                , profKind = ProfFanout $
                    FanoutProf (ProfileName "a" :| [ProfileName "b"])
                }
            )
          , ( ProfileName "top"
            , Profile
                { profName = ProfileName "top"
                , profTrigger = TriggerPrefix "top"
                , profKind = ProfFanout $
                    FanoutProf (ProfileName "a" :| [ProfileName "b"])
                }
            )
          ]
        , cfgExtras = Just (ConfigOverride emptyOverrideConfig)
        }
    )

  , ( "config accepts distinct services"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' },\n\
        \  { name = 'openai', protocol = 'openai', base_url = 'https://example.invalid/v1', key_name = 'TEST_API_KEY' }\n\
        \]\n\
        \\n\
        \profiles = []"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          , ( ServiceName "openai"
            , Service
                { svcName = ServiceName "openai"
                , svcConfig = SvcOpenAI $ OpenAIConfig
                    "https://example.invalid/v1"
                    "TEST_API_KEY"
                }
            )
          ]
        , cfgProfiles = M.fromList []
        , cfgExtras = Just (ConfigOverride emptyOverrideConfig)
        }
    )

  , ( "config accepts service profile with known service"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "ollama"
            , Service
                { svcName = ServiceName "ollama"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "p"
            , Profile
                { profName = ProfileName "p"
                , profTrigger = TriggerNone
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "ollama"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          ]
        , cfgExtras = Just (ConfigOverride emptyOverrideConfig)
        }
    )

  , ( "nested fanout preserves flattened leaf order"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'local', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'leaf1', type = 'service', service = 'local', model = 'qwen3:latest' },\n\
        \  { name = 'leaf2', type = 'service', service = 'local', model = 'qwen3:latest' },\n\
        \  { name = 'inner', type = 'fanout', profiles = ['leaf1', 'leaf2'] },\n\
        \  { name = 'outer', type = 'fanout', profiles = ['inner', 'leaf1'] }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "local"
            , Service
                { svcName = ServiceName "local"
                , svcConfig = SvcOllama (OllamaConfig Nothing)
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "inner"
            , Profile
                { profName = ProfileName "inner"
                , profTrigger = TriggerNone
                , profKind = ProfFanout $
                    FanoutProf
                      (ProfileName "leaf1" :| [ProfileName "leaf2"])
                }
            )
          , ( ProfileName "leaf1"
            , Profile
                { profName = ProfileName "leaf1"
                , profTrigger = TriggerNone
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "local"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          , ( ProfileName "leaf2"
            , Profile
                { profName = ProfileName "leaf2"
                , profTrigger = TriggerNone
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "local"
                  , profModel = ModelName "qwen3:latest"
                  , profTemplate = Nothing
                  , profModelOptions = Nothing
                  , profNumExpr = Nothing
                  , profIncludeDocs = Nothing
                  }
                }
            )
          , ( ProfileName "outer"
            , Profile
                { profName = ProfileName "outer"
                , profTrigger = TriggerNone
                , profKind = ProfFanout $
                    FanoutProf
                      ( ProfileName "leaf1"
                      :| [ ProfileName "leaf2"
                         , ProfileName "leaf1"
                         ]
                      )
                }
            )
          ]
        , cfgExtras = Just (ConfigOverride emptyOverrideConfig)
        }
    )

  , ( "service profile fields are preserved by validation"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'remote', protocol = 'openai', base_url = 'https://example.invalid/v1', key_name = 'TEST_API_KEY' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'rich', type = 'service', trigger = 'all', service = 'remote', model = 'gpt-test', template = 'prompt', num_expr = 17, include_docs = true }\n\
        \]"
      )
    , defaultConfigOfMode $ ConfigFancy FancyConfig
        { cfgServices = M.fromList
          [ ( ServiceName "remote"
            , Service
                { svcName = ServiceName "remote"
                , svcConfig = SvcOpenAI $ OpenAIConfig
                    "https://example.invalid/v1"
                    "TEST_API_KEY"
                }
            )
          ]
        , cfgProfiles = M.fromList
          [ ( ProfileName "rich"
            , Profile
                { profName = ProfileName "rich"
                , profTrigger = TriggerAll
                , profKind = ProfService ServiceProf
                  { profService = ServiceName "remote"
                  , profModel = ModelName "gpt-test"
                  , profTemplate = Just (NamedTemplate (unsafeCreateRawTemplateName "prompt"))
                  , profModelOptions = Nothing
                  , profNumExpr = Just 17
                  , profIncludeDocs = Just True
                  }
                }
            )
          ]
        , cfgExtras = Just (ConfigOverride emptyOverrideConfig)
        }
    )
  ]

tests_buildConfig_unit_validate_failure
  :: [(TestName, Maybe FilePath -> Flags, Maybe (String, Text), ConfigError -> Assertion)]
tests_buildConfig_unit_validate_failure =
  [ ( "duplicate service names are rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , duplicateServicesToml
      )
    , \err ->
        err @?= DuplicateServiceName (ServiceName "ollama")
    )

  , ( "duplicate profile names are rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , duplicateProfilesToml
      )
    , \err ->
        err @?= DuplicateProfileName (ProfileName "p")
    )

  , ( "unknown service reference is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , unknownServiceToml
      )
    , \err ->
        err @?= UnknownServiceReference
          (ProfileName "p")
          (ServiceName "missing")
    )

  , ( "unknown fanout profile reference is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , unknownFanoutProfileToml
      )
    , \err ->
        err @?= UnknownProfileReference
          (ProfileName "fan")
          (ProfileName "missing")
    )

  , ( "cyclic fanout profile reference is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , cyclicFanoutToml
      )
    , \err ->
        assertBool
          ("expected cyclic profile error, got: " <> show err)
          (isCyclicProfileError (Left err))
    )

  , ( "ambiguous profile triggers are rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , ambiguousTriggersToml
      )
    , \err ->
        assertBool
          ("expected ambiguous trigger error, got: " <> show err)
          (isAmbiguousTriggerError (Left err))
    )

  , ( "config rejects duplicate services"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'ollama', protocol = 'ollama' },\n\
        \  { name = 'ollama', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = []"
      )
    , \err ->
        err @?= DuplicateServiceName (ServiceName "ollama")
    )

  , ( "config rejects service profile with unknown service"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = []\n\
        \profiles = [\n\
        \  { name = 'p', type = 'service', service = 'ollama', model = 'qwen3:latest' }\n\
        \]"
      )
    , \err ->
        err @?= UnknownServiceReference
          (ProfileName "p")
          (ServiceName "ollama")
    )

  , ( "self-cycle in fanout profile is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'local', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'leaf', type = 'service', service = 'local', model = 'qwen3:latest' },\n\
        \  { name = 'self', type = 'fanout', profiles = ['self'] }\n\
        \]"
      )
    , \err ->
        err @?= CyclicProfileReference
          [ ProfileName "self"
          , ProfileName "self"
          ]
    )

  , ( "mutual cycle between fanout profiles is rejected"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'local', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'leaf', type = 'service', service = 'local', model = 'qwen3:latest' },\n\
        \  { name = 'a', type = 'fanout', profiles = ['b'] },\n\
        \  { name = 'b', type = 'fanout', profiles = ['a'] }\n\
        \]"
      )
    , \err ->
        err @?= CyclicProfileReference
          [ ProfileName "a"
          , ProfileName "b"
          , ProfileName "a"
          ]
    )

  , ( "fanout validates transitive service dependencies"
    , \path -> mempty
        { config_path = ConfigExplicit <$> path
        }
    , Just
      ( "config.toml"
      , "services = [\n\
        \  { name = 'local', protocol = 'ollama' }\n\
        \]\n\
        \\n\
        \profiles = [\n\
        \  { name = 'good', type = 'service', service = 'local', model = 'qwen3:latest' },\n\
        \  { name = 'bad', type = 'service', service = 'missing', model = 'qwen3:latest' },\n\
        \  { name = 'pair', type = 'fanout', profiles = ['good', 'bad'] }\n\
        \]"
      )
    , \err ->
        err @?= UnknownServiceReference
          (ProfileName "bad")
          (ServiceName "missing")
    )
  ]





-- TOML fixtures
----------------

basicFancyToml :: Text
basicFancyToml =
  T.unlines
    [ "services = ["
    , "  { name = \"ollama\", protocol = \"ollama\" }"
    , "]"
    , ""
    , "profiles = ["
    , "  { name = \"p\", type = \"service\", trigger = \"prefix:llm\", service = \"ollama\", model = \"qwen3:latest\" }"
    , "]"
    ]


duplicateServicesToml :: Text
duplicateServicesToml =
  T.unlines
    [ "services = ["
    , "  { name = \"ollama\", protocol = \"ollama\" },"
    , "  { name = \"ollama\", protocol = \"ollama\" }"
    , "]"
    , ""
    , "profiles = []"
    ]


duplicateProfilesToml :: Text
duplicateProfilesToml =
  T.unlines
    [ "services = ["
    , "  { name = \"ollama\", protocol = \"ollama\" }"
    , "]"
    , ""
    , "profiles = ["
    , "  { name = \"p\", type = \"service\", service = \"ollama\", model = \"m1\" },"
    , "  { name = \"p\", type = \"service\", service = \"ollama\", model = \"m2\" }"
    , "]"
    ]


unknownServiceToml :: Text
unknownServiceToml =
  T.unlines
    [ "services = []"
    , ""
    , "profiles = ["
    , "  { name = \"p\", type = \"service\", service = \"missing\", model = \"m\" }"
    , "]"
    ]


unknownFanoutProfileToml :: Text
unknownFanoutProfileToml =
  T.unlines
    [ "services = ["
    , "  { name = \"ollama\", protocol = \"ollama\" }"
    , "]"
    , ""
    , "profiles = ["
    , "  { name = \"fan\", type = \"fanout\", trigger = \"prefix:fan\", profiles = [\"missing\"] }"
    , "]"
    ]


cyclicFanoutToml :: Text
cyclicFanoutToml =
  T.unlines
    [ "services = ["
    , "  { name = \"ollama\", protocol = \"ollama\" }"
    , "]"
    , ""
    , "profiles = ["
    , "  { name = \"a\", type = \"fanout\", trigger = \"prefix:a\", profiles = [\"b\"] },"
    , "  { name = \"b\", type = \"fanout\", trigger = \"prefix:b\", profiles = [\"a\"] }"
    , "]"
    ]


nestedFanoutToml :: Text
nestedFanoutToml =
  T.unlines
    [ "services = ["
    , "  { name = \"ollama\", protocol = \"ollama\" }"
    , "]"
    , ""
    , "profiles = ["
    , "  { name = \"a\", type = \"service\", service = \"ollama\", model = \"ma\" },"
    , "  { name = \"b\", type = \"service\", service = \"ollama\", model = \"mb\" },"
    , "  { name = \"mid\", type = \"fanout\", trigger = \"prefix:mid\", profiles = [\"a\", \"b\"] },"
    , "  { name = \"top\", type = \"fanout\", trigger = \"prefix:top\", profiles = [\"mid\"] }"
    , "]"
    ]


ambiguousTriggersToml :: Text
ambiguousTriggersToml =
  T.unlines
    [ "services = ["
    , "  { name = \"ollama\", protocol = \"ollama\" }"
    , "]"
    , ""
    , "profiles = ["
    , "  { name = \"a\", type = \"service\", trigger = \"prefix:llm\", service = \"ollama\", model = \"ma\" },"
    , "  { name = \"b\", type = \"service\", trigger = \"prefix:llm\", service = \"ollama\", model = \"mb\" }"
    , "]"
    ]


-- Shared fixtures
------------------

openAIService :: Service
openAIService =
  Service
    { svcName = ServiceName "openai"
    , svcConfig =
        SvcOpenAI $
          OpenAIConfig
            "https://api.openai.com"
            "OPENAI_API_KEY"
    }


basicProfile :: Profile
basicProfile =
  Profile
    { profName = ProfileName "p"
    , profTrigger = TriggerPrefix "llm"
    , profKind =
        ProfService ServiceProf
          { profService = ServiceName "ollama"
          , profModel = ModelName "qwen3:latest"
          , profTemplate = Nothing
          , profModelOptions = Nothing
          , profNumExpr = Nothing
          , profIncludeDocs = Nothing
          }
    }


emptyOverrideConfig :: OverrideConfig
emptyOverrideConfig =
  OverrideConfig
    { overrideModelName = Nothing
    , overrideNumExpr = Nothing
    , overrideIncludeDocs = Nothing
    , overrideModelOptions = Nothing
    , overrideTemplate = Nothing
    }
