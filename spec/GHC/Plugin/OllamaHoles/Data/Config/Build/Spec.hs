module GHC.Plugin.OllamaHoles.Data.Config.Build.Spec
  ( tests
  ) where

import Control.Monad.Except
  ( runExceptT )

import Data.Aeson
  ( Value(..) )

import Data.Either
  ( isLeft )

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
  ( createDirectoryIfMissing )

import System.FilePath
  ( (</>) )

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import System.IO.Temp
  ( withSystemTempDirectory )

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Config.Build
import GHC.Plugin.OllamaHoles.Data.Config.Error
import GHC.Plugin.OllamaHoles.Data.Config.Types
import GHC.Plugin.OllamaHoles.Data.Flags.Types
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Template



tests :: TestTree
tests =
  testGroup "GHC.Plugin.OllamaHoles.Data.Config.Build"
    [ simpleConfigTests
    , fancyConfigTests
    , validationTests
    , mapBuilderTests
    , propertyTests
    ]


-- Simple config
----------------

simpleConfigTests :: TestTree
simpleConfigTests =
  testGroup "simple config"
    [ testCase "config=disabled builds simple config with defaults" $ do
        result <-
          runBuild mempty
            { config_path = Just ConfigDisabled
            }

        case result of
          Left err ->
            assertFailure ("unexpected config error: " <> show err)

          Right (ConfigSimple cfg) -> do
            simpleTrigger cfg @?= defaultTriggerPolicy

            simpleService cfg @?= Service
              { svcName = ServiceName "__simple__"
              , svcConfig = SvcOllama (OllamaConfig Nothing)
              }

            simpleProfile cfg @?= ServiceProf
              { profService = ServiceName "__simple__"
              , profModel = ModelName "qwen3:latest"
              , profTemplate = Nothing
              , profModelOptions = Nothing
              , profNumExpr = Just 5
              , profIncludeDocs = Just False
              }

          Right other ->
            assertFailure ("expected ConfigSimple, got: " <> show other)

    , testCase "simple config honors command-line service/profile flags" $ do
        result <-
          runBuild mempty
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

        case result of
          Left err ->
            assertFailure ("unexpected config error: " <> show err)

          Right (ConfigSimple cfg) -> do
            simpleTrigger cfg @?= TriggerAll

            simpleService cfg @?= Service
              { svcName = ServiceName "__simple__"
              , svcConfig =
                  SvcOpenAI $
                    OpenAIConfig
                      "https://example.invalid/v1"
                      "TEST_API_KEY"
              }

            simpleProfile cfg @?= ServiceProf
              { profService = ServiceName "__simple__"
              , profModel = ModelName "gpt-test"
              , profTemplate = Just (TemplateFile "prompt.txt")
              , profModelOptions = Just (String "opts")
              , profNumExpr = Just 12
              , profIncludeDocs = Just True
              }

          Right other ->
            assertFailure ("expected ConfigSimple, got: " <> show other)

    , testCase "explicit missing config file is an error" $
        withSystemTempDirectory "ollama-holes-config-build" $ \dir -> do
          let path =
                dir </> "missing.toml"

          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          result @?= Left (ConfigFileNotFound path)
    ]


-- Fancy config
---------------

fancyConfigTests :: TestTree
fancyConfigTests =
  testGroup "fancy config"
    [ testCase "explicit config file builds fancy config with override extras by default" $
        withConfigFile basicFancyToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          case result of
            Left err ->
              assertFailure ("unexpected config error: " <> show err)

            Right (ConfigFancy cfg) -> do
              M.keys (cfgServices cfg) @?= [ServiceName "ollama"]

              M.keys (cfgProfiles cfg) @?= [ProfileName "p"]

              cfgExtras cfg @?= Just
                (ConfigOverride emptyOverrideConfig)

              M.lookup (ProfileName "p") (cfgProfiles cfg)
                @?= Just
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

            Right other ->
              assertFailure ("expected ConfigFancy, got: " <> show other)

    , testCase "fancy config uses ConfigOverride for non-overlay command-line flags" $
        withConfigFile basicFancyToml $ \path -> do
          templateName <-
            assertRight $
              parseTemplateName "compact"

          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              , model_name = Just "override-model"
              , num_expr = Just 3
              , include_docs = Just True
              , model_options = Just (String "override-options")
              , template_name = Just templateName
              }

          case result of
            Left err ->
              assertFailure ("unexpected config error: " <> show err)

            Right (ConfigFancy cfg) ->
              cfgExtras cfg @?= Just
                ( ConfigOverride OverrideConfig
                    { overrideModelName = Just (ModelName "override-model")
                    , overrideNumExpr = Just 3
                    , overrideIncludeDocs = Just True
                    , overrideModelOptions = Just (String "override-options")
                    , overrideTemplate = Just (NamedTemplate templateName)
                    }
                )

            Right other ->
              assertFailure ("expected ConfigFancy, got: " <> show other)

    , testCase "fancy config creates overlay when backend-ish flags are set" $
        withConfigFile basicFancyToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              , backend_name = Just OpenAI
              , openai_base_url = Just "https://example.invalid/v1"
              , openai_key_name = Just "TEST_API_KEY"
              , model_name = Just "overlay-model"
              , trigger_policy = Just (TriggerPrefix "ask")
              }

          case result of
            Left err ->
              assertFailure ("unexpected config error: " <> show err)

            Right (ConfigFancy cfg) -> do
              M.member (ServiceName "__simple__") (cfgServices cfg)
                @?= True

              cfgExtras cfg @?= Just
                ( ConfigOverlay SimpleConfig
                    { simpleTrigger = TriggerPrefix "ask"
                    , simpleService =
                        Service
                          { svcName = ServiceName "__simple__"
                          , svcConfig =
                              SvcOpenAI $
                                OpenAIConfig
                                  "https://example.invalid/v1"
                                  "TEST_API_KEY"
                          }
                    , simpleProfile =
                        ServiceProf
                          { profService = ServiceName "__simple__"
                          , profModel = ModelName "overlay-model"
                          , profTemplate = Nothing
                          , profModelOptions = Nothing
                          , profNumExpr = Just 5
                          , profIncludeDocs = Just False
                          }
                    }
                )

            Right other ->
              assertFailure ("expected ConfigFancy, got: " <> show other)

    , testCase "fancy config reports parse errors" $
        withConfigFile "not valid toml = [\n" $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          assertBool
            ("expected parse failure, got: " <> show result)
            (isConfigParseError result)
    ]


-- Validation
-------------

validationTests :: TestTree
validationTests =
  testGroup "validation"
    [ testCase "duplicate service names are rejected" $
        withConfigFile duplicateServicesToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          result @?= Left (DuplicateServiceName (ServiceName "ollama"))

    , testCase "duplicate profile names are rejected" $
        withConfigFile duplicateProfilesToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          result @?= Left (DuplicateProfileName (ProfileName "p"))

    , testCase "unknown service reference is rejected" $
        withConfigFile unknownServiceToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          result @?= Left
            (UnknownServiceReference
              (ProfileName "p")
              (ServiceName "missing"))

    , testCase "unknown fanout profile reference is rejected" $
        withConfigFile unknownFanoutProfileToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          result @?= Left
            (UnknownProfileReference
              (ProfileName "fan")
              (ProfileName "missing"))

    , testCase "cyclic fanout profile reference is rejected" $
        withConfigFile cyclicFanoutToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          assertBool
            ("expected cyclic profile error, got: " <> show result)
            (isCyclicProfileError result)

    , testCase "fanout profiles are flattened to service leaves" $
        withConfigFile nestedFanoutToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          case result of
            Left err ->
              assertFailure ("unexpected config error: " <> show err)

            Right (ConfigFancy cfg) ->
              M.lookup (ProfileName "top") (cfgProfiles cfg)
                @?= Just
                  Profile
                    { profName = ProfileName "top"
                    , profTrigger = TriggerPrefix "top"
                    , profKind =
                        ProfFanout $
                          FanoutProf
                            (ProfileName "a" :| [ProfileName "b"])
                    }

            Right other ->
              assertFailure ("expected ConfigFancy, got: " <> show other)

    , testCase "ambiguous profile triggers are rejected" $
        withConfigFile ambiguousTriggersToml $ \path -> do
          result <-
            runBuild mempty
              { config_path = Just (ConfigExplicit path)
              }

          assertBool
            ("expected ambiguous trigger error, got: " <> show result)
            (isAmbiguousTriggerError result)
    ]


-- Direct map-builder tests
---------------------------

mapBuilderTests :: TestTree
mapBuilderTests =
  testGroup "map builders"
    [ testCase "buildServiceMap accepts distinct services" $
        buildServiceMap [ollamaService, openAIService]
          @?= Right
                ( M.fromList
                    [ (ServiceName "ollama", ollamaService)
                    , (ServiceName "openai", openAIService)
                    ]
                )

    , testCase "buildServiceMap rejects duplicate services" $
        buildServiceMap [ollamaService, ollamaService]
          @?= Left (DuplicateServiceName (ServiceName "ollama"))

    , testCase "buildProfileMap accepts service profile with known service" $
        buildProfileMap
          (M.fromList [(ServiceName "ollama", ollamaService)])
          [basicProfile]
          @?= Right
                (M.fromList [(ProfileName "p", basicProfile)])

    , testCase "buildProfileMap rejects service profile with unknown service" $
        buildProfileMap
          M.empty
          [basicProfile]
          @?= Left
                (UnknownServiceReference
                  (ProfileName "p")
                  (ServiceName "ollama"))
    ]


-- Properties
-------------

propertyTests :: TestTree
propertyTests =
  testGroup "properties"
    [ QC.testProperty "config=disabled always builds simple config" $
        QC.forAll genSimpleFlags $ \flags0 ->
          let
            flags =
              flags0 { config_path = Just ConfigDisabled }
          in
            QC.ioProperty $ do
              result <- runBuild flags

              pure $
                case result of
                  Right (ConfigSimple _) ->
                    QC.property True

                  other ->
                    QC.counterexample
                      ("expected ConfigSimple, got: " <> show other)
                      False

    , QC.testProperty "explicit missing config paths are rejected" $
        QC.forAll genSafeFileName $ \fileName ->
          QC.ioProperty $
            withSystemTempDirectory "ollama-holes-config-build" $ \dir -> do
              let path =
                    dir </> fileName

              result <-
                runBuild mempty
                  { config_path = Just (ConfigExplicit path)
                  }

              pure $
                result QC.=== Left (ConfigFileNotFound path)
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

ollamaService :: Service
ollamaService =
  Service
    { svcName = ServiceName "ollama"
    , svcConfig = SvcOllama (OllamaConfig Nothing)
    }


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


-- Test utilities
-----------------

runBuild :: Flags -> IO (Either ConfigError Config)
runBuild =
  runExceptT . buildConfig


withConfigFile :: Text -> (FilePath -> IO a) -> IO a
withConfigFile contents action =
  withSystemTempDirectory "ollama-holes-config-build" $ \dir -> do
    let path =
          dir </> "ollama-holes.toml"

    createDirectoryIfMissing True dir
    T.writeFile path contents
    action path


assertRight :: Show e => Either e a -> IO a
assertRight result =
  case result of
    Left err ->
      assertFailure ("expected Right, got Left: " <> show err)

    Right value ->
      pure value


isConfigParseError :: Either ConfigError Config -> Bool
isConfigParseError =
  \case
    Left (ConfigParseErrors _ _) ->
      True

    _ ->
      False


isCyclicProfileError :: Either ConfigError Config -> Bool
isCyclicProfileError =
  \case
    Left (CyclicProfileReference _) ->
      True

    _ ->
      False


isAmbiguousTriggerError :: Either ConfigError Config -> Bool
isAmbiguousTriggerError =
  \case
    Left (AmbiguousProfileTriggers _) ->
      True

    _ ->
      False


genSimpleFlags :: QC.Gen Flags
genSimpleFlags = do
  model <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just . T.pack <$> QC.elements ["m1", "m2", "qwen3:latest"])
      ]

  n <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just <$> QC.chooseInt (1, 20))
      ]

  includeDocs <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just <$> QC.arbitrary)
      ]

  trigger <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just <$> genTriggerPolicy)
      ]

  pure mempty
    { model_name = model
    , num_expr = n
    , include_docs = includeDocs
    , trigger_policy = trigger
    }


genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.oneof
    [ pure TriggerAll
    , pure TriggerNone
    , TriggerPrefix . T.pack <$> QC.elements ["llm", "ask", "hole"]
    ]


genSafeFileName :: QC.Gen FilePath
genSafeFileName = do
  n <- QC.chooseInt (1, 999999 :: Int)
  pure ("missing-" <> show n <> ".toml")