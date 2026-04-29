{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Config.Spec (tests) where

import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Config
  ( Config(..)
  , ConfigError(..)
  , buildProfileMap
  , buildServiceMap
  , resolveConfig
  )
import GHC.Plugin.OllamaHoles.Config.Preferences
  ( Preferences(..)
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
import GHC.Plugin.OllamaHoles.Trigger
  ( TriggerPolicy(..)
  )
import GHC.Plugin.OllamaHoles.Template
  ( TemplateSource(..)
  )

tests :: TestTree
tests =
  testGroup "Config"
    [ unitTests
    , propertyTests
    ]


-- Unit tests
-------------

unitTests :: TestTree
unitTests =
  testGroup "unit"
    [ buildServiceMapTests
    , buildProfileMapTests
    , resolveConfigTests
    ]

buildServiceMapTests :: TestTree
buildServiceMapTests =
  testGroup "buildServiceMap"
    [ testCase "builds map for unique services" $ do
        let svcs =
              [ mkOllamaService "local"
              , mkOpenAIService "remote"
              ]
        result <- pure (buildServiceMap svcs)
        case result of
          Left err ->
            assertFailure ("unexpected error: " <> show err)
          Right mp -> do
            M.size mp @?= 2
            M.lookup (svcN "local") mp @?= Just (mkOllamaService "local")
            M.lookup (svcN "remote") mp @?= Just (mkOpenAIService "remote")

    , testCase "rejects duplicate service names" $ do
        let dup = mkOllamaService "local"
        buildServiceMap [dup, mkOpenAIService "remote", dup]
          @?= Left (DuplicateServiceName (svcN "local"))
    ]

buildProfileMapTests :: TestTree
buildProfileMapTests =
  testGroup "buildProfileMap"
    [ testCase "builds map for service profiles with known services" $ do
        let svcMap = M.fromList
              [ (svcN "local", mkOllamaService "local")
              ]
            profs =
              [ mkServiceProfile "p1" "local"
              , mkServiceProfile "p2" "local"
              ]
        result <- pure (buildProfileMap svcMap profs)
        case result of
          Left err ->
            assertFailure ("unexpected error: " <> show err)
          Right mp -> do
            M.size mp @?= 2
            M.lookup (profN "p1") mp @?= Just (mkServiceProfile "p1" "local")
            M.lookup (profN "p2") mp @?= Just (mkServiceProfile "p2" "local")

    , testCase "rejects duplicate profile names" $ do
        let svcMap = M.fromList
              [ (svcN "local", mkOllamaService "local") ]
            dup = mkServiceProfile "p1" "local"
        buildProfileMap svcMap [dup, mkServiceProfile "p2" "local", dup]
          @?= Left (DuplicateProfileName (profN "p1"))
    ]

resolveConfigTests :: TestTree
resolveConfigTests =
  testGroup "resolveConfig"
    [ testCase "resolves simple service-only configuration" $ do
        let prefs = Preferences
              { prefServices =
                  [ mkOllamaService "local"
                  , mkOpenAIService "remote"
                  ]
              , prefProfiles =
                  [ mkServiceProfile "p1" "local"
                  , mkRichServiceProfile "p2" "remote"
                  ]
              }
        result <- pure (resolveConfig prefs)
        case result of
          Left err ->
            assertFailure ("unexpected error: " <> show err)
          Right cfg -> do
            M.size (cfgServices cfg) @?= 2
            M.size (cfgProfiles cfg) @?= 2

    , testCase "rejects service profile with unknown service" $ do
        let prefs = Preferences
              { prefServices = [mkOllamaService "local"]
              , prefProfiles = [mkServiceProfile "p1" "missing"]
              }
        resolveConfig prefs
          @?= Left (UnknownServiceReference (profN "p1") (svcN "missing"))

    , testCase "rejects fanout profile with unknown profile reference" $ do
        let prefs = Preferences
              { prefServices = [mkOllamaService "local"]
              , prefProfiles =
                  [ mkServiceProfile "p1" "local"
                  , mkFanoutProfile "pair" ("p1" :| ["missing"])
                  ]
              }
        resolveConfig prefs
          @?= Left (UnknownProfileReference (profN "pair") (profN "missing"))

    , testCase "rejects self-cycle in fanout profile" $ do
        let prefs = Preferences
              { prefServices = [mkOllamaService "local"]
              , prefProfiles =
                  [ mkServiceProfile "leaf" "local"
                  , mkFanoutProfile "self" ("self" :| [])
                  ]
              }
        resolveConfig prefs
          @?= Left (CyclicProfileReference [profN "self", profN "self"])

    , testCase "rejects mutual cycle between fanout profiles" $ do
        let prefs = Preferences
              { prefServices = [mkOllamaService "local"]
              , prefProfiles =
                  [ mkServiceProfile "leaf" "local"
                  , mkFanoutProfile "a" ("b" :| [])
                  , mkFanoutProfile "b" ("a" :| [])
                  ]
              }
        resolveConfig prefs
          @?= Left (CyclicProfileReference [profN "a", profN "b", profN "a"])

    , testCase "fanout validates transitive service dependencies" $ do
        let prefs = Preferences
              { prefServices = [mkOllamaService "local"]
              , prefProfiles =
                  [ mkServiceProfile "good" "local"
                  , mkServiceProfile "bad" "missing"
                  , mkFanoutProfile "pair" ("good" :| ["bad"])
                  ]
              }
        resolveConfig prefs
          @?= Left (UnknownServiceReference (profN "bad") (svcN "missing"))

    , testCase "nested fanout is flattened to leaf profile names" $ do
        let prefs = Preferences
              { prefServices = [mkOllamaService "local"]
              , prefProfiles =
                  [ mkServiceProfile "leaf1" "local"
                  , mkServiceProfile "leaf2" "local"
                  , mkFanoutProfile "inner" ("leaf1" :| ["leaf2"])
                  , mkFanoutProfile "outer" ("inner" :| ["leaf1"])
                  ]
              }
        result <- pure (resolveConfig prefs)
        case result of
          Left err ->
            assertFailure ("unexpected error: " <> show err)
          Right cfg ->
            case M.lookup (profN "outer") (cfgProfiles cfg) of
              Nothing ->
                assertFailure "missing resolved profile: outer"
              Just Profile{profKind = ProfFanout (FanoutProf xs)} ->
                xs @?= (profN "leaf1" :| [profN "leaf2", profN "leaf1"])
              Just other ->
                assertFailure ("expected outer to be resolved fanout, got: " <> show other)

    , testCase "service profile fields are preserved by validation" $ do
        let prof = mkRichServiceProfile "rich" "remote"
            prefs = Preferences
              { prefServices = [mkOpenAIService "remote"]
              , prefProfiles = [prof]
              }
        result <- pure (resolveConfig prefs)
        case result of
          Left err ->
            assertFailure ("unexpected error: " <> show err)
          Right cfg ->
            M.lookup (profN "rich") (cfgProfiles cfg) @?= Just prof
    ]


-- Property tests
-----------------

propertyTests :: TestTree
propertyTests =
  testGroup "properties"
    [ QC.testProperty "buildServiceMap succeeds on unique service names" $
        QC.forAll genUniqueServiceNames $ \names ->
          case buildServiceMap (map mkOllamaService names) of
            Left err ->
              QC.counterexample ("unexpected error: " <> show err) False
            Right mp ->
              M.size mp QC.=== length names

    , QC.testProperty "buildServiceMap rejects an introduced duplicate" $
        QC.forAll genUniqueServiceNamesNonEmpty $ \names ->
          let xs = map mkOllamaService names
              dup = mkOllamaService (head names)
          in buildServiceMap (xs <> [dup])
               QC.=== Left (DuplicateServiceName (svcN (head names)))

    , QC.testProperty "resolveConfig succeeds for valid service-only preferences" $
        QC.forAll genValidServiceOnlyPreferences $ \prefs ->
          case resolveConfig prefs of
            Left err ->
              QC.counterexample ("unexpected error: " <> show err <> "\n" <> show prefs) False
            Right cfg ->
              let profCount = length (prefProfiles prefs)
                  svcCount  = length (prefServices prefs)
              in M.size (cfgProfiles cfg) == profCount
                 QC..&&.
                 M.size (cfgServices cfg) == svcCount

    , QC.testProperty "resolveConfig rejects missing service references" $
        QC.forAll genMissingServicePreferences $ \prefs ->
          case resolveConfig prefs of
            Left (UnknownServiceReference _ _) -> QC.property True
            other ->
              QC.counterexample ("expected UnknownServiceReference, got: " <> show other) False

    , QC.testProperty "resolveConfig rejects self-cycles" $
        QC.forAll genSimpleSelfCyclePreferences $ \prefs ->
          case resolveConfig prefs of
            Left (CyclicProfileReference _) -> QC.property True
            other ->
              QC.counterexample ("expected CyclicProfileReference, got: " <> show other) False
    ]


-- Helpers
----------

svcN :: Text -> ServiceName
svcN = ServiceName

profN :: Text -> ProfileName
profN = ProfileName

modelN :: Text -> ModelName
modelN = ModelName

mkOllamaService :: Text -> Service
mkOllamaService nm =
  Service
    { svcName = svcN nm
    , svcConfig = SvcOllama (OllamaConfig Nothing)
    }

mkOpenAIService :: Text -> Service
mkOpenAIService nm =
  Service
    { svcName = svcN nm
    , svcConfig = SvcOpenAI (OpenAIConfig
        { svcOpenAIBaseUrl = "https://example.invalid/v1"
        , svcOpenAIKeyName = "EXAMPLE_API_KEY"
        })
    }

mkGeminiService :: Text -> Service
mkGeminiService nm =
  Service
    { svcName = svcN nm
    , svcConfig = SvcGemini (GeminiConfig "GEMINI_API_KEY")
    }

mkServiceProfile :: Text -> Text -> Profile
mkServiceProfile prof service =
  Profile
    { profName = profN prof
    , profTrigger = TriggerNone
    , profKind = ProfService ServiceProf
        { profService      = svcN service
        , profModel        = modelN "qwen3:4b"
        , profTemplate     = Nothing
        , profModelOptions = Nothing
        , profNumExpr      = Nothing
        , profIncludeDocs  = Nothing
        }
    }

mkRichServiceProfile :: Text -> Text -> Profile
mkRichServiceProfile prof service =
  Profile
    { profName = profN prof
    , profTrigger      = TriggerPrefix "llm"
    , profKind = ProfService $ ServiceProf
        { profService      = svcN service
        , profModel        = modelN "gpt-4.1-mini"
        , profTemplate     = Just DefaultTemplate
        , profModelOptions = Just (Aeson.object ["temperature" Aeson..= (0.3 :: Double)])
        , profNumExpr      = Just 8
        , profIncludeDocs  = Just True
        }
    }

mkFanoutProfile :: Text -> NonEmpty Text -> Profile
mkFanoutProfile prof children =
  Profile
    { profName = profN prof
    , profTrigger = TriggerNone
    , profKind = ProfFanout (FanoutProf (fmap profN children))
    }


-- Generators
-------------

genNameText :: QC.Gen Text
genNameText = do
  c0 <- QC.elements ['a' .. 'z']
  rest <- QC.listOf (QC.elements (['a' .. 'z'] <> ['0' .. '9']))
  pure (T.pack (c0 : rest))

genUniqueServiceNames :: QC.Gen [Text]
genUniqueServiceNames =
  fmap (map T.pack)
    (QC.listOf (QC.listOf1 (QC.elements (['a' .. 'z'] <> ['0' .. '9']))))
    `QC.suchThat` allDistinct
  where
    allDistinct xs = length xs == length (nubOrd xs)

genUniqueServiceNamesNonEmpty :: QC.Gen [Text]
genUniqueServiceNamesNonEmpty =
  genUniqueServiceNames `QC.suchThat` (not . null)

genValidServiceOnlyPreferences :: QC.Gen Preferences
genValidServiceOnlyPreferences = do
  serviceNames <- genUniqueServiceNamesNonEmpty
  profileNames <- genDistinctNames
  refs <- QC.vectorOf (length profileNames) (QC.elements serviceNames)
  let svcs = map mkOllamaService serviceNames
      profs = zipWith mkServiceProfile profileNames refs
  pure Preferences
    { prefServices = svcs
    , prefProfiles = profs
    }

genMissingServicePreferences :: QC.Gen Preferences
genMissingServicePreferences = do
  serviceNames <- genUniqueServiceNamesNonEmpty
  missing <- genFreshName serviceNames
  prof <- genFreshName []
  let svcs = map mkOllamaService serviceNames
      profs = [mkServiceProfile prof missing]
  pure Preferences
    { prefServices = svcs
    , prefProfiles = profs
    }

genSimpleSelfCyclePreferences :: QC.Gen Preferences
genSimpleSelfCyclePreferences = do
  svc <- genFreshName []
  leaf <- genFreshName [svc]
  self <- genFreshName [svc, leaf]
  pure Preferences
    { prefServices = [mkOllamaService svc]
    , prefProfiles =
        [ mkServiceProfile leaf svc
        , mkFanoutProfile self (self :| [])
        ]
    }

genDistinctNames :: QC.Gen [Text]
genDistinctNames =
  fmap (map T.pack)
    (QC.listOf1 (QC.listOf1 (QC.elements (['a' .. 'z'] <> ['0' .. '9']))))
    `QC.suchThat` allDistinct
  where
    allDistinct xs = length xs == length (nubOrd xs)

genFreshName :: [Text] -> QC.Gen Text
genFreshName used =
  genNameText `QC.suchThat` (`notElem` used)

nubOrd :: Ord a => [a] -> [a]
nubOrd = go M.empty
  where
    go _ [] = []
    go seen (x:xs)
      | M.member x seen = go seen xs
      | otherwise       = x : go (M.insert x () seen) xs