module GHC.Plugin.OllamaHoles.Data.ServiceCall.Route.Spec
  ( tests
  ) where

import Data.Aeson (Value(..))
import Data.List (nub)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Route
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Template


tests :: TestTree
tests =
  testGroup "GHC.Plugin.OllamaHoles.Data.ServiceCall.Route"
    [ unitTests
    , propertyTests
    ]


unitTests :: TestTree
unitTests =
  testGroup "unit tests"
    [ testCase "simple config returns one service call when trigger matches" $
        routeServiceCalls
          (ConfigSimple (simpleConfig (TriggerPrefix "llm") svcA profA))
          (holeName "_llm")
          @?= Right (Just [callA])

    , testCase "simple config returns Nothing when trigger does not match" $
        routeServiceCalls
          (ConfigSimple (simpleConfig (TriggerPrefix "llm") svcA profA))
          (holeName "_other")
          @?= Right Nothing

    , testCase "TriggerNone in simple config never routes" $
        routeServiceCalls
          (ConfigSimple (simpleConfig TriggerNone svcA profA))
          (holeName "_llm")
          @?= Right Nothing

    , testCase "TriggerAll in simple config always routes" $
        routeServiceCalls
          (ConfigSimple (simpleConfig TriggerAll svcA profA))
          (holeName "_anything")
          @?= Right (Just [callA])

    , testCase "fancy overlay takes priority over matching config profile" $
        let
          overlay =
            simpleConfig TriggerAll svcOverlay profOverlay

          profile =
            serviceProfile "p" TriggerAll profA

          cfg =
            fancyConfig
              [svcA]
              [profile]
              (Just (ConfigOverlay overlay))
        in
          routeServiceCalls cfg (holeName "_anything")
            @?= Right (Just [callOverlay])

    , testCase "fancy override routes matching service profile" $
        let
          profile =
            serviceProfile "p" (TriggerPrefix "llm") profA

          cfg =
            fancyConfig
              [svcA]
              [profile]
              (Just (ConfigOverride emptyOverrides))
        in
          routeServiceCalls cfg (holeName "_llm")
            @?= Right (Just [callA])

    , testCase "fancy config returns Nothing when no profile trigger matches" $
        let
          profile =
            serviceProfile "p" (TriggerPrefix "llm") profA

          cfg =
            fancyConfig
              [svcA]
              [profile]
              (Just (ConfigOverride emptyOverrides))
        in
          routeServiceCalls cfg (holeName "_other")
            @?= Right Nothing

    , testCase "fancy config reports ambiguous matching profiles" $
        let
          pA =
            serviceProfile "a" TriggerAll profA

          pB =
            serviceProfile "b" TriggerAll profB

          cfg =
            fancyConfig
              [svcA, svcB]
              [pB, pA]
              (Just (ConfigOverride emptyOverrides))
        in
          routeServiceCalls cfg (holeName "_anything")
            @?= Left
                  (RouteAmbiguousProfiles
                    (holeName "_anything")
                    [ProfileName "a", ProfileName "b"])

    , testCase "fancy config reports unknown service" $
        let
          missingProf =
            profA { profService = ServiceName "missing" }

          profile =
            serviceProfile "p" TriggerAll missingProf

          cfg =
            fancyConfig
              []
              [profile]
              (Just (ConfigOverride emptyOverrides))
        in
          routeServiceCalls cfg (holeName "_anything")
            @?= Left (RouteUnknownService (ServiceName "missing"))

    , testCase "fanout expands child profiles in fanout order" $
        let
          childA =
            serviceProfile "a" TriggerNone profA

          childB =
            serviceProfile "b" TriggerNone profB

          fan =
            fanoutProfile
              "fan"
              (TriggerPrefix "fan")
              (NE.fromList [ProfileName "b", ProfileName "a"])

          cfg =
            fancyConfig
              [svcA, svcB]
              [childA, childB, fan]
              (Just (ConfigOverride emptyOverrides))
        in
          routeServiceCalls cfg (holeName "_fan")
            @?= Right (Just [callB, callA])

    , testCase "fanout reports unknown child profile" $
        let
          fan =
            fanoutProfile
              "fan"
              TriggerAll
              (NE.fromList [ProfileName "missing"])

          cfg =
            fancyConfig
              []
              [fan]
              (Just (ConfigOverride emptyOverrides))
        in
          routeServiceCalls cfg (holeName "_anything")
            @?= Left (RouteUnknownProfile (ProfileName "missing"))

    , testCase "override config overrides service profile fields" $
        let
          original =
            profA
              { profModel = ModelName "original-model"
              , profTemplate = Just (TemplateFile "original.tmpl")
              , profModelOptions = Just (String "original-options")
              , profNumExpr = Just 1
              , profIncludeDocs = Just False
              }

          expected =
            original
              { profModel = ModelName "override-model"
              , profTemplate = Just (TemplateFile "override.tmpl")
              , profModelOptions = Just (String "override-options")
              , profNumExpr = Just 99
              , profIncludeDocs = Just True
              }

          overrides =
            OverrideConfig
              { overrideModelName = Just (ModelName "override-model")
              , overrideTemplate = Just (TemplateFile "override.tmpl")
              , overrideModelOptions = Just (String "override-options")
              , overrideNumExpr = Just 99
              , overrideIncludeDocs = Just True
              }

          profile =
            serviceProfile "p" TriggerAll original

          cfg =
            fancyConfig
              [svcA]
              [profile]
              (Just (ConfigOverride overrides))
        in
          routeServiceCalls cfg (holeName "_anything")
            @?= Right
                  (Just
                    [ ServiceCall
                        { callProfile = expected
                        , callService = svcA
                        }
                    ])

    , testCase "empty override preserves service profile fields" $
        let
          profile =
            serviceProfile "p" TriggerAll profA

          cfg =
            fancyConfig
              [svcA]
              [profile]
              (Just (ConfigOverride emptyOverrides))
        in
          routeServiceCalls cfg (holeName "_anything")
            @?= Right (Just [callA])
    ]


propertyTests :: TestTree
propertyTests =
  testGroup "properties"
    [ QC.testProperty "simple config agrees with shouldTriggerHole" $
        QC.forAll genTriggerPolicy $ \trigger ->
        QC.forAll genHoleName $ \hn ->
          let
            cfg =
              ConfigSimple (simpleConfig trigger svcA profA)

            expected =
              if shouldTriggerHole trigger hn
                then Just [callA]
                else Nothing
          in
            routeServiceCalls cfg hn QC.=== Right expected

    , QC.testProperty "fancy overlay wins whenever overlay trigger matches" $
        QC.forAll genHoleName $ \hn ->
          let
            overlay =
              simpleConfig TriggerAll svcOverlay profOverlay

            profile =
              serviceProfile "p" TriggerAll profA

            cfg =
              fancyConfig
                [svcA]
                [profile]
                (Just (ConfigOverlay overlay))
          in
            routeServiceCalls cfg hn QC.=== Right (Just [callOverlay])

    , QC.testProperty "fanout preserves child order" $
        QC.forAll genDistinctIndices $ \indices ->
          let
            children =
              [ indexedServiceProfile i
              | i <- indices
              ]

            services =
              [ indexedService i
              | i <- indices
              ]

            fan =
              fanoutProfile
                "fan"
                TriggerAll
                (NE.fromList [ indexedProfileName i
                | i <- indices
                ])

            cfg =
              fancyConfig
                services
                (fan : children)
                (Just (ConfigOverride emptyOverrides))

            expected =
              [ ServiceCall
                  { callProfile = indexedServiceProf i
                  , callService = indexedService i
                  }
              | i <- indices
              ]
          in
            routeServiceCalls cfg (holeName "_anything")
              QC.=== Right (Just expected)

    , QC.testProperty "empty override preserves routed service profile" $
        QC.forAll genHoleName $ \hn ->
          let
            profile =
              serviceProfile "p" TriggerAll profA

            cfg =
              fancyConfig
                [svcA]
                [profile]
                (Just (ConfigOverride emptyOverrides))
          in
            routeServiceCalls cfg hn QC.=== Right (Just [callA])
    ]


-- Helpers
----------

simpleConfig :: TriggerPolicy -> Service -> ServiceProf -> SimpleConfig
simpleConfig trigger service serviceProf =
  SimpleConfig
    { simpleTrigger = trigger
    , simpleService = service
    , simpleProfile = serviceProf
    }


fancyConfig
  :: [Service]
  -> [Profile]
  -> Maybe ExtraConfig
  -> Config
fancyConfig services profiles extras =
  ConfigFancy FancyConfig
    { cfgServices =
        M.fromList
          [ (svcName service, service)
          | service <- services
          ]

    , cfgProfiles =
        M.fromList
          [ (profName profile, profile)
          | profile <- profiles
          ]

    , cfgExtras =
        extras
    }


serviceProfile :: ProfileName -> TriggerPolicy -> ServiceProf -> Profile
serviceProfile name trigger serviceProf =
  Profile
    { profName = name
    , profTrigger = trigger
    , profKind = ProfService serviceProf
    }


fanoutProfile :: ProfileName -> TriggerPolicy -> NonEmpty ProfileName -> Profile
fanoutProfile name trigger children =
  Profile
    { profName = name
    , profTrigger = trigger
    , profKind = ProfFanout FanoutProf
        { profProfiles = children
        }
    }


emptyOverrides :: OverrideConfig
emptyOverrides =
  OverrideConfig
    { overrideModelName = Nothing
    , overrideTemplate = Nothing
    , overrideModelOptions = Nothing
    , overrideNumExpr = Nothing
    , overrideIncludeDocs = Nothing
    }


svcA :: Service
svcA =
  service "svc-a"


svcB :: Service
svcB =
  service "svc-b"


svcOverlay :: Service
svcOverlay =
  service "svc-overlay"


service :: ServiceName -> Service
service name =
  Service
    { svcName = name
    , svcConfig = SvcOllama (OllamaConfig Nothing)
    }


profA :: ServiceProf
profA =
  serviceProf "svc-a" "model-a"


profB :: ServiceProf
profB =
  serviceProf "svc-b" "model-b"


profOverlay :: ServiceProf
profOverlay =
  serviceProf "svc-overlay" "model-overlay"


serviceProf :: ServiceName -> ModelName -> ServiceProf
serviceProf serviceName modelName =
  ServiceProf
    { profService = serviceName
    , profModel = modelName
    , profTemplate = Nothing
    , profModelOptions = Nothing
    , profNumExpr = Just 5
    , profIncludeDocs = Just False
    }


callA :: ServiceCall
callA =
  ServiceCall
    { callProfile = profA
    , callService = svcA
    }


callB :: ServiceCall
callB =
  ServiceCall
    { callProfile = profB
    , callService = svcB
    }


callOverlay :: ServiceCall
callOverlay =
  ServiceCall
    { callProfile = profOverlay
    , callService = svcOverlay
    }


holeName :: Text -> HoleName
holeName = id


-- Indexed fixtures for order properties
----------------------------------------

indexedProfileName :: Int -> ProfileName
indexedProfileName i =
  ProfileName ("p" <> T.pack (show i))


indexedServiceName :: Int -> ServiceName
indexedServiceName i =
  ServiceName ("svc" <> T.pack (show i))


indexedModelName :: Int -> ModelName
indexedModelName i =
  ModelName ("model" <> T.pack (show i))


indexedService :: Int -> Service
indexedService i =
  service (indexedServiceName i)


indexedServiceProf :: Int -> ServiceProf
indexedServiceProf i =
  serviceProf (indexedServiceName i) (indexedModelName i)


indexedServiceProfile :: Int -> Profile
indexedServiceProfile i =
  serviceProfile
    (indexedProfileName i)
    TriggerNone
    (indexedServiceProf i)


-- Generators
-------------

genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.oneof
    [ pure TriggerNone
    , pure TriggerAll
    , TriggerPrefix <$> genPrefixText
    ]


genPrefixText :: QC.Gen Text
genPrefixText =
  T.pack <$> QC.listOf1 genIdentChar


genHoleName :: QC.Gen HoleName
genHoleName =
  QC.oneof
    [ holeName . ("_" <>) <$> genPrefixText
    , holeName . ("_" <>) <$> genPrefixTextWithSuffix
    ]


genPrefixTextWithSuffix :: QC.Gen Text
genPrefixTextWithSuffix = do
  prefix <- genPrefixText
  suffix <- T.pack <$> QC.listOf genIdentChar
  pure (prefix <> suffix)


genIdentChar :: QC.Gen Char
genIdentChar =
  QC.elements $
    ['a' .. 'z']
      <> ['A' .. 'Z']
      <> ['0' .. '9']
      <> "_"


genDistinctIndices :: QC.Gen [Int]
genDistinctIndices = do
  n <- QC.chooseInt (1, 8)
  xs <- QC.shuffle [1 .. n]
  pure (nub xs)