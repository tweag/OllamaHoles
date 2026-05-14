module GHC.Plugin.OllamaHoles.Data.ServiceCall.Route.Spec
  ( tests
  ) where

import Control.Monad.Except
import Data.Aeson (Value(..))
import Data.Functor ((<&>))
import Data.List (nub)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Route
import GHC.Plugin.OllamaHoles.Data.ServiceCall.TestM
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Trigger

import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types.Gen



tests :: TestTree
tests = testGroup "GHC.Plugin.OllamaHoles.Data.ServiceCall.Route"
  [ tests_prepareServiceCalls_unit
  , tests_prepareServiceCalls_prop
  ]



run_prepareServiceCalls
  :: Config
  -> Text
  -> ServiceCallTestEnv
  -> Either ServiceCallError CheckedServiceCalls
run_prepareServiceCalls config holeName env =
  runTestM $
    runExceptT $
      prepareServiceCalls
        (listModelsFromEnv env)
        config
        holeName

tests_prepareServiceCalls_unit :: TestTree
tests_prepareServiceCalls_unit = testGroup "prepareServiceCalls (unit)"
  [ testGroup "success" $
      tests_prepareServiceCalls_unit_success <&>
        \(name, (config, holeName), env, expected) ->
          testCase name $
            run_prepareServiceCalls config holeName env @?= Right expected

  , testGroup "failure" $
      tests_prepareServiceCalls_unit_failure <&>
        \(name, (config, holeName), env) ->
          testCase name $
            case run_prepareServiceCalls config holeName env of
              Left _ -> pure ()

              Right ok -> assertFailure $
                "expected failed service-call preparation but got: " <> show ok
  ]

tests_prepareServiceCalls_prop :: TestTree
tests_prepareServiceCalls_prop = testGroup "prepareServiceCalls (prop)"
  [ QC.testProperty "simple config agrees with shouldTriggerHole when model is available" $
      QC.forAll genTriggerPolicy $ \trigger ->
      QC.forAll genHoleName $ \hn ->
        let
          config = ConfigSimple SimpleConfig
            { simpleTrigger = trigger
            , simpleService = svcA
            , simpleProfile = profA
            }

          env = ServiceCallTestEnv
            { testModels = M.fromList
              [ (ServiceName "svc-a", Just [ModelName "model-a"])
              ]
            , testResponses = M.empty
            }

          expected = if shouldTriggerHole trigger hn
            then Right CheckedServiceCalls
              { checkedAccepted =
                [ ServiceCall
                    { callService = svcA
                    , callProfile = profA
                    }
                ]
              , checkedWarnings = []
              }
            else Left $
              ServiceCallModelError $
                NoServiceCallsRouted hn
        in
          run_prepareServiceCalls config hn env QC.=== expected

  , QC.testProperty "fanout preserves child order when all models are available" $
      QC.forAll genDistinctIndices $ \indices ->
        let
          services = [ indexedService i | i <- indices ]
          children = [ indexedServiceProfile i | i <- indices ]

          fan = Profile
            { profName = ProfileName "fan"
            , profTrigger = TriggerAll
            , profKind = ProfFanout FanoutProf
              { profProfiles = NE.fromList
                  [ indexedProfileName i | i <- indices ]
              }
            }

          config = ConfigFancy FancyConfig
            { cfgServices = M.fromList
              [ (svcName svc, svc) | svc <- services ]
            , cfgProfiles = M.fromList
              [ (profName profile, profile) | profile <- fan : children ]
            , cfgExtras = Just (ConfigOverride emptyOverrides)
            }

          env =
            ServiceCallTestEnv
              { testModels = M.fromList
                [ ( indexedServiceName i, Just [indexedModelName i] )
                | i <- indices
                ]
              , testResponses = M.empty
              }

          expected =
            Right CheckedServiceCalls
              { checkedAccepted =
                [ ServiceCall
                    { callService = indexedService i
                    , callProfile = indexedServiceProf i
                    }
                | i <- indices
                ]
              , checkedWarnings = []
              }
        in
          run_prepareServiceCalls config "_anything" env QC.=== expected
  ]



tests_prepareServiceCalls_unit_success
  :: [(String, (Config, Text), ServiceCallTestEnv, CheckedServiceCalls)]
tests_prepareServiceCalls_unit_success =
  [ ( "simple config returns one service call when trigger matches"
    , ( ConfigSimple SimpleConfig
          { simpleTrigger = TriggerPrefix "llm"
          , simpleService = svcA
          , simpleProfile = profA
          }
      , "_llm"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          ]
        , testResponses = M.empty
        }
    , CheckedServiceCalls
        { checkedAccepted =
          [ ServiceCall
              { callService = svcA
              , callProfile = profA
              }
          ]
        , checkedWarnings = []
        }
    )

  , ( "TriggerAll in simple config always routes"
    , ( ConfigSimple SimpleConfig
          { simpleTrigger = TriggerAll
          , simpleService = svcA
          , simpleProfile = profA
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          ]
        , testResponses = M.empty
        }
    , CheckedServiceCalls
        { checkedAccepted =
          [ ServiceCall
              { callService = svcA
              , callProfile = profA
              }
          ]
        , checkedWarnings = []
        }
    )

  , ( "fancy overlay takes priority over matching config profile"
    , ( ConfigFancy FancyConfig
          { cfgServices = M.fromList
            [ (ServiceName "svc-a", svcA)
            ]
          , cfgProfiles = M.fromList
            [ ( ProfileName "p"
              , Profile
                  { profName = ProfileName "p"
                  , profTrigger = TriggerAll
                  , profKind = ProfService profA
                  }
              )
            ]
          , cfgExtras = Just $
              ConfigOverlay SimpleConfig
                { simpleTrigger = TriggerAll
                , simpleService = svcOverlay
                , simpleProfile = profOverlay
                }
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-overlay", Just [ModelName "model-overlay"])
          ]
        , testResponses = M.empty
        }
    , CheckedServiceCalls
        { checkedAccepted =
          [ ServiceCall
              { callService = svcOverlay
              , callProfile = profOverlay
              }
          ]
        , checkedWarnings = []
        }
    )

  , ( "fanout expands child profiles in fanout order"
    , ( ConfigFancy FancyConfig
          { cfgServices = M.fromList
            [ (ServiceName "svc-a", svcA)
            , (ServiceName "svc-b", svcB)
            ]
          , cfgProfiles = M.fromList
            [ ( ProfileName "a"
              , Profile
                  { profName = ProfileName "a"
                  , profTrigger = TriggerNone
                  , profKind = ProfService profA
                  }
              )
            , ( ProfileName "b"
              , Profile
                  { profName = ProfileName "b"
                  , profTrigger = TriggerNone
                  , profKind = ProfService profB
                  }
              )
            , ( ProfileName "fan"
              , Profile
                  { profName = ProfileName "fan"
                  , profTrigger = TriggerPrefix "fan"
                  , profKind = ProfFanout FanoutProf
                    { profProfiles =
                        ProfileName "b" NE.:| [ProfileName "a"]
                    }
                  }
              )
            ]
          , cfgExtras = Just (ConfigOverride emptyOverrides)
          }
      , "_fan"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          , (ServiceName "svc-b", Just [ModelName "model-b"])
          ]
        , testResponses = M.empty
        }
    , CheckedServiceCalls
        { checkedAccepted =
          [ ServiceCall
              { callService = svcB
              , callProfile = profB
              }
          , ServiceCall
              { callService = svcA
              , callProfile = profA
              }
          ]
        , checkedWarnings = []
        }
    )

  , ( "one missing model is warned while other routed services remain"
    , ( ConfigFancy FancyConfig
          { cfgServices = M.fromList
            [ (ServiceName "svc-a", svcA)
            , (ServiceName "svc-b", svcB)
            ]
          , cfgProfiles = M.fromList
            [ ( ProfileName "a"
              , Profile
                  { profName = ProfileName "a"
                  , profTrigger = TriggerNone
                  , profKind = ProfService profA
                  }
              )
            , ( ProfileName "b"
              , Profile
                  { profName = ProfileName "b"
                  , profTrigger = TriggerNone
                  , profKind = ProfService profB
                  }
              )
            , ( ProfileName "fan"
              , Profile
                  { profName = ProfileName "fan"
                  , profTrigger = TriggerAll
                  , profKind = ProfFanout FanoutProf
                    { profProfiles =
                        ProfileName "a" NE.:| [ProfileName "b"]
                    }
                  }
              )
            ]
          , cfgExtras = Just (ConfigOverride emptyOverrides)
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          , (ServiceName "svc-b", Just [ModelName "not-model-b"])
          ]
        , testResponses = M.empty
        }
    , CheckedServiceCalls
        { checkedAccepted =
          [ ServiceCall
              { callService = svcA
              , callProfile = profA
              }
          ]
        , checkedWarnings =
          [ SkippedServiceMissingModel
              (ServiceName "svc-b")
              (ModelName "model-b")
              [ModelName "not-model-b"]
          ]
        }
    )
  ]

tests_prepareServiceCalls_unit_failure
  :: [(String, (Config, Text), ServiceCallTestEnv)]
tests_prepareServiceCalls_unit_failure =
  [ ( "simple config returns error when trigger does not match"
    , ( ConfigSimple SimpleConfig
          { simpleTrigger = TriggerPrefix "llm"
          , simpleService = svcA
          , simpleProfile = profA
          }
      , "_other"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          ]
        , testResponses = M.empty
        }
    )

  , ( "TriggerNone in simple config never routes"
    , ( ConfigSimple SimpleConfig
          { simpleTrigger = TriggerNone
          , simpleService = svcA
          , simpleProfile = profA
          }
      , "_llm"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          ]
        , testResponses = M.empty
        }
    )

  , ( "fancy config reports ambiguous matching profiles"
    , ( ConfigFancy FancyConfig
          { cfgServices = M.fromList
            [ (ServiceName "svc-a", svcA)
            , (ServiceName "svc-b", svcB)
            ]
          , cfgProfiles = M.fromList
            [ ( ProfileName "a"
              , Profile
                  { profName = ProfileName "a"
                  , profTrigger = TriggerAll
                  , profKind = ProfService profA
                  }
              )
            , ( ProfileName "b"
              , Profile
                  { profName = ProfileName "b"
                  , profTrigger = TriggerAll
                  , profKind = ProfService profB
                  }
              )
            ]
          , cfgExtras = Just (ConfigOverride emptyOverrides)
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.empty
        , testResponses = M.empty
        }
    )

  , ( "fancy config reports unknown service"
    , ( ConfigFancy FancyConfig
          { cfgServices = M.fromList []
          , cfgProfiles = M.fromList
            [ ( ProfileName "p"
              , Profile
                  { profName = ProfileName "p"
                  , profTrigger = TriggerAll
                  , profKind = ProfService profA
                    { profService = ServiceName "missing"
                    }
                  }
              )
            ]
          , cfgExtras = Just (ConfigOverride emptyOverrides)
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.empty
        , testResponses = M.empty
        }
    )

  , ( "fanout reports unknown child profile"
    , ( ConfigFancy FancyConfig
          { cfgServices = M.fromList []
          , cfgProfiles = M.fromList
            [ ( ProfileName "fan"
              , Profile
                  { profName = ProfileName "fan"
                  , profTrigger = TriggerAll
                  , profKind = ProfFanout FanoutProf
                    { profProfiles =
                        ProfileName "missing" NE.:| []
                    }
                  }
              )
            ]
          , cfgExtras = Just (ConfigOverride emptyOverrides)
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.empty
        , testResponses = M.empty
        }
    )

  , ( "all routed services are rejected when backend cannot list models"
    , ( ConfigSimple SimpleConfig
          { simpleTrigger = TriggerAll
          , simpleService = svcA
          , simpleProfile = profA
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Nothing)
          ]
        , testResponses = M.empty
        }
    )

  , ( "all routed services are rejected when model is missing"
    , ( ConfigSimple SimpleConfig
          { simpleTrigger = TriggerAll
          , simpleService = svcA
          , simpleProfile = profA
          }
      , "_anything"
      )
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "other-model"])
          ]
        , testResponses = M.empty
        }
    )
  ]
