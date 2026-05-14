module GHC.Plugin.OllamaHoles.Data.ServiceCall.Submit.Spec
  ( tests
  ) where

import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.List.NonEmpty qualified as NE

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Submit
import GHC.Plugin.OllamaHoles.Data.ServiceCall.TestM
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.Trigger

import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types.Gen



tests :: TestTree
tests = testGroup "GHC.Plugin.OllamaHoles.Data.ServiceCall.Submit"
  [ tests_submitRoutedServiceCalls_unit
  , tests_submitRoutedServiceCalls_prop
  ]



run_submitRoutedServiceCalls
  :: Config -> String -> ServiceCallTestEnv
  -> Either ServiceCallError ([PromptResponse], [ModelSelectionWarning])
run_submitRoutedServiceCalls config holeName env =
  case runTestM $ runExceptT $ submitRoutedServiceCalls
    (serviceCallOps env) "." config (T.pack holeName) unusedPromptContext
  of
    Left err -> Left err
    Right responses -> Right
      ( serviceCallResponses responses
      , serviceCallWarnings responses
      )

tests_submitRoutedServiceCalls_unit :: TestTree
tests_submitRoutedServiceCalls_unit = testGroup "submitRoutedServiceCalls (unit)"
  [ testGroup "success" $
      tests_submitRoutedServiceCalls_unit_success <&>
        \(name, config, holeName, env, expected) ->
          testCase name $
            run_submitRoutedServiceCalls config holeName env @?= Right expected

  , testGroup "failure" $
      tests_submitRoutedServiceCalls_unit_failure <&>
        \(name, config, holeName, env) ->
          testCase name $
            case run_submitRoutedServiceCalls config holeName env of
              Left _ -> pure ()
              Right ok -> assertFailure $
                "expected failed routed service submission but got: " <> show ok
  ]

tests_submitRoutedServiceCalls_prop :: TestTree
tests_submitRoutedServiceCalls_prop = testGroup "submitRoutedServiceCalls (prop)"
  [ QC.testProperty "fanout response order follows routed service order" $
      QC.forAll genDistinctIndices $ \indices ->
        let
          services = [ indexedService i | i <- indices ]
          children = [ indexedServiceProfile i | i <- indices ]

          fan = Profile
            { profName = ProfileName "fan"
            , profTrigger = TriggerAll
            , profKind = ProfFanout FanoutProf
            { profProfiles =
                NE.fromList
                    [ indexedProfileName i
                    | i <- indices
                    ]
            }
            }

          config =
            ConfigFancy FancyConfig
              { cfgServices = M.fromList
                [ (svcName svc, svc) | svc <- services ]
              , cfgProfiles = M.fromList
                [ (profName profile, profile) | profile <- fan : children ]
              , cfgExtras = Just (ConfigOverride emptyOverrides)
              }

          env =
            ServiceCallTestEnv
              { testModels = M.fromList
                [ ( indexedServiceName i , Just [indexedModelName i] )
                | i <- indices
                ]
              , testResponses = M.fromList
                [ ( indexedServiceName i
                  , PromptResponse ("response-" <> T.pack (show i)) )
                | i <- indices
                ]
              }

          expectedResponses =
            [ PromptResponse ("response-" <> T.pack (show i)) | i <- indices ]
        in
          run_submitRoutedServiceCalls config "_anything" env
            QC.=== Right (expectedResponses, [])
  ]



tests_submitRoutedServiceCalls_unit_success
  :: [(String, Config, String, ServiceCallTestEnv, ([PromptResponse], [ModelSelectionWarning]))]
tests_submitRoutedServiceCalls_unit_success =
  [ ( "submits one routed simple service"
    , ConfigSimple SimpleConfig
        { simpleTrigger = TriggerAll
        , simpleService = svcA
        , simpleProfile = profA
        }
    , "_anything"
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          ]
        , testResponses = M.fromList
          [ (ServiceName "svc-a", PromptResponse "response-a")
          ]
        }
    , ( [PromptResponse "response-a"]
      , []
      )
    )

  , ( "submits fanout services in routed order"
    , ConfigFancy FancyConfig
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
                      ProfileName "b" NE.:| [ProfileName "a"]
                  }
                }
            )
          ]
        , cfgExtras = Just (ConfigOverride emptyOverrides)
        }
    , "_anything"
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          , (ServiceName "svc-b", Just [ModelName "model-b"])
          ]
        , testResponses = M.fromList
          [ (ServiceName "svc-a", PromptResponse "response-a")
          , (ServiceName "svc-b", PromptResponse "response-b")
          ]
        }
    , ( [ PromptResponse "response-b"
        , PromptResponse "response-a"
        ]
      , []
      )
    )

  , ( "returns model-selection warnings from skipped routed services"
    , ConfigFancy FancyConfig
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
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          , (ServiceName "svc-b", Nothing)
          ]
        , testResponses = M.fromList
          [ (ServiceName "svc-a", PromptResponse "response-a")
          ]
        }
    , ( [PromptResponse "response-a"]
      , [SkippedServiceCannotListModels (ServiceName "svc-b")]
      )
    )
  ]

tests_submitRoutedServiceCalls_unit_failure
  :: [(String, Config, String, ServiceCallTestEnv)]
tests_submitRoutedServiceCalls_unit_failure =
  [ ( "fails when no service routes"
    , ConfigSimple SimpleConfig
        { simpleTrigger = TriggerPrefix "llm"
        , simpleService = svcA
        , simpleProfile = profA
        }
    , "_other"
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          ]
        , testResponses = M.empty
        }
    )

  , ( "fails when all routed services are filtered before submission"
    , ConfigSimple SimpleConfig
        { simpleTrigger = TriggerAll
        , simpleService = svcA
        , simpleProfile = profA
        }
    , "_anything"
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "not-model-a"])
          ]
        , testResponses = M.empty
        }
    )

  , ( "fails when submitter has no fake response for accepted service"
    , ConfigSimple SimpleConfig
        { simpleTrigger = TriggerAll
        , simpleService = svcA
        , simpleProfile = profA
        }
    , "_anything"
    , ServiceCallTestEnv
        { testModels = M.fromList
          [ (ServiceName "svc-a", Just [ModelName "model-a"])
          ]
        , testResponses = M.empty
        }
    )
  ]
