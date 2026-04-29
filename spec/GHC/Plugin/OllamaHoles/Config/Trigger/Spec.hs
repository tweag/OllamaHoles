{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Config.Trigger.Spec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Config.Trigger
import GHC.Plugin.OllamaHoles.Config.Types
import GHC.Plugin.OllamaHoles.Trigger


tests :: TestTree
tests =
  testGroup "config trigger validation"
    [ testCase "accepts distinct prefixes" $
        validateProfileTriggers
          [ profile (ProfileName "ollama") (TriggerPrefix "llm")
          , profile (ProfileName "claude") (TriggerPrefix "claude")
          ]
          @?= Right ()

    , testCase "rejects duplicate prefixes" $
        validateProfileTriggers
          [ profile "a" (TriggerPrefix "llm")
          , profile "b" (TriggerPrefix "llm")
          ]
          @?= Left
                (DuplicateTriggerPrefix
                  (ProfileName "a")
                  (ProfileName "b")
                  "llm")

    , testCase "rejects proper prefix overlap" $
        validateProfileTriggers
          [ profile "default" (TriggerPrefix "llm")
          , profile "fast" (TriggerPrefix "llm_fast")
          ]
          @?= Left
                (TriggerPrefixOverlap
                  (ProfileName "default")
                  "llm"
                  (ProfileName "fast")
                  "llm_fast")

    , testCase "ignores TriggerNone" $
        validateProfileTriggers
          [ profile "disabled" TriggerNone
          , profile "default" (TriggerPrefix "llm")
          ]
          @?= Right ()

    , testCase "rejects multiple TriggerAll profiles" $
        validateProfileTriggers
          [ profile "a" TriggerAll
          , profile "b" TriggerAll
          ]
          @?= Left
                (MultipleTriggerAll
                  (ProfileName "a")
                  (ProfileName "b"))

    , testCase "rejects TriggerAll mixed with prefix trigger" $
        validateProfileTriggers
          [ profile "fallback" TriggerAll
          , profile "default" (TriggerPrefix "llm")
          ]
          @?= Left
                (TriggerAllOverlaps
                  (ProfileName "fallback")
                  (ProfileName "default")
                  (TriggerPrefix "llm"))
    ]


profile :: ProfileName -> TriggerPolicy -> Profile
profile name trigger =
  Profile
    { profName = name
    , profTrigger = trigger
    , profKind = ProfService $ ServiceProf
        { profService      = ServiceName "service"
        , profModel        = ModelName "model"
        , profTemplate     = Nothing
        , profModelOptions = Nothing
        , profNumExpr      = Nothing
        , profIncludeDocs  = Nothing
        }
    }