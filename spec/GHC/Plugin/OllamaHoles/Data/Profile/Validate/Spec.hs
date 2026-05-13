module GHC.Plugin.OllamaHoles.Data.Profile.Validate.Spec (tests) where

import Data.Functor ((<&>))
import Data.Text qualified as T

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Profile.Error
import GHC.Plugin.OllamaHoles.Data.Profile.Types
import GHC.Plugin.OllamaHoles.Data.Profile.Types.Gen
import GHC.Plugin.OllamaHoles.Data.Profile.Validate
import GHC.Plugin.OllamaHoles.Data.Service.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Types


tests :: TestTree
tests = testGroup "Profile.Validate"
  [ tests_validateProfileTriggers_unit
  , tests_validateProfileTriggers_prop
  ]

tests_validateProfileTriggers_unit :: TestTree
tests_validateProfileTriggers_unit = testGroup "validateProfileTriggers unit"
  [ testGroup "valid trigger sets" $
      tests_validateProfileTriggers_unit_success <&> \(name, profiles) ->
        testCase name $ validateProfileTriggers profiles @?= Right ()

  , testGroup "invalid trigger sets" $
      tests_validateProfileTriggers_unit_failure <&> \(name, profiles, expected) ->
        testCase name $ validateProfileTriggers profiles @?= Left expected
  ]

tests_validateProfileTriggers_prop :: TestTree
tests_validateProfileTriggers_prop = testGroup "validateProfileTriggers properties"
  [ QC.testProperty "profiles with only TriggerNone are always valid" $
      QC.forAll (QC.listOf genProfileName) $ \names ->
        let
          profiles =
            [ serviceProfileWithTrigger name TriggerNone
            | name <- names
            ]
        in
          validateProfileTriggers profiles QC.=== Right ()

  , QC.testProperty "profiles with disjoint prefix triggers are valid" $
      QC.forAll genDisjointTriggerPrefixTexts $ \prefixes ->
        let
          profiles =
            zipWith
              (\ix prefix ->
                serviceProfileWithTrigger
                  (ProfileName ("p" <> T.pack (show ix)))
                  (TriggerPrefix prefix)
              )
              [(0 :: Int) ..]
              prefixes
        in
          validateProfileTriggers profiles QC.=== Right ()

  , QC.testProperty "duplicate prefix triggers are rejected" $
      QC.forAll genTriggerPrefixText $ \prefix ->
        let
          profiles =
            [ serviceProfileWithTrigger (ProfileName "a") (TriggerPrefix prefix)
            , serviceProfileWithTrigger (ProfileName "b") (TriggerPrefix prefix)
            ]
        in
          validateProfileTriggers profiles
            QC.=== Left
              (DuplicateTriggerPrefix
                (ProfileName "a")
                (ProfileName "b")
                prefix)

  , QC.testProperty "TriggerAll conflicts with any active prefix trigger" $
      QC.forAll genTriggerPrefixText $ \prefix ->
        let
          profiles =
            [ serviceProfileWithTrigger (ProfileName "all") TriggerAll
            , serviceProfileWithTrigger (ProfileName "p") (TriggerPrefix prefix)
            ]
        in
          validateProfileTriggers profiles
            QC.=== Left
              (TriggerAllOverlaps
                (ProfileName "all")
                (ProfileName "p")
                (TriggerPrefix prefix))
  ]



tests_validateProfileTriggers_unit_success :: [(String, [Profile])]
tests_validateProfileTriggers_unit_success =
  [ ( "empty profile list is valid"
    , []
    )

  , ( "all TriggerNone profiles are valid"
    , [ serviceProfileWithTrigger (ProfileName "a") TriggerNone
      , serviceProfileWithTrigger (ProfileName "b") TriggerNone
      ]
    )

  , ( "one TriggerAll profile is valid"
    , [ serviceProfileWithTrigger (ProfileName "a") TriggerAll
      ]
    )

  , ( "one TriggerPrefix profile is valid"
    , [ serviceProfileWithTrigger (ProfileName "a") (TriggerPrefix "llm")
      ]
    )

  , ( "disjoint TriggerPrefix profiles are valid"
    , [ serviceProfileWithTrigger (ProfileName "a") (TriggerPrefix "llm")
      , serviceProfileWithTrigger (ProfileName "b") (TriggerPrefix "ask")
      , serviceProfileWithTrigger (ProfileName "c") (TriggerPrefix "hole")
      ]
    )

  , ( "TriggerNone is ignored beside an active prefix"
    , [ serviceProfileWithTrigger (ProfileName "disabled") TriggerNone
      , serviceProfileWithTrigger (ProfileName "default") (TriggerPrefix "llm")
      ]
    )
  ]

tests_validateProfileTriggers_unit_failure
  :: [(String, [Profile], TriggerConflict)]
tests_validateProfileTriggers_unit_failure =
  [ ( "duplicate prefix triggers conflict"
    , [ serviceProfileWithTrigger (ProfileName "a") (TriggerPrefix "llm")
      , serviceProfileWithTrigger (ProfileName "b") (TriggerPrefix "llm")
      ]
    , DuplicateTriggerPrefix (ProfileName "a") (ProfileName "b") "llm"
    )

  , ( "earlier shorter prefix overlaps later longer prefix"
    , [ serviceProfileWithTrigger (ProfileName "a") (TriggerPrefix "l")
      , serviceProfileWithTrigger (ProfileName "b") (TriggerPrefix "llm")
      ]
    , TriggerPrefixOverlap (ProfileName "a") "l" (ProfileName "b") "llm"
    )

  , ( "later shorter prefix overlaps earlier longer prefix"
    , [ serviceProfileWithTrigger (ProfileName "a") (TriggerPrefix "llm")
      , serviceProfileWithTrigger (ProfileName "b") (TriggerPrefix "l")
      ]
    , TriggerPrefixOverlap (ProfileName "b") "l" (ProfileName "a") "llm"
    )

  , ( "multiple TriggerAll profiles conflict"
    , [ serviceProfileWithTrigger (ProfileName "a") TriggerAll
      , serviceProfileWithTrigger (ProfileName "b") TriggerAll
      ]
    , MultipleTriggerAll (ProfileName "a") (ProfileName "b")
    )

  , ( "TriggerAll overlaps a prefix trigger"
    , [ serviceProfileWithTrigger (ProfileName "all") TriggerAll
      , serviceProfileWithTrigger (ProfileName "p") (TriggerPrefix "llm")
      ]
    , TriggerAllOverlaps (ProfileName "all") (ProfileName "p") (TriggerPrefix "llm")
    )

  , ( "TriggerNone is ignored when checking conflicts"
    , [ serviceProfileWithTrigger (ProfileName "none") TriggerNone
      , serviceProfileWithTrigger (ProfileName "all") TriggerAll
      , serviceProfileWithTrigger (ProfileName "p") (TriggerPrefix "llm")
      ]
    , TriggerAllOverlaps (ProfileName "all") (ProfileName "p") (TriggerPrefix "llm")
    )

  , ( "plugin-style prefix overlap is rejected"
    , [ serviceProfileWithTrigger (ProfileName "default") (TriggerPrefix "llm")
      , serviceProfileWithTrigger (ProfileName "fast") (TriggerPrefix "llm_fast")
      ]
    , TriggerPrefixOverlap
        (ProfileName "default")
        "llm"
        (ProfileName "fast")
        "llm_fast"
    )
  ]



serviceProfileWithTrigger :: ProfileName -> TriggerPolicy -> Profile
serviceProfileWithTrigger name trigger = Profile
  { profName = name
  , profKind = ProfService ServiceProf
    { profService = ServiceName "ollama"
    , profModel = ModelName "qwen3:latest"
    , profTemplate = Nothing
    , profModelOptions = Nothing
    , profNumExpr = Nothing
    , profIncludeDocs = Nothing
    }
  , profTrigger = trigger
  }
