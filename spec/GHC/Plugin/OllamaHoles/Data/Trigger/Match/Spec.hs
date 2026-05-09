module GHC.Plugin.OllamaHoles.Data.Trigger.Match.Spec (tests) where

import Data.Functor ((<&>))
import Data.Maybe (isNothing, isJust)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Types.Gen
import GHC.Plugin.OllamaHoles.Data.Trigger.Match



tests :: TestTree
tests = testGroup "Service.Parse"
  [ tests_matchTriggerPolicy_unit
  , tests_matchTriggerPolicy_prop
  ]



tests_matchTriggerPolicy_unit :: TestTree
tests_matchTriggerPolicy_unit = testGroup "matchTriggerPolicy (unit)"
  [ testGroup "success" $
      tests_matchTriggerPolicy_unit_success <&> \(name, (policy, hole), expected) ->
        testCase name $ matchTriggerPolicy policy hole @?= Just expected

  , testGroup "failure" $
      tests_matchTriggerPolicy_unit_failure <&> \(name, (policy, hole)) ->
        testCase name $ case matchTriggerPolicy policy hole of
          Nothing -> pure ()
          Just ok -> assertFailure $ "Expected match failure but got " <> show ok
  ]

tests_matchTriggerPolicy_prop :: TestTree
tests_matchTriggerPolicy_prop = testGroup "matchTriggerPolicy (prop)"
  [ QC.testProperty "TriggerAll always matches any generated hole name" $
    QC.forAll genHoleName $ \nm ->
      matchTriggerPolicy TriggerAll nm QC.=== Just (TriggerMatchAll nm)

  , QC.testProperty "TriggerNone never matches any generated hole name" $
    QC.forAll genHoleName $ \nm ->
      isNothing (matchTriggerPolicy TriggerNone nm)

  , QC.testProperty "constructed triggered names always trigger for same prefix" $
    QC.forAll genValidPrefix $ \pfx ->
    QC.forAll genValidSuffix $ \sfx ->
      isJust $ matchTriggerPolicy (TriggerPrefix pfx) ("_" <> pfx <> sfx)
  ]



tests_matchTriggerPolicy_unit_success
  :: [(String, (TriggerPolicy, Text), TriggerMatch)]
tests_matchTriggerPolicy_unit_success =
  [ ( "TriggerAll matches anything"
    , ( TriggerAll
      , "_anything"
      )
    , TriggerMatchAll "_anything"
    )

  , ( "prefix match succeeds on exact prefix hole"
    , ( TriggerPrefix "foo"
      , "_foo"
      )
    , TriggerMatchPrefix "_foo" (MatchedPrefix "foo") (MatchedSuffix "")
    )

  , ( "prefix match succeeds with numeric suffix"
    , ( TriggerPrefix "foo"
      , "_foo1"
      )
    , TriggerMatchPrefix "_foo1" (MatchedPrefix "foo") (MatchedSuffix "1")
    )

  , ( "prefix match succeeds with alphabetic suffix"
    , ( TriggerPrefix "foo"
      , "_foodef"
      )
    , TriggerMatchPrefix "_foodef" (MatchedPrefix "foo") (MatchedSuffix "def")
    )

  , ( "prefix match succeeds with leading underscore in suffix"
    , ( TriggerPrefix "foo"
      , "_foo_bar"
      )
    , TriggerMatchPrefix "_foo_bar" (MatchedPrefix "foo") (MatchedSuffix "_bar")
    )

  , ( "prefix match succeeds with leading apostrophe in suffix"
    , ( TriggerPrefix "foo"
      , "_foo'bar"
      )
    , TriggerMatchPrefix "_foo'bar" (MatchedPrefix "foo") (MatchedSuffix "'bar")
    )

  -- Default Policy
  -----------------

  , ( "default trigger policy matches _llm"
    , ( defaultTriggerPolicy
      , "_llm"
      )
    , TriggerMatchPrefix "_llm" (MatchedPrefix "llm") (MatchedSuffix "")
    )

  , ( "default trigger policy matches _llm1"
    , ( defaultTriggerPolicy
      , "_llm1"
      )
    , TriggerMatchPrefix "_llm1" (MatchedPrefix "llm") (MatchedSuffix "1")
    )
  ]

tests_matchTriggerPolicy_unit_failure
  :: [(String, (TriggerPolicy, Text))]
tests_matchTriggerPolicy_unit_failure =
  [ ( "TriggerNone never matches"
    , ( TriggerNone
      , "_anything"
      )
    )

  , ( "prefix match rejects missing leading underscore"
    , ( TriggerPrefix "foo"
      , "foo"
      )
    )

  , ( "prefix match rejects wrong prefix"
    , ( TriggerPrefix "foo"
      , "_bar"
      )
    )

  , ( "prefix match rejects invalid suffix char"
    , ( TriggerPrefix "foo"
      , "_foo-bar"
      )
    )

  , ( "prefix match rejects empty prefix policy at match time"
    , ( TriggerPrefix ""
      , "_anything"
      )
    )

  -- Default Policy
  -----------------

  , ( "default policy rejects plain holes"
    , ( defaultTriggerPolicy
      , "_"
      )
    )
  ]
