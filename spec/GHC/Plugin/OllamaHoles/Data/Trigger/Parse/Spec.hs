module GHC.Plugin.OllamaHoles.Data.Trigger.Parse.Spec (tests) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Trigger.Types.Gen



tests :: TestTree
tests = testGroup "Service.Parse"
  [ tests_parseTriggerPolicy_unit
  , tests_parseTriggerPolicy_prop
  ]



tests_parseTriggerPolicy_unit :: TestTree
tests_parseTriggerPolicy_unit = testGroup "parseTriggerPolicy (unit)"
  [ testGroup "success" $
      tests_parseTriggerPolicy_unit_success <&> \(name, input, expected) ->
        testCase name $ parseTriggerPolicy input @?= Right expected

  , testGroup "failure" $
      tests_parseTriggerPolicy_unit_failure <&> \(name, input) ->
        testCase name $ case parseTriggerPolicy input of
          Left _ -> pure ()
          Right ok -> assertFailure $ "Expected parse failure but got " <> show ok
  ]

tests_parseTriggerPolicy_prop :: TestTree
tests_parseTriggerPolicy_prop = testGroup "parseTriggerPolicy (unit)"
  [ QC.testProperty "render/parse roundtrip for valid policies" $
    QC.forAll genTriggerPolicy $ \pol ->
      parseTriggerPolicy (renderTriggerPolicy pol) QC.=== Right pol

  , QC.testProperty "valid prefix policies parse successfully" $
    QC.forAll genValidPrefix $ \pfx ->
      parseTriggerPolicy ("prefix:" <> pfx) QC.=== Right (TriggerPrefix pfx)
  ]



tests_parseTriggerPolicy_unit_success
  :: [(String, Text, TriggerPolicy)]
tests_parseTriggerPolicy_unit_success =
  [ ( "parses 'all'"
    , "all"
    , TriggerAll
    )

  , ( "parses 'none'"
    , "none"
    , TriggerNone
    )

  , ( "parses prefix policy"
    , "prefix:foo"
    , TriggerPrefix "foo"
    )

  , ( "trims outer whitespace"
    , "   prefix:foo   "
    , TriggerPrefix "foo"
    )

  , ( "prefix with nonleading digit"
    , "prefix:foo1"
    , TriggerPrefix "foo1"
    )

  , ( "prefix with nonleading underscore"
    , "prefix:foo_bar"
    , TriggerPrefix "foo_bar"
    )

  , ( "prefix with trailing underscore"
    , "prefix:foo_"
    , TriggerPrefix "foo_"
    )

  , ( "prefix with nonleading apostrophe"
    , "prefix:foo'bar"
    , TriggerPrefix "foo'bar"
    )

  , ( "prefix with trailing apostrophe"
    , "prefix:foo'"
    , TriggerPrefix "foo'"
    )
  ]

tests_parseTriggerPolicy_unit_failure
  :: [(String, Text)]
tests_parseTriggerPolicy_unit_failure =
  [ ( "empty policy"
    , ""
    )

  , ( "unknown policy"
    , "wat"
    )

  , ( "missing prefix"
    , "prefix:"
    )

  , ( "leading underscore in prefix"
    , "prefix:_foo"
    )

  , ( "leading uppercase char in prefix"
    , "prefix:Foo"
    )

  , ( "leading digit in prefix"
    , "prefix:1foo"
    )

  , ( "hyphen in prefix"
    , "prefix:foo-bar"
    )

  , ( "slash in prefix"
    , "prefix:foo/bar"
    )

  , ( "dot in prefix"
    , "prefix:foo.bar"
    )
  ]
