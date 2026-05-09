module GHC.Plugin.OllamaHoles.Data.Trigger.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Trigger.Parse.Spec qualified as ParseSpec
import GHC.Plugin.OllamaHoles.Data.Trigger.Match.Spec qualified as MatchSpec



tests :: TestTree
tests = testGroup "Trigger"
  [ ParseSpec.tests
  , MatchSpec.tests
  ]
