module GHC.Plugin.OllamaHoles.Data.Prefs.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Prefs.Parse.Spec qualified as ParseSpec



tests :: TestTree
tests = testGroup "Prefs"
  [ ParseSpec.tests
  ]