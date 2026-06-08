module GHC.Plugin.OllamaHoles.Data.Flags.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Flags.Parse.Spec qualified as ParseSpec



tests :: TestTree
tests = testGroup "Flags"
  [ ParseSpec.tests
  ]
