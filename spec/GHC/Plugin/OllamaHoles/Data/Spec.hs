module GHC.Plugin.OllamaHoles.Data.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Service.Spec qualified as ServiceSpec



tests :: TestTree
tests = testGroup "GHC.Plugin.OllamaHoles.Data"
  [ ServiceSpec.tests
  ]
