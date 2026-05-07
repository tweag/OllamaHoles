module GHC.Plugin.OllamaHoles.Data.Profile.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Profile.Parse.Spec qualified as ParseSpec
import GHC.Plugin.OllamaHoles.Data.Profile.Validate.Spec qualified as ValidateSpec



tests :: TestTree
tests = testGroup "Profile"
  [ ParseSpec.tests
  , ValidateSpec.tests
  ]
