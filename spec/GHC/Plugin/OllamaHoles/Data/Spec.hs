module GHC.Plugin.OllamaHoles.Data.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Service.Spec qualified as ServiceSpec
import GHC.Plugin.OllamaHoles.Data.Profile.Spec qualified as ProfileSpec
import GHC.Plugin.OllamaHoles.Data.Prefs.Spec qualified as PrefsSpec



tests :: TestTree
tests = testGroup "GHC.Plugin.OllamaHoles.Data"
  [ ServiceSpec.tests
  , ProfileSpec.tests
  , PrefsSpec.tests
  ]
