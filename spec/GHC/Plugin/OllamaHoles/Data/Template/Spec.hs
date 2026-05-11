module GHC.Plugin.OllamaHoles.Data.Template.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Template.Parse.Spec qualified as ParseSpec
import GHC.Plugin.OllamaHoles.Data.Template.Load.Spec qualified as LoadSpec
import GHC.Plugin.OllamaHoles.Data.Template.Expand.Spec qualified as ExpandSpec



tests :: TestTree
tests = testGroup "Template"
  [ ParseSpec.tests
  , LoadSpec.tests
  , ExpandSpec.tests
  ]
