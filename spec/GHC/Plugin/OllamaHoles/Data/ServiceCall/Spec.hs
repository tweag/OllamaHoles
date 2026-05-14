module GHC.Plugin.OllamaHoles.Data.ServiceCall.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.ServiceCall.Route.Spec qualified as RouteSpec
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Submit.Spec qualified as SubmitSpec

tests :: TestTree
tests = testGroup "ServiceCall"
  [ RouteSpec.tests
  , SubmitSpec.tests
  ]
