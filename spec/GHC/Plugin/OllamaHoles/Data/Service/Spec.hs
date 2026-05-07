{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.Service.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Service.Parse.Spec qualified as ParseSpec


tests :: TestTree
tests = testGroup "Service"
  [ ParseSpec.tests
  ]
