{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Backend.Static.Spec (tests) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp (withSystemTempDirectory)

import GHC.Plugin.OllamaHoles.Backend.Common
import GHC.Plugin.OllamaHoles.Backend.Static

tests :: TestTree
tests = testGroup "GHC.Plugin.OllamaHoles.Backend.Static"
  [ testCase "returns inline static response" testInlineResponse
  , testCase "returns file static response" testFileResponse
  , testCase "lists static model" testListModels
  ]

testInlineResponse :: Assertion
testInlineResponse = do
  let
    backend = staticBackend $ StaticConfig
      { svcStaticResponse = StaticInline "candidate one\ncandidate two"
      }

  result <- generateFits backend "ignored prompt" "ignored model" Nothing
  result @?= Right "candidate one\ncandidate two"

testFileResponse :: Assertion
testFileResponse =
  withSystemTempDirectory "ollama-holes-static" $ \dir -> do
    let path = dir </> "candidates.txt"

    T.writeFile path $ T.unlines
      [ "UserId <$> readMaybe s"
      , "Just (UserId (read s))"
      ]

    let
      backend = staticBackend $ StaticConfig
        { svcStaticResponse = StaticFile path
        }

    result <- generateFits backend "ignored prompt" "ignored model" Nothing

    result @?=
      Right (T.unlines
        [ "UserId <$> readMaybe s"
        , "Just (UserId (read s))"
        ])

testListModels :: Assertion
testListModels = do
  let
    backend = staticBackend $ StaticConfig
      { svcStaticResponse = StaticInline "x"
      }

  models <- listModels backend
  models @?= Just ["static"]