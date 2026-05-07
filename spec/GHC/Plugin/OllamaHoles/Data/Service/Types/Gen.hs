{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.Service.Types.Gen
  ( genServiceNameText
  , genHostText
  , genEnvVarText
  , genUrlText
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Test.Tasty.QuickCheck qualified as QC



genServiceNameText :: QC.Gen Text
genServiceNameText =
  T.pack <$> genIdentLikeString


genHostText :: QC.Gen Text
genHostText = do
  port <- QC.chooseInt (1024, 49151)
  pure ("http://localhost:" <> T.pack (show port))


genUrlText :: QC.Gen Text
genUrlText = do
  name <- genServiceNameText
  pure ("https://" <> name <> ".example.com/v1")


genEnvVarText :: QC.Gen Text
genEnvVarText = do
  pieces <- QC.listOf1 (QC.elements ["OPENAI", "GEMINI", "LOCAL", "TEST", "API", "KEY"])
  pure (T.intercalate "_" pieces)


genIdentLikeString :: QC.Gen String
genIdentLikeString = do
  first <- QC.elements (['a' .. 'z'] <> ['A' .. 'Z'])
  rest <- QC.listOf (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_"))
  pure (first : rest)
