{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Plugin.OllamaHoles.Data.Service.Parse
  ( tomlService
  , tomlServiceName
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Toml.Schema
  (ParseTable, FromValue(..), optKey, parseTableFromValue, reqKey)
import Toml.Schema qualified as Toml

import GHC.Plugin.OllamaHoles.Backend
  (OpenAIConfig(..), GeminiConfig(..), OllamaConfig(..), BackendConfig(..))

import GHC.Plugin.OllamaHoles.Data.Service.Types



tomlServiceName :: Toml.Value' l -> Toml.Matcher l ServiceName
tomlServiceName = fmap ServiceName . fromValue



tomlService :: Toml.Value' l -> Toml.Matcher l Service
tomlService = parseTableFromValue $ do
  svcName <- Toml.reqKeyOf "name" tomlServiceName
  svcConfig <- reqKey "protocol" >>= tomlBackendConfigFor
  pure Service {..}

tomlBackendConfigFor :: Text -> ParseTable l BackendConfig
tomlBackendConfigFor = \case
  "ollama" -> SvcOllama
    <$> (OllamaConfig
      <$> optKey "host")

  "openai" -> SvcOpenAI
    <$> (OpenAIConfig
      <$> reqKey "base_url"
      <*> reqKey "key_name")

  "openai-compatible" -> SvcOpenAI
    <$> (OpenAIConfig
      <$> reqKey "base_url"
      <*> reqKey "key_name")

  "gemini" -> SvcGemini
    <$> (GeminiConfig
      <$> reqKey "key_name")

  bad ->
    fail ("invalid service protocol: " <> T.unpack bad)
