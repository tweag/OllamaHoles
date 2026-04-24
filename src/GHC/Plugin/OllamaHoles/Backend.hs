{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Backend
  ( module GHC.Plugin.OllamaHoles.Backend.Common
  , module GHC.Plugin.OllamaHoles.Backend.Gemini
  , module GHC.Plugin.OllamaHoles.Backend.Ollama
  , module GHC.Plugin.OllamaHoles.Backend.OpenAI
  , BackendSlug(..)
  , parseBackendSlug
  , renderBackendSlug
  ) where

import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Backend.Common
import GHC.Plugin.OllamaHoles.Backend.Gemini
import GHC.Plugin.OllamaHoles.Backend.Ollama
import GHC.Plugin.OllamaHoles.Backend.OpenAI

data BackendSlug
  = Gemini
  | Ollama
  | OpenAI
  deriving (Eq, Show)

parseBackendSlug :: Text -> Maybe BackendSlug
parseBackendSlug = \case
  "gemini" -> Just Gemini
  "ollama" -> Just Ollama
  "openai" -> Just OpenAI
  _        -> Nothing

renderBackendSlug :: BackendSlug -> Text
renderBackendSlug = \case
  Gemini -> "gemini"
  Ollama -> "ollama"
  OpenAI -> "openai"
