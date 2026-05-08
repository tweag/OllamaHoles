module GHC.Plugin.OllamaHoles.Backend
  ( module GHC.Plugin.OllamaHoles.Backend.Common
  , module GHC.Plugin.OllamaHoles.Backend.Gemini
  , module GHC.Plugin.OllamaHoles.Backend.Ollama
  , module GHC.Plugin.OllamaHoles.Backend.OpenAI
  , BackendSlug(..)
  , parseBackendSlug
  , renderBackendSlug
  , BackendConfig(..)
  , configureBackend
  ) where

import GHC.Generics (Generic)
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



data BackendConfig
  = SvcOllama OllamaConfig
  | SvcOpenAI OpenAIConfig
  | SvcGemini GeminiConfig
  deriving (Eq, Show, Generic)

configureBackend :: BackendConfig -> Backend
configureBackend = \case
  SvcOllama cfg -> ollamaBackend cfg
  SvcOpenAI cfg -> openAICompatibleBackend cfg
  SvcGemini cfg -> geminiBackend cfg
