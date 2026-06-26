module GHC.Plugin.OllamaHoles.Backend
  ( module GHC.Plugin.OllamaHoles.Backend.Common
  , module GHC.Plugin.OllamaHoles.Backend.Gemini
  , module GHC.Plugin.OllamaHoles.Backend.Ollama
  , module GHC.Plugin.OllamaHoles.Backend.OpenAI
  , module GHC.Plugin.OllamaHoles.Backend.Static
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
import GHC.Plugin.OllamaHoles.Backend.Static

data BackendSlug
  = Gemini
  | Ollama
  | OpenAI
  | Static
  deriving (Eq, Show)

parseBackendSlug :: Text -> Maybe BackendSlug
parseBackendSlug = \case
  "gemini" -> Just Gemini
  "ollama" -> Just Ollama
  "openai" -> Just OpenAI
  "static" -> Just Static
  _        -> Nothing

renderBackendSlug :: BackendSlug -> Text
renderBackendSlug = \case
  Gemini -> "gemini"
  Ollama -> "ollama"
  OpenAI -> "openai"
  Static -> "static"



data BackendConfig
  = SvcOllama OllamaConfig
  | SvcOpenAI OpenAIConfig
  | SvcGemini GeminiConfig
  | SvcStatic StaticConfig
  deriving (Eq, Ord, Show, Generic)

configureBackend :: BackendConfig -> Backend
configureBackend = \case
  SvcOllama cfg -> ollamaBackend cfg
  SvcOpenAI cfg -> openAICompatibleBackend cfg
  SvcGemini cfg -> geminiBackend cfg
  SvcStatic cfg -> staticBackend cfg
