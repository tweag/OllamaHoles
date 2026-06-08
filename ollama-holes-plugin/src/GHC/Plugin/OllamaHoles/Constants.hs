module GHC.Plugin.OllamaHoles.Constants where

import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Flags



pluginName :: Text
pluginName = "Ollama Plugin"



defaultModelName :: Text
defaultModelName = "qwen3:latest"

defaultBackendName :: BackendSlug
defaultBackendName = Ollama

defaultNumExpr :: Int
defaultNumExpr = 5

defaultDebug :: Bool
defaultDebug = True

defaultIncludeDocs :: Bool
defaultIncludeDocs = True

defaultConfigPath :: ConfigPathSpec
defaultConfigPath = ConfigDefault

defaultOpenAIBaseUrl :: Text
defaultOpenAIBaseUrl = "https://api.openai.com"

defaultOpenAIKeyName :: Text
defaultOpenAIKeyName = "OPENAI_API_KEY"

defaultTemplateSearchDir :: Text
defaultTemplateSearchDir = "."
