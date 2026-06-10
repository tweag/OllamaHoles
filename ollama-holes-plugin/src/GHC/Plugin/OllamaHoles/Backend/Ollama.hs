{-# LANGUAGE RecordWildCards #-}

-- | The locally hosted ollama backend
module GHC.Plugin.OllamaHoles.Backend.Ollama
  ( OllamaConfig(..)
  , ollamaBackend
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)

import Ollama (GenerateOps (..))
import Ollama qualified

import GHC.Plugin.OllamaHoles.Backend.Common



data OllamaConfig = OllamaConfig
  { svcOllamaHost :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)



-- | The locally hosted ollama backend
ollamaBackend :: OllamaConfig -> Backend
ollamaBackend _ = Backend{..}
  where
    listModels = fmap getMs <$> Ollama.list
    getMs (Ollama.Models models) = fmap Ollama.name models
    generateFits prompt modelName options = do
        fmap Ollama.response_ <$> Ollama.generate Ollama.defaultGenerateOps{prompt = prompt,
                                                                            modelName = modelName,
                                                                            options = options}

