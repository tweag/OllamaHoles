{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | The locally hosted ollama backend
module GHC.Plugin.OllamaHoles.Backend.Ollama
  ( OllamaConfig(..)
  , ollamaBackend
  ) where

import GHC.Generics (Generic)
import Data.Aeson (Value, FromJSON, Value(..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseMaybe, parseJSON)
import Data.Text (Text)

import Ollama (GenerateOps (..))
import Ollama qualified

import GHC.Plugin.OllamaHoles.Backend.Common



data OllamaConfig = OllamaConfig
  { svcOllamaHost :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)



-- | The locally hosted ollama backend
ollamaBackend :: OllamaConfig -> Backend
ollamaBackend conf = Backend
  { listModels = listOllamaModels conf
  , generateFits = generateOllamaFits conf
  }

listOllamaModels :: OllamaConfig -> IO (Maybe [Text])
listOllamaModels conf = do
  listAll <- do
#if MIN_VERSION_ollama_haskell(0,2,0)
    let conf' = parseOllamaConfig conf
    result <- Ollama.list (Just conf')
    pure $ case result of
      Left _ -> Nothing
      Right ok -> Just ok
#else
    Ollama.list
#endif
  let getMs (Ollama.Models models) = fmap Ollama.name models
  pure $ fmap getMs listAll

#if MIN_VERSION_ollama_haskell(0,2,0)
parseOllamaConfig :: OllamaConfig -> Ollama.OllamaConfig
parseOllamaConfig conf = setHost Ollama.defaultOllamaConfig
  where
    setHost :: Ollama.OllamaConfig -> Ollama.OllamaConfig
    setHost x = case svcOllamaHost conf of
      Nothing -> x
      Just hs -> x { Ollama.hostUrl = hs }
#endif

generateOllamaFits
  :: OllamaConfig -> Text -> Text -> Maybe Value -> IO (Either String Text)
generateOllamaFits conf prompt modelName options = do
  let ops = Ollama.defaultGenerateOps
        { prompt = prompt
        , modelName = modelName
        }
#if MIN_VERSION_ollama_haskell(0,2,0)
  let
    parseModelOptions :: Value -> Ollama.ModelOptions
    parseModelOptions v = Ollama.ModelOptions
      { numKeep = extractAtKey "num_keep" v
      , seed = extractAtKey "seed" v
      , numPredict = extractAtKey "num_predict" v
      , topK = extractAtKey "top_k" v
      , topP = extractAtKey "top_p" v
      , minP = extractAtKey "min_p" v
      , typicalP = extractAtKey "typical_p" v
      , repeatLastN = extractAtKey "repeat_last_n" v
      , temperature = extractAtKey "temperature" v
      , repeatPenalty = extractAtKey "repeat_penalty" v
      , presencePenalty = extractAtKey "presence_penalty" v
      , frequencyPenalty = extractAtKey "frequency_penalty" v
      , penalizeNewline = extractAtKey "penalize_newline" v
      , stop = extractAtKey "stop" v
      , numa = extractAtKey "numa" v
      , numCtx = extractAtKey "num_ctx" v
      , numBatch = extractAtKey "num_batch" v
      , numGpu = extractAtKey "num_gpu" v
      , mainGpu = extractAtKey "main_gpu" v
      , useMmap = extractAtKey "use_mmap" v
      , numThread = extractAtKey "num_thread" v
      }

    ops' = ops { options = fmap parseModelOptions options }
  result <- fmap Ollama.genResponse <$> Ollama.generate ops' Nothing
  pure $ case result of
    Right ok -> Right ok
    Left err -> Left $ show err
#else
  let ops' = ops { options = options }
  fmap Ollama.response_ <$> Ollama.generate ops'
#endif



extractAtKey :: FromJSON a => String -> Value -> Maybe a
extractAtKey key = \case
  Object obj -> KeyMap.lookup (Key.fromString key) obj
    >>= parseMaybe parseJSON
  _ -> Nothing
