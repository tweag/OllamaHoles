{-# LANGUAGE RecordWildCards #-}

module GHC.Plugin.OllamaHoles.Backend.Static
  ( StaticConfig(..)
  , StaticResponse(..)
  , staticBackend
  ) where

import GHC.Generics (Generic)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Maybe (fromMaybe)

import GHC.Plugin.OllamaHoles.Backend.Common

data StaticConfig = StaticConfig
  { svcStaticResponse :: StaticResponse
  } deriving (Eq, Ord, Show, Generic)

data StaticResponse
  = StaticInline Text
  | StaticFile FilePath
  deriving (Eq, Ord, Show)

staticBackend :: StaticConfig -> Backend
staticBackend StaticConfig{..} = Backend{..}
  where
    listModels :: IO (Maybe [Text])
    listModels = pure $ Just ["static"]

    generateFits :: Text -> Text -> Maybe a -> IO (Either String Text)
    generateFits _prompt _modelName _options =
      case svcStaticResponse of
        StaticInline response ->
          pure $ Right response

        StaticFile path ->
          Right <$> T.readFile path
