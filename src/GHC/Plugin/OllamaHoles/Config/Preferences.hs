{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Config.Preferences where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Toml (Result(..), decode)
import Toml.Schema (FromValue(..), reqKey, parseTableFromValue)

import GHC.Plugin.OllamaHoles.Config.Types



-- | @Preferences@ is a raw representation of the user's
-- config file. Internal references have not yet been
-- resolved. The configuration includes
--   - services: backends and how to communicate with them
--   - profiles: what to do with a typed hole
data Preferences = Preferences
  { prefServices :: [Service]
  , prefProfiles :: [Profile]
  } deriving (Eq, Show, Generic)

instance FromValue Preferences where
  fromValue = parseTableFromValue $
    Preferences
      <$> reqKey "services"
      <*> reqKey "profiles"



data TomlParseResult a
  = TomlParseFailure [Text]
  | TomlParseSuccess [Text] a
  deriving (Eq, Show)

parsePreferencesToml :: Text -> TomlParseResult Preferences
parsePreferencesToml input =
  case decode input of
    Failure errs ->
      TomlParseFailure (fmap T.pack errs)
    Success warn prefs ->
      TomlParseSuccess (fmap T.pack warn) prefs

