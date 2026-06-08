module GHC.Plugin.OllamaHoles.Data.Prefs.Parse
  ( parsePreferencesToml
  , TomlParseError(..)
  , tomlPreferences
  ) where

import Data.Text (Text)
import Toml qualified as Toml
import Toml.Schema qualified as Toml
import Toml.Syntax.Position qualified as Toml

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile

import GHC.Plugin.OllamaHoles.Data.Prefs.Types



tomlPreferences :: Toml.Value' l -> Toml.Matcher l Preferences
tomlPreferences = Toml.parseTableFromValue $ Preferences
  <$> Toml.reqKeyOf "services" (Toml.listOf $ const tomlService)
  <*> Toml.reqKeyOf "profiles" (Toml.listOf $ const tomlProfile)

data TomlParseError
  = TomlReadError String
  | TomlParseFailure [Toml.MatchMessage Toml.Position]
  | TomlParseWarning [Toml.MatchMessage Toml.Position]
  deriving (Eq, Show)

parsePreferencesToml
  :: Text -> Either TomlParseError Preferences
parsePreferencesToml input = do
  case fmap (Toml.Table' Toml.startPos) $ Toml.parse input of
    Left err -> Left $ TomlReadError err
    Right result -> case Toml.runMatcher (tomlPreferences result) of
      Toml.Failure errs -> Left $ TomlParseFailure errs
      Toml.Success warns prefs -> if not $ null warns
        then Left $ TomlParseWarning warns
        else Right prefs
