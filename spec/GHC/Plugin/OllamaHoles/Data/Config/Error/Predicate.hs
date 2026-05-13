module GHC.Plugin.OllamaHoles.Data.Config.Error.Predicate
  ( isConfigParseError
  , isCyclicProfileError
  , isAmbiguousTriggerError
  ) where

import GHC.Plugin.OllamaHoles.Data.Config



isConfigParseError :: Either ConfigError Config -> Bool
isConfigParseError = \case
  Left (ConfigParseErrors _ _) -> True
  _ -> False

isCyclicProfileError :: Either ConfigError Config -> Bool
isCyclicProfileError = \case
  Left (CyclicProfileReference _) -> True
  _ -> False

isAmbiguousTriggerError :: Either ConfigError Config -> Bool
isAmbiguousTriggerError = \case
  Left (AmbiguousProfileTriggers _) -> True
  _ -> False
