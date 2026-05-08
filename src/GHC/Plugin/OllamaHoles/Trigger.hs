module GHC.Plugin.OllamaHoles.Trigger
    ( mkTriggeredHoleName
    ) where

import Data.Char (isAlphaNum, isLower)
import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Parse
import GHC.Plugin.OllamaHoles.Data.Trigger.Error





-- | Reconstruct a triggered hole name from a prefix and suffix.
--
-- This is the companion to @matchPrefixTrigger@.
mkTriggeredHoleName :: Text -> Text -> Text
mkTriggeredHoleName pfx suffix = "_" <> pfx <> suffix
