module GHC.Plugin.OllamaHoles.Data.Trigger.Match
  ( shouldTriggerHole
  , matchTriggerPolicy
  ) where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Trigger.Types




shouldTriggerHole :: TriggerPolicy -> Text -> Bool
shouldTriggerHole pol holeName =
    case matchTriggerPolicy pol holeName of
        Nothing -> False
        Just _  -> True

matchTriggerPolicy :: TriggerPolicy -> Text -> Maybe TriggerMatch
matchTriggerPolicy pol holeName = case pol of
    TriggerNone -> Nothing
    TriggerAll -> Just $ TriggerMatchAll holeName
    TriggerPrefix pfx -> matchPrefixTrigger pfx holeName

matchPrefixTrigger :: Text -> Text -> Maybe TriggerMatch
matchPrefixTrigger pfx holeName
    | T.null pfx = Nothing
    | otherwise = case T.stripPrefix ("_" <> pfx) holeName of
        Just suffix
            | isValidTriggerSuffix suffix -> Just $
                TriggerMatchPrefix holeName (MatchedPrefix pfx) (MatchedSuffix suffix)
        _ -> Nothing
  where
    isValidTriggerSuffix = T.all isIdentifierContinue
    isIdentifierContinue c = isAlphaNum c || c == '_' || c == '\''
