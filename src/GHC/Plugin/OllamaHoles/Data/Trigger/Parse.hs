{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.Trigger.Parse
  ( parseTriggerPolicy
  ) where

import Data.Char (isLower, isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Error



-- | Parse a trigger policy. Accepted forms:
--
--   * @"all"@
--   * @"none"@
--   * @"prefix:<ident>"@
--
-- The prefix does /not/ include the leading underscore. For example:
--
--   * @"prefix:llm"@ matches holes like @_llm@, @_llm1@, @_llmFoo@
--
-- The prefix must be a valid Haskell variable-identifier fragment after the
-- leading underscore:
--
--   * non-empty
--   * first character must be a lowercase identifier-start character
--   * remaining characters may be identifier-continue characters
--
-- This parser is intentionally conservative and focuses on ordinary Haskell
-- identifiers used for named typed holes.
--
parseTriggerPolicy :: Text -> Either TriggerPolicyError TriggerPolicy
parseTriggerPolicy raw0 =
  let raw = T.strip raw0
  in case raw of
    "" -> Left EmptyTriggerPolicy
    "all" -> Right TriggerAll
    "none" -> Right TriggerNone
    _ -> case T.stripPrefix "prefix:" raw of
      Nothing -> Left (UnknownTriggerPolicy raw)
      Just suffix
        | T.null suffix -> Left MissingTriggerPrefix
        | isValidTriggerPrefix suffix -> Right (TriggerPrefix suffix)
        | otherwise -> Left (InvalidTriggerPrefix suffix)



-- Identifier validity
----------------------

-- | Prefixes are the hole name without the leading underscore.
--
-- We require:
--
--   * non-empty
--   * no leading underscore
--   * first character is a lowercase identifier-start character
--   * remaining characters are identifier-continue characters
--
isValidTriggerPrefix :: Text -> Bool
isValidTriggerPrefix txt = case T.uncons txt of
    Nothing -> False
    Just (c0, rest) ->
        isIdentifierStartNoUnderscore c0
            && T.all isIdentifierContinue rest

-- | The suffix after the prefix may be empty, or any sequence of
-- identifier-continue characters.
--
-- This lets @_foo@ and @_foodefault@ remain distinct:
--
--   * @_foo@        has suffix @""
--   * @_foodefault@ has suffix @"default"@
--
isValidTriggerSuffix :: Text -> Bool
isValidTriggerSuffix = T.all isIdentifierContinue

isIdentifierStartNoUnderscore :: Char -> Bool
isIdentifierStartNoUnderscore c = isLower c

isIdentifierContinue :: Char -> Bool
isIdentifierContinue c =
       isAlphaNum c
    || c == '_'
    || c == '\''
