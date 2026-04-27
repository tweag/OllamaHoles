{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Trigger
    ( TriggerPolicy(..)
    , TriggerPolicyError(..)
    , TriggerMatch(..)
    , defaultTriggerPolicy
    , parseTriggerPolicy
    , renderTriggerPolicy
    , shouldTriggerHole
    , matchTriggerPolicy
    , mkTriggeredHoleName
    ) where

import Data.Char (isAlphaNum, isLower)
import Data.Text (Text)
import Data.Text qualified as T



-- Trigger policies
-------------------

-- | A @TriggerPolicy@ determines which typed holes are
-- submitted to the LLM.
data TriggerPolicy
    = TriggerAll
    | TriggerNone
    | TriggerPrefix Text
    deriving (Eq, Show)

defaultTriggerPolicy :: TriggerPolicy
defaultTriggerPolicy = TriggerPrefix "llm"



-- Errors
---------

data TriggerPolicyError
    = EmptyTriggerPolicy
    | UnknownTriggerPolicy Text
    | MissingTriggerPrefix
    | InvalidTriggerPrefix Text
    deriving (Eq, Show)



-- Parsing / rendering
----------------------

-- | Parse a trigger policy from command-line text.
--
-- Accepted forms:
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
parseTriggerPolicy :: Text -> Either TriggerPolicyError TriggerPolicy
parseTriggerPolicy raw0 =
    let raw = T.strip raw0
    in case raw of
        "" ->
            Left EmptyTriggerPolicy

        "all" ->
            Right TriggerAll

        "none" ->
            Right TriggerNone

        _ -> case T.stripPrefix "prefix:" raw of
            Just suffix
                | T.null suffix ->
                    Left MissingTriggerPrefix
                | isValidTriggerPrefix suffix ->
                    Right (TriggerPrefix suffix)
                | otherwise ->
                    Left (InvalidTriggerPrefix suffix)

            Nothing ->
                Left (UnknownTriggerPolicy raw)

renderTriggerPolicy :: TriggerPolicy -> Text
renderTriggerPolicy = \case
    TriggerAll        -> "all"
    TriggerNone       -> "none"
    TriggerPrefix pfx -> "prefix:" <> pfx



-- Matching
-----------

-- | Result of matching a hole name against a trigger policy.
--
-- For prefix triggers, @tmSuffix@ is the portion of the hole name after
-- the triggering prefix, and may be the empty string.
--
-- Example:
--
--   * policy = @TriggerPrefix "foo"@
--   * hole   = @"_foo"@
--   * match  = @TriggerMatch "_foo" ""@
--
--   * policy = @TriggerPrefix "foo"@
--   * hole   = @"_foo1"@
--   * match  = @TriggerMatch "_foo1" "1"@
--
-- Keeping the empty suffix as @""@ preserves a bijection:
--
--   > mkTriggeredHoleName "foo" suffix == originalHoleName
--
data TriggerMatch = TriggerMatch
    { tmHoleName :: Text
    , tmSuffix   :: Text
    } deriving (Eq, Show)

shouldTriggerHole :: TriggerPolicy -> Text -> Bool
shouldTriggerHole pol holeName =
    case matchTriggerPolicy pol holeName of
        Nothing -> False
        Just _  -> True

matchTriggerPolicy :: TriggerPolicy -> Text -> Maybe TriggerMatch
matchTriggerPolicy pol holeName = case pol of
    TriggerNone -> Nothing
    TriggerAll -> Just TriggerMatch
        { tmHoleName = holeName
        , tmSuffix   = holeName
        }
    TriggerPrefix pfx -> matchPrefixTrigger pfx holeName

matchPrefixTrigger :: Text -> Text -> Maybe TriggerMatch
matchPrefixTrigger pfx holeName
    | T.null pfx = Nothing
    | otherwise = case T.stripPrefix ("_" <> pfx) holeName of
        Just suffix
            | isValidTriggerSuffix suffix ->
                Just TriggerMatch
                    { tmHoleName = holeName
                    , tmSuffix   = suffix
                    }
        _ -> Nothing

-- | Reconstruct a triggered hole name from a prefix and suffix.
--
-- This is the companion to @matchPrefixTrigger@.
mkTriggeredHoleName :: Text -> Text -> Text
mkTriggeredHoleName pfx suffix = "_" <> pfx <> suffix



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