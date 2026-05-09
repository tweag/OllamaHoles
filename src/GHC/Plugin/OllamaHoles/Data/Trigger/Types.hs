module GHC.Plugin.OllamaHoles.Data.Trigger.Types
  ( TriggerPolicy(..)
  , defaultTriggerPolicy
  , renderTriggerPolicy
  , isActiveTrigger
  , TriggerMatch(..)
  , MatchedPrefix(..)
  , MatchedSuffix(..)
  , HoleName
  ) where

import Data.Text (Text)



-- | A @TriggerPolicy@ determines which typed
-- holes are submitted to the LLM.
data TriggerPolicy
  = TriggerAll
  | TriggerNone
  | TriggerPrefix Text
  deriving (Eq, Show)

defaultTriggerPolicy :: TriggerPolicy
defaultTriggerPolicy = TriggerPrefix "llm"

renderTriggerPolicy :: TriggerPolicy -> Text
renderTriggerPolicy = \case
  TriggerAll        -> "all"
  TriggerNone       -> "none"
  TriggerPrefix pfx -> "prefix:" <> pfx

isActiveTrigger :: TriggerPolicy -> Bool
isActiveTrigger trigger = case trigger of
  TriggerNone -> False
  TriggerAll -> True
  TriggerPrefix _ -> True



-- | Result of matching a hole name against a trigger policy.
--
-- Examples:
--
--   * policy = @TriggerAll@
--   * hole   = @"_foobar"@
--   * match  = @TriggerMatchAll "_foobar"@
--
--   * policy = @TriggerPrefix "foo"@
--   * hole   = @"_foo"@
--   * match  = @TriggerMatchPrefix "_foo" "foo" ""@
--
--   * policy = @TriggerPrefix "foo"@
--   * hole   = @"_foo1"@
--   * match  = @TriggerMatchPrefix "_foo1" "foo" "1"@
data TriggerMatch
  = TriggerMatchAll HoleName
  | TriggerMatchPrefix HoleName MatchedPrefix MatchedSuffix
  deriving (Eq, Show)

newtype MatchedPrefix = MatchedPrefix
  { unMatchedPrefix :: Text
  } deriving (Eq, Show)

newtype MatchedSuffix = MatchedSuffix
  { unMatchedSuffix :: Text
  } deriving (Eq, Show)

type HoleName = Text
