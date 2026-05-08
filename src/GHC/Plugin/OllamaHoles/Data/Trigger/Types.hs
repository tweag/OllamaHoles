module GHC.Plugin.OllamaHoles.Data.Trigger.Types
  ( TriggerPolicy(..)
  , defaultTriggerPolicy
  , renderTriggerPolicy
  , isActiveTrigger
  , TriggerMatch(..)
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
--   > mkTriggeredHoleName "foo" suffix == originalHoleName
data TriggerMatch = TriggerMatch
  { tmHoleName :: HoleName
  , tmSuffix   :: Maybe Text
  } deriving (Eq, Show)

type HoleName = Text
