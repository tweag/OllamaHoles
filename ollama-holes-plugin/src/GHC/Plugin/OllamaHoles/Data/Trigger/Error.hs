module GHC.Plugin.OllamaHoles.Data.Trigger.Error
  ( TriggerPolicyError(..)
  ) where

import Data.Text (Text)



data TriggerPolicyError
    = EmptyTriggerPolicy
    | UnknownTriggerPolicy Text
    | MissingTriggerPrefix
    | InvalidTriggerPrefix Text
    deriving (Eq, Show)
