module GHC.Plugin.OllamaHoles.Data.Profile.Error where

import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Data.Service.Types
import GHC.Plugin.OllamaHoles.Data.Profile.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Types



data TriggerConflict
  = DuplicateTriggerPrefix ProfileName ProfileName Text
  | TriggerPrefixOverlap ProfileName Text ProfileName Text
  | MultipleTriggerAll ProfileName ProfileName
  | TriggerAllOverlaps ProfileName ProfileName TriggerPolicy
  deriving (Eq, Show)

data ProfileRouteError
  = AmbiguousTriggeredProfiles Text [(ProfileName, TriggerPolicy)]
  deriving (Eq, Show)

data ProfileSubmitError
  = SubmitUnknownProfile ProfileName
  | SubmitUnknownService ProfileName ServiceName
  | SubmitBackendFailed ProfileName ServiceName Text
  deriving (Eq, Show)
