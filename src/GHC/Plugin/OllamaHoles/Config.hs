module GHC.Plugin.OllamaHoles.Config where

import Data.Aeson (Value)
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import GHC.Plugin.OllamaHoles.Config.Types
import GHC.Plugin.OllamaHoles.Trigger
  ( TriggerPolicy(..)
  )



data Config = Config
  { cfgServices :: Map ServiceName Service
  , cfgProfiles :: Map ProfileName Profile
  } deriving (Eq, Show, Generic)
