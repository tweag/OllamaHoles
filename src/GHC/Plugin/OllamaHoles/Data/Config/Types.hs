module GHC.Plugin.OllamaHoles.Data.Config.Types where

import Data.Aeson (Value)
import Data.Map (Map)
import GHC.Generics (Generic)

import GHC.Plugin.OllamaHoles.Data.Trigger.Types (TriggerPolicy)
import GHC.Plugin.OllamaHoles.Data.Template (TemplateSource)

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile



data Config
  = ConfigSimple SimpleConfig
  | ConfigFancy FancyConfig
  deriving (Eq, Show, Generic)

data SimpleConfig = SimpleConfig
  { simpleTrigger :: TriggerPolicy
  , simpleService :: Service
  , simpleProfile :: ServiceProf
  } deriving (Eq, Show, Generic)

data FancyConfig = FancyConfig
  { cfgServices :: Map ServiceName Service
  , cfgProfiles :: Map ProfileName Profile
  , cfgExtras   :: Maybe ExtraConfig
  } deriving (Eq, Show, Generic)

-- From command line arguments
data ExtraConfig
  = ConfigOverlay SimpleConfig
  | ConfigOverride OverrideConfig
  deriving (Eq, Show, Generic)

data OverrideConfig = OverrideConfig
  { overrideModelName     :: Maybe ModelName
  , overrideNumExpr       :: Maybe Int
  , overrideIncludeDocs   :: Maybe Bool
  , overrideModelOptions  :: Maybe Value
  , overrideTemplate      :: Maybe TemplateSource
  } deriving (Eq, Show, Generic)
