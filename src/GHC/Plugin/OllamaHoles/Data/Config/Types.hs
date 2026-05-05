{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Data.Config.Types where

import Data.Aeson (Value)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import GHC.Plugin.OllamaHoles.Data.Trigger.Types (TriggerPolicy)
import GHC.Plugin.OllamaHoles.Template (Template)

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile



data Config
  = ConfigSimple ServiceProf
  | ConfigFancy FancyConfig
  deriving (Eq, Show, Generic)

data FancyConfig = FancyConfig
  { cfgServices :: Map ServiceName Service
  , cfgProfiles :: Map ProfileName Profile
  , cfgExtras   :: Maybe ExtraConfig
  } deriving (Eq, Show, Generic)

-- From command line arguments
data ExtraConfig
  = ConfigOverlay ServiceProf
  | ConfigOverride OverrideConfig
  deriving (Eq, Show, Generic)

data OverrideConfig = OverrideConfig
  { overrideModelName     :: Maybe Text
  , overrideNumExpr       :: Maybe Int
  , overrideIncludeDocs   :: Maybe Bool
  , overrideModelOptions  :: Maybe Value
  , overrideTriggerPolicy :: Maybe TriggerPolicy
  , overrideTemplate      :: Maybe Template
  } deriving (Eq, Show, Generic)
