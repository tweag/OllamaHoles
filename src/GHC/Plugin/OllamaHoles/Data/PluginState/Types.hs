module GHC.Plugin.OllamaHoles.Data.PluginState.Types
  ( PluginState(..)
  , setCandidates
  , isDebugMode
  ) where

import GHC.Tc.Errors.Hole.FitTypes (HoleFitCandidate)

import GHC.Plugin.OllamaHoles.Logger qualified as Log
import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.ServiceCall



data PluginState m = PluginState
  { candidates     :: [HoleFitCandidate]
  , writeLogEvent  :: Log.Logger
  , configuration  :: Config
  , serviceCallOps :: ServiceCallOps m
  }

setCandidates :: [HoleFitCandidate] -> PluginState m -> PluginState m
setCandidates cs st = st { candidates = cs }

isDebugMode :: PluginState m -> Bool
isDebugMode = configDebug . configuration
