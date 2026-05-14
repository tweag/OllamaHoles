module GHC.Plugin.OllamaHoles.Data.PluginState.Types
  ( PluginState(..)
  , setCandidates
  , isDebugMode
  , debugMsg
  , warnMsg
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.Tc.Errors.Hole.FitTypes (HoleFitCandidate)

import GHC.Plugin.OllamaHoles.Logger qualified as Log
import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.ServiceCall
import GHC.Plugin.OllamaHoles.Constants



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



debugMsg :: (MonadIO m) => PluginState m' -> Text -> m ()
debugMsg st txt = liftIO $ when (isDebugMode st) $
  T.putStrLn $ pluginName <> ": " <> txt

warnMsg :: (MonadIO m) => PluginState m' -> Text -> m ()
warnMsg st txt = liftIO $ when (isDebugMode st) $
  T.putStrLn $ pluginName <> ": " <> txt
