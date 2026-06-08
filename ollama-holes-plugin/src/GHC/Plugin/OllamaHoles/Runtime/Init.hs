module GHC.Plugin.OllamaHoles.Runtime.Init
  ( pluginInit
  ) where

import Control.Monad ((>=>))
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Text qualified as T
import GHC.Plugins (CommandLineOption)

import GHC.Plugin.OllamaHoles.Logger qualified as Log
import GHC.Plugin.OllamaHoles.Error
import GHC.Plugin.OllamaHoles.Console
import GHC.Plugin.OllamaHoles.Data.Flags
import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Runtime.Ops
import GHC.Plugin.OllamaHoles.Data.PluginState



pluginInit
  :: (MonadIO m) => [CommandLineOption]
  -> m (Either PluginError (PluginState m))
pluginInit = (runExceptT . tryPluginInit) >=> printRenderedError

-- | Initialize the plugin state
tryPluginInit
  :: (MonadIO m) => [CommandLineOption] -> ExceptT PluginError m (PluginState m)
tryPluginInit opts = do
  flags <- case parseFlags opts of
    Right (fs, []) -> pure fs
    Right (_, unk) -> throwError $ UnknownOptionError unk
    Left err       -> throwError $ OptionParseError err
  logger <- liftIO $ Log.initLogger (log_mode flags) (log_dir flags)
  config <- modifyError SomeConfigError $ buildConfig flags
  ops <- lift newServiceCallOps
  let st = PluginState
        { candidates     = []
        , writeLogEvent  = logger
        , configuration  = config
        , serviceCallOps = ops
        }
  lift $ debugMsg st $ "running with flags: " <> T.pack (show flags)
  pure st
