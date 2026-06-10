module GHC.Plugin.OllamaHoles.Console
  ( debugMsg
  , warnMsg
  , printRenderedError
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import Data.Text.IO qualified as T

import GHC.Plugin.OllamaHoles.Constants
import GHC.Plugin.OllamaHoles.Error
import GHC.Plugin.OllamaHoles.Data.PluginState



debugMsg :: (MonadIO m) => PluginState u -> Text -> m ()
debugMsg st txt = liftIO $ when (isDebugMode st) $
  T.putStrLn $ pluginName <> ": " <> txt

warnMsg :: (MonadIO m) => PluginState u -> Text -> m ()
warnMsg st txt = liftIO $ when (isDebugMode st) $
  T.putStrLn $ pluginName <> ": " <> txt



printRenderedError
  :: (MonadIO m) => Either PluginError u -> m (Either PluginError u)
printRenderedError x = case x of
  Right u -> pure (Right u)
  Left err -> do
    liftIO $ T.putStrLn $ renderPluginError err
    pure (Left err)
