module GHC.Plugin.OllamaHoles.Runtime.Ops
  ( newServiceCallOps
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Map qualified as M
import GHC.IORef

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.ServiceCall



-- | This type is only used internally.
newtype BackendCache = BackendCache
  { unBackendCache :: IORef (M.Map BackendConfig Backend)
  }

newBackendCache :: IO BackendCache
newBackendCache =
  BackendCache <$> newIORef M.empty



newServiceCallOps :: (MonadIO m) => m (ServiceCallOps m)
newServiceCallOps =
  fmap mkServiceCallOps $ liftIO newBackendCache

mkServiceCallOps
  :: (MonadIO m) => BackendCache -> ServiceCallOps m
mkServiceCallOps backendCache = ServiceCallOps
  { opsListModels = \service -> liftIO $ do
      backend <- backendForService backendCache service
      fmap (map ModelName) <$> listModels backend

  , opsGetServiceCallTemplate = getServiceCallTemplate

  , opsSubmitServiceCall = \request call -> do
      backend <- liftIO $
        backendForService backendCache (callService call)
      submitServiceCallWithBackend backend request call
  }

backendForService
  :: BackendCache -> Service -> IO Backend
backendForService cache service =
  backendForConfig cache (svcConfig service)

backendForConfig
  :: BackendCache -> BackendConfig -> IO Backend
backendForConfig bc config = do
  let ref = unBackendCache bc
  cache <- readIORef ref
  case M.lookup config cache of
    Just backend -> pure backend
    Nothing -> do
      let backend = configureBackend config
      atomicModifyIORef' ref $ \cache0 ->
        ( M.insert config backend cache0
        , backend
        )
