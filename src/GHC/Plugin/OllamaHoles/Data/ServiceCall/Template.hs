module GHC.Plugin.OllamaHoles.Data.ServiceCall.Template
  ( getServiceCallTemplate
  ) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Map (Map)

import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Profile

import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error

getServiceCallTemplate
  :: (MonadIO m)
  => FilePath -> Map TemplateName Template -> ServiceCall
  -> ExceptT ServiceCallError m Template
getServiceCallTemplate path tmplMap caller = do
  result <- liftIO $ case profTemplate $ callProfile caller of
    Nothing -> loadTemplate $ TemplateSpec path tmplMap DefaultTemplate
    Just spec -> loadTemplate $ TemplateSpec path tmplMap spec
  case result of
    Left err -> throwError $ ServiceCallTemplateError err
    Right ok -> pure ok
