{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.ServiceCall.Submit
  ( submitServiceCall
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except
import GHC.Tc.Types (TcM)

import GHC.Plugin.OllamaHoles.Prompt
import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Template
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error



submitServiceCall
  :: PromptRequest -> ServiceCall
  -> ExceptT ServiceCallError TcM PromptResponse
submitServiceCall req caller = do
  prompt <- renderPromptForServiceCall req caller
  let
    backend = configureBackend $ svcConfig $ callService caller
    serviceProf = callProfile caller
    model = unModelName $ profModel serviceProf
    options = profModelOptions serviceProf
  result <- liftIO $ generateFits backend prompt model options
  case result of
    Left err -> throwError $ ServiceCallError err
    Right response -> pure $ PromptResponse response

renderPromptForServiceCall
  :: PromptRequest -> ServiceCall -> ExceptT ServiceCallError TcM Text
renderPromptForServiceCall req caller = do
  let template = requestTemplate req
  let docs = mempty
--  docs <- lift $
--    if effectiveIncludeDocs (callProfile caller)
--      then getDocs (candidates st)
--      else pure ""
  let effectiveNumExpr = const 10

  let result = expandTemplateWith template $ mkTemplateEnv
        [ ("backend", unServiceName (svcName (callService caller)))
        , ("model", unModelName (profModel (callProfile caller)))
        , ("numexpr", T.pack (show (effectiveNumExpr (callProfile caller))))
        , ("docs", T.pack docs)
        , ("context", encodePromptContext $ requestContext req)
        ]
  case result of
    Left err -> throwError $ ServiceCallTemplateError err
    Right ok -> pure ok
