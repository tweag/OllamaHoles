module GHC.Plugin.OllamaHoles.Data.ServiceCall.Submit
  ( submitRoutedServiceCalls
  , submitServiceCallWithBackend
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Tc.Types (TcM)

import GHC.Plugin.OllamaHoles.Prompt
import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Template
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Route



submitServiceCallWithBackend
  :: Backend -> PromptRequest -> ServiceCall
  -> ExceptT ServiceCallError TcM PromptResponse
submitServiceCallWithBackend backend req caller = do
  prompt <- renderPromptForServiceCall req caller
  let
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
  -- TODO: need to get the docs properly
--  docs <- lift $
--    if effectiveIncludeDocs (callProfile caller)
--      then getDocs (candidates st)
--      else pure ""
  let effectiveNumExpr profile =
        fromMaybe 10 (profNumExpr profile)

  let result = expandTemplate template $ mkTemplateEnv
        [ ("backend", unServiceName (svcName (callService caller)))
        , ("model", unModelName (profModel (callProfile caller)))
        , ("numexpr", T.pack (show (effectiveNumExpr (callProfile caller))))
        , ("docs", T.pack docs)
        , ("context", encodePromptContext $ requestContext req)
        ]
  case result of
    Left err -> throwError $ ServiceCallTemplateError err
    Right ok -> pure ok



submitRoutedServiceCalls
  :: (Monad m)
  => ServiceCallOps m -> FilePath -> Config
  -> Text {- HoleName -} -> PromptContext
  -> ExceptT ServiceCallError m ServiceCallResponses
submitRoutedServiceCalls ops templateSearchDir config holeName ctx = do
  CheckedServiceCalls calls warnings <- prepareServiceCalls
    (opsListModels ops) config holeName
  responses <- for calls $ \call -> do
    template <- opsGetServiceCallTemplate ops templateSearchDir call
    opsSubmitServiceCall ops (PromptRequest ctx template) call
  pure $ ServiceCallResponses responses warnings
