module GHC.Plugin.OllamaHoles.Data.ServiceCall.Error where

import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Template



data RouteConfigError
  = RouteAmbiguousProfiles HoleName [ProfileName]
  | RouteUnknownProfile ProfileName
  | RouteUnknownService ServiceName
  deriving (Eq, Show)

renderRouteConfigError :: RouteConfigError -> Text
renderRouteConfigError = \case
  RouteUnknownService serviceName ->
    "unknown service reference: " <> unServiceName serviceName

  RouteUnknownProfile profileName ->
    "unknown profile reference: " <> unProfileName profileName

  RouteAmbiguousProfiles holeName profileNames ->
    "multiple profiles match hole "
      <> holeName
      <> ": "
      <> T.intercalate ", " (map unProfileName profileNames)



data ServiceCallError
  = ServiceCallError String
  | ServiceCallModelError ModelSelectionError
  | ServiceCallRouteError RouteConfigError
  | ServiceCallTemplateError TemplateError
  deriving (Eq, Show)

renderServiceCallError :: ServiceCallError -> Text
renderServiceCallError = \case
  ServiceCallError msg ->
    "service call error: " <> T.pack msg

  ServiceCallModelError err ->
    "service call model error: " <> renderModelSelectionError err

  ServiceCallRouteError err ->
    renderRouteConfigError err

  ServiceCallTemplateError templateError ->
    "failed to load template: " <> T.pack (show templateError)



data ModelSelectionError
  = NoServiceCallsRouted Text -- hole name
  | CannotListModels ServiceName
  | ModelNameNotFound ServiceName ModelName [ModelName]
  | NoServiceCallsAfterModelFiltering [ModelSelectionWarning]
  deriving (Eq, Show)

renderModelSelectionError :: ModelSelectionError -> Text
renderModelSelectionError = \case
  NoServiceCallsRouted holeName ->
    "no services routed for hole: " <> holeName

  CannotListModels serviceName ->
    "could not list models for service: " <> unServiceName serviceName

  ModelNameNotFound serviceName modelName models ->
    "model "
      <> unModelName modelName
      <> " not found for service "
      <> unServiceName serviceName
      <> "; available models: "
      <> T.intercalate ", " (map unModelName models)

  NoServiceCallsAfterModelFiltering warnings ->
    "no routed services remain after model filtering: "
      <> T.intercalate "; " (map renderModelSelectionWarning warnings)



data ModelSelectionWarning
  = SkippedServiceCannotListModels ServiceName
  | SkippedServiceMissingModel ServiceName ModelName [ModelName]
  deriving (Eq, Show)

renderModelSelectionWarning :: ModelSelectionWarning -> Text
renderModelSelectionWarning = \case
  SkippedServiceCannotListModels serviceName ->
    "skipped "
      <> unServiceName serviceName
      <> " because the backend could not list models"

  SkippedServiceMissingModel serviceName modelName models ->
    "skipped "
      <> unServiceName serviceName
      <> " because model "
      <> unModelName modelName
      <> " was not found; available models: "
      <> T.intercalate ", " (map unModelName models)
