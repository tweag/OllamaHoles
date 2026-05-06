{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.ServiceCall.Error where

import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Template



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
  | ServiceCallTemplateError TemplateError
  deriving (Eq, Show)

renderServiceCallError :: ServiceCallError -> Text
renderServiceCallError = \case
  ServiceCallError msg ->
    "service call error: " <> T.pack msg

  ServiceCallTemplateError templateError ->
    "failed to load template: " <> T.pack (show templateError)
