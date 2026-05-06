module GHC.Plugin.OllamaHoles.Data.ServiceCall.Error where

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Template



data RouteConfigError
  = RouteAmbiguousProfiles HoleName [ProfileName]
  | RouteUnknownProfile ProfileName
  | RouteUnknownService ServiceName
  deriving (Eq, Show)

data ServiceCallError
  = ServiceCallError String
  | ServiceCallTemplateError TemplateError
  deriving (Eq, Show)