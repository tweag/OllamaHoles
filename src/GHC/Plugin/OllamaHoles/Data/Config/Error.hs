module GHC.Plugin.OllamaHoles.Data.Config.Error where

import Control.Exception (IOException)

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Prefs


data ConfigError
  = DuplicateServiceName ServiceName
  | DuplicateProfileName ProfileName
  | DuplicateTemplateName TemplateName
  | UnknownServiceReference ProfileName ServiceName
  | UnknownProfileReference ProfileName ProfileName
  | UnknownTemplateReference ProfileName TemplateName
  | CyclicProfileReference [ProfileName]
  | AmbiguousProfileTriggers TriggerConflict
  | ConfigFileNotFound FilePath
  | ConfigParseErrors FilePath TomlParseError
  | ConfigFileStatusFailed IOException FilePath
  | ConfigFileReadFailed IOException FilePath
  | NamedTemplateRequiresConfig TemplateName
  | UnknownExtraTemplateReference TemplateName
  deriving (Eq, Show)