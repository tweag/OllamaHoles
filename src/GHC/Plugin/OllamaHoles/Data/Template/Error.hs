module GHC.Plugin.OllamaHoles.Data.Template.Error where

import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Data.Template.Types



data TemplateError
    = TemplateFileNotFound FilePath
    | UnknownTemplateName FilePath Text
    | UnknownPlaceholders [Placeholder]
    | MalformedTemplate Line Col TemplateParseError
    | InvalidTemplateName Text
    | TemplateLoadError FilePath
    deriving (Eq, Show)

data TemplateParseError
    = MalformedPlaceholder Text
    deriving (Eq, Ord, Show)
