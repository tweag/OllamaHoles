module GHC.Plugin.OllamaHoles.Data.Template.Error
  ( TemplateError(..)
  , TemplateParseError(..)
  , renderTemplateError
  , renderTemplateParseError
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Template.Types



data TemplateError
    = TemplateFileNotFound FilePath
    | UnknownTemplateName TemplateName
    | UnknownPlaceholders [Placeholder]
    | MalformedTemplate Line Col TemplateParseError
    | InvalidTemplateName Text
    | TemplateLoadError FilePath
    deriving (Eq, Show)

data TemplateParseError
    = MalformedPlaceholder Text
    deriving (Eq, Ord, Show)



renderTemplateError :: TemplateError -> Text
renderTemplateError = \case
  TemplateFileNotFound path ->
    "Template file not found: " <> T.pack path

  UnknownTemplateName name ->
    "Unknown template name: "
      <> quote (unTemplateName name)

  UnknownPlaceholders placeholders ->
    "Unknown template placeholder"
      <> plural placeholders
      <> ": "
      <> T.intercalate ", " (map unPlaceholder placeholders)

  MalformedTemplate line col err ->
    "Malformed template at line "
      <> T.pack (show line)
      <> ", column "
      <> T.pack (show col)
      <> ": "
      <> renderTemplateParseError err

  InvalidTemplateName name ->
    "Invalid template name: "
      <> quote name

  TemplateLoadError path ->
    "Could not load template file: " <> T.pack path

renderTemplateParseError :: TemplateParseError -> Text
renderTemplateParseError = \case
  MalformedPlaceholder raw ->
    "malformed placeholder " <> quote raw

quote :: Text -> Text
quote txt =
  "\"" <> txt <> "\""

plural :: [a] -> Text
plural xs =
  case xs of
    [_] -> ""
    _ -> "s"
