{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Error where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Tc.Errors.Hole.FitTypes (TypedHole)

import GHC.Plugin.OllamaHoles.Backend (BackendSlug(..), renderBackendSlug)
import GHC.Plugin.OllamaHoles.Options (OptError(), Token(..))
import GHC.Plugin.OllamaHoles.Template (TemplateError())

data PluginError
  = OptionParseError OptError
  | UnknownOptionError [Token]
  | TemplateSpecError TemplateError
  | TemplateParseError TemplateError
  | TemplateSubError TemplateError
  | NoModelsAvailable
  | ModelNotFound Text [Text] BackendSlug
  | TypedHoleNotFound TypedHole
  | ResponseFailed Text
  | HoleMissingTriggerName
  | HoleNameDoesNotMatchPolicy Text

isSilentError :: PluginError -> Bool
isSilentError = \case
  OptionParseError           _ -> True -- \
  UnknownOptionError         _ -> True --  | These are initialization errors, and
  TemplateSpecError          _ -> True --  | are printed by printRenderedError.
  TemplateParseError         _ -> True -- /
  TypedHoleNotFound          _ -> True -- This just means there are no typed holes.
  HoleNameDoesNotMatchPolicy _ -> True -- The hole exists but doesn't match the trigger.
  _                            -> False

renderPluginError :: PluginError -> Text
renderPluginError = \case
  OptionParseError err ->
    "option parse error: " <> T.pack (show err)

  UnknownOptionError toks ->
    "unrecognized plugin option(s): "
      <> T.intercalate ", " (map renderToken toks)

  TemplateSpecError err ->
    "template specification error: " <> T.pack (show err)

  TemplateParseError err ->
    "template load/parse error: " <> T.pack (show err)

  TemplateSubError err ->
    "template substitution error: " <> T.pack (show err)

  NoModelsAvailable ->
    "no models available; check your backend configuration"

  ModelNotFound modelName models backendName -> mconcat
    [ "model "
    , modelName
    , " not found for backend "
    , renderBackendSlug backendName
    , ". "
    , if backendName == Ollama
        then "Use `ollama pull` to download the model, or "
        else ""
    , "specify another model using "
    , "`-fplugin-opt=GHC.Plugin.OllamaHoles:model=`.\n"
    , "Available models:\n"
    , T.unlines models
    ]

  TypedHoleNotFound _ ->
    "could not locate the typed hole in the current context"

  ResponseFailed msg ->
    "backend request failed: " <> msg

  HoleNameDoesNotMatchPolicy holeName ->
    "skipping " <> holeName <> " because it does not match the configured trigger policy"
  where
    renderToken :: Token -> Text
    renderToken = \case
      BooleanToken key -> key
      ValueToken key val -> key <> "=" <> val
