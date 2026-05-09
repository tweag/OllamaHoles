module GHC.Plugin.OllamaHoles.Error where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Tc.Errors.Hole.FitTypes (TypedHole)
import Control.Monad.Except (withExceptT, ExceptT())

import GHC.Plugin.OllamaHoles.Backend (BackendSlug(..), renderBackendSlug)
import GHC.Plugin.OllamaHoles.Data.Flags (FlagError(), FlagToken(..))
import GHC.Plugin.OllamaHoles.Template (TemplateError())
import GHC.Plugin.OllamaHoles.Data.Config.Error (ConfigError(..))
import GHC.Plugin.OllamaHoles.Data.Profile.Error (ProfileSubmitError(..), ProfileRouteError(..))
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error

data PluginError
  = OptionParseError FlagError
  | UnknownOptionError [FlagToken]
  | TemplateSpecError TemplateError
  | TemplateParseError TemplateError
  | TemplateSubError TemplateError
  | NoModelsAvailable
  | ModelNotFound Text [Text] BackendSlug
  | TypedHoleNotFound TypedHole
  | ResponseFailed Text
  | HoleNameDoesNotMatchPolicy Text
  | SomeConfigError ConfigError
  | ProfileRouteFailed ProfileRouteError
  | ProfileSubmitFailed ProfileSubmitError
  | RouteConfigPluginError RouteConfigError
  | ServiceCallPluginError ServiceCallError

liftPluginError :: (Functor m) => (e -> PluginError) -> ExceptT e m a -> ExceptT PluginError m a
liftPluginError f = withExceptT f

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

  SomeConfigError err ->
    "config error: " <> T.pack (show err)

  ProfileRouteFailed msg ->
    "profile routing failed: " <> T.pack (show msg)

  ProfileSubmitFailed msg ->
    "profile execution failed: " <> T.pack (show msg)

  RouteConfigPluginError err ->
    "route config error: " <> renderRouteConfigError err

  ServiceCallPluginError err ->
    "service call error: " <> renderServiceCallError err
  where
    renderToken :: FlagToken -> Text
    renderToken = \case
      BooleanToken key -> key
      ValueToken key val -> key <> "=" <> val
