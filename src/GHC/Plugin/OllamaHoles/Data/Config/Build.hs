{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.Config.Build
  ( buildConfig
  , ConfigBuildError(..)
  ) where

import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT, MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (Value)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.IO qualified as T
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Config.Types
import GHC.Plugin.OllamaHoles.Data.Flags.Types
import GHC.Plugin.OllamaHoles.Data.Profile.Types
import GHC.Plugin.OllamaHoles.Data.Service.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Template (TemplateSource(..), parseTemplateName)



data ConfigBuildError
  = ConfigFileDoesNotExist FilePath
  | ConfigFileStatusFailed IOException FilePath
  | ConfigFileReadFailed IOException FilePath
  deriving (Eq, Show)



buildConfig
  :: (MonadIO m) => Flags -> ExceptT ConfigBuildError m Config
buildConfig flags = do
  intent <- inferConfigKindIntent flags
  case intent of
    SimpleConfigIntent -> fmap ConfigSimple $ buildSimpleConfig flags
    FancyConfigIntent path raw -> fmap ConfigFancy $ buildFancyConfig path flags raw



-- Intent Inference
-------------------

-- | Infer what kind of config the user wishes to use.
inferConfigKindIntent
  :: (MonadIO m) => Flags -> ExceptT ConfigBuildError m ConfigKindIntent
inferConfigKindIntent flags = do
  defaultPath <- getDefaultConfigPath
  let mConfigPath = case config_path flags of
        Just pathSpec -> case pathSpec of
          ConfigDisabled -> Nothing
          ConfigDefault -> Just (defaultPath, ConfigFileRequired)
          ConfigExplicit path -> Just (path, ConfigFileRequired)
        Nothing -> Just (defaultPath, ConfigFileOptional)
  case mConfigPath of
    Nothing -> pure SimpleConfigIntent
    Just (path, required) -> do
      mExists <- liftIO $ try $ doesFileExist path
      case mExists of
        Left err -> throwError $ ConfigFileStatusFailed err path
        Right exists -> if exists
          then do
            mContents <- liftIO $ try $ T.readFile path
            case mContents of
              Left err -> throwError $ ConfigFileReadFailed err path
              Right contents -> pure $ FancyConfigIntent path contents
          else case required of
            ConfigFileOptional -> pure SimpleConfigIntent
            ConfigFileRequired -> throwError $ ConfigFileDoesNotExist path

defaultConfigFileName :: FilePath
defaultConfigFileName = "ollama-holes.toml"

getDefaultConfigPath :: MonadIO m => m FilePath
getDefaultConfigPath = do
  home <- liftIO getHomeDirectory
  pure (home </> defaultConfigFileName)

data ConfigKindIntent
  = SimpleConfigIntent
  | FancyConfigIntent FilePath Text
  deriving (Eq, Show)

data ConfigRequirement
  = ConfigFileRequired
  | ConfigFileOptional
  deriving (Eq, Show)



-- Simple Config
----------------

buildSimpleConfig
  :: (Monad m) => Flags -> ExceptT ConfigBuildError m SimpleConfig
buildSimpleConfig = pure . simpleConfigFromFlags

simpleConfigFromFlags :: Flags -> SimpleConfig
simpleConfigFromFlags flags = SimpleConfig
  { simpleTrigger = fromMaybe defaultTriggerPolicy (trigger_policy flags)
  , simpleService = Service
    { svcName = serviceName
    , svcConfig = case fromMaybe defaultBackendSlug (backend_name flags) of
        Ollama -> SvcOllama $ OllamaConfig Nothing
        OpenAI -> SvcOpenAI $ OpenAIConfig
          (fromMaybe "https://api.openai.com" (openai_base_url flags))
          (fromMaybe "OPENAI_API_KEY" (openai_key_name flags))
        Gemini -> SvcGemini $ GeminiConfig
          (fromMaybe "GEMINI_API_KEY" (openai_key_name flags))
    }
  , simpleProfile = ServiceProf
    { profService = serviceName
    , profModel = fromMaybe defaultModelName (ModelName <$> model_name flags)
    , profTemplate = case template_path flags of
        Just path -> Just (TemplateFile path)
        _ -> case template_name flags of
          Just name -> Just (NamedTemplate name)
          _ -> Nothing
    , profModelOptions = model_options flags
    , profNumExpr = Just (fromMaybe defaultNumExpr (num_expr flags))
    , profIncludeDocs = Just (fromMaybe defaultIncludeDocs (include_docs flags))
    }
  }
  where
    serviceName = ServiceName "__simple__"






-- Fancy Config
---------------

buildFancyConfig
  :: FilePath -> Flags -> Text -> ExceptT ConfigBuildError m FancyConfig
buildFancyConfig path flags rawConfig = undefined



-- Defaults
-----------

defaultBackendSlug :: BackendSlug
defaultBackendSlug = Ollama

defaultModelName :: ModelName
defaultModelName = ModelName "qwen3:latest"

defaultNumExpr :: Int
defaultNumExpr = 5

defaultIncludeDocs :: Bool
defaultIncludeDocs = False



{-


data ConfigInput
  = NoConfigFile
  | LoadedFancyConfig
      { loadedServices :: Map ServiceName Service
      , loadedProfiles :: Map ProfileName Profile
      }
  deriving (Eq, Show)

data ConfigBuildError
  = ConfigBuildImpossible String
  deriving (Eq, Show)









configFromFlags :: Flags -> ConfigInput -> Either ConfigBuildError Config
configFromFlags flags input =
  case input of
    NoConfigFile ->
      Right $
        ConfigSimple $
          simpleConfigFromFlags flags

    LoadedFancyConfig services profiles ->
      Right $
        ConfigFancy $
          fancyConfigFromFlags flags services profiles



fancyConfigFromFlags
  :: Flags
  -> Map ServiceName Service
  -> Map ProfileName Profile
  -> FancyConfig
fancyConfigFromFlags flags services profiles
  | flagsWantOverlay flags =
      let
        overlay =
          overlayConfigFromFlags flags

        servicesWithOverlay =
          M.insert
            (svcName (overlayService overlay))
            (overlayService overlay)
            services
      in
        FancyConfig
          { cfgServices = servicesWithOverlay
          , cfgProfiles = profiles
          , cfgExtras = ConfigOverlay overlay
          }

  | otherwise =
      FancyConfig
        { cfgServices = services
        , cfgProfiles = profiles
        , cfgExtras = ConfigOverride (overrideConfigFromFlags flags)
        }

overlayConfigFromFlags :: Flags -> OverlayConfig
overlayConfigFromFlags flags =
  OverlayConfig
    { overlayTrigger =
        fromMaybe defaultTriggerPolicy (trigger_policy flags)

    , overlayService =
        serviceFromFlags commandLineServiceName flags

    , overlayProfile =
        serviceProfFromFlagsWithDefaults commandLineServiceName flags
    }

overrideConfigFromFlags :: Flags -> OverrideConfig
overrideConfigFromFlags flags =
  OverrideConfig
    { overrideModelName =
        ModelName <$> model_name flags

    , overrideNumExpr =
        num_expr flags

    , overrideIncludeDocs =
        include_docs flags

    , overrideModelOptions =
        model_options flags

    , overrideTemplate =
        templateSourceFromFlags flags
    }

flagsWantOverlay :: Flags -> Bool
flagsWantOverlay flags =
  or
    [ isJust (backend_name flags)
    , isJust (openai_base_url flags)
    , isJust (openai_key_name flags)
    ]

serviceFromFlags :: ServiceName -> Flags -> Service
serviceFromFlags name flags =
  Service
    { svcName = name
    , svcConfig = serviceConfigFromFlags flags
    }

serviceConfigFromFlags :: Flags -> ServiceConfig
serviceConfigFromFlags flags =
  case fromMaybe defaultBackendSlug (backend_name flags) of
    Ollama ->
      SvcOllama $
        OllamaConfig Nothing

    OpenAI ->
      SvcOpenAI $
        OpenAIConfig
          (fromMaybe "https://api.openai.com" (openai_base_url flags))
          (fromMaybe "OPENAI_API_KEY" (openai_key_name flags))

    Gemini ->
      SvcGemini $
        GeminiConfig
          (fromMaybe "GEMINI_API_KEY" (openai_key_name flags))





applyOverrideConfig :: OverrideConfig -> ServiceProf -> ServiceProf
applyOverrideConfig overrides sp =
  sp
    { profModel =
        fromMaybe (profModel sp) (overrideModelName overrides)

    , profTemplate =
        overrideTemplate overrides <|> profTemplate sp

    , profModelOptions =
        overrideModelOptions overrides <|> profModelOptions sp

    , profNumExpr =
        overrideNumExpr overrides <|> profNumExpr sp

    , profIncludeDocs =
        overrideIncludeDocs overrides <|> profIncludeDocs sp
    }

data ServiceCall = ServiceCall
  { callService :: Service
  , callProfile :: ServiceProf
  }
  deriving (Eq, Show)

-}