module GHC.Plugin.OllamaHoles.Data.Config.Build
  ( buildConfig
  ) where

import Control.Exception (try)
import Control.Monad (foldM, when)
import Control.Monad.Except (ExceptT(..), MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.List.NonEmpty (NonEmpty(..))
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
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Prefs.Parse
import GHC.Plugin.OllamaHoles.Data.Prefs.Types
import GHC.Plugin.OllamaHoles.Data.Template (TemplateSource(..), Template, TemplateName)

import GHC.Plugin.OllamaHoles.Data.Config.Error







buildConfig
  :: (MonadIO m) => Flags -> ExceptT ConfigError m Config
buildConfig flags = do
  intent <- inferConfigKindIntent flags
  configMode <- case intent of
    SimpleConfigIntent -> fmap ConfigSimple $ buildSimpleConfig flags
    FancyConfigIntent path raw -> fmap ConfigFancy $ buildFancyConfig path flags raw
  let configDebug = maybe False id $ debug flags
  let configTemplateSearchDir = maybe "." id $ template_search_dir flags
  pure $ Config {configMode, configDebug, configTemplateSearchDir}



-- Intent Inference
-------------------

-- | Infer what kind of config the user wishes to use.
inferConfigKindIntent
  :: (MonadIO m) => Flags -> ExceptT ConfigError m ConfigKindIntent
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
            ConfigFileRequired -> throwError $ ConfigFileNotFound path

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
  :: (MonadIO m) => Flags -> ExceptT ConfigError m SimpleConfig
buildSimpleConfig flags = case simpleTemplateSourceFromFlags flags of
  Left err -> throwError err
  Right source -> pure $ simpleConfigFromFlags flags source

simpleConfigFromFlags :: Flags -> Maybe TemplateSource -> SimpleConfig
simpleConfigFromFlags flags source = SimpleConfig
  { simpleTrigger = fromMaybe defaultTriggerPolicy (trigger_policy flags)
  , simpleService = Service
    { svcName = serviceName
    , svcConfig = case fromMaybe defaultBackendSlug (backend_name flags) of
        Ollama -> SvcOllama $ OllamaConfig (ollama_host flags)
        OpenAI -> SvcOpenAI $ OpenAIConfig
          (fromMaybe "https://api.openai.com" (openai_base_url flags))
          (fromMaybe "OPENAI_API_KEY" (openai_key_name flags))
        Gemini -> SvcGemini $ GeminiConfig
          (fromMaybe "GEMINI_API_KEY" (openai_key_name flags))
    }
  , simpleProfile = ServiceProf
    { profService = serviceName
    , profModel = fromMaybe defaultModelName (ModelName <$> model_name flags)
    , profTemplate = source
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
  :: (MonadIO m) => FilePath -> Flags -> Text -> ExceptT ConfigError m FancyConfig
buildFancyConfig path flags rawConfig = do
  case parsePreferencesToml rawConfig of
    Left err -> throwError $ ConfigParseErrors path err
    Right prefs -> resolveConfig flags prefs



-- Validation
-------------

-- | The @Preferences@ type is a raw representation of
-- the configuration; it needs to be validated to ensure
-- that e.g. references to services, profiles, and templates
-- resolve and don't have cyclic dependencies.
resolveConfig :: (MonadIO m) => Flags -> Preferences -> ExceptT ConfigError m FancyConfig
resolveConfig flags prefs = do
  svcMap  <- ExceptT $ pure $ buildServiceMap (prefServices prefs)
  tmplMap <- ExceptT $ pure $ buildTemplateMap (prefTemplates prefs)
  profMap <- ExceptT $ pure $ buildProfileMap svcMap tmplMap (prefProfiles prefs)

  case validateProfileTriggers (prefProfiles prefs) of
    Left err -> throwError $ AmbiguousProfileTriggers err
    Right () -> pure ()

  addFlagExtras flags $ FancyConfig
    { cfgServices  = svcMap
    , cfgProfiles  = profMap
    , cfgTemplates = tmplMap
    , cfgExtras    = Nothing
    }

buildServiceMap
  :: [Service]
  -> Either ConfigError (Map ServiceName Service)
buildServiceMap = flip foldM mempty $ \acc pref ->
  if svcName pref `M.member` acc
    then Left (DuplicateServiceName (svcName pref))
    else Right (M.insert (svcName pref) pref acc)

buildTemplateMap
  :: [(TemplateName, Template)]
  -> Either ConfigError (Map TemplateName Template)
buildTemplateMap = flip foldM mempty $ \acc (name, tmpl) ->
  if name `M.member` acc
    then Left (DuplicateTemplateName name)
    else Right (M.insert name tmpl acc)

buildProfileMap
  :: Map ServiceName Service
  -> Map TemplateName Template
  -> [Profile]
  -> Either ConfigError (Map ProfileName Profile)
buildProfileMap svcMap tmplMap prefs = do
  prefMap <- buildProfilePreferenceMap prefs
  traverse (resolveProfile prefMap svcMap tmplMap []) prefMap

buildProfilePreferenceMap
  :: [Profile]
  -> Either ConfigError (Map ProfileName Profile)
buildProfilePreferenceMap = flip foldM mempty $ \acc prof ->
  if profName prof `M.member` acc
    then Left (DuplicateProfileName (profName prof))
    else Right (M.insert (profName prof) prof acc)

resolveProfile
  :: Map ProfileName Profile
  -> Map ServiceName Service
  -> Map TemplateName Template
  -> [ProfileName]
  -> Profile
  -> Either ConfigError Profile
resolveProfile profMap svcMap tmplMap stack prof =
  case profKind prof of
    ProfService svcProf -> do
      -- Ensure that the service exists in config.
      case M.lookup (profService svcProf) svcMap of
        Nothing -> Left (UnknownServiceReference (profName prof) (profService svcProf))
        Just _ -> case profTemplate svcProf of
          Just (NamedTemplate name) -> case M.lookup name tmplMap of
            Just _ -> pure prof
            _ -> Left (UnknownTemplateReference (profName prof) name)
          _ -> pure prof

    ProfFanout fpp -> do
      -- Ensure dependencies exist and do not have cycles.
      leaves <- traverse
        (resolveFanoutMember profMap svcMap tmplMap (profName prof : stack) (profName prof))
        (profProfiles fpp)
      pure prof
        { profKind = ProfFanout $ FanoutProf $ foldl1 (<>) leaves
        }

resolveFanoutMember
  :: Map ProfileName Profile
  -> Map ServiceName Service
  -> Map TemplateName Template
  -> [ProfileName] -- stack of profiles
  -> ProfileName   -- parent
  -> ProfileName   -- child
  -> Either ConfigError (NonEmpty ProfileName)
resolveFanoutMember profMap svcMap tmplMap stack parent child =
  if child `elem` stack
    then Left (CyclicProfileReference (reverse (child : stack)))
    else case M.lookup child profMap of
      Nothing -> Left $ UnknownProfileReference parent child
      Just prof -> do
        prof' <- resolveProfile profMap svcMap tmplMap stack prof
        case profKind prof' of
          ProfService _ -> Right (profName prof' :| [])
          ProfFanout (FanoutProf xs) -> Right xs



-- Extras
---------

addFlagExtras
  :: Monad m
  => Flags
  -> FancyConfig
  -> ExceptT ConfigError m FancyConfig
addFlagExtras flags cfg
  | flagsWantOverlay flags = do
      let overlay = simpleConfigFromFlags flags (configTemplateSourceFromFlags flags)
      validateSimpleConfigTemplate (cfgTemplates cfg) overlay

      let overlaySvc = simpleService overlay
          overlaySvcName = svcName overlaySvc

      if overlaySvcName `M.member` cfgServices cfg
        then throwError $ DuplicateServiceName overlaySvcName
        else pure cfg
          { cfgServices = M.insert overlaySvcName overlaySvc (cfgServices cfg)
          , cfgExtras = Just (ConfigOverlay overlay)
          }

  | otherwise = do
      let override = overrideConfigFromFlags flags
      validateOverrideTemplate (cfgTemplates cfg) override
      pure cfg
        { cfgExtras = Just (ConfigOverride override)
        }

validateSimpleConfigTemplate
  :: MonadError ConfigError m
  => Map TemplateName Template
  -> SimpleConfig
  -> m ()
validateSimpleConfigTemplate tmplMap simple =
  validateMaybeTemplateSource tmplMap $
    profTemplate (simpleProfile simple)

validateOverrideTemplate
  :: MonadError ConfigError m
  => Map TemplateName Template
  -> OverrideConfig
  -> m ()
validateOverrideTemplate tmplMap override =
  validateMaybeTemplateSource tmplMap $
    overrideTemplate override

validateMaybeTemplateSource
  :: MonadError ConfigError m
  => Map TemplateName Template
  -> Maybe TemplateSource
  -> m ()
validateMaybeTemplateSource tmplMap = \case
  Just (NamedTemplate name)
    | name `M.member` tmplMap ->
        pure ()

    | otherwise ->
        throwError $ UnknownExtraTemplateReference name

  _ ->
    pure ()

flagsWantOverlay :: Flags -> Bool
flagsWantOverlay flags = or
  [ isJust (backend_name flags)
  , isJust (openai_base_url flags)
  , isJust (openai_key_name flags)
  ]

overrideConfigFromFlags :: Flags -> OverrideConfig
overrideConfigFromFlags flags = OverrideConfig
  { overrideModelName = ModelName <$> model_name flags
  , overrideNumExpr = num_expr flags
  , overrideIncludeDocs = include_docs flags
  , overrideModelOptions = model_options flags
  , overrideTemplate = configTemplateSourceFromFlags flags
  }

simpleTemplateSourceFromFlags :: Flags -> Either ConfigError (Maybe TemplateSource)
simpleTemplateSourceFromFlags flags = case template_path flags of
  Just path -> pure $ Just $ TemplateFile path
  Nothing -> case template_name flags of
    Just name -> Left $ NamedTemplateRequiresConfig name
    Nothing -> pure Nothing

configTemplateSourceFromFlags
  :: Flags -> Maybe TemplateSource
configTemplateSourceFromFlags flags =
  case template_path flags of
    Just path -> Just $ TemplateFile path
    Nothing -> NamedTemplate <$> template_name flags

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
