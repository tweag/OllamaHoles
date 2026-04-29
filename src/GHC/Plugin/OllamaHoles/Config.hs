{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Config where

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

import GHC.Plugin.OllamaHoles.Backend (BackendSlug())
import GHC.Plugin.OllamaHoles.Config.Types
import GHC.Plugin.OllamaHoles.Config.Preferences
import GHC.Plugin.OllamaHoles.Trigger (TriggerPolicy(..))
import GHC.Plugin.OllamaHoles.Config.Trigger
import GHC.Plugin.OllamaHoles.Template (Template())



data Config = Config
  { cfgServices :: Map ServiceName Service
  , cfgProfiles :: Map ProfileName Profile
  , cfgDefault  :: DefaultConfig
  } deriving (Eq, Show, Generic)

setDefault :: DefaultConfig -> Config -> Config
setDefault def cfg = cfg { cfgDefault = def }

data ConfigPathSpec
  = ConfigDefault
  | ConfigExplicit FilePath
  | ConfigDisabled
  deriving (Eq, Show, Generic)

data DefaultConfig = DefaultConfig
  { defModelName     :: Text
  , defBackendName   :: BackendSlug
  , defNumExpr       :: Int
  , defIncludeDocs   :: Bool
  , defModelOptions  :: Maybe Value
  , defTriggerPolicy :: TriggerPolicy
  , defTemplate      :: Template
  } deriving (Eq, Show, Generic)



-- Validation
-------------

-- | The @Preferences@ type is a raw representation of
-- the configuration; it needs to be validated to ensure
-- that e.g. references to services, profiles, and templates
-- resolve and don't have cyclic dependencies.
resolveConfig :: Preferences -> Either ConfigError Config
resolveConfig prefs = do
  svcMap  <- buildServiceMap (prefServices prefs)
  profMap <- buildProfileMap svcMap (prefProfiles prefs)

  case validateProfileTriggers (prefProfiles prefs) of
    Left err -> Left $ AmbiguousProfileTriggers err
    Right () -> pure ()

  pure Config
    { cfgServices = svcMap
    , cfgProfiles = profMap
    }

buildServiceMap
  :: [Service]
  -> Either ConfigError (Map ServiceName Service)
buildServiceMap = flip foldM mempty $ \acc pref ->
  if svcName pref `M.member` acc
    then Left (DuplicateServiceName (svcName pref))
    else Right (M.insert (svcName pref) pref acc)

buildProfileMap
  :: Map ServiceName Service
  -> [Profile]
  -> Either ConfigError (Map ProfileName Profile)
buildProfileMap svcMap prefs = do
  prefMap <- buildProfilePreferenceMap prefs
  traverse (resolveProfile prefMap svcMap []) prefMap

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
  -> [ProfileName]
  -> Profile
  -> Either ConfigError Profile
resolveProfile profMap svcMap stack prof =
  case profKind prof of
    ProfService svcProf -> do
      -- Ensure that the service exists in config.
      svc <- case M.lookup (profService svcProf) svcMap of
        Nothing -> Left (UnknownServiceReference (profName prof) (profService svcProf))
        Just x -> Right x
      pure prof

    ProfFanout fpp -> do
      -- Ensure dependencies exist and do not have cycles.
      leaves <- traverse
        (resolveFanoutMember profMap svcMap (profName prof : stack) (profName prof))
        (profProfiles fpp)
      pure Profile
        { profName = profName prof
        , profKind = ProfFanout $ FanoutProf $ foldl1 (<>) leaves
        }

resolveFanoutMember
  :: Map ProfileName Profile
  -> Map ServiceName Service
  -> [ProfileName]
  -> ProfileName
  -> ProfileName
  -> Either ConfigError (NonEmpty ProfileName)
resolveFanoutMember profMap svcMap stack parent child =
  if child `elem` stack
    then Left (CyclicProfileReference (reverse (child : stack)))
    else case M.lookup child profMap of
      Nothing -> Left (UnknownProfileReference parent child)
      Just prof -> case profKind prof of
        ProfService _ -> do
          prof' <- resolveProfile profMap svcMap stack prof
          case profKind prof of
            ProfService sp -> Right (profName prof' :| [])
            ProfFanout (FanoutProf xs)  -> Right xs

        ProfFanout _ -> do
          prof' <- resolveProfile profMap svcMap stack prof
          case profKind prof' of
            ProfService _ -> Right (profName prof' :| [])
            ProfFanout (FanoutProf xs)  -> Right xs



-- Loading
----------

loadConfig
  :: DefaultConfig -> ConfigPathSpec -> IO (Either ConfigError (Maybe Config))
loadConfig defConfig = \case
  ConfigDisabled -> pure $ Right Nothing
  ConfigDefault -> error "default config"
  ConfigExplicit path -> do
    exists <- doesFileExist path
    if exists
      then do
        contents <- T.readFile path
        case parsePreferencesToml contents of
          TomlParseFailure errs -> pure $ Left $ ConfigParseErrors errs
          TomlParseSuccess warn result -> pure $ fmap Just $
            fmap (setDefault defConfig) $ resolveConfig result
      else pure $ Left $ ConfigFileNotFound path



-- Errors
---------

data ConfigError
  = DuplicateServiceName ServiceName
  | DuplicateProfileName ProfileName
  | UnknownServiceReference ProfileName ServiceName
  | UnknownProfileReference ProfileName ProfileName
  | CyclicProfileReference [ProfileName]
  | AmbiguousProfileTriggers TriggerConflict
  | ConfigFileNotFound FilePath
  | ConfigParseErrors [Text]
  deriving (Eq, Show)
