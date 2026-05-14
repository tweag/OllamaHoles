module GHC.Plugin.OllamaHoles.Data.ServiceCall.Route
  ( prepareServiceCalls
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Except (ExceptT(..), throwError)
import Control.Monad.Trans (MonadTrans(..))
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Config

import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error



-- | Given a @Config@ object and a hole name, route
-- to the matching @ServiceCall@s and filter out any
-- for which the model name is invalid.
prepareServiceCalls
  :: (Monad m)
  => (Service -> m (Maybe [ModelName]))
  -> Config -> Text {- HoleName -}
  -> ExceptT ServiceCallError m CheckedServiceCalls
prepareServiceCalls listServiceModels config holeName = do
  routed <- case routeServiceCalls config holeName of
    Left err -> throwError (ServiceCallRouteError err)
    Right Nothing -> throwError $
      ServiceCallModelError $ NoServiceCallsRouted holeName
    Right (Just calls) -> pure calls

  calls@(CheckedServiceCalls accepted warnings) <- lift $
    filterServiceCallsByAvailableModels listServiceModels routed

  when (null accepted) $ throwError $
    ServiceCallModelError $ NoServiceCallsAfterModelFiltering warnings

  pure calls

filterServiceCallsByAvailableModels
  :: Monad m
  => (Service -> m (Maybe [ModelName]))
  -> [ServiceCall]
  -> m CheckedServiceCalls
filterServiceCallsByAvailableModels listServiceModels calls = do
  results <- for calls $ \call -> do
    let
      service = callService call
      serviceName = svcName service
      wantedModel = profModel (callProfile call)

    mModels <- listServiceModels service
    case mModels of
      Nothing -> pure $ Left $
        SkippedServiceCannotListModels serviceName
      Just models
        | wantedModel `elem` models -> pure (Right call)
        | otherwise -> pure $ Left $
            SkippedServiceMissingModel serviceName wantedModel models

  pure CheckedServiceCalls
    { checkedAccepted = [ call | Right call <- results ]
    , checkedWarnings = [ warning | Left warning <- results ]
    }




-- | Given a @Config@ and a hole name, return all the
-- @ServiceCall@s matching the name.
routeServiceCalls
  :: Config -> HoleName
  -> Either RouteConfigError (Maybe [ServiceCall])
routeServiceCalls config holeName = case config of
  ConfigSimple simple -> Right $ routeSimpleConfig simple holeName
  ConfigFancy  fancy  -> routeFancyConfig fancy holeName

routeSimpleConfig
  :: SimpleConfig -> HoleName -> Maybe [ServiceCall]
routeSimpleConfig cfg holeName =
  if shouldTriggerHole (simpleTrigger cfg) holeName
    then Just
      [ ServiceCall
        { callProfile = simpleProfile cfg
        , callService = simpleService cfg
        }
      ]
    else Nothing

routeFancyConfig
  :: FancyConfig -> HoleName
  -> Either RouteConfigError (Maybe [ServiceCall])
routeFancyConfig fancy holeName = do
  case routeFancyOverlay fancy holeName of
    Just calls -> pure (Just calls)
    Nothing -> routeFancyProfiles fancy holeName

routeFancyOverlay
  :: FancyConfig -> HoleName -> Maybe [ServiceCall]
routeFancyOverlay fancy holeName = case cfgExtras fancy of
  Just (ConfigOverride _) -> Nothing
  -- If there is an overlay, route it like a simple config.
  Just (ConfigOverlay overlay) -> routeSimpleConfig overlay holeName
  Nothing -> Nothing

routeFancyProfiles
  :: FancyConfig -> HoleName
  -> Either RouteConfigError (Maybe [ServiceCall])
routeFancyProfiles fancy holeName =
  case matchingProfiles of
    []   -> Right Nothing
    [ok] -> Just <$> serviceCallsForProfile fancy ok
    more -> Left $ RouteAmbiguousProfiles holeName (map profName more)
  where
    matchingProfiles = filter
      (\profile -> shouldTriggerHole (profTrigger profile) holeName)
      (sortOn profName $ M.elems $ cfgProfiles fancy)

serviceCallsForProfile
  :: FancyConfig -> Profile
  -> Either RouteConfigError [ServiceCall]
serviceCallsForProfile fancy profile = case profKind profile of
  ProfService serviceProf -> (:[]) <$> serviceCallForServiceProf fancy serviceProf
  ProfFanout fanout -> concat <$> traverse resolveChild (profProfiles fanout)
  where
    resolveChild childName = case M.lookup childName (cfgProfiles fancy) of
      Nothing -> Left (RouteUnknownProfile childName)
      Just childProfile -> serviceCallsForProfile fancy childProfile

serviceCallForServiceProf
  :: FancyConfig -> ServiceProf
  -> Either RouteConfigError ServiceCall
serviceCallForServiceProf fancy prof =
  case M.lookup (profService prof) (cfgServices fancy) of
    Nothing -> Left $ RouteUnknownService $ profService prof
    Just service -> Right ServiceCall
      { callProfile = applyExtraConfig fancy prof
      , callService = service
      }

applyExtraConfig :: FancyConfig -> ServiceProf -> ServiceProf
applyExtraConfig fancy serviceProf = case cfgExtras fancy of
  Nothing -> serviceProf
  Just (ConfigOverlay _) -> serviceProf
  Just (ConfigOverride overrides) -> applyOverrideConfig overrides serviceProf

applyOverrideConfig :: OverrideConfig -> ServiceProf -> ServiceProf
applyOverrideConfig overrides serviceProf = serviceProf
  { profTemplate = overrideTemplate overrides <|> profTemplate serviceProf
  , profModelOptions = overrideModelOptions overrides <|> profModelOptions serviceProf
  , profNumExpr = overrideNumExpr overrides <|> profNumExpr serviceProf
  , profIncludeDocs = overrideIncludeDocs overrides <|> profIncludeDocs serviceProf
  , profModel = fromMaybe (profModel serviceProf) (overrideModelName overrides)
  }
