module GHC.Plugin.OllamaHoles.Data.ServiceCall.Route
  ( routeServiceCalls
  ) where

import Control.Applicative
import Data.List (sortOn)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)

import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Trigger
import GHC.Plugin.OllamaHoles.Data.Config

import GHC.Plugin.OllamaHoles.Template
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error



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
  _ -> error "routeFancyOverlay"

routeFancyProfiles
  :: FancyConfig -> HoleName
  -> Either RouteConfigError (Maybe [ServiceCall])
routeFancyProfiles fancy holeName =
  case matchingProfiles of
    []   -> Right Nothing
    [ok] -> Just <$> serviceCallsForProfile fancy ok
    many -> Left $ RouteAmbiguousProfiles holeName (map profName many)
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
  Nothing -> error "applyExtraConfig"
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
