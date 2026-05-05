module GHC.Plugin.OllamaHoles.Data.Config.Route
  ( routeProfileForHole
  ) where

import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Match
import GHC.Plugin.OllamaHoles.Data.Profile.Types
import GHC.Plugin.OllamaHoles.Data.Profile.Error
import GHC.Plugin.OllamaHoles.Data.Config.Types



routeProfileForHole
  :: FancyConfig
  -> Text
  -> Either ProfileRouteError (Maybe RoutedProfile)
routeProfileForHole fancy holeName =
  case matches of
    [] ->
      Right Nothing

    [(profile, triggerMatch)] ->
      Right (Just RoutedProfile
        { routedProfile = profile
        , routedMatch = triggerMatch
        })

    many ->
      Left $
        AmbiguousTriggeredProfiles holeName
          [ (profName profile, profTrigger profile)
          | (profile, _triggerMatch) <- many
          ]
  where
    matches :: [(Profile, TriggerMatch)]
    matches =
      [ (profile, triggerMatch)
      | profile <- M.elems (cfgProfiles fancy)
      , Just triggerMatch <- [matchTriggerPolicy (profTrigger profile) holeName]
      ]

