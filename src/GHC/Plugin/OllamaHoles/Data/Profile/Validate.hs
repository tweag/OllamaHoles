module GHC.Plugin.OllamaHoles.Data.Profile.Validate
  ( validateProfileTriggers
  ) where

import Data.List (tails)
import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Profile.Types
import GHC.Plugin.OllamaHoles.Data.Profile.Error
import GHC.Plugin.OllamaHoles.Data.Trigger.Types



validateProfileTriggers :: [Profile] -> Either TriggerConflict ()
validateProfileTriggers profiles =
  case firstConflict activeTriggers of
    Nothing       -> Right ()
    Just conflict -> Left conflict
  where
    activeTriggers :: [(ProfileName, TriggerPolicy)]
    activeTriggers =
      [ (profName profile, profTrigger profile)
      | profile <- profiles
      , isActiveTrigger (profTrigger profile)
      ]


firstConflict :: [(ProfileName, TriggerPolicy)] -> Maybe TriggerConflict
firstConflict triggers = case triggerAlls of
  [] -> firstPrefixConflict prefixTriggers
  [(allProfile, _)] -> case prefixTriggers of
    [] -> Nothing
    (name, trigger) : _ -> Just $
      TriggerAllOverlaps allProfile name (TriggerPrefix trigger)
  (profileA, _) : (profileB, _) : _ ->
    Just (MultipleTriggerAll profileA profileB)
  where
    triggerAlls =
      [ pair | pair@(_, TriggerAll) <- triggers ]
    prefixTriggers =
      [ (profName, prefix) | (profName, TriggerPrefix prefix) <- triggers ]


firstPrefixConflict :: [(ProfileName, Text)] -> Maybe TriggerConflict
firstPrefixConflict prefixTriggers =
  firstJust
    [ prefixConflict a b
    | a : rest <- tails prefixTriggers
    , b <- rest
    ]


prefixConflict
  :: (ProfileName, Text)
  -> (ProfileName, Text)
  -> Maybe TriggerConflict
prefixConflict (profileA, prefixA) (profileB, prefixB)
  | prefixA == prefixB =
      Just (DuplicateTriggerPrefix profileA profileB prefixA)

  | prefixA `T.isPrefixOf` prefixB =
      Just (TriggerPrefixOverlap profileA prefixA profileB prefixB)

  | prefixB `T.isPrefixOf` prefixA =
      Just (TriggerPrefixOverlap profileB prefixB profileA prefixA)

  | otherwise =
      Nothing


firstJust :: [Maybe a] -> Maybe a
firstJust =
  foldr pick Nothing
  where
    pick candidate acc =
      case candidate of
        Just _  -> candidate
        Nothing -> acc
