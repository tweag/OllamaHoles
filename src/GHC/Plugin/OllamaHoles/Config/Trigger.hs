{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Config.Trigger
  ( TriggerConflict(..)
  , validateProfileTriggers
  ) where

import Data.List (tails)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Plugin.OllamaHoles.Config.Types
  ( Profile(..)
  , ProfileName(..)
  , ServiceProf(..)
  )

import GHC.Plugin.OllamaHoles.Trigger
  ( TriggerPolicy(..)
  )


data TriggerConflict
  = DuplicateTriggerPrefix ProfileName ProfileName Text
  | TriggerPrefixOverlap ProfileName Text ProfileName Text
  | MultipleTriggerAll ProfileName ProfileName
  | TriggerAllOverlaps ProfileName ProfileName TriggerPolicy
  deriving (Eq, Show)


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


isActiveTrigger :: TriggerPolicy -> Bool
isActiveTrigger trigger =
  case trigger of
    TriggerNone -> False
    TriggerAll -> True
    TriggerPrefix _ -> True


firstConflict :: [(ProfileName, TriggerPolicy)] -> Maybe TriggerConflict
firstConflict triggers =
  case triggerAlls of
    [] ->
      firstPrefixConflict prefixTriggers

    [_] ->
      case prefixTriggers of
        [] -> Nothing
        (name, trigger) : _ ->
          Just (TriggerAllOverlaps allProfile name (TriggerPrefix trigger))
      where
        (allProfile, _) =
          head triggerAlls

    (profileA, _) : (profileB, _) : _ ->
      Just (MultipleTriggerAll profileA profileB)
  where
    triggerAlls :: [(ProfileName, TriggerPolicy)]
    triggerAlls =
      [ pair
      | pair@(_, TriggerAll) <- triggers
      ]

    prefixTriggers :: [(ProfileName, Text)]
    prefixTriggers =
      [ (profName, prefix)
      | (profName, TriggerPrefix prefix) <- triggers
      ]


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