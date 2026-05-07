{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.Prefs.Types.Gen
  ( genValidTriggerPolicyText
  , genValidTriggerPolicyCase
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Trigger.Types



genValidTriggerPolicyText :: QC.Gen Text
genValidTriggerPolicyText =
  QC.oneof
    [ pure "all"
    , pure "none"
    , do
        c0 <- QC.elements ['a' .. 'z']
        rest <- QC.listOf (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_'"))
        pure ("prefix:" <> T.pack (c0 : rest))
    ]

genValidTriggerPolicyCase :: QC.Gen (Text, TriggerPolicy)
genValidTriggerPolicyCase =
  QC.oneof
    [ pure ("all", TriggerAll)
    , pure ("none", TriggerNone)
    , do
        prefix <- fmap T.pack $ QC.vectorOf 5 $ QC.oneof $ fmap pure ['a'..'z']
        pure ("prefix:" <> prefix, TriggerPrefix prefix)
    ]