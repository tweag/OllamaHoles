module GHC.Plugin.OllamaHoles.Data.ServiceCall.Types.Gen where

import Data.List.NonEmpty qualified as NE
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Trigger



genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.oneof
    [ pure TriggerNone
    , pure TriggerAll
    , TriggerPrefix <$> genPrefixText
    ]

genPrefixText :: QC.Gen Text
genPrefixText =
  T.pack <$> QC.listOf1 genIdentChar

genHoleName :: QC.Gen Text
genHoleName =
  QC.oneof
    [ ("_" <>) <$> genPrefixText
    , ("_" <>) <$> genPrefixTextWithSuffix
    ]

genPrefixTextWithSuffix :: QC.Gen Text
genPrefixTextWithSuffix = do
  prefix <- genPrefixText
  suffix <- T.pack <$> QC.listOf genIdentChar
  pure (prefix <> suffix)

genIdentChar :: QC.Gen Char
genIdentChar =
  QC.elements $
    ['a' .. 'z']
      <> ['A' .. 'Z']
      <> ['0' .. '9']
      <> "_"

genDistinctIndices :: QC.Gen [Int]
genDistinctIndices = do
  n <- QC.chooseInt (1, 8)
  xs <- QC.shuffle [1 .. n]
  pure (nub xs)
