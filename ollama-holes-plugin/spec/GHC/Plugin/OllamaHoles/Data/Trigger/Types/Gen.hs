module GHC.Plugin.OllamaHoles.Data.Trigger.Types.Gen
  ( genTriggerPolicy
  , genValidPrefix
  , genValidSuffix
  , genHoleName
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Trigger



genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.oneof
    [ pure TriggerAll
    , pure TriggerNone
    , TriggerPrefix <$> genValidPrefix
    ]

genValidPrefix :: QC.Gen Text
genValidPrefix = do
  c0 <- QC.elements ['a' .. 'z']
  rest <- QC.listOf genIdentifierContinueChar
  pure (T.pack (c0 : rest))

genValidSuffix :: QC.Gen Text
genValidSuffix =
  T.pack <$> QC.listOf genIdentifierContinueChar

genDistinctSuffixPair :: QC.Gen (Text, Text)
genDistinctSuffixPair =
  QC.suchThat
    ((,) <$> genValidSuffix <*> genValidSuffix)
    (uncurry (/=))

genHoleName :: QC.Gen Text
genHoleName =
  QC.oneof
    [ pure "_"
    , ("_" <>) <$> ((<>) <$> genValidPrefix <*> genValidSuffix)
    , T.pack <$> QC.listOf1 QC.arbitraryASCIIChar
    ]

genIdentifierContinueChar :: QC.Gen Char
genIdentifierContinueChar =
  QC.frequency
    [ (10, QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']))
    , (1, pure '_')
    , (1, pure '\'')
    ]
