module GHC.Plugin.OllamaHoles.Data.Template.Types.Gen
  ( genPlainChunk1
  , genPlaceholderName
  ) where

import Data.Char (isAscii, isAlpha)
import Test.Tasty.QuickCheck qualified as QC



genPlainChunk1 :: QC.Gen String
genPlainChunk1 =
    fmap concat $ QC.listOf1 $
        -- ensure we never generate two consecutive '{'
        QC.oneof [pure (:[]), pure (:['{'])]
            <*> QC.suchThat QC.arbitrary (/= '{')

genPlaceholderName :: QC.Gen String
genPlaceholderName = QC.listOf1 $
    QC.suchThat QC.arbitrary (\c -> isAscii c && isAlpha c)
