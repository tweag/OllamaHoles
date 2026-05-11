module GHC.Plugin.OllamaHoles.Data.Template.Types.Gen
  ( genPlainChunk1
  , genPlaceholderName
  , genTemplateText
  , genTemplateNameText
  , genInvalidTemplateNameText
  , genSafeFileNameText
  ) where

import Data.Char (isAscii, isAlpha, isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T
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

genTemplateText :: QC.Gen Text
genTemplateText = do
  n <- QC.chooseInt (0, 12)
  pieces <- QC.vectorOf n genTemplatePiece
  pure (T.concat pieces)

genTemplatePiece :: QC.Gen Text
genTemplatePiece =
  QC.frequency
    [ (4, genTemplateChunkText)
    , (1, genPlaceholderText)
    ]

genTemplateChunkText :: QC.Gen Text
genTemplateChunkText = do
  n <- QC.chooseInt (0, 24)
  chars <- QC.vectorOf n $
    QC.elements $
      ['a' .. 'z']
        <> ['A' .. 'Z']
        <> ['0' .. '9']
        <> " \n\t.,:;!?()[]-_/"
  pure (T.pack chars)

genPlaceholderText :: QC.Gen Text
genPlaceholderText = do
  name <- genPlaceholderNameText
  pure ("{{" <> name <> "}}")

genPlaceholderNameText :: QC.Gen Text
genPlaceholderNameText = do
  n <- QC.chooseInt (1, 12)
  chars <- QC.vectorOf n $
    QC.elements $
      ['a' .. 'z'] <> ['A' .. 'Z']
  pure (T.pack chars)

genTemplateNameText :: QC.Gen Text
genTemplateNameText = do
  first <- QC.elements templateNameChars
  n <- QC.chooseInt (0, 12)
  rest <- QC.vectorOf n (QC.elements templateNameChars)
  pure . T.pack $ first : rest

genSafeFileNameText :: QC.Gen Text
genSafeFileNameText =
  genTemplateNameText

genInvalidTemplateNameText :: QC.Gen Text
genInvalidTemplateNameText =
  QC.oneof
    [ pure ""
    , do
        name <- genTemplateNameText
        bad <- QC.elements ['/', '\\', '.', ' ', '\n', ':']
        suffix <- genTemplateNameText
        pure (name <> T.singleton bad <> suffix)
    ]
  `QC.suchThat` \name ->
    T.null name || T.any (not . nameSafeChar) name

templateNameChars :: [Char]
templateNameChars =
  ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_"

nameSafeChar :: Char -> Bool
nameSafeChar c = isAlphaNum c || c == '-' || c == '_'
