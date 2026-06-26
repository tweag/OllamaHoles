module GHC.Plugin.OllamaHoles.Data.Service.Types.Gen
  ( genServiceNameText
  , genHostText
  , genEnvVarText
  , genUrlText
  , genTemplateFilePathText
  , genTemplateValueText
  , genStaticResponseText
  , genStaticResponseFileText
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Test.Tasty.QuickCheck qualified as QC



genServiceNameText :: QC.Gen Text
genServiceNameText =
  T.pack <$> genIdentLikeString


genHostText :: QC.Gen Text
genHostText = do
  port <- QC.chooseInt (1024, 49151)
  pure ("http://localhost:" <> T.pack (show port))


genUrlText :: QC.Gen Text
genUrlText = do
  name <- genServiceNameText
  pure ("https://" <> name <> ".example.com/v1")


genEnvVarText :: QC.Gen Text
genEnvVarText = do
  pieces <- QC.listOf1 (QC.elements ["OPENAI", "GEMINI", "LOCAL", "TEST", "API", "KEY"])
  pure (T.intercalate "_" pieces)


genIdentLikeString :: QC.Gen String
genIdentLikeString = do
  first <- QC.elements (['a' .. 'z'] <> ['A' .. 'Z'])
  rest <- QC.listOf (QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_"))
  pure (first : rest)

genPathSegmentText :: QC.Gen Text
genPathSegmentText = do
  first <- QC.elements pathHeadChars
  rest <- QC.listOf $
    QC.elements pathTailChars
  pure . T.pack $ first : rest
  where
    pathHeadChars =
      ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

    pathTailChars =
      pathHeadChars <> "-_."


genRelativeFilePathText :: QC.Gen Text
genRelativeFilePathText = do
  n <- QC.chooseInt (1, 4)
  segments <- QC.vectorOf n genPathSegmentText
  pure (T.intercalate "/" segments)


genTemplateFilePathText :: QC.Gen Text
genTemplateFilePathText = do
  dirCount <- QC.chooseInt (0, 3)
  dirs <- QC.vectorOf dirCount genPathSegmentText
  base <- genPathSegmentText
  ext <- QC.elements ["txt", "tmpl", "prompt", "md"]
  let file = base <> "." <> ext
  pure (T.intercalate "/" (dirs <> [file]))

genTemplateValueText :: QC.Gen Text
genTemplateValueText =
  QC.frequency
    [ (1, pure "default")
    , (5, do
        first <- QC.elements ['a' .. 'z']
        rest <- QC.listOf $ QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> "-_")
        pure . T.pack $ first : rest
      )
    ]

genStaticResponseText :: QC.Gen Text
genStaticResponseText =
  T.unlines <$> QC.listOf1 genCandidateLineText

genCandidateLineText :: QC.Gen Text
genCandidateLineText =
  QC.elements
    [ "Just (UserId 0)"
    , "Just (UserId (read s))"
    , "UserId <$> readMaybe s"
    , "UserId <$> Just (length s)"
    ]

genStaticResponseFileText :: QC.Gen Text
genStaticResponseFileText =
  QC.elements
    [ "test/fixtures/userid.candidates"
    , "spec/fixtures/static.candidates"
    , "fixtures/candidates.txt"
    ]
