module GHC.Plugin.OllamaHoles.Data.Template.Types.Gen
  ( genPlainChunk1
  , genPlaceholderName
  , genTemplateText
  , genTemplateNameText
  , genInvalidTemplateNameText
  , genSafeFileNameText
  , genPlaceholderNameText
  , genKnownExpansionCase
  , genUnknownExpansionCase
  , genTemplateChunks
  , genUnusedEnvPairs
  , genUnusedEnvPairsFor
  , genValidTemplate
  , genValidTemplateName
  , genSafeTemplateNameText
  , genTemplateValueText
  ) where

import Data.Char (isAscii, isAlpha, isAlphaNum)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal



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

genPlaceholder :: QC.Gen Placeholder
genPlaceholder =
  Placeholder . T.pack <$> genPlaceholderName

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


genValidTemplateName :: QC.Gen TemplateName
genValidTemplateName =
  unsafeCreateRawTemplateName <$> genTemplateNameText


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



genTemplateChunks :: QC.Gen [Text]
genTemplateChunks = do
  n <- QC.chooseInt (0, 16)
  QC.vectorOf n genChunkText

genKnownExpansionCase :: QC.Gen (Template, [(Placeholder, Text)], Text)
genKnownExpansionCase = do
  envPairs <- genDistinctEnvPairs
  exprs <- genKnownTemplateExprs envPairs
  let templ = Template exprs
      envMap = M.fromList envPairs
      expected = expandExpected envMap exprs
  pure (templ, envPairs, expected)

genUnknownExpansionCase :: QC.Gen (Template, [(Placeholder, Text)], [Placeholder])
genUnknownExpansionCase = do
  knownPairs <- genDistinctEnvPairs
  unknowns <- genUnknownNames (fst <$> knownPairs)
  exprs <- genMixedTemplateExprs knownPairs unknowns
  let missing =
        [ name
        | TemplateVar name <- exprs
        , name `notElem` fmap fst knownPairs
        ]
  pure (Template exprs, knownPairs, missing)

genDistinctEnvPairs :: QC.Gen [(Placeholder, Text)]
genDistinctEnvPairs = do
  n <- QC.chooseInt (0, 12)
  names <- genDistinctPlaceholderNames n
  vals <- QC.vectorOf (length names) genValueText
  pure (zip names vals)

genUnusedEnvPairs :: QC.Gen [(Placeholder, Text)]
genUnusedEnvPairs = do
  n <- QC.chooseInt (0, 8)
  names <- genDistinctPlaceholderNames n
  vals <- QC.vectorOf (length names) genValueText
  pure (zip names vals)

genValidTemplate :: QC.Gen Template
genValidTemplate =
  QC.sized $ \n -> do
    k <- QC.chooseInt (0, min 8 n)
    Template <$> QC.vectorOf k genTemplateExpr

genTemplateExpr :: QC.Gen TemplateExpr
genTemplateExpr =
  QC.oneof
    [ TemplateChunk <$> genTemplateChunkText
    , TemplateVar <$> genKnownPlaceholder
    ]

genKnownPlaceholder :: QC.Gen Placeholder
genKnownPlaceholder =
  Placeholder <$> QC.elements
    [ "hole_variable"
    , "hole_type"
    , "module"
    , "imports"
    , "constraints"
    , "known_fits"
    , "guidance"
    ]

genTemplateMapEntry :: QC.Gen (TemplateName, Template)
genTemplateMapEntry = (,)
  <$> genValidTemplateName <*> genValidTemplate


genKnownTemplateExprs :: [(Placeholder, Text)] -> QC.Gen [TemplateExpr]
genKnownTemplateExprs envPairs = do
  n <- QC.chooseInt (0, 24)
  QC.vectorOf n $
    QC.frequency
      [ (3, TemplateChunk <$> genChunkText)
      , ( if null envPairs then 0 else 2
        , TemplateVar . fst <$> QC.elements envPairs
        )
      ]

genMixedTemplateExprs
  :: [(Placeholder, Text)]
  -> [Placeholder]
  -> QC.Gen [TemplateExpr]
genMixedTemplateExprs knownPairs unknowns = do
  n <- QC.chooseInt (0, 16)
  prefix <- QC.vectorOf n $
    QC.frequency
      [ (3, TemplateChunk <$> genChunkText)
      , ( if null knownPairs then 0 else 2
        , TemplateVar . fst <$> QC.elements knownPairs
        )
      , (2, TemplateVar <$> QC.elements unknowns)
      ]

  -- Ensure the case actually contains at least one unknown placeholder.
  forced <- TemplateVar <$> QC.elements unknowns
  suffix <- QC.vectorOf n $
    QC.frequency
      [ (3, TemplateChunk <$> genChunkText)
      , ( if null knownPairs then 0 else 2
        , TemplateVar . fst <$> QC.elements knownPairs
        )
      , (2, TemplateVar <$> QC.elements unknowns)
      ]

  pure (prefix <> [forced] <> suffix)

genUnknownNames :: [Placeholder] -> QC.Gen [Placeholder]
genUnknownNames known = do
  n <- QC.chooseInt (1, 8)
  QC.vectorOf n $
    genPlaceholder `QC.suchThat` (`notElem` known)

genDistinctPlaceholderNames :: Int -> QC.Gen [Placeholder]
genDistinctPlaceholderNames n =
  go [] n
  where
    go acc k
      | k <= 0 = pure (reverse acc)
      | otherwise = do
          name <- genPlaceholder `QC.suchThat` (`notElem` acc)
          go (name : acc) (k - 1)

genDistinctPlaceholderNamesAvoiding :: [Placeholder] -> Int -> QC.Gen [Placeholder]
genDistinctPlaceholderNamesAvoiding avoid n =
  go [] n
  where
    go acc k
      | k <= 0 = pure (reverse acc)
      | otherwise = do
          name <- genPlaceholder
            `QC.suchThat` \x -> x `notElem` avoid && x `notElem` acc
          go (name : acc) (k - 1)



genChunkText :: QC.Gen Text
genChunkText = do
  n <- QC.chooseInt (0, 24)
  chars <- QC.vectorOf n $
    QC.elements $
      ['a' .. 'z']
        <> ['A' .. 'Z']
        <> ['0' .. '9']
        <> " \n\t.,:;!?()[]-_/"
  pure (T.pack chars)

genValueText :: QC.Gen Text
genValueText = do
  n <- QC.chooseInt (0, 24)
  chars <- QC.vectorOf n $
    QC.elements $
      ['a' .. 'z']
        <> ['A' .. 'Z']
        <> ['0' .. '9']
        <> " \n\t.,:;!?()[]-_/"
  pure (T.pack chars)

expandExpected :: M.Map Placeholder Text -> [TemplateExpr] -> Text
expandExpected envMap =
  T.concat . fmap go
  where
    go = \case
      TemplateChunk txt ->
        txt
      TemplateVar name ->
        case M.lookup name envMap of
          Nothing ->
            ""
          Just val ->
            val

placeholderChars :: [Char]
placeholderChars =
  ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "_"

genUnusedEnvPairsFor
  :: Template
  -> [(Placeholder, Text)]
  -> QC.Gen [(Placeholder, Text)]
genUnusedEnvPairsFor (Template exprs) envPairs = do
  let used =
        [ name
        | TemplateVar name <- exprs
        ]

      existing = fmap fst envPairs
      avoid = used <> existing

  n <- QC.chooseInt (0, 8)
  names <- genDistinctPlaceholderNamesAvoiding avoid n
  vals <- QC.vectorOf (length names) genValueText
  pure (zip names vals)

genTemplateValueText :: QC.Gen Text
genTemplateValueText = do
  first <- QC.elements ['a' .. 'z']
  rest <- QC.listOf $
    QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> "-_")
  pure . T.pack $ first : rest

genSafeTemplateNameText :: QC.Gen Text
genSafeTemplateNameText =
  QC.suchThat genCandidate $ \raw ->
    case parseTemplateName raw of
      Right _ -> True
      Left _ -> False
 where
  genCandidate = do
    first <- QC.elements $
      ['a'..'z'] <> ['A'..'Z']

    n <- QC.chooseInt (0, 16)

    rest <- QC.vectorOf n $
      QC.elements $
        ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "_-"

    pure $ T.pack (first : rest)
