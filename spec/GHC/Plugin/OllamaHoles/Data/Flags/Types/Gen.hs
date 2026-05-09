module GHC.Plugin.OllamaHoles.Data.Flags.Types.Gen
  ( genModelNameText
  , genBackendSlug
  , genBackendSlugText
  , genPositiveNumExpr
  , genOpenAIBaseUrlText
  , genOpenAIKeyNameText
  , genLogMode
  , genLogModeText
  , genTemplateNameText
  , genTemplateNameTextAndValue
  , genTemplatePathText
  , genTemplateSearchDirText
  , genTriggerPolicy
  , genTriggerPolicyText
  , genConfigPathSpec
  , genConfigPathSpecText
  , genUnknownBooleanFlag
  , genUnknownValueFlag
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Data.Flags.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Logger
import GHC.Plugin.OllamaHoles.Data.Template


genModelNameText :: QC.Gen Text
genModelNameText = do
  familyName <- QC.elements ["qwen3", "qwen2.5-coder", "llama3.2", "codellama", "phi4"]
  tag <- QC.elements ["latest", "7b", "14b", "instruct"]
  pure (familyName <> ":" <> tag)


genBackendSlug :: QC.Gen BackendSlug
genBackendSlug =
  QC.elements [Ollama, OpenAI, Gemini]


genBackendSlugText :: QC.Gen (Text, BackendSlug)
genBackendSlugText =
  QC.elements
    [ ("ollama", Ollama)
    , ("openai", OpenAI)
    , ("gemini", Gemini)
    ]


genPositiveNumExpr :: QC.Gen Int
genPositiveNumExpr =
  QC.chooseInt (1, 100)


genOpenAIBaseUrlText :: QC.Gen Text
genOpenAIBaseUrlText = do
  host <- genIdentText
  pure ("https://" <> host <> ".example.com/v1")


genOpenAIKeyNameText :: QC.Gen Text
genOpenAIKeyNameText = do
  pieces <- QC.listOf1 $
    QC.elements ["OPENAI", "GEMINI", "LOCAL", "TEST", "API", "KEY"]
  pure (T.intercalate "_" pieces)


genLogMode :: QC.Gen LogMode
genLogMode =
  QC.elements [LogOff, LogBasic, LogFull]


genLogModeText :: QC.Gen (Text, LogMode)
genLogModeText =
  QC.elements
    [ ("off", LogOff)
    , ("basic", LogBasic)
    , ("full", LogFull)
    ]


genTemplateNameText :: QC.Gen Text
genTemplateNameText = do
  first <- QC.elements ['a' .. 'z']
  rest <- QC.listOf $
    QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> "-_")
  pure . T.pack $ first : rest


genTemplateNameTextAndValue :: QC.Gen (Text, TemplateName)
genTemplateNameTextAndValue =
  QC.suchThatMap genTemplateNameText $ \raw ->
    case parseTemplateName raw of
      Left _ -> Nothing
      Right name -> Just (raw, name)


genTemplatePathText :: QC.Gen Text
genTemplatePathText = do
  dirCount <- QC.chooseInt (0, 3)
  dirs <- QC.vectorOf dirCount genPathSegmentText
  base <- genPathSegmentText
  ext <- QC.elements ["txt", "tmpl", "prompt", "md"]
  let file = base <> "." <> ext
  pure (T.intercalate "/" (dirs <> [file]))


genTemplateSearchDirText :: QC.Gen Text
genTemplateSearchDirText = do
  dirCount <- QC.chooseInt (1, 4)
  dirs <- QC.vectorOf dirCount genPathSegmentText
  pure ("/tmp/" <> T.intercalate "/" dirs)


genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.oneof
    [ pure TriggerAll
    , pure TriggerNone
    , TriggerPrefix <$> genTriggerPrefixText
    ]


genTriggerPolicyText :: QC.Gen (Text, TriggerPolicy)
genTriggerPolicyText =
  QC.oneof
    [ pure ("all", TriggerAll)
    , pure ("none", TriggerNone)
    , do
        prefix <- genTriggerPrefixText
        pure ("prefix:" <> prefix, TriggerPrefix prefix)
    ]


genConfigPathSpec :: QC.Gen ConfigPathSpec
genConfigPathSpec =
  QC.oneof
    [ pure ConfigDisabled
    , pure ConfigDefault
    , (ConfigExplicit . T.unpack) <$> genTemplatePathText
    ]


genConfigPathSpecText :: QC.Gen (Text, ConfigPathSpec)
genConfigPathSpecText =
  QC.oneof
    [ pure ("none", ConfigDisabled)
    , pure ("default", ConfigDefault)
    , do
        path <- genTemplatePathText
        pure (path, ConfigExplicit $ T.unpack path)
    ]


genUnknownBooleanFlag :: QC.Gen Text
genUnknownBooleanFlag =
  QC.elements
    [ "bogus"
    , "wat"
    , "unknown-option"
    , "not-a-real-flag"
    , "please-ignore"
    ]


genUnknownValueFlag :: QC.Gen (Text, Text)
genUnknownValueFlag = do
  key <- QC.elements
    [ "bogus"
    , "unknown"
    , "not-a-real-option"
    , "custom"
    ]
  value <- genIdentText
  pure (key, value)


genTriggerPrefixText :: QC.Gen Text
genTriggerPrefixText = do
  first <- QC.elements ['a' .. 'z']
  len <- QC.chooseInt (0, 12)
  if len == 0
    then pure (T.singleton first)
    else do
      middle <- QC.vectorOf (len - 1) $
        QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> "_")
      lastChar <- QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> "_")
      pure . T.pack $ first : middle <> [lastChar]


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


genIdentText :: QC.Gen Text
genIdentText = do
  first <- QC.elements (['a' .. 'z'] <> ['A' .. 'Z'])
  rest <- QC.listOf $
    QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_")
  pure . T.pack $ first : rest