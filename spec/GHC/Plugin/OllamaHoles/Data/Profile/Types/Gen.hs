{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Data.Profile.Types.Gen
  ( genProfileNameText
  , genProfileName
  , genModelNameText
  , genModelName
  , genServiceProf
  , genFanoutProf
  , genServiceProfile
  , genFanoutProfile
  , genTriggerPrefixText
  , genDisjointTriggerPrefixTexts
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Text qualified as T

import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Profile.Types
import GHC.Plugin.OllamaHoles.Data.Service.Types
import GHC.Plugin.OllamaHoles.Data.Service.Types.Gen
import GHC.Plugin.OllamaHoles.Data.Trigger.Types


genProfileNameText :: QC.Gen Text
genProfileNameText =
  T.pack <$> genIdentLikeString


genProfileName :: QC.Gen ProfileName
genProfileName =
  ProfileName <$> genProfileNameText


genModelNameText :: QC.Gen Text
genModelNameText =
  T.pack <$> do
    familyName <- QC.elements ["qwen3", "qwen2.5-coder", "llama3.2", "codellama"]
    tag <- QC.elements ["latest", "7b", "14b", "instruct"]
    pure (familyName <> ":" <> tag)


genModelName :: QC.Gen ModelName
genModelName =
  ModelName <$> genModelNameText


genTriggerPrefixText :: QC.Gen Text
genTriggerPrefixText =
  T.pack <$> do
    first <- QC.elements ['a' .. 'z']
    rest <- QC.listOf (QC.elements (['a' .. 'z'] <> ['0' .. '9'] <> "_"))
    pure (first : rest)


genDisjointTriggerPrefixTexts :: QC.Gen [Text]
genDisjointTriggerPrefixTexts = do
  chars <- QC.shuffle ['a' .. 'z']
  n <- QC.chooseInt (0, min 10 (length chars))
  suffix <- QC.chooseInt (0, 999 :: Int)
  pure
    [ T.pack [c] <> "_" <> T.pack (show suffix)
    | c <- take n chars
    ]


genServiceProf :: QC.Gen ServiceProf
genServiceProf = do
  serviceName <- ServiceName <$> genServiceNameText
  modelName <- genModelName
  numExpr <- QC.frequency
    [ (2, pure Nothing)
    , (1, Just <$> QC.chooseInt (1, 20))
    ]
  includeDocs <- QC.frequency
    [ (2, pure Nothing)
    , (1, Just <$> QC.arbitrary)
    ]

  pure ServiceProf
    { profService = serviceName
    , profModel = modelName
    , profTemplate = Nothing
    , profModelOptions = Nothing
    , profNumExpr = numExpr
    , profIncludeDocs = includeDocs
    }


genFanoutProf :: QC.Gen FanoutProf
genFanoutProf = do
  first <- genProfileName
  rest <- QC.listOf genProfileName
  pure FanoutProf
    { profProfiles = first :| rest
    }


genServiceProfile :: QC.Gen Profile
genServiceProfile = do
  name <- genProfileName
  serviceProf <- genServiceProf
  trigger <- genTriggerPolicy
  pure Profile
    { profName = name
    , profKind = ProfService serviceProf
    , profTrigger = trigger
    }


genFanoutProfile :: QC.Gen Profile
genFanoutProfile = do
  name <- genProfileName
  fanoutProf <- genFanoutProf
  trigger <- genTriggerPolicy
  pure Profile
    { profName = name
    , profKind = ProfFanout fanoutProf
    , profTrigger = trigger
    }


genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.frequency
    [ (3, pure TriggerNone)
    , (1, pure TriggerAll)
    , (3, TriggerPrefix <$> genTriggerPrefixText)
    ]


genIdentLikeString :: QC.Gen String
genIdentLikeString = do
  first <- QC.elements (['a' .. 'z'] <> ['A' .. 'Z'])
  rest <- QC.listOf $
    QC.elements (['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_")
  pure (first : rest)
