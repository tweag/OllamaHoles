module GHC.Plugin.OllamaHoles.Data.Config.Types.Gen
  ( genSimpleFlags
  , genTriggerPolicy
  , genSafeFileName
  , genValidServiceOnlyConfigToml
  , genMissingServiceConfigToml
  , genSelfCycleConfigToml
  ) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Flags
import GHC.Plugin.OllamaHoles.Data.Trigger



genSimpleFlags :: QC.Gen Flags
genSimpleFlags = do
  model <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just . T.pack <$> QC.elements ["m1", "m2", "qwen3:latest"])
      ]

  n <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just <$> QC.chooseInt (1, 20))
      ]

  includeDocs <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just <$> QC.arbitrary)
      ]

  trigger <-
    QC.frequency
      [ (2, pure Nothing)
      , (1, Just <$> genTriggerPolicy)
      ]

  pure mempty
    { model_name = model
    , num_expr = n
    , include_docs = includeDocs
    , trigger_policy = trigger
    }


genTriggerPolicy :: QC.Gen TriggerPolicy
genTriggerPolicy =
  QC.oneof
    [ pure TriggerAll
    , pure TriggerNone
    , TriggerPrefix . T.pack <$> QC.elements ["llm", "ask", "hole"]
    ]


genSafeFileName :: QC.Gen FilePath
genSafeFileName = do
  n <- QC.chooseInt (1, 999999 :: Int)
  pure ("missing-" <> show n <> ".toml")


genValidServiceOnlyConfigToml
  :: QC.Gen (Text, Int, Int)
genValidServiceOnlyConfigToml = do
  serviceNames <-
    genUniqueNames "svc" 1 8

  profileNames <-
    genUniqueNames "prof" 0 12

  profileServices <-
    traverse
      (\_ -> QC.elements serviceNames)
      profileNames

  let
    serviceRows =
      serviceNames <&> \name ->
        "  { name = '" <> name <> "', protocol = 'ollama' }"

    profileRows =
      zip profileNames profileServices <&> \(profileName, serviceName) ->
        "  { name = '" <> profileName <> "', type = 'service', service = '" <> serviceName <> "', model = 'qwen3:latest' }"

    toml =
      renderConfigToml serviceRows profileRows

  pure
    ( toml
    , length serviceNames
    , length profileNames
    )

genMissingServiceConfigToml
  :: QC.Gen (Text, ProfileName, ServiceName)
genMissingServiceConfigToml = do
  serviceNames <-
    genUniqueNames "svc" 1 8

  profileNameText <-
    genNameWithPrefix "prof"

  missingServiceText <-
    genNameNotIn "missing" serviceNames

  let
    serviceRows =
      serviceNames <&> \name ->
        "  { name = '" <> name <> "', protocol = 'ollama' }"

    profileRows =
      [ "  { name = '" <> profileNameText <> "', type = 'service', service = '" <> missingServiceText <> "', model = 'qwen3:latest' }"
      ]

    toml =
      renderConfigToml serviceRows profileRows

  pure
    ( toml
    , ProfileName profileNameText
    , ServiceName missingServiceText
    )

genSelfCycleConfigToml
  :: QC.Gen (Text, ProfileName)
genSelfCycleConfigToml = do
  cyclicProfileText <-
    genNameWithPrefix "self"

  let
    serviceRows =
      [ "  { name = 'local', protocol = 'ollama' }"
      ]

    profileRows =
      [ "  { name = 'leaf', type = 'service', service = 'local', model = 'qwen3:latest' }"
      , "  { name = '" <> cyclicProfileText <> "', type = 'fanout', profiles = ['" <> cyclicProfileText <> "'] }"
      ]

    toml =
      renderConfigToml serviceRows profileRows

  pure
    ( toml
    , ProfileName cyclicProfileText
    )

renderConfigToml
  :: [Text] -> [Text] -> Text
renderConfigToml serviceRows profileRows =
  T.unlines $
    [ "services = ["
    ]
    <> commaTerminate serviceRows
    <>
    [ "]"
    , ""
    , "profiles = ["
    ]
    <> commaTerminate profileRows
    <>
    [ "]"
    ]

commaTerminate :: [Text] -> [Text]
commaTerminate [] =
  []

commaTerminate [x] =
  [x]

commaTerminate (x : xs) =
  (x <> ",") : commaTerminate xs

genUniqueNames
  :: Text -> Int -> Int -> QC.Gen [Text]
genUniqueNames prefix minCount maxCount = do
  n <- QC.chooseInt (minCount, maxCount)
  go n []
  where
    go 0 acc =
      pure (reverse acc)

    go k acc = do
      name <- genNameWithPrefix prefix
      if name `elem` acc
        then go k acc
        else go (k - 1) (name : acc)

genNameNotIn
  :: Text -> [Text] -> QC.Gen Text
genNameNotIn prefix forbidden = do
  name <- genNameWithPrefix prefix
  if name `elem` forbidden
    then genNameNotIn prefix forbidden
    else pure name


genNameWithPrefix
  :: Text -> QC.Gen Text
genNameWithPrefix prefix = do
  n <- QC.chooseInt (0, 9999)
  pure (prefix <> T.pack (show n))
