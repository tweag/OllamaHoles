{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Config.Types where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import Toml.Schema
  (FromValue(..), optKey, parseTableFromValue, reqKey)
import Toml.Schema.ParseTable (ParseTable)
import Toml.Semantics.Types qualified as Toml

import GHC.Plugin.OllamaHoles.Template
  (TemplateSource(..), parseTemplateName)
import GHC.Plugin.OllamaHoles.Trigger



newtype ProfileName = ProfileName Text
  deriving (Eq, Ord, Show)

newtype ServiceName = ServiceName Text
  deriving (Eq, Ord, Show)

newtype ModelName = ModelName Text
  deriving (Eq, Ord, Show)

instance IsString ProfileName where
  fromString = ProfileName . fromString

instance IsString ServiceName where
  fromString = ServiceName . fromString

instance IsString ModelName where
  fromString = ModelName . fromString

instance FromValue ProfileName where
  fromValue v = ProfileName <$> fromValue v

instance FromValue ServiceName where
  fromValue v = ServiceName <$> fromValue v

instance FromValue ModelName where
  fromValue v = ModelName <$> fromValue v



-- Services
-----------

-- | A @Service@ accepts prompts and returns responses.
-- It has a name and a configuration specifying how to
-- communicate with it.
data Service = Service
  { svcName   :: ServiceName
  , svcConfig :: ServiceConfig
  } deriving (Eq, Show, Generic)

data ServiceConfig
  = SvcOllama OllamaConfig
  | SvcOpenAI OpenAIConfig
  | SvcGemini GeminiConfig
  deriving (Eq, Show, Generic)

data OllamaConfig = OllamaConfig
  { svcOllamaHost :: Maybe Text
  } deriving (Eq, Show, Generic)

data OpenAIConfig = OpenAIConfig
  { svcOpenAIBaseUrl :: Text
  , svcOpenAIKeyName :: Text
  } deriving (Eq, Show, Generic)

data GeminiConfig = GeminiConfig
  { svcGeminiKeyName :: Text
  } deriving (Eq, Show, Generic)



instance FromValue Service where
  fromValue = parseTableFromValue $ do
    name <- reqKey "name"
    protocol <- reqKey "protocol"
    cfg <- parseServiceConfig protocol
    pure Service
      { svcName = name
      , svcConfig = cfg
      }

parseServiceConfig :: Text -> ParseTable l ServiceConfig
parseServiceConfig = \case
  "ollama" ->
    SvcOllama <$> (OllamaConfig <$> optKey "host")

  "openai" ->
    SvcOpenAI <$> (OpenAIConfig
      <$> reqKey "base_url"
      <*> reqKey "key_name")

  "openai-compatible" ->
    SvcOpenAI <$> (OpenAIConfig
      <$> reqKey "base_url"
      <*> reqKey "key_name")

  "gemini" ->
    SvcGemini <$> (GeminiConfig
      <$> reqKey "key_name")

  bad ->
    fail ("invalid service protocol: " <> T.unpack bad)



-- Profiles
-----------

-- | A @Profile@ consists of details about how
-- to construct a prompt for a service.
data Profile = Profile
  { profName    :: ProfileName
  , profKind    :: ProfileKind
  , profTrigger :: TriggerPolicy
  } deriving (Eq, Show, Generic)

data ProfileKind
  = ProfService ServiceProf
  | ProfFanout  FanoutProf
  deriving (Eq, Show, Generic)

data ServiceProf = ServiceProf
  { profService      :: ServiceName
  , profModel        :: ModelName
  , profTemplate     :: Maybe TemplateSource
  , profModelOptions :: Maybe Value
  , profNumExpr      :: Maybe Int
  , profIncludeDocs  :: Maybe Bool
  } deriving (Eq, Show, Generic)

data FanoutProf = FanoutProf
  { profProfiles :: NonEmpty ProfileName
  } deriving (Eq, Show, Generic)



instance FromValue Profile where
  fromValue = parseTableFromValue $ do
    name <- reqKey "name"
    typ  <- reqKey "type"
    kind <- parseProfileKind typ
    triggerTxt <- optKey "trigger"
    trigger    <- traverse parseTriggerPolicyField triggerTxt
    pure Profile
      { profName    = name
      , profKind    = kind
      , profTrigger = maybe TriggerNone id trigger
      }

parseProfileKind :: Text -> ParseTable l ProfileKind
parseProfileKind = \case
  "service" ->
    ProfService <$> parseServiceProfFields

  "profiles" ->
    ProfFanout <$> parseFanoutProfFields

  "fanout" ->
    ProfFanout <$> parseFanoutProfFields

  bad ->
    fail ("invalid profile type: " <> T.unpack bad)

parseServiceProfFields :: ParseTable l ServiceProf
parseServiceProfFields = do
  service      <- reqKey "service"
  model        <- reqKey "model"
  template     <- parseTemplateSourceFields
  modelOptions <- fmap tomlToAeson <$> (optKey "model_options" :: ParseTable l (Maybe Toml.Value))
  numExpr      <- optKey "num_expr"
  includeDocs  <- optKey "include_docs"

  pure ServiceProf
    { profService      = service
    , profModel        = model
    , profTemplate     = template
    , profModelOptions = modelOptions
    , profNumExpr      = numExpr
    , profIncludeDocs  = includeDocs
    }

parseFanoutProfFields :: ParseTable l FanoutProf
parseFanoutProfFields = do
  profs <- reqKey "profiles"
  case NE.nonEmpty profs of
    Nothing -> fail "profiles must be a non-empty list"
    Just xs -> pure FanoutProf
      { profProfiles = xs
      }

parseTemplateSourceFields :: ParseTable l (Maybe TemplateSource)
parseTemplateSourceFields = do
  mTemplate     <- optKey "template"      :: ParseTable l (Maybe Text)
  mTemplateFile <- optKey "template_file" :: ParseTable l (Maybe Text)
  case (mTemplate, mTemplateFile) of
    (Nothing, Nothing) ->
      pure Nothing

    (Just "default", Nothing) ->
      pure (Just DefaultTemplate)

    (Just nm, Nothing) -> case parseTemplateName nm of
      Right name -> pure (Just (NamedTemplate name))
      Left err -> fail $ show err

    (Nothing, Just fp) ->
      pure (Just (TemplateFile (T.unpack fp)))

    (Just _, Just _) ->
      fail "cannot specify both template and template_file"

parseTriggerPolicyField :: Text -> ParseTable l TriggerPolicy
parseTriggerPolicyField txt = case parseTriggerPolicy txt of
  Left err -> fail ("invalid trigger policy: " <> show err)
  Right pol -> pure pol



-- Conversion

tomlToAeson :: Toml.Value -> Value
tomlToAeson = \case
  Toml.Integer n -> Aeson.toJSON n
  Toml.Double x -> Aeson.toJSON x
  Toml.Bool b -> Aeson.toJSON b
  Toml.Text t -> Aeson.String t
  Toml.List xs -> Aeson.Array (Vector.fromList (map tomlToAeson xs))
  Toml.Table tbl -> Aeson.Object (tomlTableToAesonObject tbl)
  Toml.Day d -> Aeson.String (T.pack (show d))
  Toml.LocalTime t -> Aeson.String (T.pack (show t))
  Toml.ZonedTime t -> Aeson.String (T.pack (show t))
  Toml.TimeOfDay t -> Aeson.String (T.pack (show t))

tomlTableToAesonObject :: Toml.Table -> Aeson.Object
tomlTableToAesonObject (Toml.MkTable m) =
  AesonKeyMap.fromList
    [ (AesonKey.fromText k, tomlToAeson v)
    | (k, (_ann, v)) <- Map.assocs m
    ]
