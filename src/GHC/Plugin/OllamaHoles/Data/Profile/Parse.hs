{-# LANGUAGE RecordWildCards #-}

module GHC.Plugin.OllamaHoles.Data.Profile.Parse
  ( tomlProfile
  ) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Toml.Schema qualified as Toml

import GHC.Plugin.OllamaHoles.Template (TemplateSource(..), parseTemplateName)
import GHC.Plugin.OllamaHoles.Data.Trigger.Types (TriggerPolicy(..))
import GHC.Plugin.OllamaHoles.Data.Trigger.Parse (parseTriggerPolicy)
import GHC.Plugin.OllamaHoles.Data.Service (tomlServiceName)

import GHC.Plugin.OllamaHoles.Data.Profile.Types



tomlProfileName :: Toml.Value' l -> Toml.Matcher l ProfileName
tomlProfileName = fmap ProfileName . Toml.fromValue

tomlModelName :: Toml.Value' l -> Toml.Matcher l ModelName
tomlModelName = fmap ModelName . Toml.fromValue



tomlProfile :: Toml.Value' l -> Toml.Matcher l Profile
tomlProfile = Toml.parseTableFromValue $ do
  profName <- Toml.reqKeyOf "name" tomlProfileName
  profKind <- Toml.reqKey "type" >>= parseProfileKind
  triggerTxt <- Toml.optKey "trigger"
  trigger    <- traverse parseTriggerPolicyField triggerTxt
  let profTrigger = maybe TriggerNone id trigger
  pure Profile{..}

parseProfileKind :: Text -> Toml.ParseTable l ProfileKind
parseProfileKind = \case
  "service"  -> ProfService <$> tomlServiceProf
  "profiles" -> ProfFanout <$> tomlFanoutProf
  "fanout"   -> ProfFanout <$> tomlFanoutProf
  bad        -> fail $ "invalid profile type: " <> T.unpack bad

tomlServiceProf :: Toml.ParseTable l ServiceProf
tomlServiceProf = do
  profService      <- Toml.reqKeyOf "service" tomlServiceName
  profModel        <- Toml.reqKeyOf "model" tomlModelName
  profTemplate     <- parseTemplateSourceFields
  profModelOptions <- fmap tomlToAeson
    <$> (Toml.optKey "model_options" :: Toml.ParseTable l (Maybe Toml.Value))
  profNumExpr      <- Toml.optKey "num_expr"
  profIncludeDocs  <- Toml.optKey "include_docs"
  pure ServiceProf{..}

tomlFanoutProf :: Toml.ParseTable l FanoutProf
tomlFanoutProf = do
  profs <- Toml.reqKeyOf "profiles" $ Toml.listOf $ const tomlProfileName
  case NE.nonEmpty profs of
    Nothing -> fail "profiles must be a non-empty list"
    Just xs -> pure FanoutProf
      { profProfiles = xs
      }

parseTemplateSourceFields :: Toml.ParseTable l (Maybe TemplateSource)
parseTemplateSourceFields = do
  mTemplate     <- Toml.optKey "template"      :: Toml.ParseTable l (Maybe Text)
  mTemplateFile <- Toml.optKey "template_file" :: Toml.ParseTable l (Maybe Text)
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

parseTriggerPolicyField :: Text -> Toml.ParseTable l TriggerPolicy
parseTriggerPolicyField txt = case parseTriggerPolicy txt of
  Left err -> fail ("invalid trigger policy: " <> show err)
  Right pol -> pure pol

tomlToAeson :: Toml.Value -> Aeson.Value
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

