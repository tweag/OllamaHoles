module GHC.Plugin.OllamaHoles.Data.Flags.Parse
  ( parseFlags
  ) where

import GHC.Driver.Plugins (CommandLineOption)

import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Monoid (Endo(..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

import GHC.Plugin.OllamaHoles.Logger (LogMode(..))
import GHC.Plugin.OllamaHoles.Backend (parseBackendSlug)
import GHC.Plugin.OllamaHoles.Data.Trigger.Parse (parseTriggerPolicy)

import GHC.Plugin.OllamaHoles.Data.Flags.Types
import GHC.Plugin.OllamaHoles.Data.Flags.Error (FlagError(..))
import GHC.Plugin.OllamaHoles.Template (TemplateName, parseTemplateName)



-- Parsing
----------

-- | Each command line option is interpreted as a function on
-- the flags record, and we apply them right to left on the
-- empty record.
parseFlags
  :: [CommandLineOption] -> Either FlagError (Flags, [FlagToken])
parseFlags opts = do
  (Endo h, unk) <- fmap mconcat $
    mapM (tokenizeFlag >=> parseFlag >=> interpretFlag) opts
  pure (h mempty, unk)



-- Tokenization
---------------

tokenizeFlag :: CommandLineOption -> Either FlagError FlagToken
tokenizeFlag opt = case T.breakOn "=" (T.pack opt) of
  (prefix, rest) -> if T.null prefix
    then Left EmptyFlag
    else case T.uncons rest of
      Nothing            -> Right (BooleanToken prefix)
      Just ('=', suffix) -> Right (ValueToken prefix suffix)
      _other             -> Left (MalformedFlag opt)



-- Flag Parsing
---------------

parseFlag :: FlagToken -> Either FlagError FlagUpdate
parseFlag token = case token of
  BooleanToken key -> case key of
    "debug"           -> Right EnableDebug
    "include-docs"    -> Right EnableDocs
    "model"           -> Left (MissingValue "model")
    "backend"         -> Left (MissingValue "backend")
    "n"               -> Left (MissingValue "n")
    "openai_base_url" -> Left (MissingValue "openai_base_url")
    "openai_key_name" -> Left (MissingValue "openai_key_name")
    "model-options"   -> Left (MissingValue "model-options")
    "template"        -> Left (MissingValue "template")
    "template-name"   -> Left (MissingValue "template-name")
    "template-dir"    -> Left (MissingValue "template-dir")
    "log"             -> Left (MissingValue "log")
    "log-dir"         -> Left (MissingValue "log-dir")
    "trigger"         -> Left (MissingValue "trigger")
    "config"          -> Left (MissingValue "config")
    _                 -> Right (NoOp token)

  ValueToken key val -> case key of
    "model"           -> Right (SetModel val)
    "backend"         -> Right (SetBackend val)
    "n"               -> Right (SetNumExpr val)
    "openai_base_url" -> Right (SetOpenAIBaseUrl val)
    "openai_key_name" -> Right (SetOpenAIKeyName val)
    "model-options"   -> Right (SetModelOptions val)
    "template"        -> Right (SetTemplatePath val)
    "template-name"   -> Right (SetTemplateName val)
    "template-dir"    -> Right (SetTemplateDir val)
    "log"             -> Right (SetLogMode val)
    "log-dir"         -> Right (SetLogDir val)
    "trigger"         -> Right (SetTriggerPolicy val)
    "config"          -> Right (SetConfigPath val)
    "debug"           -> Left (UnexpectedValue "debug" val)
    "include-docs"    -> Left (UnexpectedValue "include-docs" val)
    _                 -> Right (NoOp token)



-- Interpretation
-----------------

-- Semantic tokens are interpreted as functions @Flags -> Flags@.
interpretFlag :: FlagUpdate -> Either FlagError (Endo Flags, [FlagToken])
interpretFlag flag = case flag of
  NoOp token -> pure (Endo id, [token])

  SetModel name -> requireNonEmpty "model" name $
    makeOk $ \fs -> fs { model_name = Just name }

  SetBackend name -> requireNonEmpty "backend" name $
    case parseBackendSlug name of
      Just slug -> makeOk $ \fs -> fs { backend_name = Just slug }
      Nothing   -> Left (InvalidBackend name)

  EnableDebug -> makeOk $ \fs -> fs { debug = Just True }

  EnableDocs -> makeOk $ \fs -> fs { include_docs = Just True }

  SetOpenAIBaseUrl url -> requireNonEmpty "openai_base_url" url $
    makeOk $ \fs -> fs { openai_base_url = Just url }

  SetOpenAIKeyName key -> requireNonEmpty "openai_key_name" key $
    makeOk $ \fs -> fs { openai_key_name = Just key }

  SetTemplatePath path -> requireNonEmpty "template" path $
    makeOk $ \fs -> fs
      { template_path = Just (T.unpack path)
      , template_name = Nothing }

  SetTemplateName name -> requireNonEmpty "template-name" name $
    case parseTemplateName name of
      Left err -> Left (InvalidTemplateNameFlag err name)
      Right ok -> makeOk $ \fs -> fs
        { template_name = Just ok
        , template_path = Nothing }

  SetTemplateDir dir -> requireNonEmpty "template-dir" dir $
    makeOk $ \fs -> fs { template_search_dir = Just $ T.unpack dir }

  SetNumExpr txt -> requireNonEmpty "n" txt $
    case readMaybe (T.unpack txt) of
      Just n -> makeOk $ \fs -> fs { num_expr = Just n }
      Nothing -> Left (InvalidInt "n" txt)

  SetModelOptions txt -> requireNonEmpty "model-options" txt $
    case Aeson.eitherDecodeStrictText txt of
      Right opts -> makeOk $ \fs -> fs { model_options = Just opts }
      Left err -> Left (InvalidJson "model-options" txt err)

  SetLogMode txt -> requireNonEmpty "log" txt $
    case txt of
      "off"   -> makeOk $ \fs -> fs { log_mode = Just LogOff }
      "basic" -> makeOk $ \fs -> fs { log_mode = Just LogBasic }
      "full"  -> makeOk $ \fs -> fs { log_mode = Just LogFull }
      _       -> Left (InvalidEnum "log" txt ["off", "basic", "full"])

  SetLogDir txt -> requireNonEmpty "log-dir" txt $
    makeOk $ \fs -> fs { log_dir = Just (T.unpack txt) }

  SetTriggerPolicy txt ->
    requireNonEmpty "trigger" txt $
      case parseTriggerPolicy txt of
        Right pol -> makeOk $ \fs -> fs { trigger_policy = Just pol }
        Left err -> Left (InvalidTriggerPolicy txt err)

  SetConfigPath txt -> requireNonEmpty "config" txt $
    makeOk $ \fs -> fs { config_path = Just $ ConfigExplicit $ T.unpack txt }

  where
    makeOk :: (Applicative f) => (a -> a) -> f (Endo a, [b])
    makeOk x = pure (Endo x, [])

    requireNonEmpty :: FlagName -> Text -> Either FlagError a -> Either FlagError a
    requireNonEmpty name txt k
      | T.null txt = Left (EmptyValue name)
      | otherwise  = k
