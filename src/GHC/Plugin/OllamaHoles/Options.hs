{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Options
    ( Flags(..)
    , defaultFlags
    , parseCommandLineOptions
    , OptError(..)
    , Token(..)
    ) where

import GHC.Driver.Plugins (CommandLineOption)

import Control.Monad (foldM, (>=>))
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Monoid (Endo(..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)



parseCommandLineOptions
    :: Flags -> [CommandLineOption] -> Either OptError (Flags, [Token])
parseCommandLineOptions defaults opts = do
    (Endo h, unk) <- fmap mconcat $
        mapM (tokenize >=> parse >=> interpret) opts
    pure (h defaults, unk)

-- | Command line options for the plugin
data Flags = Flags
    { model_name          :: Text
    , backend_name        :: Text
    , num_expr            :: Int
    , debug               :: Bool
    , include_docs        :: Bool
    , openai_base_url     :: Text
    , openai_key_name     :: Text
    , model_options       :: Maybe Value
    , template_path       :: Maybe FilePath
    , template_name       :: Maybe Text
    , template_search_dir :: FilePath
    } deriving (Eq, Show)

-- | Default flags for the plugin
defaultFlags :: Flags
defaultFlags = Flags
    { model_name          = "qwen3:latest"
    , backend_name        = "ollama"
    , num_expr            = 5
    , debug               = False
    , include_docs        = False
    , openai_base_url     = "https://api.openai.com"
    , openai_key_name     = "OPENAI_API_KEY"
    , model_options       = Nothing
    , template_path       = Nothing
    , template_name       = Nothing
    , template_search_dir = "."
    }



-- Tokenization
---------------

-- Tokens are raw text flags or key value pairs.
data Token
    = BooleanToken FlagName
    | ValueToken FlagName Text
    deriving (Eq, Show)

type FlagName = Text

tokenize :: CommandLineOption -> Either OptError Token
tokenize opt = case T.breakOn "=" (T.pack opt) of
    (prefix, rest) -> if T.null prefix
        then Left EmptyOption
        else case T.uncons rest of
            Nothing -> Right (BooleanToken prefix)
            Just ('=', suffix) -> Right (ValueToken prefix suffix)
            _other -> Left (MalformedOption opt)



-- Parsing
----------

-- Raw tokens are parsed into semantic tokens.
data Flag
    = NoOp Token
    | SetModel Text
    | SetBackend Text
    | SetNumExpr Text
    | EnableDebug
    | EnableDocs
    | SetOpenAIBaseUrl Text
    | SetOpenAIKeyName Text
    | SetModelOptions Text
    | SetTemplatePath Text
    | SetTemplateName Text
    | SetTemplateDir Text
    deriving (Eq, Show)

parse :: Token -> Either OptError Flag
parse token = case token of
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
        "debug"           -> Left (UnexpectedValue "debug" val)
        "include-docs"    -> Left (UnexpectedValue "include-docs" val)
        _                 -> Right (NoOp token)



-- Interpretation
-----------------

-- Semantic tokens are interpreted as functions @Flags -> Flags@.
interpret :: Flag -> Either OptError (Endo Flags, [Token])
interpret flag = case flag of
    NoOp token -> pure (Endo id, [token])

    SetModel name -> requireNonEmpty "model" name $
        makeOk $ \fs -> fs { model_name = name }

    SetBackend name -> requireNonEmpty "backend" name $
        makeOk $ \fs -> fs { backend_name = name }

    EnableDebug -> makeOk $ \fs -> fs
        { debug = True }

    EnableDocs -> makeOk $ \fs -> fs
        { include_docs = True }

    SetOpenAIBaseUrl url -> requireNonEmpty "openai_base_url" url $
        makeOk $ \fs -> fs { openai_base_url = url }

    SetOpenAIKeyName key -> requireNonEmpty "openai_key_name" key $
        makeOk $ \fs -> fs { openai_key_name = key }

    SetTemplatePath path -> requireNonEmpty "template" path $
        makeOk $ \fs -> fs
            { template_path = Just (T.unpack path)
            , template_name = Nothing }

    SetTemplateName name -> requireNonEmpty "template-name" name $
        makeOk $ \fs -> fs
            { template_name = Just name
            , template_path = Nothing }

    SetTemplateDir dir -> requireNonEmpty "template-dir" dir $
        makeOk $ \fs -> fs { template_search_dir = T.unpack dir }

    SetNumExpr txt -> requireNonEmpty "n" txt $
        case readMaybe (T.unpack txt) of
            Just n -> makeOk $ \fs -> fs { num_expr = n }
            Nothing -> Left (InvalidInt "n" txt)

    SetModelOptions txt -> requireNonEmpty "model-options" txt $
        case Aeson.eitherDecodeStrictText txt of
            Right opts -> makeOk $ \fs -> fs { model_options = Just opts }
            Left err -> Left (InvalidJson "model-options" txt err)

    where
        makeOk :: (Applicative f) => (a -> a) -> f (Endo a, [b])
        makeOk x = pure (Endo x, [])

        requireNonEmpty :: FlagName -> Text -> Either OptError a -> Either OptError a
        requireNonEmpty name txt k
            | T.null txt = Left (EmptyValue name)
            | otherwise  = k



-- Errors
---------

data OptError
    = EmptyOption
    | MalformedOption CommandLineOption
    | MissingValue FlagName
    | UnexpectedValue FlagName Text
    | EmptyValue FlagName
    | InvalidInt FlagName Text
    | InvalidJson FlagName Text String
    deriving (Eq, Show)
