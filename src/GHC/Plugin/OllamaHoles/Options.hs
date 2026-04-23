{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Options
    ( Flags(..)
    , defaultFlags
    , parseFlags
    , parseCommandLineOptions
    ) where

import GHC.Driver.Plugins (CommandLineOption)

import Control.Monad (foldM)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T



parseCommandLineOptions
    :: Flags -> [CommandLineOption]
    -> Either [OptError] (Flags, [(Text, Maybe Text)])
parseCommandLineOptions defaults opts = do
    let (flags, errs, unk) = parseOptions $ reverse opts
    if not (null errs)
        then Left (reverse errs)
        else case applyFlags defaults flags of
            Left err -> Left (err : reverse errs)
            Right ok -> Right (ok, reverse unk)



-- Flags
--------

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



-- Updating
-----------

data Flag
    = BooleanFlag FlagName
    | ValueFlag FlagName Text

type FlagName = Text

applyFlags :: Flags -> [Flag] -> Either OptError Flags
applyFlags = foldM applyFlag

applyFlag :: Flags -> Flag -> Either OptError Flags
applyFlag fs flag = case flag of
    -- unrecognized; do nothing
    otherwise -> Right fs



-- Parsing
----------

parseOptions :: [CommandLineOption] -> ([Flag], [OptError], [(Text, Maybe Text)])
parseOptions = mconcat . fmap (parseOption thOptions)

parseOption
    :: ((Text, Maybe Text) -> ([Flag], [OptError], [(Text, Maybe Text)]))
    -> CommandLineOption -> ([Flag], [OptError], [(Text, Maybe Text)])
parseOption parse opt = case breakOpt opt of
    Nothing -> (mempty, [MalformedOption opt], mempty)
    Just ok -> parse ok
    where
        breakOpt :: CommandLineOption -> Maybe (Text, Maybe Text)
        breakOpt str = case T.breakOn "=" (T.pack str) of
            (prefix, rest) -> if T.null prefix
                then Nothing
                else case rest of
                    T.Empty -> Just (prefix, Nothing)
                    '=' T.:< suffix -> Just (prefix, Just suffix)
                    otherwise -> Nothing

thOptions :: (Text, Maybe Text) -> ([Flag], [OptError], [(Text, Maybe Text)])
thOptions (key, mVal) = case key of
    -- well formed but unrecognized
    otherwise -> (mempty, mempty, [(key, mVal)])



-- Errors
---------

data OptError
    = MalformedOption CommandLineOption
    deriving (Eq, Show)



-- Old
------

parseFlags :: [CommandLineOption] -> Flags
parseFlags opts = case parseCommandLineOptions defaultFlags opts of
    Left _ -> defaultFlags
    Right (ok, _) -> ok

-- | Parse command line options
parseFlagsOld :: [CommandLineOption] -> Flags
parseFlagsOld = parseFlags' defaultFlags . reverse -- reverse so outside options come first
  where
    parseFlags' :: Flags -> [CommandLineOption] -> Flags
    parseFlags' flags [] = flags
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "model=" (T.pack opt) =
            let model_name = T.drop (T.length "model=") (T.pack opt)
             in parseFlags' flags{model_name = model_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "backend=" (T.pack opt) =
            let backend_name = T.drop (T.length "backend=") (T.pack opt)
             in parseFlags' flags{backend_name = backend_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "openai_base_url=" (T.pack opt) =
            let openai_base_url = T.drop (T.length "openai_base_url=") (T.pack opt)
             in parseFlags' flags{openai_base_url = openai_base_url} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "openai_key_name=" (T.pack opt) =
            let openai_key_name = T.drop (T.length "openai_key_name=") (T.pack opt)
             in parseFlags' flags{openai_key_name = openai_key_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "debug" (T.pack opt) = parseFlags' flags{debug = True} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "include-docs" (T.pack opt) = parseFlags' flags{include_docs = True} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "n=" (T.pack opt) =
            let num_expr = T.unpack $ T.drop (T.length "n=") (T.pack opt)
             in parseFlags' flags{num_expr = read num_expr} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "model-options=" (T.pack opt) =
            let model_options = T.drop (T.length "model-options=") (T.pack opt)
                m_opts = Aeson.eitherDecodeStrictText model_options
            in case m_opts of
                Right o -> parseFlags' flags{model_options = Just o } opts
                Left err -> error $ "Failed to parse model-options: " <> err
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "template=" (T.pack opt) =
            let template_path = Just . T.unpack $ T.drop (T.length "template=") (T.pack opt)
            in parseFlags' flags{template_path = template_path, template_name = Nothing} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "template-name=" (T.pack opt) =
            let template_name = Just $ T.drop (T.length "template-name=") (T.pack opt)
            in parseFlags' flags{template_name = template_name, template_path = Nothing} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "template-dir=" (T.pack opt) =
            let template_search_dir = T.unpack $ T.drop (T.length "template-dir=") (T.pack opt)
            in parseFlags' flags{template_search_dir = template_search_dir} opts
    parseFlags' flags _ = flags
