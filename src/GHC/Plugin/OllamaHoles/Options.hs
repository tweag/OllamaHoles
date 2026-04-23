{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Options
    ( Flags(..)
    , defaultFlags
    , parseFlags
    ) where

import GHC.Driver.Plugins (CommandLineOption)

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T



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
    } deriving (Show)

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



-- | Parse command line options
parseFlags :: [CommandLineOption] -> Flags
parseFlags = parseFlags' defaultFlags . reverse -- reverse so outside options come first
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
