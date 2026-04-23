module GHC.Plugin.OllamaHoles.Options where

import Data.Aeson (Value)
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
