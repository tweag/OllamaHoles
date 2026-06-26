module GHC.Plugin.OllamaHoles.Data.Flags.Types
  ( Flags(..)
  , FlagToken(..)
  , FlagName
  , FlagUpdate(..)
  , ConfigPathSpec(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import GHC.Generics (Generic)

import GHC.Plugin.OllamaHoles.Backend (BackendSlug(..))
import GHC.Plugin.OllamaHoles.Logger (LogMode(..))
import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Template



-- Flags
--------

-- | Command line options for the plugin
data Flags = Flags
    { model_name          :: Maybe Text
    , backend_name        :: Maybe BackendSlug
    , num_expr            :: Maybe Int
    , debug               :: Maybe Bool
    , include_docs        :: Maybe Bool
    , openai_base_url     :: Maybe Text
    , openai_key_name     :: Maybe Text
    , ollama_host         :: Maybe Text
    , model_options       :: Maybe Value
    , template_path       :: Maybe FilePath
    , template_name       :: Maybe TemplateName
    , template_search_dir :: Maybe FilePath
    , log_mode            :: Maybe LogMode
    , log_dir             :: Maybe FilePath
    , trigger_policy      :: Maybe TriggerPolicy
    , config_path         :: Maybe ConfigPathSpec
    , static_response_path :: Maybe FilePath
    } deriving (Eq, Show)

-- | Componentwise left zero semigroup with adjoined unit.
instance Semigroup Flags where
  f1 <> f2 = Flags
    { model_name          = merge model_name
    , backend_name        = merge backend_name
    , num_expr            = merge num_expr
    , debug               = merge debug
    , include_docs        = merge include_docs
    , openai_base_url     = merge openai_base_url
    , openai_key_name     = merge openai_key_name
    , ollama_host         = merge ollama_host
    , model_options       = merge model_options
    , template_path       = merge template_path
    , template_name       = merge template_name
    , template_search_dir = merge template_search_dir
    , log_mode            = merge log_mode
    , log_dir             = merge log_dir
    , trigger_policy      = merge trigger_policy
    , config_path         = merge config_path
    , static_response_path = merge static_response_path
    }
    where
      merge :: forall a. (Flags -> Maybe a) -> Maybe a
      merge g = case g f1 of Nothing -> g f2; Just a -> Just a

instance Monoid Flags where
  -- Default flags for the plugin
  mempty = Flags
    { model_name          = Nothing
    , backend_name        = Nothing
    , num_expr            = Nothing
    , debug               = Nothing
    , include_docs        = Nothing
    , openai_base_url     = Nothing
    , openai_key_name     = Nothing
    , ollama_host         = Nothing
    , model_options       = Nothing
    , template_path       = Nothing
    , template_name       = Nothing
    , template_search_dir = Nothing
    , log_mode            = Nothing
    , log_dir             = Nothing
    , trigger_policy      = Nothing
    , config_path         = Nothing
    , static_response_path = Nothing
    }

data ConfigPathSpec
  = ConfigDefault
  | ConfigExplicit FilePath
  | ConfigDisabled
  deriving (Eq, Show, Generic)



-- Parsing
----------

-- Tokens are raw text flags or key value pairs.
data FlagToken
    = BooleanToken FlagName
    | ValueToken FlagName Text
    deriving (Eq, Show)

type FlagName = Text

-- Raw tokens are parsed into semantic tokens.
data FlagUpdate
    = NoOp FlagToken
    | SetModel Text
    | SetBackend Text
    | SetNumExpr Text
    | EnableDebug
    | DisableDebug
    | EnableDocs
    | DisableDocs
    | SetOpenAIBaseUrl Text
    | SetOpenAIKeyName Text
    | SetOllamaHost Text
    | SetModelOptions Text
    | SetTemplatePath Text
    | SetTemplateName Text
    | SetTemplateDir Text
    | SetLogMode Text
    | SetLogDir Text
    | SetTriggerPolicy Text
    | SetConfigPath Text
    | SetStaticResponsePath Text
    deriving (Eq, Show)
