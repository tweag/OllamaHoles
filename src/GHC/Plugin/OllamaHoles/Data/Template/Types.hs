module GHC.Plugin.OllamaHoles.Data.Template.Types
  ( Template(..)
  , TemplateExpr(..)
  , Placeholder(..)
  , TemplateEnv(..)
  , TemplateSpec(..)
  , TemplateSource(..)
  , TemplateName(..)
  , unTemplateName
  , defaultTemplateText
  , Line
  , Col
  ) where

import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal



newtype Template
  = Template [TemplateExpr]
  deriving (Eq, Show)

data TemplateExpr
  = TemplateChunk Text
  | TemplateVar Placeholder
  deriving (Eq, Show)

newtype Placeholder = Placeholder
  { unPlaceholder :: Text
  } deriving (Eq, Ord)

instance IsString Placeholder where
  fromString = Placeholder . fromString

instance Show Placeholder where
  show (Placeholder txt) = T.unpack txt

data TemplateEnv
  = TemplateEnv (Map Placeholder Text)
  deriving (Eq, Show)

-- | Runtime specification for selecting a template.
data TemplateSpec = TemplateSpec
    { tsSearchDir :: FilePath                  -- if defined in filesystem
    , tsTmplMap   :: Map TemplateName Template -- if defined in config
    , tsSource    :: TemplateSource            -- how to look
    } deriving (Eq, Show)

data TemplateSource
    = DefaultTemplate             -- Used if the spec is not specified
    | TemplateFile FilePath       -- When using a specific template by path
    | NamedTemplate TemplateName  -- Template defined in the fancy config
    deriving (Eq, Show)

type Line = Int
type Col  = Int



defaultTemplateText :: Text
defaultTemplateText = T.pack $ unlines
  [ "Preliminaries:"
  , "{{docs}}"
  , "--------------------------------------------------------------------"
  , "You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler."
  , "You are given a hole in a Haskell program, and you need to fill it in."
  , "The hole is represented by the following JSON encoded information:"
  , "{{context}}"
  , "Provide one or more Haskell expressions that could fill this hole."
  , "This means coming up with an expression of the correct type that satisfies the constraints."
  , "Pay special attention to the type of the hole, specifically whether it is a function."
  , "Make sure you synthesize an expression that matches the type of the hole."
  , "Output ONLY the raw Haskell expression(s), one per line."
  , "Do not try to bind the hole variable, e.g. `_b = ...`. Produce only the expression."
  , "Do not include explanations, introductions, or any surrounding text."
  , "If you are using a function from scope, make sure to use the qualified name from the list of things in scope."
  , "Output a maximum of {{numexpr}} expressions."
  ]
