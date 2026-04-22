module GHC.Plugin.OllamaHoles.Template
    ( Template(..)
    , TemplateSpec(..)
    , TemplateSource(..)
    , TemplateError(..)
    , loadTemplate
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)



-- Loading
----------

newtype Template = Template Text
    deriving (Eq, Show)

-- | Runtime specification for selecting a template.
data TemplateSpec = TemplateSpec
    { tsSearchDir :: FilePath       -- where to look
    , tsSource    :: TemplateSource -- how to look
    } deriving (Eq, Show)

data TemplateSource
    = DefaultTemplate       -- Used if the spec is missing or malformed
    | TemplateFile FilePath -- When using a specific template by path
    | NamedTemplate Text    -- When using a template by name in @tsSearchDir@
    deriving (Eq, Show)

data TemplateError
    = TemplateFileNotFound FilePath
    | UnknownTemplateName FilePath Text
    deriving (Eq, Show)

loadTemplate :: TemplateSpec -> IO (Either TemplateError Template)
loadTemplate spec = do
    let TemplateSpec
          { tsSearchDir = searchDir
          , tsSource = source
          } = spec
    case source of
        DefaultTemplate ->
            pure (Right defaultTemplateText)

        TemplateFile path -> do
            exists <- doesFileExist path
            if exists
                then fmap (Right . Template) $ T.readFile path
                else pure $ Left $ TemplateFileNotFound path

        NamedTemplate name -> do
            let path = searchDir <> "/" <> T.unpack name <> ".txt"
            exists <- doesFileExist path
            if exists
                then fmap (Right . Template) $ T.readFile path
                else pure (Left (UnknownTemplateName searchDir name))



-- Default Template
-------------------

defaultTemplateText :: Template
defaultTemplateText = Template $ T.pack $ unlines
    [ "Preliminaries:"
    , "{docs}"
    , "--------------------------------------------------------------------"
    , "You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler."
    , "You are given a hole in a Haskell program, and you need to fill it in."
    , "The hole is represented by the following JSON encoded information:"
    , "{context}"
    , "Provide one or more Haskell expressions that could fill this hole."
    , "This means coming up with an expression of the correct type that satisfies the constraints."
    , "Pay special attention to the type of the hole, specifically whether it is a function."
    , "Make sure you synthesize an expression that matches the type of the hole."
    , "Output ONLY the raw Haskell expression(s), one per line."
    , "Do not try to bind the hole variable, e.g. `_b = ...`. Produce only the expression."
    , "Do not include explanations, introductions, or any surrounding text."
    , "If you are using a function from scope, make sure to use the qualified name from the list of things in scope."
    , "Output a maximum of {numexpr} expressions."
    ]
