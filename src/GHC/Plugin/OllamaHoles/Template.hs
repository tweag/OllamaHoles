{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Template
    ( Template(..)
    , TemplateExpr(..)
    , Placeholder(..)
    , TemplateSpec(..)
    , TemplateSource(..)
    , TemplateName()
    , parseTemplateName
    , TemplateError(..)
    , TemplateParseError(..)
    , mkTemplateEnv
    , loadTemplate
    , parseTemplate
    , expandTemplate
    , expandTemplateWith
    --
    , unsafeCreateRawTemplateName
    ) where

import Data.Char (isAlpha, isAscii, isAlphaNum)
import Data.Map (Map)
import Data.Map qualified as M
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))



-- Templates
------------

newtype Template
    = Template [TemplateExpr]
    deriving (Eq, Show)

data TemplateExpr
    = TemplateChunk Text
    | TemplateVar Placeholder
    deriving (Eq, Show)

newtype Placeholder
    = Placeholder Text
    deriving (Eq, Ord)

instance IsString Placeholder where
    fromString = Placeholder . fromString

instance Show Placeholder where
    show (Placeholder txt) = T.unpack txt



-- Substitution
---------------

data TemplateEnv
    = TemplateEnv (Map Placeholder Text)
    deriving (Eq, Show)

mkTemplateEnv :: [(Text, Text)] -> TemplateEnv
mkTemplateEnv = TemplateEnv . M.fromList .
    fmap (\(k,v) -> (Placeholder k, v))

lookupPlaceholder
    :: TemplateEnv -> Placeholder -> Either Placeholder Text
lookupPlaceholder (TemplateEnv m) name =
    maybe (Left name) Right (M.lookup name m)

expandTemplate
  :: TemplateEnv -> Template -> Either TemplateError Text
expandTemplate env (Template exprs) = do
    collectEithers UnknownPlaceholders mconcat $
        fmap (expandTemplateExpr env) exprs
    where
        expandTemplateExpr
            :: TemplateEnv -> TemplateExpr -> Either Placeholder Text
        expandTemplateExpr env expr = case expr of
            TemplateChunk txt -> Right txt
            TemplateVar var -> lookupPlaceholder env var

-- | Argument flipped @expandTemplate@.
expandTemplateWith :: Template -> TemplateEnv -> Either TemplateError Text
expandTemplateWith = flip expandTemplate

collectEithers
    :: forall a b u v. ([a] -> u) -> ([b] -> v) -> [Either a b] -> Either u v
collectEithers f g = go [] []
    where
        go :: [a] -> [b] -> [Either a b] -> Either u v
        go as bs xs = case xs of
            [] -> if null as
                then Right $ g $ reverse bs
                else Left  $ f $ reverse as
            x:rest -> case x of
                Left  a -> go (a:as) bs rest
                Right b -> go as (b:bs) rest



-- Parsing
----------

-- | Template variables are strings of ascii letters.
-- They occur in the template by name wrapped in
-- {{double_braces}}, but the braces are not part
-- of the name.

type Line = Int
type Col  = Int

data TemplateParseError
    = MalformedPlaceholder Text
    deriving (Eq, Ord, Show)

parseTemplate
    :: Text -> Either TemplateError Template
parseTemplate raw = fmap Template $ go [] (T.unpack raw) 0 0
  where
    makeChunk, makeVar :: [Char] -> TemplateExpr
    makeChunk = TemplateChunk . T.pack
    makeVar   = TemplateVar . Placeholder . T.pack

    validPlaceholderChar :: Char -> Bool
    validPlaceholderChar c = isAscii c && isAlpha c

    go :: [TemplateExpr] -> [Char] -> Line -> Col
       -> Either TemplateError [TemplateExpr]
    go tokens input ln col = case input of
        [] ->
            Right (reverse tokens)

        '{':'{':rest0 ->
            let (body, closed, rest1) = takePlaceholderBody rest0
            in case closed of
                False ->
                    Left $ MalformedTemplate ln col
                        (MalformedPlaceholder (T.pack body))

                True
                    | null body || not (all validPlaceholderChar body) ->
                        Left $ MalformedTemplate ln col
                            (MalformedPlaceholder (T.pack body))

                    | otherwise ->
                        let (ln', col') = advancePos ("{{" ++ body ++ "}}") ln col
                        in go (makeVar body : tokens) rest1 ln' col'

        _ ->
            let (chunk, rest1) = takeChunk input
                tokens' =
                    if null chunk
                        then tokens
                        else makeChunk chunk : tokens
                (ln', col') = advancePos chunk ln col
            in go tokens' rest1 ln' col'

    -- Consume ordinary text until the next "{{" or end of input.
    takeChunk :: [Char] -> ([Char], [Char])
    takeChunk xs = case xs of
        [] -> ([], [])
        '{':'{':_ -> ([], xs)
        c:rest ->
            let (chunk, rest') = takeChunk rest
            in (c : chunk, rest')

    -- Consume placeholder contents after seeing "{{".
    -- Returns:
    --   (body, foundClosingDelim, restAfterClosing)
    takePlaceholderBody :: [Char] -> ([Char], Bool, [Char])
    takePlaceholderBody xs = case xs of
        [] -> ([], False, [])
        '}':'}':rest -> ([], True, rest)
        c:rest ->
            let (body, closed, rest') = takePlaceholderBody rest
            in (c : body, closed, rest')

    advancePos :: [Char] -> Line -> Col -> (Line, Col)
    advancePos cs ln col = foldl step (ln, col) cs
      where
        step :: (Line, Col) -> Char -> (Line, Col)
        step (l, c) ch = case ch of
            '\n' -> (l + 1, 0)
            _    -> (l, c + 1)






-- Loading
----------

-- | Runtime specification for selecting a template.
data TemplateSpec = TemplateSpec
    { tsSearchDir :: FilePath       -- where to look
    , tsSource    :: TemplateSource -- how to look
    } deriving (Eq, Show)

data TemplateSource
    = DefaultTemplate            -- Used if the spec is not specified
    | TemplateFile FilePath      -- When using a specific template by path
    | NamedTemplate TemplateName -- When using a template by name in @tsSearchDir@
    deriving (Eq, Show)

newtype TemplateName
    = TemplateName Text
    deriving (Eq, Show)

-- For tests
unsafeCreateRawTemplateName :: Text -> TemplateName
unsafeCreateRawTemplateName = TemplateName

-- This is spliced into a string and read as a filename;
-- restricting to alphanumerics, -, and _ avoids malicious
-- names like ".." or "foo\bar".
parseTemplateName :: Text -> Either TemplateError TemplateName
parseTemplateName t
    | T.null t = Left (InvalidTemplateName t)
    | T.all nameSafeChar t = Right (TemplateName t)
    | otherwise = Left (InvalidTemplateName t)

nameSafeChar :: Char -> Bool
nameSafeChar c = isAlphaNum c || c == '-' || c == '_'

loadTemplate :: TemplateSpec -> IO (Either TemplateError Template)
loadTemplate spec = do
    let TemplateSpec
          { tsSearchDir = searchDir
          , tsSource = source
          } = spec
    case source of
        DefaultTemplate ->
            pure (parseTemplate defaultTemplateText)

        TemplateFile path -> do
            exists <- doesFileExist path
            if exists
                then fmap parseTemplate $ T.readFile path
                else pure $ Left $ TemplateFileNotFound path

        NamedTemplate (TemplateName name) -> do
            if T.any (not . nameSafeChar) name || T.null name
                then pure (Left $ InvalidTemplateName name)
                else do
                    let path = searchDir </> T.unpack name <> ".txt"
                    exists <- doesFileExist path
                    if exists
                        then fmap parseTemplate $ T.readFile path
                        else pure (Left (UnknownTemplateName searchDir name))



-- Errors
---------

data TemplateError
    = TemplateFileNotFound FilePath
    | UnknownTemplateName FilePath Text
    | UnknownPlaceholders [Placeholder]
    | MalformedTemplate Line Col TemplateParseError
    | InvalidTemplateName Text
    deriving (Eq, Show)



-- Default Template
-------------------

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
