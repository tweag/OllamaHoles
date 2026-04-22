{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Template
    ( Template(..)
    , TemplateSpec(..)
    , TemplateSource(..)
    , TemplateError(..)
    , TemplateExpr(..)
    , Placeholder(..)
    , TemplateParseError(..)
    , loadTemplate
    , parseTemplate
    ) where

import Data.Char (isAlpha, isAscii)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))



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
    = DefaultTemplate       -- Used if the spec is not specified
    | TemplateFile FilePath -- When using a specific template by path
    | NamedTemplate Text    -- When using a template by name in @tsSearchDir@
    deriving (Eq, Show)

data TemplateError
    = TemplateFileNotFound FilePath
    | UnknownTemplateName FilePath Text
    | UnknownPlaceholders [Placeholder]
    | MalformedTemplate Line Col TemplateParseError
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
            let path = searchDir </> T.unpack name <> ".txt"
            exists <- doesFileExist path
            if exists
                then fmap (Right . Template) $ T.readFile path
                else pure (Left (UnknownTemplateName searchDir name))



-- Substitution
---------------

-- | Template variables are strings of ascii letters.
-- They occur in the template by name wrapped in
-- {{double_braces}}, but the braces are not part
-- of the name.
newtype Placeholder
    = Placeholder Text
    deriving (Eq, Ord)

instance Show Placeholder where
    show (Placeholder txt) = T.unpack txt

data TemplateEnv
    = TemplateEnv (Map Placeholder Text)
    deriving (Eq, Show)

lookupPlaceholder
    :: TemplateEnv -> Placeholder -> Either Placeholder Text
lookupPlaceholder (TemplateEnv m) name =
    maybe (Left name) Right (M.lookup name m)

data TemplateExpr
    = TemplateChunk Text
    | TemplateVar Placeholder
    deriving (Eq, Show)

expandTemplateExpr
    :: TemplateEnv -> TemplateExpr -> Either Placeholder Text
expandTemplateExpr env expr = case expr of
    TemplateChunk txt -> Right txt
    TemplateVar var -> lookupPlaceholder env var

expandTemplateExprs
    :: TemplateEnv -> [TemplateExpr] -> Either TemplateError Text
expandTemplateExprs env =
    collectEithers UnknownPlaceholders mconcat . fmap (expandTemplateExpr env)

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

type Line = Int
type Col  = Int

data TemplateParseError
    = MalformedPlaceholder Text
    deriving (Eq, Ord, Show)

parseTemplate
    :: Template -> Either TemplateError [TemplateExpr]
parseTemplate (Template raw) = go [] (T.unpack raw) 0 0
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



-- Default Template
-------------------

defaultTemplateText :: Template
defaultTemplateText = Template $ T.pack $ unlines
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
