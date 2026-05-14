module GHC.Plugin.OllamaHoles.Data.Template.Parse
  ( parseTemplate
  , parseTemplateName
  , nameSafeChar
  , tomlTemplate
  ) where

import Data.Char (isAscii, isAlpha, isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T
import Toml qualified as Toml
import Toml.Schema qualified as Toml

import Toml.Extra
import GHC.Plugin.OllamaHoles.Data.Template.Types
import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal
import GHC.Plugin.OllamaHoles.Data.Template.Error


-- | Template variables are strings of ascii letters.
-- They occur in the template by name wrapped in
-- {{double_braces}}, but the braces are not part
-- of the name.
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



-- This is spliced into a string and read as a filename;
-- restricting to alphanumerics, -, and _ avoids malicious
-- names like ".." or "foo\bar".
parseTemplateName :: Text -> Either TemplateError TemplateName
parseTemplateName t
    | T.null t = Left (InvalidTemplateName t)
    | T.all nameSafeChar t = Right (unsafeCreateRawTemplateName t)
    | otherwise = Left (InvalidTemplateName t)

nameSafeChar :: Char -> Bool
nameSafeChar c = isAlphaNum c || c == '-' || c == '_'


{-
tomlTemplate :: Toml.Value' l -> Toml.Matcher l (TemplateName, Template)
tomlTemplate value = do
  table <- Toml.parseTableFromValue value
  rawName <- Toml.reqKeyOf "name" table
  rawBody <- Toml.reqKeyOf "body" table
  name <- either (Toml.failAt value . T.unpack . renderTemplateError)
      pure (parseTemplateName rawName)
  template <-
    either (Toml.failAt value . T.unpack . renderTemplateError)
      pure (parseTemplate rawBody)
  pure (name, template) -}



tomlTemplate :: Toml.Value' l -> Toml.Matcher l (TemplateName, Template)
tomlTemplate = Toml.parseTableFromValue $ do
  name <- Toml.reqKeyOf "name" $
    tomlValidateText parseTemplateName renderTemplateError
  body <- Toml.reqKeyOf "body" $
    tomlValidateText parseTemplate renderTemplateError
  pure (name, body)