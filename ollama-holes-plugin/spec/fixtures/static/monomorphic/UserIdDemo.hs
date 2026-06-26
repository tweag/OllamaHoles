{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -fplugin=GHC.Plugin.OllamaHoles #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:debug #-}

module Main where

import Data.Char (isDigit)
import Text.Read (readMaybe)

newtype UserId = UserId Int
  deriving (Show, Eq)

parseUserId :: String -> Maybe UserId
parseUserId s =
  if all isDigit s
    then _llmUserId
    else Nothing

main :: IO ()
main = pure ()
