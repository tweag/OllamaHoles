{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

newtype UserId =
  UserId Int
  deriving (Eq, Show)

parseDigits :: String -> Maybe Int
parseDigits s =
  case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

mkUserId :: Int -> UserId
mkUserId = UserId

choose :: a -> a -> a
choose x _ = x

local :: String -> Maybe UserId
local s =
  let n :: Maybe Int
      n = parseDigits s

      f :: Int -> UserId
      f = mkUserId

      fallback :: Maybe UserId
      fallback =
        Just (UserId 0)
  in
    _llmLocal

main :: IO ()
main = pure ()