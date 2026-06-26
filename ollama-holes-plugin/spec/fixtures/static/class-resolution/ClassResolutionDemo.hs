{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

newtype UserId =
  UserId Int
  deriving (Eq, Show)

class Decode a where
  decode :: String -> Maybe a

instance Decode UserId where
  decode s =
    case reads s of
      [(n, "")] -> Just (UserId n)
      _ -> Nothing

instance Decode Int where
  decode s =
    case reads s of
      [(n, "")] -> Just n
      _ -> Nothing

class Mystery a where
  mystery :: String -> Maybe a

needsDecode :: Decode a => String -> Maybe a
needsDecode = decode

mkUserId :: Int -> UserId
mkUserId = UserId

target :: String -> Maybe UserId
target s =
  let fallback :: Maybe UserId
      fallback =
        Just (UserId 0)

      localDecode :: Decode a => Maybe a
      localDecode =
        decode s
  in
    _llmClass

main :: IO ()
main = pure ()
