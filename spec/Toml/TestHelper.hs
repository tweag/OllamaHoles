module Toml.TestHelper where

import Data.Text (Text)

import Test.Tasty.HUnit ((@?=), Assertion, assertFailure)
import Test.Tasty.QuickCheck qualified as QC
import Toml qualified as Toml
import Toml.Schema qualified as Toml



parseTomlWith
  :: (Toml.Value -> Toml.Matcher () a) -> Text -> Either TomlError a
parseTomlWith parser input = case Toml.decode input of
  Toml.Failure errs -> Left $ TomlParseError errs
  Toml.Success warnings value
    | not (null warnings) -> Left $ TomlWarning warnings
    | otherwise -> case Toml.runMatcherIgnoreWarn (parser value) of
        Left err -> Left (TomlMatchError err); Right x -> Right x

data TomlError
  = TomlParseError [String]
  | TomlWarning [String]
  | TomlMatchError [Toml.MatchMessage ()]
  deriving (Eq, Show)



-- HUnit Helpers
----------------

assertTomlParsesAs
  :: (Eq a, Show a)
  => (Toml.Value -> Toml.Matcher () a)
  -> Text -> a -> Assertion
assertTomlParsesAs toml input expected =
  case parseTomlWith toml input of
    Left err -> assertFailure $ show err
    Right actual -> actual @?= expected

assertTomlParseFails
  :: (Show a)
  => (Toml.Value -> Toml.Matcher () a)
  -> Text -> Assertion
assertTomlParseFails toml input =
  case parseTomlWith toml input of
    Left err -> pure ()
    Right ok -> assertFailure $ show ok



-- QuickCheck Helpers
---------------------

propTomlParseSuccess
  :: (Eq a, Show a) => (Toml.Value -> Toml.Matcher () a)
  -> (Text, a) -> QC.Property
propTomlParseSuccess toml (input, expect) =
  (QC.===) (parseTomlWith toml input) (Right expect)
