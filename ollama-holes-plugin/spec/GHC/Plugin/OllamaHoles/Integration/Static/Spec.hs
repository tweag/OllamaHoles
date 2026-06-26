{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Integration.Static.Spec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Integration.Static"
  [ testCase
      "static backend validates candidates in real typed-hole context"
      test_userIdStatic
  , testCase
      "static backend validates candidates which subsume the hole type"
      test_subsumptionStatic
  , testCase
      "static backend validates local polymorphic candidates"
      test_localPolymorphicStatic
  , testCase
      "static backend validates candidates with nontrivial class constraints"
      test_classResolutionStatic
  ]

test_userIdStatic :: Assertion
test_userIdStatic = do
  output <-
    buildFixtureComponent "ollama-holes-static-fixture"

  assertContains output "--- raw candidates (6) ---"
  assertContains output "UserId <$> readMaybe s"
  assertContains output "Just (UserId (read s))"
  assertContains output "UserId <$> Just (length s)"
  assertContains output "readMaybe s"
  assertContains output "UserId (read s)"
  assertContains output "Just s"

  assertContains output "--- candidate validation failures (3) ---"
  assertContains output "rejected: readMaybe s"
  assertContains output "rejected: UserId (read s)"
  assertContains output "rejected: Just s"

  assertContains output "--- prepared for semantic-ish deduplication (3) ---"
  assertContains output "source: UserId <$> readMaybe s"
  assertContains output "source: Just (UserId (read s))"
  assertContains output "source: UserId <$> Just (length s)"

  assertContains output "--- semantic-ish uniques (3) ---"
  assertContains output "UserId <$> readMaybe s"
  assertContains output "Just (UserId (read s))"
  assertContains output "UserId <$> Just (length s)"

  assertNotContains output "<hole-fit-validation>"
  assertNotContains output "Variable not in scope: s"

test_subsumptionStatic :: Assertion
test_subsumptionStatic = do
  output <-
    buildFixtureComponent "ollama-holes-subsumption-fixture"

  assertContains output "--- raw candidates (4) ---"
  assertContains output "id"
  assertContains output "\\x -> x"
  assertContains output "(+ 1)"
  assertContains output "show"

  assertContains output "--- candidate validation failures (1) ---"
  assertContains output "rejected: show"

  assertContains output "--- prepared for semantic-ish deduplication (3) ---"
  assertContains output "source: id"
  assertContains output "source: \\x -> x"
  assertContains output "source: (+ 1)"

  assertContains output "--- semantic-ish uniques (2) ---"
  assertContains output "id"
  assertContains output "(+ 1)"

test_localPolymorphicStatic :: Assertion
test_localPolymorphicStatic = do
  output <-
    buildFixtureComponent "ollama-holes-local-polymorphic-fixture"

  assertContains output "--- raw candidates (18) ---"
  assertContains output "--- syntactic uniques (18) ---"

  assertContains output "--- candidate validation failures (8) ---"
  assertContains output "rejected: Just s"
  assertContains output "rejected: f <$> Just s"
  assertContains output "rejected: choose s fallback"
  assertContains output "rejected: f"
  assertContains output "rejected: n"
  assertContains output "rejected: const s fallback"
  assertContains output "rejected: ($) s <$> n"
  assertContains output "rejected: choose n fallback"

  assertContains output "--- prepared for semantic-ish deduplication (10) ---"
  assertContains output "source: f <$> n"
  assertContains output "source: Just (f 3)"
  assertContains output "source: fallback"
  assertContains output "source: choose fallback (Just (UserId 1))"
  assertContains output "source: Nothing"
  assertContains output "source: pure (UserId 2)"
  assertContains output "source: UserId <$> n"
  assertContains output "source: ($) f <$> n"
  assertContains output "source: choose Nothing fallback"
  assertContains output "source: const fallback s"

  assertContains output "--- semantic-ish uniques (10) ---"
  assertContains output "Nothing"
  assertContains output "fallback"
  assertContains output "UserId <$> n"
  assertContains output "f <$> n"
  assertContains output "($) f <$> n"
  assertContains output "Just (f 3)"
  assertContains output "choose Nothing fallback"
  assertContains output "choose fallback (Just (UserId 1))"
  assertContains output "const fallback s"
  assertContains output "pure (UserId 2)"

  assertNotContains output "<hole-fit-validation>"
  assertNotContains output "Variable not in scope"

test_classResolutionStatic :: Assertion
test_classResolutionStatic = do
  output <-
    buildFixtureComponent "ollama-holes-class-resolution-fixture"

  assertContains output "--- raw candidates (14) ---"
  assertContains output "--- syntactic uniques (14) ---"

  assertContains output "--- candidate validation failures (6) ---"
  assertContains output "rejected: read s"
  assertContains output "rejected: show s"
  assertContains output "rejected: decode"
  assertContains output "rejected: needsDecode"
  assertContains output "rejected: localDecode :: Maybe Int"
  assertContains output "rejected: mystery s"

  assertContains output "--- prepared for semantic-ish deduplication (8) ---"
  assertContains output "source: decode s"
  assertContains output "source: needsDecode s"
  assertContains output "source: localDecode"
  assertContains output "source: fmap UserId (decode s)"
  assertContains output "source: fmap mkUserId (needsDecode s)"
  assertContains output "source: pure (UserId 1)"
  assertContains output "source: fallback"
  assertContains output "source: decode \"123\""

  assertNotContains output "<hole-fit-validation>"
  assertNotContains output "Variable not in scope"

buildFixtureComponent :: String -> IO Text
buildFixtureComponent componentName =
  withSystemTempDirectory ("ollama-holes-" <> componentName) $ \buildDir -> do
    let target =
          "test:" <> componentName

        args =
          [ "build"
          , target
          , "--builddir=" <> buildDir
          , "--ghc-options=-fforce-recomp"
          , "--verbose=0"
          ]

        commandLine =
          unwords ("cabal" : args)

    (exitCode, out, err) <-
      readProcessWithExitCode "cabal" args ""

    let output =
          T.pack out <> "\n" <> T.pack err

    assertBool
      (unlines
        [ "expected fixture component to build"
        , "command: " <> commandLine
        , "exit code: " <> show exitCode
        , ""
        , "stdout:"
        , out
        , ""
        , "stderr:"
        , err
        ])
      (exitCode == ExitSuccess)

    pure output

assertContains :: Text -> Text -> Assertion
assertContains haystack needle =
  assertBool
    (unlines
      [ "expected output to contain: " <> T.unpack needle
      , ""
      , "full output:"
      , T.unpack haystack
      ])
    (needle `T.isInfixOf` haystack)

assertNotContains :: Text -> Text -> Assertion
assertNotContains haystack needle =
  assertBool
    (unlines
      [ "expected output not to contain: " <> T.unpack needle
      , ""
      , "full output:"
      , T.unpack haystack
      ])
    (not (needle `T.isInfixOf` haystack))