{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Integration.Static.Spec (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
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
      "static backend documents current subsumption limitation"
      test_subsumptionStatic
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

  -- This fixture documents the current conservative exact-type checker.
  --
  -- These are morally useful fits for a hole of type `Int -> Int`:
  --
  --   id
  --   \x -> x
  --   (+ 1)
  --
  -- But accepting them requires instantiation/subsumption rather than exact
  -- zonked type equality. When checkCandidateFit grows that ability, this
  -- fixture should be updated to expect the first three to pass and `show`
  -- to fail.
  assertContains output "--- candidate validation failures (4) ---"
  assertContains output "rejected: id"
  assertContains output "rejected: \\x -> x"
  assertContains output "rejected: (+ 1)"
  assertContains output "rejected: show"

  assertNotContains output "--- prepared for semantic-ish deduplication"
  assertNotContains output "--- semantic-ish uniques"

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