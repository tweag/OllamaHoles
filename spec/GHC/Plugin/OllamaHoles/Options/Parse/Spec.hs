{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GHC.Plugin.OllamaHoles.Options.Parse.Spec (tests) where

import Control.Exception (SomeException, evaluate, try)
import Data.Aeson (Value(..))
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Options
  ( Flags(..)
  , parseCommandLineOptions
  , defaultFlags
  , parseFlags
  , OptError(..)
  , Token(..)
  )

tests :: TestTree
tests = testGroup "parseFlags (non-template options)"
    [ defaultsTests
    , simpleOptionTests
    , precedenceTests
    , failureBehaviorTests
    ]

defaultsTests :: TestTree
defaultsTests = testGroup "defaults"
    [ testCase "empty options yields defaultFlags" $ do
        parseFlags [] @?= defaultFlags
    ]

simpleOptionTests :: TestTree
simpleOptionTests = testGroup "simple options"
    [ testCase "model= sets model_name" $ do
        let flags = parseFlags ["model=phi4"]
        model_name flags @?= "phi4"

    , testCase "backend= sets backend_name" $ do
        let flags = parseFlags ["backend=openai"]
        backend_name flags @?= "openai"

    , testCase "openai_base_url= sets openai_base_url" $ do
        let flags = parseFlags ["openai_base_url=https://example.com/v1"]
        openai_base_url flags @?= "https://example.com/v1"

    , testCase "openai_key_name= sets openai_key_name" $ do
        let flags = parseFlags ["openai_key_name=MY_API_KEY"]
        openai_key_name flags @?= "MY_API_KEY"

    , testCase "debug enables debug" $ do
        let flags = parseFlags ["debug"]
        debug flags @?= True

    , testCase "include-docs enables include_docs" $ do
        let flags = parseFlags ["include-docs"]
        include_docs flags @?= True

    , testCase "n= sets num_expr" $ do
        let flags = parseFlags ["n=17"]
        num_expr flags @?= 17

    , testCase "model-options= parses valid JSON object" $ do
        let flags = parseFlags ["model-options={\"temperature\":1.0,\"num_ctx\":32000}"]
        case model_options flags of
          Nothing ->
            assertFailure "expected model_options to be set"
          Just (Object obj) -> do
            assertBool "expected temperature key" ("temperature" `KM.member` obj)
            assertBool "expected num_ctx key" ("num_ctx" `KM.member` obj)
          Just other ->
            assertFailure ("expected JSON object, got: " <> show other)

    , testCase "unknown options are ignored" $ do
        let flags = parseFlags ["bogus-option", "another=thing"]
        flags @?= defaultFlags

    , testCase "unknown options are reported but do not block recognized options" $ do
        let result = parseCommandLineOptions defaultFlags
                ["bogus-option", "model=qwen3", "debug"]
        case result of
            Left err ->
                assertFailure ("unexpected error: " <> show err)
            Right (flags, unknowns) -> do
                model_name flags @?= "qwen3"
                debug flags @?= True
                unknowns @?= [BooleanToken "bogus-option"]
    ]

precedenceTests :: TestTree
precedenceTests = testGroup "precedence"
    [ testCase "leftmost model= wins" $ do
        let flags = parseFlags ["model=first", "model=second"]
        model_name flags @?= "first"

    , testCase "leftmost backend= wins" $ do
        let flags = parseFlags ["backend=ollama", "backend=openai"]
        backend_name flags @?= "ollama"

    , testCase "leftmost openai_base_url= wins" $ do
        let flags = parseFlags
              [ "openai_base_url=https://first.example"
              , "openai_base_url=https://second.example"
              ]
        openai_base_url flags @?= "https://first.example"

    , testCase "leftmost openai_key_name= wins" $ do
        let flags = parseFlags
              [ "openai_key_name=FIRST_KEY"
              , "openai_key_name=SECOND_KEY"
              ]
        openai_key_name flags @?= "FIRST_KEY"

    , testCase "leftmost n= wins" $ do
        let flags = parseFlags ["n=3", "n=9"]
        num_expr flags @?= 3

    , testCase "boolean flags are sticky" $ do
        let flags = parseFlags ["debug", "debug", "include-docs", "include-docs"]
        debug flags @?= True
        include_docs flags @?= True

    , testCase "leftmost model-options= wins" $ do
        let flags = parseFlags
              [ "model-options={\"temperature\":0.1}"
              , "model-options={\"temperature\":0.9}"
              ]
        case model_options flags of
          Nothing ->
            assertFailure "expected model_options to be set"
          Just (Object obj) ->
            assertBool "expected temperature key from leftmost option"
              ("temperature" `KM.member` obj)
          Just other ->
            assertFailure ("expected JSON object, got: " <> show other)

    , testCase "leftmost wins even when interleaved with other options" $ do
        let flags = parseFlags
              [ "model=first"
              , "debug"
              , "backend=ollama"
              , "model=second"
              , "include-docs"
              , "backend=openai"
              ]
        model_name flags @?= "first"
        backend_name flags @?= "ollama"
        debug flags @?= True
        include_docs flags @?= True
    ]

failureBehaviorTests :: TestTree
failureBehaviorTests = testGroup "current failure behavior"
    [ testCase "invalid n returns structured error" $ do
        parseCommandLineOptions defaultFlags ["n=not-an-int"]
            @?= Left (InvalidInt "n" "not-an-int")

    , testCase "invalid model-options returns structured error" $ do
        case parseCommandLineOptions defaultFlags ["model-options={not json}"] of
            Left (InvalidJson "model-options" "{not json}" _) ->
                pure ()
            other ->
                assertFailure ("expected InvalidJson, got: " <> show other)
    ]

assertThrows :: IO a -> Assertion
assertThrows action = do
    result <- try action
    case result of
        Left (_ :: SomeException) -> pure ()
        Right _ -> assertFailure "expected exception, but computation succeeded"