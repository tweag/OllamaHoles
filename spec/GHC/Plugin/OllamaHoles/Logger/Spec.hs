{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Logger.Spec (tests) where

import Control.Monad (forM)
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf)
import Data.Text qualified as T
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Logger
  ( LogMode(..)
  , initLogger
  , mkLogEvent
  , writeLogEvent
  , LogEvent
  )

tests :: TestTree
tests =
  testGroup "Logger"
    [ testCase "LogOff writes no event file and no blobs" $
        withSystemTempDirectory "ollama-holes-logger-spec" $ \dir -> do
          logger <- initLogger (Just LogOff) (Just dir)
          writeLogEvent logger (sampleEvent "prompt-a" "response-a")

          let eventsFile = dir </> "hole-fit-logs.jsonl"
              blobDir    = dir </> "blob"

          eventsExists <- doesFileExist eventsFile
          eventsExists @?= False

          blobFiles <- listFilesRecursive blobDir
          blobFiles @?= []

    , testCase "LogBasic writes event file but no blobs" $
        withSystemTempDirectory "ollama-holes-logger-spec" $ \dir -> do
          logger <- initLogger (Just LogBasic) (Just dir)
          writeLogEvent logger (sampleEvent "prompt-b" "response-b")

          let eventsFile = dir </> "hole-fit-logs.jsonl"
              blobDir    = dir </> "blob"

          eventsExists <- doesFileExist eventsFile
          assertBool "expected JSONL event file" eventsExists

          contents <- LBS.readFile eventsFile
          assertBool "expected non-empty event file" (not (LBS.null contents))

          blobFiles <- listFilesRecursive blobDir
          blobFiles @?= []

    , testCase "LogFull writes event file and prompt/response blobs" $
        withSystemTempDirectory "ollama-holes-logger-spec" $ \dir -> do
          logger <- initLogger (Just LogFull) (Just dir)
          writeLogEvent logger (sampleEvent "prompt-c" "response-c")

          let eventsFile = dir </> "hole-fit-logs.jsonl"
              blobDir    = dir </> "blob"

          eventsExists <- doesFileExist eventsFile
          assertBool "expected JSONL event file" eventsExists

          eventContents <- LBS.readFile eventsFile
          let rendered = show eventContents
          assertBool "event file should mention prompt hash field"
            ("prompt_hash" `isInfixOf` rendered)
          assertBool "event file should mention response hash field"
            ("response_hash" `isInfixOf` rendered)

          blobFiles <- listFilesRecursive blobDir
          assertEqual "expected exactly two blobs" 2 (length blobFiles)

    , testCase "custom log-dir is respected" $
        withSystemTempDirectory "ollama-holes-logger-spec" $ \dir -> do
          let customRoot = dir </> "my-custom-log-root"

          logger <- initLogger (Just LogBasic) (Just customRoot)
          writeLogEvent logger (sampleEvent "prompt-d" "response-d")

          let expectedEvents = customRoot </> "hole-fit-logs.jsonl"
              unexpectedEvents = dir </> "hole-fit-logs.jsonl"

          expectedExists <- doesFileExist expectedEvents
          unexpectedExists <- doesFileExist unexpectedEvents

          assertBool "expected event file under custom root" expectedExists
          assertBool "did not expect event file at parent temp root" (not unexpectedExists)
    ]

sampleEvent :: String -> String -> LogEvent
sampleEvent prompt response =
  mkLogEvent
    (T.pack prompt)
    (T.pack response)
    5
    3
    2

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else go root
  where
    go dir = do
      names <- listDirectory dir
      paths <- forM names $ \name -> do
        let path = dir </> name
        isDir <- doesDirectoryExist path
        if isDir
          then go path
          else pure [path]
      pure (concat paths)