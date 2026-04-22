{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Template.Load.Spec (tests) where

import Data.List qualified as L
import Data.Text qualified as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Template
  ( Template(..)
  , TemplateSpec(..)
  , TemplateSource(..)
  , TemplateError(..)
  , TemplateExpr(..)
  , Placeholder(..)
  , TemplateParseError(..)
  , loadTemplate
  , parseTemplate
  )

tests :: TestTree
tests =
  testGroup "Template"
    [ loadTemplateTests
    , loadAndParseTests
    ]

loadTemplateTests :: TestTree
loadTemplateTests =
  testGroup "loadTemplate"
    [ testCase "DefaultTemplate loads successfully" $ do
        result <- loadTemplate (TemplateSpec "" DefaultTemplate)
        case result of
          Right (Template txt) ->
            assertBool "expected non-empty default template" (not (null txt))
          Left err ->
            assertFailure ("expected default template to load, got: " <> show err)

    , testCase "TemplateFile loads an existing file" $
        withSystemTempDirectory "ollama-holes-template-spec" $ \dir -> do
          let fp = dir </> "prompt.txt"
          writeFile fp "hello {{name}}"
          result <- loadTemplate (TemplateSpec dir (TemplateFile fp))
          result @?= Right (Template [TemplateChunk "hello ", TemplateVar "name"])

    , testCase "TemplateFile reports missing file" $
        withSystemTempDirectory "ollama-holes-template-spec" $ \dir -> do
          let fp = dir </> "missing.txt"
          result <- loadTemplate (TemplateSpec dir (TemplateFile fp))
          result @?= Left (TemplateFileNotFound fp)

    , testCase "NamedTemplate loads <searchDir>/<name>.txt" $
        withSystemTempDirectory "ollama-holes-template-spec" $ \dir -> do
          let fp = dir </> "qwen.txt"
          writeFile fp "hello {{context}}"
          result <- loadTemplate (TemplateSpec dir (NamedTemplate "qwen"))
          result @?= Right (Template [TemplateChunk "hello ", TemplateVar "context"])

    , testCase "NamedTemplate reports unknown name" $
        withSystemTempDirectory "ollama-holes-template-spec" $ \dir -> do
          result <- loadTemplate (TemplateSpec dir (NamedTemplate "missing"))
          result @?= Left (UnknownTemplateName dir "missing")
    ]

loadAndParseTests :: TestTree
loadAndParseTests =
  testGroup "load + parse"
    [ testCase "loaded file parses into expected chunks and vars" $
        withSystemTempDirectory "ollama-holes-template-spec" $ \dir -> do
          let fp = dir </> "prompt.txt"
          writeFile fp "A {{foo}} B {{bar}}."
          loaded <- loadTemplate (TemplateSpec dir (TemplateFile fp))
          case loaded of
            Left err ->
              assertFailure ("unexpected load error: " <> show err)
            Right (Template exprs) ->
              exprs @?=
                  [ TemplateChunk "A "
                  , TemplateVar (Placeholder "foo")
                  , TemplateChunk " B "
                  , TemplateVar (Placeholder "bar")
                  , TemplateChunk "."
                  ]

    , testCase "default template parses placeholders for docs/context/numexpr" $ do
        loaded <- loadTemplate (TemplateSpec "" DefaultTemplate)
        case loaded of
          Left err ->
            assertFailure ("unexpected load error: " <> show err)
          Right (Template exprs) -> do
                let vars = [ v | TemplateVar v <- exprs ]
                assertBool
                  ("expected docs placeholder in default template, saw: " <> show vars)
                  (Placeholder "docs" `elem` vars)
                assertBool
                  ("expected context placeholder in default template, saw: " <> show vars)
                  (Placeholder "context" `elem` vars)
                assertBool
                  ("expected numexpr placeholder in default template, saw: " <> show vars)
                  (Placeholder "numexpr" `elem` vars)

    , testCase "default template includes at least one variable" $ do
        loaded <- loadTemplate (TemplateSpec "" DefaultTemplate)
        case loaded of
            Left err ->
                assertFailure ("unexpected load error: " <> show err)
            Right (Template exprs) -> assertBool
                "expected at least one TemplateVar in default template"
                (any isVar exprs)
    ]
  where
    isVar (TemplateVar _) = True
    isVar _               = False