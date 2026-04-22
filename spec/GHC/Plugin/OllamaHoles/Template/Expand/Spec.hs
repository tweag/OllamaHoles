{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Template.Expand.Spec (tests) where

import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Template
  ( Template(..)
  , TemplateExpr(..)
  , Placeholder(..)
  , TemplateSpec(..)
  , TemplateSource(..)
  , TemplateError(..)
  , mkTemplateEnv
  , loadTemplate
  , expandTemplate
  )

tests :: TestTree
tests =
  testGroup "Template expansion"
    [ basicExpansionTests
    , failureExpansionTests
    , defaultTemplateTests
    ]

basicExpansionTests :: TestTree
basicExpansionTests =
  testGroup "basic expansion"
    [ testCase "chunks-only template expands unchanged" $ do
        let env  = mkTemplateEnv []
            tmpl = Template [TemplateChunk "hello world"]
        expandTemplate env tmpl
          @?= Right "hello world"

    , testCase "single placeholder expands" $ do
        let env  = mkTemplateEnv [("name", "Nathan")]
            tmpl = Template [TemplateChunk "hello ", TemplateVar "name", TemplateChunk "!"]
        expandTemplate env tmpl
          @?= Right "hello Nathan!"

    , testCase "adjacent placeholders expand in order" $ do
        let env =
              mkTemplateEnv
                [ ("x", "A")
                , ("y", "B")
                ]
            tmpl =
              Template
                [ TemplateVar "x"
                , TemplateVar "y"
                ]
        expandTemplate env tmpl
          @?= Right "AB"

    , testCase "mixed chunks and placeholders expand correctly" $ do
        let env =
              mkTemplateEnv
                [ ("greeting", "hello")
                , ("name", "world")
                ]
            tmpl =
              Template
                [ TemplateVar "greeting"
                , TemplateChunk ", "
                , TemplateVar "name"
                , TemplateChunk "!"
                ]
        expandTemplate env tmpl
          @?= Right "hello, world!"

    , testCase "unused environment entries are ignored" $ do
        let env =
              mkTemplateEnv
                [ ("used", "ok")
                , ("unused", "ignored")
                ]
            tmpl =
              Template
                [ TemplateChunk "value="
                , TemplateVar "used"
                ]
        expandTemplate env tmpl
          @?= Right "value=ok"
    ]

failureExpansionTests :: TestTree
failureExpansionTests =
  testGroup "expansion failures"
    [ testCase "unknown placeholder is reported" $ do
        let env  = mkTemplateEnv [("context", "ctx")]
            tmpl = Template [TemplateChunk "x=", TemplateVar "missing"]
        expandTemplate env tmpl
          @?= Left (UnknownPlaceholders ["missing"])

    , testCase "repeated unknown placeholders are reported in occurrence order" $ do
        let env = mkTemplateEnv []
            tmpl =
              Template
                [ TemplateVar "missing"
                , TemplateChunk "-"
                , TemplateVar "missing"
                ]
        expandTemplate env tmpl
          @?= Left (UnknownPlaceholders ["missing", "missing"])

    , testCase "mixed known and unknown placeholders reports only unknown ones" $ do
        let env =
              mkTemplateEnv
                [ ("known", "ok") ]
            tmpl =
              Template
                [ TemplateVar "first"
                , TemplateChunk "-"
                , TemplateVar "known"
                , TemplateChunk "-"
                , TemplateVar "second"
                ]
        expandTemplate env tmpl
          @?= Left (UnknownPlaceholders ["first", "second"])
    ]

defaultTemplateTests :: TestTree
defaultTemplateTests =
  testGroup "default template"
    [ testCase "default template expands with required variables" $ do
        tmplE <- loadTemplate (TemplateSpec "." DefaultTemplate)
        tmpl <- case tmplE of
          Left err ->
            assertFailure ("unexpected load failure: " <> show err) >> fail "unreachable"
          Right t ->
            pure t

        let env =
              mkTemplateEnv
                [ ("docs", "some docs")
                , ("context", "{\"hole\":\"x\"}")
                , ("numexpr", "5")
                ]

        rendered <- case expandTemplate env tmpl of
          Left err ->
            assertFailure ("unexpected expansion failure: " <> show err) >> fail "unreachable"
          Right txt ->
            pure txt

        assertBool "docs substituted"
          ("some docs" `T.isInfixOf` rendered)
        assertBool "context substituted"
          ("{\"hole\":\"x\"}" `T.isInfixOf` rendered)
        assertBool "numexpr substituted"
          ("Output a maximum of 5 expressions." `T.isInfixOf` rendered)

        assertBool "no raw docs placeholder remains"
          (not ("{{docs}}" `T.isInfixOf` rendered))
        assertBool "no raw context placeholder remains"
          (not ("{{context}}" `T.isInfixOf` rendered))
        assertBool "no raw numexpr placeholder remains"
          (not ("{{numexpr}}" `T.isInfixOf` rendered))

    , testCase "default template expansion fails if required placeholders are missing" $ do
        tmplE <- loadTemplate (TemplateSpec "." DefaultTemplate)
        tmpl <- case tmplE of
          Left err ->
            assertFailure ("unexpected load failure: " <> show err) >> fail "unreachable"
          Right t ->
            pure t

        let env = mkTemplateEnv []

        expandTemplate env tmpl
          @?= Left (UnknownPlaceholders ["docs", "context", "numexpr"])
    ]