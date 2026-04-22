{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Template.Parse.Spec (tests) where

import Data.Char (isAlpha, isAscii)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Template
  ( Template(..)
  , TemplateExpr(..)
  , Placeholder(..)
  , TemplateError(..)
  , TemplateParseError(..)
  , parseTemplate
  )

tests :: TestTree
tests =
  testGroup "Template parser"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests =
  testGroup "unit"
    [ testCase "empty template parses to empty token list" $
        parseTemplate (Template "") @?= Right []

    , testCase "plain text parses to one chunk" $
        parseTemplate (Template "hello world")
          @?= Right [TemplateChunk "hello world"]

    , testCase "single placeholder parses" $
        parseTemplate (Template "{{name}}")
          @?= Right [TemplateVar (Placeholder "name")]

    , testCase "text around placeholder parses" $
        parseTemplate (Template "hello {{name}}!")
          @?=
            Right
              [ TemplateChunk "hello "
              , TemplateVar (Placeholder "name")
              , TemplateChunk "!"
              ]

    , testCase "adjacent placeholders parse" $
        parseTemplate (Template "{{foo}}{{bar}}")
          @?=
            Right
              [ TemplateVar (Placeholder "foo")
              , TemplateVar (Placeholder "bar")
              ]

    , testCase "multiple chunks and placeholders parse" $
        parseTemplate (Template "a{{x}}b{{y}}c")
          @?=
            Right
              [ TemplateChunk "a"
              , TemplateVar (Placeholder "x")
              , TemplateChunk "b"
              , TemplateVar (Placeholder "y")
              , TemplateChunk "c"
              ]

    , testCase "empty placeholder is rejected" $
        parseTemplate (Template "{{}}")
          @?= Left (MalformedTemplate 0 0 (MalformedPlaceholder ""))

    , testCase "placeholder with hyphen is rejected" $
        parseTemplate (Template "{{foo-bar}}")
          @?= Left (MalformedTemplate 0 0 (MalformedPlaceholder "foo-bar"))

    , testCase "placeholder with space is rejected" $
        parseTemplate (Template "{{foo bar}}")
          @?= Left (MalformedTemplate 0 0 (MalformedPlaceholder "foo bar"))

    , testCase "unclosed placeholder is rejected" $
        parseTemplate (Template "{{foo")
          @?= Left (MalformedTemplate 0 0 (MalformedPlaceholder "foo"))

    , testCase "lone opening brace stays ordinary text" $
        parseTemplate (Template "{foo")
          @?= Right [TemplateChunk "{foo"]

    , testCase "single braces around name stay ordinary text" $
        parseTemplate (Template "{name}")
          @?= Right [TemplateChunk "{name}"]

    , testCase "malformed placeholder after prefix reports start position" $
        parseTemplate (Template "abc{{foo-bar}}")
          @?= Left (MalformedTemplate 0 3 (MalformedPlaceholder "foo-bar"))

    , testCase "malformed placeholder after newline reports line and column" $
        parseTemplate (Template "abc\n{{foo-bar}}")
          @?= Left (MalformedTemplate 1 0 (MalformedPlaceholder "foo-bar"))

    , testCase "valid placeholder after newline reports no error" $
        parseTemplate (Template "abc\n{{foo}}")
          @?=
            Right
              [ TemplateChunk "abc\n"
              , TemplateVar (Placeholder "foo")
              ]
    ]

propertyTests :: TestTree
propertyTests =
  testGroup "properties"
    [ QC.testProperty "nonempty plain ascii text without '{{' parses as a single chunk" $
        QC.forAll genPlainChunk1 $ \s ->
          parseTemplate (Template (T.pack s))
            == Right [TemplateChunk (T.pack s)]

    , QC.testProperty "valid placeholder names parse as variables" $
        QC.forAll genPlaceholderName $ \nm ->
          parseTemplate (Template ("{{" <> T.pack nm <> "}}"))
            == Right [TemplateVar (Placeholder (T.pack nm))]
    ]

genPlainChunk1 :: QC.Gen String
genPlainChunk1 =
    fmap concat $ QC.listOf1 $
        -- ensure we never generate two consecutive '{'
        QC.oneof [pure (:[]), pure (:['{'])]
            <*> QC.suchThat QC.arbitrary (/= '{')

genPlaceholderName :: QC.Gen String
genPlaceholderName = QC.listOf1 $
    QC.suchThat QC.arbitrary (\c -> isAscii c && isAlpha c)
