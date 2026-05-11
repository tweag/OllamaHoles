module GHC.Plugin.OllamaHoles.Data.Template.Parse.Spec (tests) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Template.Types.Gen



tests :: TestTree
tests =
  testGroup "Template parser"
    [ test_parseTemplate_unit
    , test_parseTemplate_prop
    ]

test_parseTemplate_unit :: TestTree
test_parseTemplate_unit = testGroup "parseTemplate (unit)"
  [ testGroup "success" $
      tests_parseTemplate_unit_success <&> \(name, input, expected) ->
        testCase name $ case parseTemplate input of
          Left err -> assertFailure $
            "expected successful parse but got this error: " <> show err
          Right actual -> actual @?= expected

  , testGroup "failure" $
      tests_parseTemplate_unit_failure <&> \(name, input) ->
        testCase name $ case parseTemplate input of
          Left _ -> pure ()
          Right ok -> assertFailure $
            "expected failed parse but got this result: " <> show ok
  ]

test_parseTemplate_prop :: TestTree
test_parseTemplate_prop =
  testGroup "properties"
    [ QC.testProperty "nonempty plain ascii text without '{{' parses as a single chunk" $
        QC.forAll genPlainChunk1 $ \s ->
          parseTemplate (T.pack s)
            == Right (Template [TemplateChunk (T.pack s)])

    , QC.testProperty "valid placeholder names parse as variables" $
        QC.forAll genPlaceholderName $ \nm ->
          parseTemplate ("{{" <> T.pack nm <> "}}")
            == Right (Template [TemplateVar (Placeholder (T.pack nm))])
    ]



tests_parseTemplate_unit_success
  :: [(String, Text, Template)]
tests_parseTemplate_unit_success =
  [ ( "empty template parses to empty token list"
    , ""
    , Template []
    )

  , ( "plain text parses to one chunk"
    , "hello world"
    , Template [TemplateChunk "hello world"]
    )

  , ( "single placeholder parses"
    , "{{name}}"
    , Template [TemplateVar (Placeholder "name")]
    )

  , ( "text around placeholder parses"
    , "hello {{name}}!"
    , Template
        [ TemplateChunk "hello "
        , TemplateVar (Placeholder "name")
        , TemplateChunk "!"
        ]
    )

  , ( "adjacent placeholders parse"
    , "{{foo}}{{bar}}"
    , Template
        [ TemplateVar (Placeholder "foo")
        , TemplateVar (Placeholder "bar")
        ]
    )

  , ( "multiple chunks and placeholders parse"
    , "a{{x}}b{{y}}c"
    , Template
        [ TemplateChunk "a"
        , TemplateVar (Placeholder "x")
        , TemplateChunk "b"
        , TemplateVar (Placeholder "y")
        , TemplateChunk "c"
        ]
    )

  , ( "lone opening brace stays ordinary text"
    , "{foo"
    , Template [TemplateChunk "{foo"]
    )

  , ( "single braces around name stay ordinary text"
    , "{name}"
    , Template [TemplateChunk "{name}"]
    )

  , ( "valid placeholder after newline reports no error"
    , "abc\n{{foo}}"
    , Template
        [ TemplateChunk "abc\n"
        , TemplateVar (Placeholder "foo")
        ]
    )
  ]

tests_parseTemplate_unit_failure
  :: [(String, Text)]
tests_parseTemplate_unit_failure =
  [ ( "empty placeholder is rejected"
    , "{{}}"
    )

  , ( "placeholder with hyphen is rejected"
    , "{{foo-bar}}"
    )

  , ( "placeholder with space is rejected"
    , "{{foo bar}}"
    )

  , ( "unclosed placeholder is rejected"
    , "{{foo"
    )
  ]
