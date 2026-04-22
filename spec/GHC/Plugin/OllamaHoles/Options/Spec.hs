{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Options.Spec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles
    ( Flags(..)
    , defaultFlags
    , parseFlags
    , mkTemplateSpec
    )
import GHC.Plugin.OllamaHoles.Template
    ( TemplateSpec(..)
    , TemplateSource(..)
    , TemplateError(..)
    , unsafeCreateRawTemplateName
    )

tests :: TestTree
tests = testGroup "template-related option parsing"
    [ parseFlagsTests
    , mkTemplateSpecTests
    ]

parseFlagsTests :: TestTree
parseFlagsTests = testGroup "parseFlags"
    [ testCase "defaults contain no template path or name" $ do
        let flags = parseFlags []
        template_path flags @?= Nothing
        template_name flags @?= Nothing
        template_search_dir flags @?= "."

    , testCase "template= sets path and clears name" $ do
        let flags = parseFlags ["template=/tmp/prompt.txt"]
        template_path flags @?= Just "/tmp/prompt.txt"
        template_name flags @?= Nothing

    , testCase "template-name= sets name and clears path" $ do
        let flags = parseFlags ["template-name=qwen"]
        template_path flags @?= Nothing
        template_name flags @?= Just "qwen"

    , testCase "template-dir= sets search dir" $ do
        let flags = parseFlags ["template-dir=/tmp/templates"]
        template_search_dir flags @?= "/tmp/templates"

    , testCase "leftmost template selector wins: path then name keeps path" $ do
        let flags = parseFlags
              [ "template=/tmp/a.txt"
              , "template-name=qwen"
              ]
        template_path flags @?= Just "/tmp/a.txt"
        template_name flags @?= Nothing

    , testCase "leftmost template selector wins: name then path keeps name" $ do
        let flags = parseFlags
              [ "template-name=qwen"
              , "template=/tmp/a.txt"
              ]
        template_path flags @?= Nothing
        template_name flags @?= Just "qwen"

    , testCase "leftmost template-dir wins" $ do
        let flags = parseFlags
              [ "template-dir=/tmp/one"
              , "template-dir=/tmp/two"
              ]
        template_search_dir flags @?= "/tmp/one"

    , testCase "template-dir combines with template-name" $ do
        let flags = parseFlags
              [ "template-dir=/tmp/templates"
              , "template-name=qwen"
              ]
        template_search_dir flags @?= "/tmp/templates"
        template_name flags @?= Just "qwen"
        template_path flags @?= Nothing

    , testCase "template-dir combines with template path" $ do
        let flags = parseFlags
              [ "template-dir=/tmp/templates"
              , "template=/tmp/prompt.txt"
              ]
        template_search_dir flags @?= "/tmp/templates"
        template_path flags @?= Just "/tmp/prompt.txt"
        template_name flags @?= Nothing

    , testCase "leftmost template selector still wins when interleaved with dir flags" $ do
        let flags = parseFlags
              [ "template-name=alpha"
              , "template-dir=/tmp/one"
              , "template=/tmp/prompt.txt"
              , "template-dir=/tmp/two"
              ]
        template_name flags @?= Just "alpha"
        template_path flags @?= Nothing
        template_search_dir flags @?= "/tmp/one"
    ]

mkTemplateSpecTests :: TestTree
mkTemplateSpecTests = testGroup "mkTemplateSpec"
    [ testCase "default flags choose DefaultTemplate" $ do
        mkTemplateSpec defaultFlags
            @?= Right (TemplateSpec
                { tsSearchDir = "."
                , tsSource = DefaultTemplate
                })

    , testCase "path chooses TemplateFile" $ do
        let flags = defaultFlags
              { template_path = Just "/tmp/prompt.txt"
              , template_name = Nothing
              , template_search_dir = "/tmp/templates"
              }
        mkTemplateSpec flags
            @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = TemplateFile "/tmp/prompt.txt"
                })

    , testCase "name chooses NamedTemplate" $ do
        let flags = defaultFlags
              { template_path = Nothing
              , template_name = Just "qwen"
              , template_search_dir = "/tmp/templates"
              }
        mkTemplateSpec flags
            @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = NamedTemplate $ unsafeCreateRawTemplateName "qwen"
                })

    , testCase "search dir is preserved with default template" $ do
        let flags = defaultFlags
              { template_search_dir = "/tmp/templates" }
        mkTemplateSpec flags
            @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = DefaultTemplate
                })

    , testCase "path beats name in mkTemplateSpec if both are present" $ do
        let flags = defaultFlags
              { template_path = Just "/tmp/prompt.txt"
              , template_name = Just "qwen"
              , template_search_dir = "/tmp/templates"
              }
        mkTemplateSpec flags
            @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = TemplateFile "/tmp/prompt.txt"
                })

    , testCase "invalid name is rejected" $ do
        let flags = defaultFlags
              { template_path = Nothing
              , template_name = Just "../secrets"
              , template_search_dir = "/tmp/templates"
              }
        mkTemplateSpec flags
            @?= Left (InvalidTemplateName "../secrets")
    ]