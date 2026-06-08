module GHC.Plugin.OllamaHoles.Options.Spec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Flags
  ( mkTemplateSpec
  )
import GHC.Plugin.OllamaHoles.Data.Flags
  (Flags(..), parseFlags, FlagToken(..))
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal


tests :: TestTree
tests =
  testGroup "Options"
    [ mkTemplateSpecTests
    ]

expectParseOk :: [String] -> IO (Flags, [FlagToken])
expectParseOk opts =
  case parseFlags opts of
    Left err ->
      assertFailure ("unexpected parse error: " <> show err) >> fail "unreachable"
    Right ok ->
      pure ok


mkTemplateSpecTests :: TestTree
mkTemplateSpecTests =
  testGroup "mkTemplateSpec"
    [ testCase "default flags choose DefaultTemplate" $ do
        mkTemplateSpec mempty
          @?= Right (TemplateSpec
                { tsSearchDir = "."
                , tsSource = DefaultTemplate
                })

    , testCase "path chooses TemplateFile" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template=/tmp/prompt.txt"
          ]
        unknowns @?= []
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = TemplateFile "/tmp/prompt.txt"
                })

    , testCase "name chooses NamedTemplate" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates"
          , "template-name=qwen"
          ]
        unknowns @?= []
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = NamedTemplate (unsafeCreateRawTemplateName "qwen")
                })

    , testCase "search dir is preserved with default template" $ do
        (flags, unknowns) <- expectParseOk
          [ "template-dir=/tmp/templates" ]
        unknowns @?= []
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = DefaultTemplate
                })

    , testCase "path beats name in mkTemplateSpec if both are present" $ do
        let flags = mempty
              { template_path = Just "/tmp/prompt.txt"
              , template_name = Just (unsafeCreateRawTemplateName "qwen")
              , template_search_dir = Just "/tmp/templates"
              }
        mkTemplateSpec flags
          @?= Right (TemplateSpec
                { tsSearchDir = "/tmp/templates"
                , tsSource = TemplateFile "/tmp/prompt.txt"
                })
    ]
