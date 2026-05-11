module GHC.Plugin.OllamaHoles.Template.Load.Spec (tests) where

import Data.List qualified as L
import Data.Text qualified as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import GHC.Plugin.OllamaHoles.Data.Template.Types
import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal
import GHC.Plugin.OllamaHoles.Data.Template.Error
import GHC.Plugin.OllamaHoles.Data.Template.Parse
import GHC.Plugin.OllamaHoles.Data.Template.Load


tests :: TestTree
tests =
  testGroup "Template"
    [ loadAndParseTests
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