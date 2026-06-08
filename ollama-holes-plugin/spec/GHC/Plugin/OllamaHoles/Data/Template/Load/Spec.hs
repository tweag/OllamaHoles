module GHC.Plugin.OllamaHoles.Data.Template.Load.Spec (tests) where

import Data.Functor ((<&>))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import Test.Tasty.QuickCheck qualified as QC
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Template.Types.Internal
import GHC.Plugin.OllamaHoles.Data.Template.Types.Gen



tests :: TestTree
tests = testGroup "Template.Load"
  [ tests_loadTemplate_unit
  , tests_loadTemplate_prop
  ]

tests_loadTemplate_unit :: TestTree
tests_loadTemplate_unit = testGroup "loadTemplate (unit)"
  [ testGroup "success" $
      tests_loadTemplate_unit_success <&> \(name, mInput, spec, expected) ->
        testCase name $ withSystemTempDirectory "template-spec" $ \dir -> do
          let fp = dir </> "prompt.txt"
          case mInput of
            Nothing -> pure ()
            Just input -> T.writeFile fp input
          result <- loadTemplate $ spec dir
          case result of
            Left err -> assertFailure $
              "expected successful load but got this error: " <> show err
            Right actual -> actual @?= expected

  , testGroup "failure" $
      tests_loadTemplate_unit_failure <&> \(name, mInput, spec) ->
        testCase name $ withSystemTempDirectory "template-spec" $ \dir -> do
          let fp = dir </> "prompt.txt"
          case mInput of
            Nothing -> pure ()
            Just input -> T.writeFile fp input
          result <- loadTemplate $ spec dir
          case result of
            Left _ -> pure ()
            Right ok -> assertFailure $
              "expected failed load but got this result: " <> show ok
  ]

tests_loadTemplate_prop :: TestTree
tests_loadTemplate_prop = testGroup "loadTemplate (prop)"
  [ QC.testProperty "DefaultTemplate loads as parseTemplate defaultTemplateText" $
      QC.ioProperty $ do
        result <- loadTemplate (TemplateSpec "" mempty DefaultTemplate)
        pure $ result QC.=== parseTemplate defaultTemplateText

  , QC.testProperty "TemplateFile loads exactly as parseTemplate file contents" $
      QC.forAll genTemplateText $ \raw ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            let fp = dir </> "prompt.txt"
            T.writeFile fp raw
            result <- loadTemplate (TemplateSpec dir mempty (TemplateFile fp))
            pure $ result QC.=== parseTemplate raw

  , QC.testProperty "TemplateFile missing reports the requested file path" $
      QC.forAll genSafeFileNameText $ \fileName ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            let fp = dir </> T.unpack fileName <> ".txt"
            result <- loadTemplate (TemplateSpec dir mempty (TemplateFile fp))
            pure $ result QC.=== Left (TemplateFileNotFound fp)

  , QC.testProperty "NamedTemplate unknown safe name reports search dir and name" $
      QC.forAll genValidTemplateName $ \name ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            result <- loadTemplate $
              TemplateSpec dir mempty $ NamedTemplate name
            pure $ result QC.=== Left (UnknownTemplateName name)

    , QC.testProperty "NamedTemplate fails when absent from template map" $
      QC.forAll genValidTemplateName $ \name ->
        QC.ioProperty $ do
          result <- loadTemplate $ TemplateSpec "" mempty (NamedTemplate name)
          pure $ case result of
            Left (UnknownTemplateName rawName) -> rawName QC.=== name
            other -> QC.counterexample
              ("expected UnknownTemplateName, got: " <> show other) False

  , QC.testProperty "NamedTemplate loads any template present in template map" $
    QC.forAll genValidTemplateName $ \name ->
    QC.forAll genValidTemplate $ \template -> QC.ioProperty $ do
      result <- loadTemplate $ TemplateSpec
        ""
        (M.fromList [(name, template)])
        (NamedTemplate name)

      pure $ result QC.=== Right template
  ]



tests_loadTemplate_unit_success
  :: [ ( String
       , Maybe Text
       , FilePath -> TemplateSpec
       , Template
       )
    ]
tests_loadTemplate_unit_success =
  [ ( "DefaultTemplate loads successfully"
    , Nothing
    , \_ -> TemplateSpec "" mempty DefaultTemplate
    , Template
        [ TemplateChunk "Preliminaries:\n"
        , TemplateVar "docs"
        , TemplateChunk
            "\n--------------------------------------------------------------------\n\
            \You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler.\n\
            \You are given a hole in a Haskell program, and you need to fill it in.\n\
            \The hole is represented by the following JSON encoded information:\n"
        , TemplateVar "context"
        , TemplateChunk
            "\nProvide one or more Haskell expressions that could fill this hole.\n\
            \This means coming up with an expression of the correct type that satisfies\
            \ the constraints.\nPay special attention to the type of the hole, specifically\
            \ whether it is a function.\nMake sure you synthesize an expression that matches\
            \ the type of the hole.\nOutput ONLY the raw Haskell expression(s), one per line.\n\
            \Do not try to bind the hole variable, e.g. `_b = ...`. Produce only the expression.\n\
            \Do not include explanations, introductions, or any surrounding text.\n\
            \If you are using a function from scope, make sure to use the qualified name from the\
            \ list of things in scope.\nOutput a maximum of "
        , TemplateVar "numexpr"
        , TemplateChunk " expressions.\n"
        ]
    )

  , ( "TemplateFile loads an existing file"
    , Just "hello {{name}}"
    , \dir -> TemplateSpec dir mempty (TemplateFile $ dir </> "prompt.txt")
    , Template [TemplateChunk "hello ", TemplateVar "name"]
    )

  , ( "NamedTemplate resolves from template map"
    , Nothing
    , \_dir -> TemplateSpec
        ""
        (M.fromList
          [ ( unsafeCreateRawTemplateName "brief"
            , Template [TemplateChunk "Return only expressions."]
            )
          ])
        (NamedTemplate (unsafeCreateRawTemplateName "brief"))
    , Template [TemplateChunk "Return only expressions."]
    )

  , ( "loaded file parses into expected chunks and vars"
    , Just "A {{foo}} B {{bar}}."
    , \dir -> TemplateSpec dir mempty (TemplateFile $ dir </> "prompt.txt")
    , Template
        [ TemplateChunk "A "
        , TemplateVar "foo"
        , TemplateChunk " B "
        , TemplateVar "bar"
        , TemplateChunk "."
        ]
    )

  , ( "TemplateFile resolves relative path under search dir"
    , Just "Return only expressions."
    , \dir -> TemplateSpec
        { tsSearchDir = dir
        , tsTmplMap = mempty
        , tsSource = TemplateFile "prompt.txt"
        }
    , expectTemplate "Return only expressions."
    )

  , ( "TemplateFile preserves absolute path"
    , Just "Return only expressions."
    , \dir -> TemplateSpec
        { tsSearchDir = dir </> "wrong-dir"
        , tsTmplMap = mempty
        , tsSource = TemplateFile (dir </> "prompt.txt")
        }
    , expectTemplate "Return only expressions."
    )
  ]

tests_loadTemplate_unit_failure
  :: [(String, Maybe Text, FilePath -> TemplateSpec)]
tests_loadTemplate_unit_failure =
  [ ( "TemplateFile reports missing file"
    , Nothing
    , \dir -> TemplateSpec dir mempty (TemplateFile $ dir </> "bogus.txt")
    )

  , ( "NamedTemplate rejects unknown name"
    , Nothing
    , \dir -> TemplateSpec dir mempty (NamedTemplate $ unsafeCreateRawTemplateName "bogus")
    )

  , ( "NamedTemplate rejects unknown config template name"
    , Nothing
    , \_dir -> TemplateSpec "" mempty (NamedTemplate $ unsafeCreateRawTemplateName "missing")
    )

  , ( "NamedTemplate does not fall back to search dir file"
    , Just "Return only expressions."
    , \dir -> TemplateSpec dir mempty (NamedTemplate $ unsafeCreateRawTemplateName "prompt")
    )

  , ( "TemplateFile missing relative path reports resolved path"
    , Nothing
    , \dir -> TemplateSpec
        { tsSearchDir = dir
        , tsTmplMap = mempty
        , tsSource = TemplateFile "missing.txt"
        }
    )
  ]



expectTemplate :: Text -> Template
expectTemplate raw = case parseTemplate raw of
  Right template -> template
  Left err -> error $ "invalid test template body: " <> show err
