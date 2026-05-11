module GHC.Plugin.OllamaHoles.Data.Template.Load.Spec (tests) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import Test.Tasty.QuickCheck qualified as QC
import System.Directory (doesFileExist)
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
        result <- loadTemplate (TemplateSpec "" DefaultTemplate)
        pure $ result QC.=== parseTemplate defaultTemplateText

  , QC.testProperty "TemplateFile loads exactly as parseTemplate file contents" $
      QC.forAll genTemplateText $ \raw ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            let fp = dir </> "prompt.txt"
            T.writeFile fp raw
            result <- loadTemplate (TemplateSpec dir (TemplateFile fp))
            pure $ result QC.=== parseTemplate raw

  , QC.testProperty "TemplateFile missing reports the requested file path" $
      QC.forAll genSafeFileNameText $ \fileName ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            let fp = dir </> T.unpack fileName <> ".txt"
            result <- loadTemplate (TemplateSpec dir (TemplateFile fp))
            pure $ result QC.=== Left (TemplateFileNotFound fp)

  , QC.testProperty "NamedTemplate loads <name>.txt exactly as parseTemplate file contents" $
      QC.forAll genTemplateNameText $ \name ->
      QC.forAll genTemplateText $ \raw ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            let fp = dir </> T.unpack name <> ".txt"
            T.writeFile fp raw
            result <- loadTemplate $
              TemplateSpec dir $ NamedTemplate $ unsafeCreateRawTemplateName name
            pure $ result QC.=== parseTemplate raw

  , QC.testProperty "NamedTemplate unknown safe name reports search dir and name" $
      QC.forAll genTemplateNameText $ \name ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            result <- loadTemplate $
              TemplateSpec dir $ NamedTemplate $ unsafeCreateRawTemplateName name
            pure $ result QC.=== Left (UnknownTemplateName dir name)

  , QC.testProperty "NamedTemplate invalid names are rejected before lookup" $
      QC.forAll genInvalidTemplateNameText $ \name ->
        QC.ioProperty $
          withSystemTempDirectory "ollama-holes-template-load-spec" $ \dir -> do
            result <- loadTemplate $
              TemplateSpec dir $ NamedTemplate $ unsafeCreateRawTemplateName name
            pure $ result QC.=== Left (InvalidTemplateName name)
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
    , \_ -> TemplateSpec "" DefaultTemplate
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
    , \dir -> TemplateSpec dir (TemplateFile $ dir </> "prompt.txt")
    , Template [TemplateChunk "hello ", TemplateVar "name"]
    )

  , ( "NamedTemplate loads <searchDir>/<name>.txt"
    , Just "hello {{context}}"
    , \dir -> TemplateSpec dir
        $ NamedTemplate $ unsafeCreateRawTemplateName "prompt"
    , Template [TemplateChunk "hello ", TemplateVar "context"]
    )
  ]

tests_loadTemplate_unit_failure
  :: [(String, Maybe Text, FilePath -> TemplateSpec)]
tests_loadTemplate_unit_failure =
  [ ( "TemplateFile reports missing file"
    , Nothing
    , \dir -> TemplateSpec dir (TemplateFile $ dir </> "bogus.txt")
    )

  , ( "NamedTemplate rejects empty name"
    , Nothing
    , \dir -> TemplateSpec dir (NamedTemplate $ unsafeCreateRawTemplateName "")
    )

  , ( "NamedTemplate rejects unknown name"
    , Nothing
    , \dir -> TemplateSpec dir (NamedTemplate $ unsafeCreateRawTemplateName "bogus")
    )

  , ( "NamedTemplate rejects path traversal with .."
    , Nothing
    , \dir -> TemplateSpec dir (NamedTemplate $ unsafeCreateRawTemplateName "../../secret")
    )

  , ( "NamedTemplate rejects slash"
    , Nothing
    , \dir -> TemplateSpec dir (NamedTemplate $ unsafeCreateRawTemplateName "foo/bar")
    )

  , ( "NamedTemplate rejects backslash"
    , Nothing
    , \dir -> TemplateSpec dir (NamedTemplate $ unsafeCreateRawTemplateName "foo\\bar")
    )
  ]
