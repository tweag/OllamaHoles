module GHC.Plugin.OllamaHoles.Data.Template.Expand.Spec (tests) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Template.Types.Gen



tests :: TestTree
tests = testGroup "Expand"
  [ tests_expandTemplate_unit
  , tests_expandTemplate_prop
  ]

tests_expandTemplate_unit :: TestTree
tests_expandTemplate_unit = testGroup "expandTemplate (unit)"
  [ testGroup "success" $
      tests_expandTemplate_unit_success <&> \(name, (templ, env), expected) ->
        testCase name $ case expandTemplate templ env of
          Left err -> assertFailure $
            "expected successful expansion but got this error: " <> show err
          Right actual -> actual @?= expected

  , testGroup "failure" $
      tests_expandTemplate_unit_failure <&> \(name, (templ, env)) ->
        testCase name $ case expandTemplate templ env of
          Left _ -> pure ()
          Right ok -> assertFailure $
            "expected failed expansion but got this result: " <> show ok
  ]

tests_expandTemplate_prop :: TestTree
tests_expandTemplate_prop = testGroup "expandTemplate (prop)"
  [ QC.testProperty "chunks-only templates expand to concatenated chunks" $
      QC.forAll genTemplateChunks $ \chunks ->
        let templ =
              Template (TemplateChunk <$> chunks)
            expected =
              T.concat chunks
        in
          expandTemplate templ (mkTemplateEnv [])
            QC.=== Right expected

  , QC.testProperty "templates with known placeholders expand by substitution" $
      QC.forAll genKnownExpansionCase $ \(templ, envPairs, expected) ->
        expandTemplate templ (mkTemplateEnv $ fmap (\(k,v) -> (unPlaceholder k, v)) envPairs)
          QC.=== Right expected

  , QC.testProperty "unused environment entries do not affect expansion" $
      QC.forAll genKnownExpansionCase $ \(templ, envPairs, expected) ->
      QC.forAll (genUnusedEnvPairsFor templ envPairs) $ \unusedPairs ->
        expandTemplate templ (mkTemplateEnv $ fmap (\(k,v) -> (unPlaceholder k, v)) (envPairs <> unusedPairs))
          QC.=== Right expected

  , QC.testProperty "unknown placeholders are reported in occurrence order" $
      QC.forAll genUnknownExpansionCase $ \(templ, envPairs, missing) ->
        expandTemplate templ (mkTemplateEnv $ fmap (\(k,v) -> (unPlaceholder k, v)) envPairs)
          QC.=== Left (UnknownPlaceholders missing)

  , QC.testProperty "repeated unknown placeholders are reported repeatedly" $
      QC.forAll genPlaceholderNameText $ \name ->
        let templ =
              Template
                [ TemplateVar $ Placeholder name
                , TemplateChunk "-"
                , TemplateVar $ Placeholder name
                ]
        in
          expandTemplate templ (mkTemplateEnv [])
            QC.=== Left (UnknownPlaceholders [Placeholder name, Placeholder name])
  ]



tests_expandTemplate_unit_success
  :: [(String, (Template, TemplateEnv), Text)]
tests_expandTemplate_unit_success =
  [ ( "chunks-only template expands unchanged"
    , ( Template [TemplateChunk "hello world"]
      , mkTemplateEnv []
      )
    , "hello world"
    )

  , ( "single placeholder expands"
    , ( Template
          [ TemplateChunk "hello "
          , TemplateVar "name"
          , TemplateChunk "!"
          ]
      , mkTemplateEnv [("name", "Nathan")]
      )
    , "hello Nathan!"
    )

  , ( "adjacent placeholders expand in order"
    , ( Template
          [ TemplateVar "x"
          , TemplateVar "y"
          ]
      , mkTemplateEnv
          [ ("x", "A")
          , ("y", "B")
          ]
      )
    , "AB"
    )

  , ( "mixed chunks and placeholders expand correctly"
    , ( Template
          [ TemplateVar "greeting"
          , TemplateChunk ", "
          , TemplateVar "name"
          , TemplateChunk "!"
          ]
      , mkTemplateEnv
          [ ("greeting", "hello")
          , ("name", "world")
          ]
      )
    , "hello, world!"
    )

  , ( "unused environment entries are ignored"
    , ( Template
          [ TemplateChunk "value="
          , TemplateVar "used"
          ]
      , mkTemplateEnv
          [ ("used", "ok")
          , ("unused", "ignored")
          ]
      )
    , "value=ok"
    )

  , ( "default template expands with required variables"
    , ( Template
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
      , mkTemplateEnv
          [ ("docs", "some docs")
          , ("context", "{\"hole\":\"x\"}")
          , ("numexpr", "5")
          ]
      )
    , "Preliminaries:\n\
      \some docs\n\
      \--------------------------------------------------------------------\n\
      \You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler.\n\
      \You are given a hole in a Haskell program, and you need to fill it in.\n\
      \The hole is represented by the following JSON encoded information:\n\
      \{\"hole\":\"x\"}\n\
      \Provide one or more Haskell expressions that could fill this hole.\n\
      \This means coming up with an expression of the correct type that satisfies the constraints.\n\
      \Pay special attention to the type of the hole, specifically whether it is a function.\n\
      \Make sure you synthesize an expression that matches the type of the hole.\n\
      \Output ONLY the raw Haskell expression(s), one per line.\n\
      \Do not try to bind the hole variable, e.g. `_b = ...`. Produce only the expression.\n\
      \Do not include explanations, introductions, or any surrounding text.\n\
      \If you are using a function from scope, make sure to use the qualified name from the list of things in scope.\n\
      \Output a maximum of 5 expressions.\n"
    )
  ]

tests_expandTemplate_unit_failure
  :: [(String, (Template, TemplateEnv))]
tests_expandTemplate_unit_failure =
  [ ( "unknown placeholder is reported"
    , ( Template
          [ TemplateChunk "x="
          , TemplateVar "missing"
          ]
      , mkTemplateEnv [("context", "ctx")]
      )
    )

  , ( "repeated unknown placeholders are reported in occurrence order"
    , ( Template
          [ TemplateVar "missing"
          , TemplateChunk "-"
          , TemplateVar "missing"
          ]
      , mkTemplateEnv []
      )
    )

  , ( "mixed known and unknown placeholders reports only unknown ones"
    , ( Template
          [ TemplateVar "first"
          , TemplateChunk "-"
          , TemplateVar "known"
          , TemplateChunk "-"
          , TemplateVar "second"
          ]
      , mkTemplateEnv [("known", "ok")]
      )
    )

  , ( "default template expansion fails if required placeholders are missing"
    , ( Template
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
      , mkTemplateEnv []
      )
    )
  ]
