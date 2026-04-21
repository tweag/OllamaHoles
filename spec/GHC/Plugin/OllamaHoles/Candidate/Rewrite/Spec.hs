{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Candidate.Rewrite.Spec (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Candidate (applyMany)
import GHC.Plugin.OllamaHoles.Candidate.Rewrite

tests :: TestTree
tests = testGroup "Candidate.Rewrite"
    [ unitTests
    , propertyTests
    ]

-- This function is **not** about putting an
-- expression into a canonical normal form! It just
-- checks that application nodes are stored in flattened
-- spine form rather than nested left-associated form.
--
-- For example, this expression is structurally canonical:
--   NApp (NFree "f") [NFree "x", NFree "y"]
-- But this one is not:
--   NApp (NApp (NFree "f") [NFree "x"]) [NFree "y"]
isStructurallyCanonical :: NormExpr -> Bool
isStructurallyCanonical = \case
    NFree _   -> True
    NBound _  -> True
    NLit _    -> True
    NOther _  -> True
    NLam _ b  -> isStructurallyCanonical b
    NApp f xs ->
        not (null xs)
        && not (isApp f)
        && isStructurallyCanonical f
        && all isStructurallyCanonical xs
    where
        isApp (NApp _ _) = True
        isApp _          = False

--------------------------------------------------------------------------------
-- Unit tests
--------------------------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "unit"
    [ testCase "compose-app rewrites explicit composition application" $ do
        let before = NApp (NFree ".")
                [ NFree "f"
                , NFree "g"
                , NBound 0
                ]
            expected = NApp (NFree "f")
                [ NApp (NFree "g") [NBound 0] ]
            (after, trace) = normalizeNormExprWith [composeAppRule] before
        after @?= expected
        map rsRule trace @?= ["compose-app"]

    , testCase "dollar-app rewrites ($) application" $ do
        let before = NApp (NFree "$")
                [ NFree "f"
                , NBound 0
                ]
            expected =
                NApp (NFree "f") [NBound 0]
            (after, trace) = normalizeNormExprWith [dollarAppRule] before
        after @?= expected
        map rsRule trace @?= ["dollar-app"]

    , testCase "id-app removes id head application" $ do
        let before = NApp (NFree "id")
                [ NFree "f"
                , NBound 0
                ]
            expected =
                NApp (NFree "f") [NBound 0]
            (after, trace) = normalizeNormExprWith [idAppRule] before
        after @?= expected
        map rsRule trace @?= ["id-app"]

    , testCase "eta-reduce-1 reduces a simple eta redex" $ do
        let before =
                NLam 1 (NApp (NFree "f") [NBound 0])
            expected =
                NFree "f"
            (after, trace) = normalizeNormExprWith [etaReduce1Rule] before
        after @?= expected
        map rsRule trace @?= ["eta-reduce-1"]

    , testCase "eta-reduce-1 does not fire when binder occurs in function" $ do
        let before =
                NLam 1 (NApp (NApp (NBound 0) [NFree "f"]) [NBound 0])
        rrStep etaReduce1Rule before @?= Nothing

    , testCase "default rules collapse pointfree and eta-expanded composition" $ do
        let pointfree = NLam 1 $
                NApp (NFree ".")
                    [ NApp (NFree "map") [NFree "show"]
                    , NFree "sort"
                    , NBound 0
                    ]

            etaExpanded = NLam 1 $
                NApp (NFree "map")
                    [ NFree "show"
                    , NApp (NFree "sort") [NBound 0]
                    ]

            (k1, _) = normalizeNormExpr pointfree
            (k2, _) = normalizeNormExpr etaExpanded

        k1 @?= k2

    , testCase "rewriting proceeds to a fixed point" $ do
        let before = NApp (NFree "$")
                [ NApp (NFree ".")
                    [ NFree "id"
                    , NFree "g"
                    ]
                , NBound 0
                ]
            (after, trace) = normalizeNormExpr before

        after @?=
            NApp (NFree "g") [NBound 0]

        assertBool "expected multiple rewrite steps" (length trace >= 2)

    , testCase "rewriteChildren flattens nested applications" $ do
        let before = NApp (NApp (NFree "f") [NFree "a"]) [NFree "b"]
            (after, _) = normalizeNormExprWith [] before
        after @?= NApp (NFree "f") [NFree "a", NFree "b"]

    , testCase "applyMany flattens nested applications" $ do
        let expr = applyMany (NApp (NFree "f") [NFree "a"]) [NFree "b", NFree "c"]
        expr @?= NApp (NFree "f") [NFree "a", NFree "b", NFree "c"]

    , testCase "compose-app preserves canonical spine form" $ do
        let before =
                NApp (NFree ".")
                [ NApp (NFree "map") [NFree "show"]
                , NFree "sort"
                , NBound 0
                ]
            expected =
                NApp (NFree "map")
                [ NFree "show"
                , NApp (NFree "sort") [NBound 0]
                ]
            (after, _) = normalizeNormExprWith [composeAppRule] before
        after @?= expected

    , testCase "dollar-app exposes compose-app redex by flattening" $ do
        let before =
                NApp (NFree "$")
                [ NApp (NFree ".") [NFree "id", NFree "g"]
                , NBound 0
                ]
            expected =
                NApp (NFree "g") [NBound 0]
            (after, _) = normalizeNormExpr before
        after @?= expected

    , testCase "normalized output is canonical" $ do
        let before =
                NApp (NApp (NFree "$") [NApp (NFree ".") [NFree "f", NFree "g"]]) [NBound 0]
            (after, _) = normalizeNormExpr before
        assertBool "expected canonical normalized form" (isStructurallyCanonical after)
    ]

--------------------------------------------------------------------------------
-- Property tests
--------------------------------------------------------------------------------

instance QC.Arbitrary NormExpr where
  arbitrary = QC.sized genExpr
    where
        atom =
            QC.oneof
                [ NFree . T.pack <$> QC.elements
                    ["f", "g", "h", ".", "$", "id", "map", "sort", "show"]
                , NBound <$> QC.chooseInt (0, 4)
                , NLit . T.pack <$> QC.elements ["0", "1", "\"x\""]
                , NOther . T.pack <$> QC.elements ["other", "opaque"]
                ]

        genExpr 0 = atom
        genExpr n = QC.oneof
            [ atom
            , do
                f  <- genExpr (n `div` 2)
                xs <- QC.listOf (genExpr (n `div` 3))
                pure (NApp f xs)
            , do
                k <- QC.chooseInt (0, 3)
                body <- genExpr (n `div` 2)
                pure (NLam k body)
            ]

propertyTests :: TestTree
propertyTests = localOption (QC.QuickCheckMaxSize 20) $ testGroup "properties"
    [ QC.testProperty "normalization is idempotent" $
        \(expr :: NormExpr) ->
            let (n1, _) = normalizeNormExpr expr
                (n2, _) = normalizeNormExpr n1
            in n1 == n2

    , QC.testProperty "normalization never increases rewrite measure" $
        \(expr :: NormExpr) ->
            let (n, _) = normalizeNormExpr expr
            in rewriteMeasure n <= rewriteMeasure expr

    , QC.testProperty "every recorded rewrite step strictly decreases measure" $
        \(expr :: NormExpr) ->
            let (_, steps) = normalizeNormExpr expr
            in all (\RewriteStep{rsBefore, rsAfter} ->
                        rewriteMeasure rsAfter < rewriteMeasure rsBefore)
                    steps

    , QC.testProperty "normalizeNormExprWith [] returns canonical application spines" $
        \(expr :: NormExpr) ->
            let (n, _) = normalizeNormExprWith [] expr
            in isStructurallyCanonical n

    , QC.testProperty "normalizeNormExprWith [] is idempotent" $
        \(expr :: NormExpr) ->
            let (n1, _) = normalizeNormExprWith [] expr
                (n2, _) = normalizeNormExprWith [] n1
            in n1 == n2

    , QC.testProperty "normalizeNormExprWith [] produces no rewrite steps" $
        \(expr :: NormExpr) ->
            let (_, steps) = normalizeNormExprWith [] expr
            in null steps

    , QC.testProperty "compose-app handles extra trailing arguments" $
        \(extraCount :: QC.NonNegative Int) ->
            let rest =
                    take (QC.getNonNegative extraCount `mod` 4) $
                    cycle [NFree "a", NFree "b"]
                before =
                    NApp (NFree ".") ([NFree "f", NFree "g", NBound 0] <> rest)
                (after, _) = normalizeNormExprWith [composeAppRule] before
                expected =
                    NApp (NFree "f") ([NApp (NFree "g") [NBound 0]] <> rest)
            in after == expected

    , QC.testProperty "normalization returns canonical application spines" $
        \(expr :: NormExpr) ->
            let (n, _) = normalizeNormExpr expr
            in isStructurallyCanonical n

    , QC.testProperty "normalized expressions have no nested application heads" $
        \(expr :: NormExpr) ->
            let (n, _) = normalizeNormExpr expr
            in not (hasNestedAppHead n)

    , QC.testProperty "canonicalizeNormExpr is idempotent" $
        \(expr :: NormExpr) ->
            canonicalizeNormExpr (canonicalizeNormExpr expr)
            == canonicalizeNormExpr expr

    , QC.testProperty "canonicalizeNormExpr produces structurally canonical expressions" $
        \(expr :: NormExpr) ->
            isStructurallyCanonical (canonicalizeNormExpr expr)
    ]

-- Check that no nested application heads survive normalization.
hasNestedAppHead :: NormExpr -> Bool
hasNestedAppHead = \case
    NApp (NApp _ _) _ -> True
    NApp f xs         -> hasNestedAppHead f || any hasNestedAppHead xs
    NLam _ b          -> hasNestedAppHead b
    _                 -> False