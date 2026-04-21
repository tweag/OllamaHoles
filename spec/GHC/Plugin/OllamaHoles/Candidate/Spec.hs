{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Candidate.Spec (tests) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC.Plugin.OllamaHoles.Candidate
  (CandidateRank(..), keepBestByKey, dedupePreparedCandidates, PreparedCandidate(..), mkPrepared)
import GHC.Plugin.OllamaHoles.Candidate.Compat
import GHC.Plugin.OllamaHoles.Candidate.Rewrite (NormExpr(..))
import GHC.Plugin.OllamaHoles.Candidate.WithRenamedExpr

--------------------------------------------------------------------------------
-- Dummy type for pure dedupe tests
--------------------------------------------------------------------------------

data Dummy = Dummy
    { dKey   :: NormExpr
    , dRank  :: CandidateRank
    , dLabel :: Text
    } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance QC.Arbitrary CandidateRank where
    arbitrary = CandidateRank
        <$> smallNat
        <*> smallNat
        <*> smallNat
        <*> smallNat
        where
            smallNat = QC.chooseInt (0, 10)

instance QC.Arbitrary NormExpr where
    arbitrary = QC.sized go
        where
            atom = QC.oneof
                [ NFree . T.pack <$> QC.elements ["f", "g", "x", "y", "map", "show"]
                , NBound <$> QC.chooseInt (0, 4)
                , NLit . T.pack <$> QC.elements ["0", "1", "\"a\""]
                , NOther . T.pack <$> QC.elements ["other", "fallback"]
                ]

            go 0 = atom
            go n = QC.oneof
                [ atom
                , NLam <$> QC.chooseInt (0, 3) <*> go (n `div` 2)
                , do
                    f  <- go (n `div` 2)
                    xs <- QC.listOf (go (n `div` 3))
                    pure (NApp f xs)
                ]

instance QC.Arbitrary Dummy where
    arbitrary = Dummy
        <$> QC.arbitrary
        <*> QC.arbitrary
        <*> (T.pack <$> QC.listOf1 (QC.elements ['a' .. 'z']))

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Candidate"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests = testGroup "unit"
    [ testCase "keepBestByKey prefers lower lexicographic rank" $ do
        let key = NFree "same"
            worse = Dummy key
                (CandidateRank 1 3 10 20)
                "eta-expanded"
            better = Dummy key
                (CandidateRank 0 0 3 3)
                "pointfree"
            out =
                keepBestByKey dKey dRank [worse, better]
        out @?= [better]

    , testCase "different keys are preserved" $ do
        let a = Dummy (NFree "a") (CandidateRank 0 0 1 1) "a"
            b = Dummy (NFree "b") (CandidateRank 0 0 1 1) "b"
            out = L.sortOn dLabel (keepBestByKey dKey dRank [a, b])
        out @?= [a, b]

    , testCase "mixed candidate set dedupes only intended equivalence classes" $
        withRenamedExpr [] "f" $ \dflags eF ->
        withRenamedExpr [] "\\x -> f x" $ \_ eFEta ->
        withRenamedExpr [] "g" $ \_ eG ->
        withRenamedExpr [] "\\y -> g y" $ \_ eGEta ->
        withRenamedExpr [] "\\z -> z" $ \_ eId -> do
            let pF    = mkPrepared dflags 1 "f" eF
                pFEta = mkPrepared dflags 1 "\\x -> f x" eFEta
                pG    = mkPrepared dflags 1 "g" eG
                pGEta = mkPrepared dflags 1 "\\y -> g y" eGEta
                pId   = mkPrepared dflags 1 "\\z -> z" eId

                out = L.sortOn prSource $
                    dedupePreparedCandidates [pFEta, pG, pGEta, pId, pF]

            map prSource out @?= ["\\z -> z", "f", "g"]

    , testCase "viewExpr gives up on case expressions" $
        withRenamedExpr [] "case x of y -> y" $ \dflags e ->
            case viewExpr dflags e of
                VUnknown _ -> pure ()
                other      -> assertFailure $ "expected VUnknown, got: " <> showExprView other

    , testCase "viewExpr gives up on let expressions" $
        withRenamedExpr [] "let z = x in z" $ \dflags e ->
            case viewExpr dflags e of
                VUnknown _ -> pure ()
                other      -> assertFailure $ "expected VUnknown, got: " <> showExprView other
    ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
    [ QC.testProperty "dedupe returns at most one representative per key" $
        \xs ->
            let ys   = keepBestByKey dKey dRank (xs :: [Dummy])
                keys = map dKey ys
            in length keys == length (L.nub keys)

    , localOption (QC.QuickCheckMaxSize 20) $
        QC.testProperty "chosen representative has minimal rank in its class" $
        \xs ->
            let ys = keepBestByKey dKey dRank (xs :: [Dummy])

                minima :: M.Map NormExpr CandidateRank
                minima = M.fromListWith min
                    [ (dKey x, dRank x) | x <- xs ]

            in all (\y -> M.lookup (dKey y) minima == Just (dRank y)) ys

    , localOption (QC.QuickCheckTests 12) $
        QC.testProperty "deduped prepared candidates have distinct normalization keys" $
        QC.ioProperty $
            withTwoRenamedExpr [] "f" "\\x -> f x" $ \dflags e1 e2 -> do
                let p1 = mkPrepared dflags 1 "f" e1
                    p2 = mkPrepared dflags 1 "\\x -> f x" e2
                    out = dedupePreparedCandidates [p1, p2]
                    keys = map prNormKey out
                pure (length keys == length (L.nub keys))
    ]