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
    ( CandidateRank(..)
    , NormExpr(..)
    , keepBestByKey
    )

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
    ]