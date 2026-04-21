{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Plugin.OllamaHoles.Candidate.Normalize.Spec (tests) where

import Data.Generics (listify)
import Data.List qualified as L
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC

import GHC
    ( Ghc
    , GhcRn
    , HsBind(..)
    , LHsBind
    , LHsExpr
    , LoadHowMuch(..)
    , Match(..)
    , MatchGroup(..)
    , RenamedSource
    , TypecheckedModule(..)
    , GRHS(..)
    , GRHSs(..)
    , guessTarget
    , getModSummary
    , getSessionDynFlags
    , load
    , mkModuleName
    , parseModule
    , runGhc
    , setSessionDynFlags
    , setTargets
    , typecheckModule
    )
import GHC qualified as GHC
import GHC.Plugins hiding ((<>))
import qualified GHC.Paths as GHC.Paths

import GHC.Plugin.OllamaHoles.Candidate
    ( CandidateRank(..)
    , PreparedCandidate(..)
    , CheckedCandidate(..)
    , dedupePreparedCandidates
    , emptyCandidateLog
    , normalizeForHoleArity
    , rankPreparedCandidate
    , prepareCandidate
    , mkPrepCtx
    , mkPrepared
    )

tests :: TestTree
tests = testGroup "Candidate normalization/ranking"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests = testGroup "unit"
    [ testCase "unary eta-expansion normalizes to same key" $
        withTwoRenamedExpr [] "f" "\\x -> f x" $ \dflags e1 e2 -> do
            let k1 = normalizeForHoleArity dflags 1 e1
                k2 = normalizeForHoleArity dflags 1 e2
            k1 @?= k2

    , testCase "binary eta-expansion normalizes to same key" $
        withTwoRenamedExpr [] "g" "\\x y -> g x y" $ \dflags e1 e2 -> do
            let k1 = normalizeForHoleArity dflags 2 e1
                k2 = normalizeForHoleArity dflags 2 e2
            k1 @?= k2

    , testCase "alpha-equivalent lambdas normalize to same key" $
        withTwoRenamedExpr [] "\\x -> x" "\\y -> y" $ \dflags e1 e2 -> do
            let k1 = normalizeForHoleArity dflags 1 e1
                k2 = normalizeForHoleArity dflags 1 e2
            k1 @?= k2

    , testCase "extra arity is not collapsed when hole arity is smaller" $
        withTwoRenamedExpr [] "g" "\\x y -> g x y" $ \dflags e1 e2 -> do
            let k1 = normalizeForHoleArity dflags 1 e1
                k2 = normalizeForHoleArity dflags 1 e2
            assertBool "expected different normalization keys" (k1 /= k2)

    , testCase "parens do not change normalization key" $
        withTwoRenamedExpr [] "f" "(((f)))" $ \dflags e1 e2 -> do
            let k1 = normalizeForHoleArity dflags 1 e1
                k2 = normalizeForHoleArity dflags 1 e2
            k1 @?= k2

    , testCase "ranking prefers pointfree form over eta-expanded form" $
        withTwoRenamedExpr [] "f" "\\x -> f x" $ \dflags ePointfree eEta -> do
            let r1 = rankPreparedCandidate dflags "f" ePointfree
                r2 = rankPreparedCandidate dflags "\\x -> f x" eEta
            assertBool
                ("expected pointfree rank <= eta rank, got " <> show r1 <> " vs " <> show r2)
                (r1 <= r2)

    , testCase "dedupe keeps best representative for same normalization key" $
        withTwoRenamedExpr [] "f" "\\x -> f x" $ \dflags ePointfree eEta -> do
            let p1 = mkPrepared dflags 1 "f" ePointfree
                p2 = mkPrepared dflags 1 "\\x -> f x" eEta
                out = dedupePreparedCandidates [p2, p1]
            map prSource out @?= ["f"]

    , testCase "dedupe preserves distinct normalized keys" $
        withTwoRenamedExpr [] "f" "g" $ \dflags eF eG -> do
            let p1 = mkPrepared dflags 1 "f" eF
                p2 = mkPrepared dflags 1 "g" eG
                out = L.sortOn prSource (dedupePreparedCandidates [p1, p2])
            map prSource out @?= ["f", "g"]
    ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
    [ localOption (QC.QuickCheckTests 12) $
        QC.testProperty "adding parentheses preserves normalization" $
        QC.forAll (QC.chooseInt (0, 6)) $ \n ->
            QC.ioProperty $ do
                let src = replicate n '(' ++ "f" ++ replicate n ')'
                withTwoRenamedExpr [] "f" src $ \dflags e1 e2 -> do
                    let k1 = normalizeForHoleArity dflags 1 e1
                        k2 = normalizeForHoleArity dflags 1 e2
                    pure (k1 == k2)

    , localOption (QC.QuickCheckTests 12) $
        QC.testProperty "dedupe choice is invariant under input order for simple eta pair" $
        QC.forAll QC.arbitrary $ \rev ->
            QC.ioProperty $
                withTwoRenamedExpr [] "f" "\\x -> f x" $ \dflags e1 e2 -> do
                    let p1 = mkPrepared dflags 1 "f" e1
                        p2 = mkPrepared dflags 1 "\\x -> f x" e2
                        inp = if rev then [p2, p1] else [p1, p2]
                        out = dedupePreparedCandidates inp
                    pure (map prSource out == ["f"])
    ]

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

withTwoRenamedExpr
    :: [String]
    -> String
    -> String
    -> (DynFlags -> LHsExpr GhcRn -> LHsExpr GhcRn -> IO a)
    -> IO a
withTwoRenamedExpr exts src1 src2 k =
    withRenamedExpr exts src1 $ \dflags e1 ->
        withRenamedExpr exts src2 $ \_ e2 ->
            k dflags e1 e2

--------------------------------------------------------------------------------
-- GHC harness
--------------------------------------------------------------------------------

withRenamedExpr
    :: [String]
    -> String
    -> (DynFlags -> LHsExpr GhcRn -> IO a)
    -> IO a
withRenamedExpr exts rhs k =
    withSystemTempDirectory "ollama-holes-candidate-spec" $ \dir -> do
        let fp = dir </> "Tmp.hs"
        writeFile fp (mkModuleSource exts rhs)

        (dflags, expr) <- runGhc (Just GHC.Paths.libdir) $ do
            dflags0 <- getSessionDynFlags
            _ <- setSessionDynFlags dflags0

#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
            target <- guessTarget fp Nothing Nothing
#else
            target <- guessTarget fp Nothing
#endif
            setTargets [target]

            result <- load LoadAllTargets
            case result of
                GHC.Failed ->
                    liftIO $ assertFailure "GHC failed to load temporary module"
                GHC.Succeeded ->
                    pure ()

            ms <- getModSummary (mkModuleName "Tmp")
            p  <- parseModule ms
            t  <- typecheckModule p
            dflags <- getSessionDynFlags

            case tm_renamed_source t >>= findExprBinding of
                Nothing ->
                    liftIO $ assertFailure "Could not find renamed binding for expr"
                Just e ->
                    pure (dflags, e)

        k dflags expr

mkModuleSource :: [String] -> String -> String
mkModuleSource exts rhs =
    unlines $
        map (\ext -> "{-# LANGUAGE " <> ext <> " #-}") exts
            <> [ "module Tmp where"
            , "f :: Int -> Int"
            , "f = id"
            , "g :: Int -> Int -> Int"
            , "g x y = x + y"
            , "x :: Int"
            , "x = 1"
            , "y :: Int"
            , "y = 2"
            , "expr = " <> rhs
            ]

findExprBinding :: RenamedSource -> Maybe (LHsExpr GhcRn)
findExprBinding renamed =
    let group = getRenamedGroup renamed
    in listToMaybe
        [ body
        | L _
            GHC.FunBind { GHC.fun_id = L _ nm
                        , GHC.fun_matches = mg
                        } <- listify isFunBind group
        , occNameString (occName nm) == "expr"
        , Just body <- [matchGroupBody mg]
        ]

getRenamedGroup :: RenamedSource -> GHC.HsGroup GhcRn
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
getRenamedGroup (group, _, _, _, _) = group
#else
getRenamedGroup (group, _, _, _) = group
#endif

isFunBind :: LHsBind GhcRn -> Bool
isFunBind (L _ GHC.FunBind{}) = True
isFunBind _                   = False

matchGroupBody :: MatchGroup GhcRn (LHsExpr GhcRn) -> Maybe (LHsExpr GhcRn)
matchGroupBody MG{mg_alts = L _ [L _ Match{m_grhss = GRHSs{grhssGRHSs = [L _ (GRHS _ [] body)]}}]} =
    Just body
matchGroupBody _ =
    Nothing