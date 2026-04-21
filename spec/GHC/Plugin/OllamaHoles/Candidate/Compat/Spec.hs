{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Plugin.OllamaHoles.Candidate.Compat.Spec (tests) where

import Control.Monad (when)
import Data.Generics (listify)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import GHC.Paths qualified as GHC.Paths
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

import GHC.Plugin.OllamaHoles.Candidate.Compat

tests :: TestTree
tests = testGroup "Candidate.Compat"
    [ unitTests
    , propertyTests
    ]

unitTests :: TestTree
unitTests = testGroup "unit"
    [ testCase "viewExpr sees variable" $
        withRenamedExpr [] "x" $ \dflags e ->
            case viewExpr dflags e of
                VVar nm ->
                    occNameString (occName nm) @?= "x"
                other ->
                    assertFailure $ "expected VVar, got: " <> showExprView other

    , testCase "viewExpr sees literal" $
        withRenamedExpr [] "1" $ \dflags e ->
            case viewExpr dflags e of
                VLit _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VLit, got: " <> showExprView other

    , testCase "viewExpr sees application" $
        withRenamedExpr [] "f x" $ \dflags e ->
            case viewExpr dflags e of
                VApp _ _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VApp, got: " <> showExprView other

    , testCase "viewExpr sees operator application" $
        withRenamedExpr [] "x + y" $ \dflags e ->
            case viewExpr dflags e of
                VOpApp _ _ _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VOpApp, got: " <> showExprView other

    , testCase "viewExpr sees negation" $
        withRenamedExpr [] "-x" $ \dflags e ->
            case viewExpr dflags e of
                VNeg _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VNeg, got: " <> showExprView other

    , testCase "viewExpr treats parens as wrapper" $
        withRenamedExpr [] "(x)" $ \dflags e ->
            case viewExpr dflags e of
                VWrapper _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VWrapper from parens, got: " <> showExprView other

    , testCase "viewExpr treats type signature as wrapper" $
        withRenamedExpr [] "(id :: Int -> Int)" $ \dflags e ->
            case viewExpr dflags e of
                VWrapper _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VWrapper from type signature, got: " <> showExprView other

    , testCase "viewExpr treats expression pragma as wrapper" $
        withRenamedExpr [] "({-# SCC \"k\" #-} f x)" $ \dflags e ->
            case viewExpr dflags e of
                VWrapper _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VWrapper from pragma, got: " <> showExprView other

#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
    , testCase "viewExpr treats visible type application as wrapper" $
        withRenamedExpr ["TypeApplications"] "id @Int" $ \dflags e ->
            case viewExpr dflags e of
                VWrapper _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VWrapper from visible type application, got: " <> showExprView other
#endif

    , testCase "viewExpr sees simple lambda" $
        withRenamedExpr [] "\\x -> x" $ \dflags e ->
            case viewExpr dflags e of
                VLam [nm] _ ->
                    occNameString (occName nm) @?= "x"
                other ->
                    assertFailure $ "expected VLam, got: " <> showExprView other

    , testCase "viewTopSimpleLam sees simple lambda" $
        withRenamedExpr [] "\\x -> x" $ \dflags e ->
            case viewTopSimpleLam dflags e of
                Just ([nm], _) ->
                    occNameString (occName nm) @?= "x"
                other ->
                    assertFailure $ "expected Just ([x], body), got: " <> showTopSimpleLamResult other

    , testCase "viewTopSimpleLam unwraps parens around lambda" $
        withRenamedExpr [] "((\\x -> x))" $ \dflags e ->
            case viewTopSimpleLam dflags e of
                Just ([nm], _) ->
                    occNameString (occName nm) @?= "x"
                other ->
                    assertFailure $ "expected wrapped lambda to unwrap, got: " <> showTopSimpleLamResult other

    , testCase "non-simple lambda is not treated as VLam" $
        withRenamedExpr [] "\\(a,b) -> a" $ \dflags e ->
            case viewExpr dflags e of
                VUnknown _ ->
                    pure ()
                other ->
                    assertFailure $ "expected VUnknown for non-simple lambda, got: " <> showExprView other

    , testCase "non-simple lambda is not a top simple lambda" $
        withRenamedExpr [] "\\(a,b) -> a" $ \dflags e ->
            case viewTopSimpleLam dflags e of
                Nothing ->
                    pure ()
                Just (nms, _) ->
                    assertFailure $
                    "expected Nothing, got Just "
                        <> show (map (occNameString . occName) nms)
                        <> " <body>"
    ]

propertyTests :: TestTree
propertyTests = testGroup "properties"
    [ localOption (QC.QuickCheckTests 12) $
        QC.testProperty "viewTopSimpleLam unwraps arbitrary parenthesized simple lambdas" $
        QC.forAll (QC.chooseInt (0, 6)) $ \n ->
            QC.ioProperty $ do
                let src = replicate n '(' ++ "\\x -> x" ++ replicate n ')'
                withRenamedExpr [] src $ \dflags e ->
                    pure $ case viewTopSimpleLam dflags e of
                        Just ([nm], _) -> occNameString (occName nm) == "x"
                        _              -> False
    ]

--------------------------------------------------------------------------------
-- GHC harness
--------------------------------------------------------------------------------

withRenamedExpr
    :: [String]
    -> String
    -> (DynFlags -> LHsExpr GhcRn -> IO a)
    -> IO a
withRenamedExpr exts rhs k =
    withSystemTempDirectory "ollama-holes-compat-spec" $ \dir -> do
        let fp = dir </> "Tmp.hs"
        writeFile fp (mkModuleSource exts rhs)

        (dflags, expr) <- runGhc (Just GHC.Paths.libdir) $ do
            dflags0 <- getSessionDynFlags
            _ <- setSessionDynFlags dflags0

            target <- guessTarget fp Nothing Nothing
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
            , "x :: Int"
            , "x = 1"
            , "y :: Int"
            , "y = 2"
            , "expr = " <> rhs
            ]

findExprBinding :: RenamedSource -> Maybe (LHsExpr GhcRn)
findExprBinding (group, _, _, _, _) =
    listToMaybe
        [ body
        | L _ GHC.FunBind{fun_id = L _ nm, fun_matches = mg} <- listify isFunBind group
        , occNameString (occName nm) == "expr"
        , Just body <- [matchGroupBody mg]
        ]

isFunBind :: LHsBind GhcRn -> Bool
isFunBind (L _ GHC.FunBind{}) = True
isFunBind _                   = False

matchGroupBody :: MatchGroup GhcRn (LHsExpr GhcRn) -> Maybe (LHsExpr GhcRn)
matchGroupBody MG{mg_alts = L _ [L _ Match{m_grhss = GRHSs{grhssGRHSs = [L _ (GRHS _ [] body)]}}]} =
    Just body
matchGroupBody _ =
    Nothing

showTopSimpleLamResult :: Maybe ([Name], LHsExpr GhcRn) -> String
showTopSimpleLamResult = \case
    Nothing ->
        "Nothing"
    Just (nms, _) ->
        "Just " <> show (map (occNameString . occName) nms) <> " <body>"