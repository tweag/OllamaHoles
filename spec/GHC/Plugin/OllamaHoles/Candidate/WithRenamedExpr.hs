{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Plugin.OllamaHoles.Candidate.WithRenamedExpr
  ( withRenamedExpr
  , withTwoRenamedExpr
  , tmpModuleForSummary
  , matchGroupBody
  ) where

import Data.Generics (listify)
import Data.Maybe (listToMaybe)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import GHC
    ( Ghc
    , GhcRn
    , HsBindLR(..)
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

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
import GHC.Unit.Module (mkModule)
import GHC.Unit.Types (mainUnit)
import qualified Data.List.NonEmpty as NE
#endif

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
tmpModuleForSummary :: Module
tmpModuleForSummary =
  mkModule mainUnit (mkModuleName "Tmp")
#else
tmpModuleForSummary :: ModuleName
tmpModuleForSummary =
  mkModuleName "Tmp"
#endif

-- | Unit test fixture to parse, rename, and typecheck a tiny
-- temporary module containing @expr = <rhs>@, then hand the
-- renamed RHS to the callback.
withRenamedExpr
    :: [String]                                -- ^ LANGUAGE pragmas
    -> String                                  -- ^ RHS source for @expr@
    -> (DynFlags -> LHsExpr GhcRn -> IO a)
    -> IO a
withRenamedExpr exts rhs k =
    withSystemTempDirectory "ollama-holes-renamed-expr" $ \dir -> do
        let fp = dir </> "Tmp.hs"
        writeFile fp (mkModuleSource exts rhs)

        (dflags, expr) <- runGhc (Just GHC.Paths.libdir) $ do
            dflags0 <- getSessionDynFlags
            _ <- setSessionDynFlags dflags0

            target <- mkTarget fp
            setTargets [target]

            result <- load LoadAllTargets
            case result of
                GHC.Failed ->
                    liftIO $ fail "GHC failed to load temporary module"
                GHC.Succeeded ->
                    pure ()

            ms <- getModSummary tmpModuleForSummary
            p  <- parseModule ms
            t  <- typecheckModule p
            dflags <- getSessionDynFlags

            case tm_renamed_source t >>= findExprBinding of
                Nothing ->
                    liftIO $ fail "Could not find renamed binding for expr"
                Just e ->
                    pure (dflags, e)

        k dflags expr

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
-- Internal helpers
--------------------------------------------------------------------------------

mkModuleSource :: [String] -> String -> String
mkModuleSource exts rhs = unlines $
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

mkTarget :: FilePath -> Ghc GHC.Target
#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
mkTarget fp = guessTarget fp Nothing Nothing
#else
mkTarget fp = guessTarget fp Nothing
#endif

findExprBinding :: RenamedSource -> Maybe (LHsExpr GhcRn)
findExprBinding renamed =
    let group = getRenamedGroup renamed
    in listToMaybe
        [ body
        | L _
            GHC.FunBind
                { GHC.fun_id = L _ nm
                , GHC.fun_matches = mg
                } <- listify isFunBind group
        , occNameString (occName nm) == "expr"
        , Just body <- [matchGroupBody mg]
        ]

getRenamedGroup :: RenamedSource -> GHC.HsGroup GhcRn
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
getRenamedGroup (group, _, _, _, _) = group
#else
getRenamedGroup (group, _, _, _) = group
#endif

isFunBind :: LHsBind GhcRn -> Bool
isFunBind (L _ FunBind{}) = True
isFunBind _               = False

matchGroupBody :: MatchGroup GhcRn (LHsExpr GhcRn) -> Maybe (LHsExpr GhcRn)
matchGroupBody MG{mg_alts = L _ [L _ Match{m_grhss = GRHSs{grhssGRHSs}}]} =
  case oneGRHS grhssGRHSs of
    Just (L _ (GRHS _ [] body)) ->
      Just body
    _ ->
      Nothing
matchGroupBody _ =
  Nothing
oneGRHS grhss =
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
  case NE.toList grhss of
    [x] -> Just x
    _   -> Nothing
#else
  case grhss of
    [x] -> Just x
    _   -> Nothing
#endif