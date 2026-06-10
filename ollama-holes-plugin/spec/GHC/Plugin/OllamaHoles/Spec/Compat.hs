{-# LANGUAGE CPP #-}

module GHC.Plugin.OllamaHoles.Spec.Compat
  ( getTmpModSummary
  , singleGRHS
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import GHC
import GHC.Unit.Module
  ( mkModule
  , mkModuleName
  )
import GHC.Unit.Types
  ( mainUnit
  )

#if MIN_VERSION_ghc(9,14,0)
getTmpModSummary :: Ghc ModSummary
getTmpModSummary =
  getModSummary (mkModule mainUnit (mkModuleName "Tmp"))
#else
getTmpModSummary :: Ghc ModSummary
getTmpModSummary =
  getModSummary (mkModuleName "Tmp")
#endif

singleGRHS
  :: GRHSs GhcRn (LHsExpr GhcRn)
  -> Maybe (LGRHS GhcRn (LHsExpr GhcRn))
singleGRHS grhss =
#if MIN_VERSION_ghc(9,14,0)
  case grhssGRHSs grhss of
    x :| [] -> Just x
    _       -> Nothing
#else
  case grhssGRHSs grhss of
    [x] -> Just x
    _   -> Nothing
#endif
