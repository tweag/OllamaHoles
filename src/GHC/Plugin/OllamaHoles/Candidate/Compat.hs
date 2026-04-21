{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Candidate.Compat
    ( viewExpr
    , viewTopSimpleLam
    , ExprView(..)
    , showExprView
    ) where



import Data.Text (Text)
import Data.Text qualified as T
import GHC (GhcRn, LHsExpr, HsExpr(..), Pat(..), GRHSs(..), GRHS(..), Match(..))
import GHC qualified as GHC
import GHC.Plugins hiding ((<>))



-- | @ExprView@ is a local approximation of haskell syntax.
-- We use it to define what "heuristically equivalent" means,
-- and also to quarantine version specific changes to GHC's
-- internal representation.
data ExprView
    = VVar Name
    | VUnbound Text
    | VLit Text
    | VApp (LHsExpr GhcRn) (LHsExpr GhcRn)
    | VOpApp (LHsExpr GhcRn) (LHsExpr GhcRn) (LHsExpr GhcRn)
    | VLam [Name] (LHsExpr GhcRn)
    | VSectionL (LHsExpr GhcRn) (LHsExpr GhcRn)
    | VSectionR (LHsExpr GhcRn) (LHsExpr GhcRn)
    | VNeg (LHsExpr GhcRn)
    | VWrapper (LHsExpr GhcRn)
    | VUnknown Text

showExprView :: ExprView -> String
showExprView = \case
    VVar nm       -> "VVar " <> occNameString (occName nm)
    VUnbound t    -> "VUnbound " <> T.unpack t
    VLit t        -> "VLit " <> T.unpack t
    VApp _ _      -> "VApp"
    VOpApp _ _ _  -> "VOpApp"
    VLam ns _     -> "VLam " <> show (map (occNameString . occName) ns)
    VSectionL _ _ -> "VSectionL"
    VSectionR _ _ -> "VSectionR"
    VNeg _        -> "VNeg"
    VWrapper _    -> "VWrapper"
    VUnknown t    -> "VUnknown " <> T.unpack t



viewExpr :: DynFlags -> LHsExpr GhcRn -> ExprView
viewExpr dflags e@(L _ e0) = case e0 of
    HsVar _ (L _ nm) ->
        VVar nm

    HsUnboundVar _ uv ->
        VUnbound (T.pack (showSDoc dflags (ppr uv)))

    HsOverLit _ ol ->
        VLit (T.pack (showSDoc dflags (ppr ol)))

    HsLit _ lit ->
        VLit (T.pack (showSDoc dflags (ppr lit)))

    HsApp _ f x ->
        VApp f x

    OpApp _ x op y ->
        VOpApp x op y

    NegApp _ x _ ->
        VNeg x

#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
    HsLam _ _ mg ->
      case viewSimpleMatchGroup mg of
        Just (ns, body) -> VLam ns body
        Nothing         -> VUnknown (T.pack (showSDoc dflags (ppr e0)))
#else
    HsLam _ mg ->
      case viewSimpleMatchGroup mg of
        Just (ns, body) -> VLam ns body
        Nothing         -> VUnknown (T.pack (showSDoc dflags (ppr e0)))
#endif

    SectionL _ x op ->
        VSectionL x op

    SectionR _ op y ->
        VSectionR op y

    HsPar _ x ->
        VWrapper x

    ExprWithTySig _ x _ ->
        VWrapper x

    HsPragE _ _ x ->
        VWrapper x

#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
    HsAppType _ x _ ->
        VWrapper x
#endif

    _ ->
        VUnknown (T.pack (showSDoc dflags (ppr e0)))

viewTopSimpleLam :: DynFlags -> LHsExpr GhcRn -> Maybe ([Name], LHsExpr GhcRn)
viewTopSimpleLam dflags e = case viewExpr dflags e of
    VWrapper x  -> viewTopSimpleLam dflags x
    VLam ns bod -> Just (ns, bod)
    _           -> Nothing

viewSimpleMatchGroup
    :: GHC.MatchGroup GhcRn (LHsExpr GhcRn)
    -> Maybe ([Name], LHsExpr GhcRn)
viewSimpleMatchGroup GHC.MG{GHC.mg_alts = L _ [L _ Match{m_pats, m_grhss}]} = do
    ns <- traverse viewVarPatName (unLoc m_pats)
    case m_grhss of
        GRHSs { grhssGRHSs = [L _ (GRHS _ [] body)] } -> Just (ns, body)
        _                                             -> Nothing
viewSimpleMatchGroup _ = Nothing

viewVarPatName :: GHC.LPat GhcRn -> Maybe Name
viewVarPatName (L _ pat) = case pat of
    VarPat _ (L _ nm) -> Just nm
    ParPat _ p        -> viewVarPatName p
    _                 -> Nothing
