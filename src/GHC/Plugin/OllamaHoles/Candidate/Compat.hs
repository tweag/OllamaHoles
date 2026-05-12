{-# LANGUAGE CPP #-}

module GHC.Plugin.OllamaHoles.Candidate.Compat
    ( viewExpr
    , viewTopSimpleLam
    , ExprView(..)
    , showExprView
    , getRenamedGroup
    ) where



import Data.Text (Text)
import Data.Text qualified as T
import GHC (GhcRn, LHsExpr, HsExpr(..), Pat(..), GRHSs(..), GRHS(..), Match(..))
import GHC qualified as GHC
import GHC.Plugins hiding ((<>))

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
import qualified Data.List.NonEmpty as NE
#endif



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

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    HsVar _ occ ->
        VVar (unLocWithUserRdr occ)
#else
    HsVar _ (L _ nm) ->
        VVar nm
#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    HsUnboundVar _ uv ->
        VUnbound (T.pack (showSDoc dflags (ppr uv)))
#endif

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

#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
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

#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
    HsPar _ x ->
        VWrapper x
#else
    HsPar _ _ x _ ->
        VWrapper x
#endif

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
viewSimpleMatchGroup GHC.MG{GHC.mg_alts = L _ [L _ match@Match{m_grhss}]} = do
  ns <- traverse viewVarPatName (viewMatchPats match)
  case m_grhss of
    GRHSs { grhssGRHSs = grhssGRHSs0 } ->

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
      case NE.toList grhssGRHSs0 of
        [L _ (GRHS _ [] body)] -> Just (ns, body)
        _ -> Nothing
#else
      case grhssGRHSs0 of
        [L _ (GRHS _ [] body)] -> Just (ns, body)
        _ -> Nothing
#endif
viewSimpleMatchGroup _ = Nothing

viewMatchPats :: Match GhcRn (LHsExpr GhcRn) -> [GHC.LPat GhcRn]
viewMatchPats Match{m_pats} =
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
  unLoc m_pats
#else
  m_pats
#endif

viewVarPatName :: GHC.LPat GhcRn -> Maybe Name
viewVarPatName (L _ pat) = case pat of
    VarPat _ (L _ nm) -> Just nm
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
    ParPat _ p        -> viewVarPatName p
#else
    ParPat _ _ p _    -> viewVarPatName p
#endif
    _                 -> Nothing



getRenamedGroup :: GHC.RenamedSource -> GHC.HsGroup GhcRn
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
getRenamedGroup (group, _, _, _, _) = group
#else
getRenamedGroup (group, _, _, _) = group
#endif
