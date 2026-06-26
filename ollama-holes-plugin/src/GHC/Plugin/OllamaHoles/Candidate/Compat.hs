{-# LANGUAGE CPP #-}

module GHC.Plugin.OllamaHoles.Candidate.Compat
    ( viewExpr
    , viewTopSimpleLam
    , ExprView(..)
    , showExprView
    , getRenamedGroup
    , tcInferCandidateExpr
    , simplifyCandidateInfer
    ) where



import Data.Text (Text)
import Data.Text qualified as T
import GHC (GhcRn, LHsExpr, HsExpr(..), Pat(..), GRHSs(..), GRHS(..), Match(..))
import GHC qualified as GHC
import GHC.Plugins hiding ((<>))

import GHC.Tc.Types (TcM)
import GHC.Tc.Types.Origin (CtOrigin(..), UserTypeCtxt(..))
import GHC.Tc.Utils.Monad qualified as GHC
import GHC.Tc.Utils.Unify qualified as GHC

import GHC.Tc.Gen.Expr qualified as GHC
import GHC.Tc.Utils.TcType qualified as GHC

#if MIN_VERSION_ghc(9,14,0)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Tc.Types.Evidence qualified as GHC
import GHC.Types.Var qualified as GHC
import GHC.Tc.Types.Constraint qualified as GHC
import GHC.Tc.Solver qualified as GHC
#endif

#if MIN_VERSION_ghc(9,14,0)
import GHC.Tc.Gen.Expr qualified as GHC (tcInferSigma)
#else
import GHC.Tc.Gen.App qualified as GHC (tcInferSigma)
import GHC.Tc.Solver qualified as GHC (simplifyInfer, InferMode(..))
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
viewExpr dflags (L _ e0) = case e0 of
    HsVar _ (L _ nm) ->
        VVar (getVarName nm)

#if !MIN_VERSION_ghc(9,14,0)
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
viewSimpleMatchGroup GHC.MG{GHC.mg_alts
  = L _ [L _ match@Match{m_grhss}]} = do
    ns <- traverse viewVarPatName (viewMatchPats match)
    case singleGRHS m_grhss of
      Just (L _ (GRHS _ [] body)) -> Just (ns, body)
      _                           -> Nothing
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

#if MIN_VERSION_ghc(9,14,0)
getVarName :: WithUserRdr Name -> Name
getVarName = \case
  WithUserRdr _ nm -> nm
#else
getVarName :: Name -> Name
getVarName = id
#endif

singleGRHS
  :: GRHSs GhcRn (LHsExpr GhcRn)
  -> Maybe (GHC.LGRHS GhcRn (LHsExpr GhcRn))
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

getRenamedGroup :: GHC.RenamedSource -> GHC.HsGroup GhcRn
#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
getRenamedGroup (group, _, _, _, _) = group
#else
getRenamedGroup (group, _, _, _) = group
#endif

#if MIN_VERSION_ghc(9,14,0)
tcInferCandidateExpr :: LHsExpr GhcRn -> TcM GHC.TcSigmaType
tcInferCandidateExpr expr = do
  (_typedExpr, ty) <- GHC.tcInferSigma expr
  pure ty
#else
tcInferCandidateExpr :: LHsExpr GhcRn -> TcM GHC.TcSigmaType
tcInferCandidateExpr =
  GHC.tcInferSigma False
#endif

#if MIN_VERSION_ghc(9,14,0)
simplifyCandidateInfer
  :: GHC.TcLevel
  -> GHC.InferMode
  -> [GHC.TcIdSigInst]
  -> [(GHC.Name, GHC.TcTauType)]
  -> GHC.WantedConstraints
  -> TcM ([GHC.TcTyVar], [GHC.EvVar], GHC.TcEvBinds, Bool)
simplifyCandidateInfer tcLvl inferMode sigs name_tys wanteds =
  GHC.simplifyInfer
    NotTopLevel
    tcLvl
    inferMode
    sigs
    name_tys
    wanteds
#else
simplifyCandidateInfer tcLvl inferMode sigs name_tys wanteds =
  GHC.simplifyInfer
    tcLvl
    inferMode
    sigs
    name_tys
    wanteds
#endif
