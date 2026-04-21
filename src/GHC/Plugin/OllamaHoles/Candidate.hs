{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Plugin.OllamaHoles.Candidate where

import Control.Monad (when)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import GHC (GhcPs, GhcRn, LHsExpr)
import GHC.Data.StringBuffer qualified as GHC (stringToStringBuffer)
import GHC.Driver.Config.Parser qualified as GHC (initParserOpts)
import GHC.Parser qualified as GHC (parseExpression)
import GHC.Parser.Lexer qualified as GHC
    (ParseResult(..), getPsErrorMessages, initParserState, unP)
import GHC.Parser.PostProcess qualified as GHC (runPV, unECP)
import GHC.Plugins hiding ((<>), NameEnv)
import GHC.Rename.Expr qualified as GHC (rnLExpr)
import GHC.Tc.Gen.App qualified as GHC (tcInferSigma)
import GHC.Tc.Errors.Hole qualified as GHC (tcCheckHoleFit, withoutUnification)
import qualified GHC.Tc.Solver as GHC
    (simplifyTop, simplifyInfer, captureTopConstraints, InferMode(..))
import qualified GHC.Tc.Solver.Monad as GHC (zonkTcType, runTcSEarlyAbort)
import GHC.Tc.Types (TcM(..))
import GHC.Tc.Types.Constraint (Hole(..))
import qualified GHC.Tc.Utils.Monad as GHC
import GHC.Tc.Utils.Monad (discardErrs, ifErrsM)
import GHC.Tc.Utils.TcType (TcSigmaType)
import qualified GHC.Tc.Utils.TcType as GHC (tyCoFVsOfType, mkPhiTy)
import GHC.Types.SrcLoc qualified as GHC (mkRealSrcLoc)

import GHC.Plugin.OllamaHoles.Candidate.Compat
import GHC.Plugin.OllamaHoles.Candidate.Rewrite



-- Contexts
-----------

data ParseCtx = ParseCtx
    { pxDynFlags :: DynFlags
    }

mkParseCtx :: DynFlags -> ParseCtx
mkParseCtx pxDynFlags = ParseCtx{pxDynFlags}

data RenameCtx = RenameCtx
    { rxDebug :: Bool
    }

mkRenameCtx :: Bool -> RenameCtx
mkRenameCtx rxDebug = RenameCtx{rxDebug}

data CheckCtx = CheckCtx
    { cxDebug :: Bool
    , cxHole  :: TypedHole
    }

mkCheckCtx :: Bool -> TypedHole -> CheckCtx
mkCheckCtx cxDebug cxHole = CheckCtx{cxDebug, cxHole}

data PrepCtx = PrepCtx
    { prxDynFlags  :: DynFlags
    , prxHoleArity :: Int
    }

mkPrepCtx :: DynFlags -> Int -> PrepCtx
mkPrepCtx prxDynFlags prxHoleArity = PrepCtx{prxDynFlags, prxHoleArity}



-- Results
----------

data ParsedCandidate = ParsedCandidate
    { pcSource :: Text
    , pcParsed :: LHsExpr GhcPs
    , pcLog    :: CandidateLog
    }

data RenamedCandidate = RenamedCandidate
    { rcSource  :: Text
    , rcParsed  :: LHsExpr GhcPs
    , rcRenamed :: LHsExpr GhcRn
    , rcLog     :: CandidateLog
    }

data CheckedCandidate = CheckedCandidate
    { ccSource   :: Text
    , ccRenamed  :: LHsExpr GhcRn
    , ccExprType :: TcSigmaType
    , ccLog      :: CandidateLog
    }

data PreparedCandidate = PreparedCandidate
    { prSource  :: Text
    , prRenamed :: LHsExpr GhcRn
    , prNormKey :: NormExpr
    , prRank    :: CandidateRank
    , prLog     :: CandidateLog
    }

mkPrepared :: DynFlags -> Int -> Text -> LHsExpr GhcRn -> PreparedCandidate
mkPrepared dflags arity src expr = PreparedCandidate
    { prSource  = src
    , prRenamed = expr
    , prNormKey = normalizeForHoleArity dflags arity expr
    , prRank    = rankPreparedCandidate dflags src expr
    , prLog     = emptyCandidateLog
    }



-- Analysis
-----------

-- We're going to compute a conservative normal form for haskell expressions which
-- is coarse enough to detect some "heuristically equivalent" pairs.

parseCandidate :: ParseCtx -> Text -> TcM (Either CandidateError ParsedCandidate)
parseCandidate ParseCtx{pxDynFlags} src = do
    let parsed = GHC.unP (GHC.parseExpression >>= \p -> GHC.runPV $ GHC.unECP p) $
            GHC.initParserState
                (GHC.initParserOpts pxDynFlags)
                (GHC.stringToStringBuffer (T.unpack src))
                (GHC.mkRealSrcLoc (mkFastString "<hole-fit-validation>") 1 1)

    pure $ case parsed of
        GHC.PFailed st -> Left $ CandidateParseError $
            T.pack (showPprUnsafe (GHC.getPsErrorMessages st))

        GHC.POk _ p_e -> Right $ ParsedCandidate
            { pcSource = src
            , pcParsed = p_e
            , pcLog    = addDecision StageParse "parsed successfully" emptyCandidateLog
            }

renameCandidate :: RenameCtx -> ParsedCandidate -> TcM (Either CandidateError RenamedCandidate)
renameCandidate RenameCtx{rxDebug} ParsedCandidate{pcSource, pcParsed, pcLog} =
    discardErrs $ do
        let onError = do
                let msg = "rename failed" :: Text
                pure (Left (CandidateRenameError msg))
        (rn_e, _) <- GHC.rnLExpr pcParsed
        ifErrsM onError $
            pure $ Right $ RenamedCandidate
                { rcSource  = pcSource
                , rcParsed  = pcParsed
                , rcRenamed = rn_e
                , rcLog     = addDecision StageRename "renamed successfully" pcLog
                }

checkCandidateFit :: CheckCtx -> RenamedCandidate -> TcM (Either CandidateError CheckedCandidate)
checkCandidateFit CheckCtx{cxDebug, cxHole} RenamedCandidate{rcSource, rcRenamed, rcLog}
    | Just h <- th_hole cxHole =
        discardErrs $ do
            ((doesFit, _), zonkedTy) <-
                GHC.withoutUnification (GHC.tyCoFVsOfType (hole_ty h)) $ do
                    ((tcLvl, exprTy), wanteds) <-
                        GHC.captureTopConstraints $
                            GHC.pushTcLevelM $
                            GHC.tcInferSigma False rcRenamed

                    fresh <- GHC.newName (mkVarOcc "hf-fit")

                    ((qtvs, dicts, _, _), residual) <-
                        GHC.captureConstraints $
                            GHC.simplifyInfer tcLvl GHC.NoRestrictions [] [(fresh, exprTy)] wanteds

                    let rTy = mkInfForAllTys qtvs $ GHC.mkPhiTy (map idType dicts) exprTy
                    _ <- GHC.simplifyTop residual
                    zonked <- GHC.runTcSEarlyAbort $ GHC.zonkTcType rTy
                    ok <- GHC.tcCheckHoleFit cxHole (hole_ty h) zonked
                    pure (ok, zonked)

            let onError = do
                    let msg = "type inference or hole-fit check failed"
                    when cxDebug $ liftIO $
                        putStrLn (T.unpack msg <> ": " <> T.unpack rcSource)
                    pure (Left (CandidateTypeError msg))

            ifErrsM onError $
                pure $ if doesFit
                    then Right $ CheckedCandidate
                        { ccSource   = rcSource
                        , ccRenamed  = rcRenamed
                        , ccExprType = zonkedTy
                        , ccLog      = addDecision StageCheck "validated against hole type" rcLog
                        }
                    else Left (CandidateRejected "candidate did not fit hole type")
    | otherwise =
        pure (Left (CandidateRejected "hole information unavailable"))



-- Normalize and Rank
---------------------

-- We normalize expressions to group them by heuristic equivalence, and
-- then rank them to decide which should be the canonical representative.

data CandidateRank = CandidateRank
    { crTopLamCount :: Int
    , crNoiseCount  :: Int
    , crNodeCount   :: Int
    , crSourceLen   :: Int
    } deriving (Eq, Ord, Show)

prepareCandidate :: PrepCtx -> CheckedCandidate -> PreparedCandidate
prepareCandidate PrepCtx{prxDynFlags, prxHoleArity} CheckedCandidate{ccSource, ccRenamed, ccLog} =
  let rawKey             = normalizeForHoleArity prxDynFlags prxHoleArity ccRenamed
      canonicalKey       = canonicalizeNormExpr rawKey
      (prNormKey, trace) = normalizeNormExpr canonicalKey
      prRank             = rankPreparedCandidate prxDynFlags ccSource ccRenamed
      prLog              =
        addDecision StagePrepare
          ("rewrite steps: " <> T.pack (show (length trace))) $
            addDecision StagePrepare
              ("normalized and ranked: " <> T.pack (show prRank)) ccLog
  in PreparedCandidate
       { prSource  = ccSource
       , prRenamed = ccRenamed
       , prNormKey
       , prRank
       , prLog
       }

type NameEnv = M.Map Name Int

normalizeForHoleArity :: DynFlags -> Int -> LHsExpr GhcRn -> NormExpr
normalizeForHoleArity dflags holeArity expr =
    let (outerBinders, body) = collectTopSimpleLams dflags expr
        explicitArity        = length outerBinders
        missingArity         = max 0 (holeArity - explicitArity)

        env0 = M.map (+ missingArity) $ bindMany outerBinders M.empty

        bodyKey  = normExpr dflags env0 body
        etaArgs  = [ NBound i | i <- reverse [0 .. missingArity - 1] ]
        bodyKey' = applyMany bodyKey etaArgs
    in NLam (explicitArity + missingArity) bodyKey'

bindMany :: [Name] -> NameEnv -> NameEnv
bindMany ns env =
    let n    = length ns
        env' = M.map (+ n) env
        newBindings =
            M.fromList
            [ (nm, n - 1 - i)
            | (i, nm) <- zip [0 :: Int ..] ns
            ]
    in newBindings <> env'

applyMany :: NormExpr -> [NormExpr] -> NormExpr
applyMany f [] = f
applyMany (NApp g xs) ys = NApp g (xs <> ys)
applyMany f ys = NApp f ys

normExpr :: DynFlags -> NameEnv -> LHsExpr GhcRn -> NormExpr
normExpr dflags env expr = case viewExpr dflags expr of
    VVar nm ->
        maybe (NFree (renderName dflags nm)) NBound (M.lookup nm env)

    VUnbound txt ->
        NOther txt

    VLit txt ->
        NLit txt

    VApp f x ->
        case normExpr dflags env f of
            NApp g xs -> NApp g (xs <> [normExpr dflags env x])
            g         -> NApp g [normExpr dflags env x]

    VOpApp x op y ->
        NApp (normExpr dflags env op) [normExpr dflags env x, normExpr dflags env y]

    VLam ns body ->
        NLam (length ns) (normExpr dflags (bindMany ns env) body)

    VSectionL x op ->
        NApp (normExpr dflags env op) [normExpr dflags env x]

    VSectionR op y ->
        NApp (normExpr dflags env op) [normExpr dflags env y]

    VNeg x ->
        NApp (NFree "negate") [normExpr dflags env x]

    VWrapper x ->
        normExpr dflags env x

    VUnknown txt ->
        NOther txt

renderName :: DynFlags -> Name -> Text
renderName dflags = T.pack . showSDoc dflags . ppr

collectTopSimpleLams :: DynFlags -> LHsExpr GhcRn -> ([Name], LHsExpr GhcRn)
collectTopSimpleLams dflags e = case viewTopSimpleLam dflags e of
    Just (ns, body) ->
        let (ns', body') = collectTopSimpleLams dflags body
        in (ns <> ns', body')
    Nothing ->
        ([], e)

rankPreparedCandidate :: DynFlags -> Text -> LHsExpr GhcRn -> CandidateRank
rankPreparedCandidate dflags src expr = CandidateRank
    { crTopLamCount = length (fst (collectTopSimpleLams dflags expr))
    , crNoiseCount  = wrapperCount dflags expr
    , crNodeCount   = exprSize dflags expr
    , crSourceLen   = T.length src
    }

wrapperCount :: DynFlags -> LHsExpr GhcRn -> Int
wrapperCount dflags = go
    where
        go expr = case viewExpr dflags expr of
            VWrapper e      -> 1 + go e
            VApp f x        -> go f + go x
            VOpApp x op y   -> go x + go op + go y
            VLam _ body     -> go body
            VSectionL x op  -> go x + go op
            VSectionR op y  -> go op + go y
            VNeg x          -> go x
            _               -> 0

exprSize :: DynFlags -> LHsExpr GhcRn -> Int
exprSize dflags = go
    where
        go expr = 1 + case viewExpr dflags expr of
            VApp f x        -> go f + go x
            VOpApp x op y   -> go x + go op + go y
            VLam _ body     -> go body
            VSectionL x op  -> go x + go op
            VSectionR op y  -> go op + go y
            VNeg x          -> go x
            VWrapper e      -> go e
            _               -> 0

keepBestByKey :: Ord k => (a -> k) -> (a -> CandidateRank) -> [a] -> [a]
keepBestByKey keyOf rankOf =
  M.elems . foldl step M.empty
  where
    step acc x =
      M.insertWith chooseBetter (keyOf x) x acc

    chooseBetter new old =
      if rankOf new <= rankOf old then new else old

dedupePreparedCandidates :: [PreparedCandidate] -> [PreparedCandidate]
dedupePreparedCandidates =
  keepBestByKey prNormKey prRank



-- | Parse, alpha normalize, type check, and approximate a
-- haskell expression.
pipelineCandidate
  :: ParseCtx -> RenameCtx -> CheckCtx -> PrepCtx -> Text
  -> TcM (Either CandidateError PreparedCandidate)
pipelineCandidate parseCtx renameCtx checkCtx prepCtx src = do
    parsedE <- parseCandidate parseCtx src
    case parsedE of
        Left err ->
            pure (Left err)
        Right parsed -> do
            renamedE <- renameCandidate renameCtx parsed
            case renamedE of
                Left err ->
                    pure (Left err)
                Right renamed -> do
                    checkedE <- checkCandidateFit checkCtx renamed
                    pure $ fmap (prepareCandidate prepCtx) checkedE



-- Errors and Logging
---------------------

data CandidateError
    = CandidateParseError Text
    | CandidateRenameError Text
    | CandidateTypeError Text
    | CandidateRejected Text
    deriving (Eq, Show)

renderCandidateError :: Text -> CandidateError -> String
renderCandidateError src = \case
    CandidateParseError msg  -> "parse failed: " <> T.unpack src <> "\n  " <> T.unpack msg
    CandidateRenameError msg -> "rename failed: " <> T.unpack src <> "\n  " <> T.unpack msg
    CandidateTypeError msg   -> "type inference or hole-fit check failed: " <> T.unpack src <> "\n  " <> T.unpack msg
    CandidateRejected msg    -> "rejected: " <> T.unpack src <> "\n  " <> T.unpack msg



-- | @CandidateLog@ presents a small API for
-- tracking why decisions are made.
newtype CandidateLog = CandidateLog
    { unCandidateLog :: [CandidateDecision]
    } deriving (Eq, Show)

data CandidateDecision = CandidateDecision
    { cdStage   :: StageTag
    , cdMessage :: Text
    } deriving (Eq, Show)

data StageTag
    = StageParse
    | StageRename
    | StageCheck
    | StagePrepare
    | StageDedupe
    deriving (Eq, Ord, Show)

emptyCandidateLog :: CandidateLog
emptyCandidateLog = CandidateLog []

addDecision :: StageTag -> Text -> CandidateLog -> CandidateLog
addDecision cdStage cdMessage (CandidateLog xs) =
    CandidateLog (CandidateDecision{cdStage, cdMessage} : xs)
