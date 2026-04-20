{-# LANGUAGE OverloadedStrings #-}
module GHC.Plugin.OllamaHoles.Candidate where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import GHC (GhcPs, GhcRn, LHsExpr)
import GHC.Data.StringBuffer qualified as GHC (stringToStringBuffer)
import GHC.Driver.Config.Parser qualified as GHC (initParserOpts)
import GHC.Parser qualified as GHC (parseExpression)
import GHC.Parser.Lexer qualified as GHC
  (ParseResult(..), getPsErrorMessages, initParserState, unP)
import GHC.Parser.PostProcess qualified as GHC (runPV, unECP)
import GHC.Plugins hiding ((<>))
import GHC.Rename.Expr qualified as GHC (rnLExpr)
import GHC.Tc.Types (TcM(..))
import GHC.Tc.Utils.Monad (discardErrs, ifErrsM)
import GHC.Types.SrcLoc qualified as GHC (mkRealSrcLoc)



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



-- Analysis
-----------

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
                when rxDebug $ liftIO $
                    putStrLn $ T.unpack (msg <> ": " <> pcSource)
                pure (Left (CandidateRenameError msg))
        (rn_e, _) <- GHC.rnLExpr pcParsed
        ifErrsM onError $
            pure $ Right $ RenamedCandidate
                { rcSource  = pcSource
                , rcParsed  = pcParsed
                , rcRenamed = rn_e
                , rcLog     = addDecision StageRename "renamed successfully" pcLog
                }



-- Errors and Logging
---------------------

data CandidateError
    = CandidateParseError Text
    | CandidateRenameError Text
    deriving (Eq, Show)



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
    deriving (Eq, Ord, Show)

emptyCandidateLog :: CandidateLog
emptyCandidateLog = CandidateLog []

addDecision :: StageTag -> Text -> CandidateLog -> CandidateLog
addDecision cdStage cdMessage (CandidateLog xs) =
    CandidateLog (CandidateDecision{cdStage, cdMessage} : xs)
