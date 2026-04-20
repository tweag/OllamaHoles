{-# LANGUAGE OverloadedStrings #-}
module GHC.Plugin.OllamaHoles.Candidate where

import Data.Text (Text)
import Data.Text qualified as T
import GHC (GhcPs, LHsExpr)
import GHC.Data.StringBuffer qualified as GHC (stringToStringBuffer)
import GHC.Driver.Config.Parser qualified as GHC (initParserOpts)
import GHC.Parser qualified as GHC (parseExpression)
import GHC.Parser.Lexer qualified as GHC
  (ParseResult(..), getPsErrorMessages, initParserState, unP)
import GHC.Parser.PostProcess qualified as GHC (runPV, unECP)
import GHC.Plugins hiding ((<>))
import GHC.Tc.Types (TcM(..))
import GHC.Types.SrcLoc qualified as GHC (mkRealSrcLoc)



-- Contexts
-----------

data ParseCtx = ParseCtx
    { pxDynFlags :: DynFlags
    }



-- Results
----------

data ParsedCandidate = ParsedCandidate
    { pcSource :: Text
    , pcParsed :: LHsExpr GhcPs
    , pcLog    :: CandidateLog
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



-- Errors and Logging
---------------------

data CandidateError
    = CandidateParseError Text
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
    deriving (Eq, Ord, Show)

emptyCandidateLog :: CandidateLog
emptyCandidateLog = CandidateLog []

addDecision :: StageTag -> Text -> CandidateLog -> CandidateLog
addDecision cdStage cdMessage (CandidateLog xs) =
    CandidateLog (CandidateDecision{cdStage, cdMessage} : xs)
