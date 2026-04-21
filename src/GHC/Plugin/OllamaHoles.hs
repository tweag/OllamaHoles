{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The Ollama plugin for GHC
module GHC.Plugin.OllamaHoles where

import Control.Monad (unless, when, forM_)
import Data.Aeson
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)
import Data.List qualified as L
import GHC.Plugins hiding ((<>))
import GHC.Tc.Types
import GHC.Tc.Types.Constraint (Hole (..))
import GHC.Tc.Utils.Monad (getGblEnv, newTcRef, readTcRef, updTcRef, discardErrs, ifErrsM)
import qualified GHC.Tc.Utils.Monad as GHC

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Backend.Gemini (geminiBackend)
import GHC.Plugin.OllamaHoles.Backend.Ollama (ollamaBackend)
import GHC.Plugin.OllamaHoles.Backend.OpenAI (openAICompatibleBackend)

import GHC (GhcPs, LHsExpr, GhcRn)
import GHC.Data.StringBuffer qualified as GHC (stringToStringBuffer)
import GHC.Driver.Config.Parser qualified as GHC (initParserOpts)
import GHC.Parser qualified as GHC (parseExpression)
import GHC.Parser.Lexer qualified as GHC (ParseResult (..), getPsErrorMessages, initParserState, unP)
import GHC.Parser.PostProcess qualified as GHC (runPV, unECP)
import GHC.Rename.Expr qualified as GHC (rnLExpr)
import GHC.Tc.Errors.Hole qualified as GHC (tcCheckHoleFit, withoutUnification)
import GHC.Tc.Gen.App qualified as GHC  (tcInferSigma)
import GHC.Types.SrcLoc qualified as GHC (mkRealSrcLoc)

#if __GLASGOW_HASKELL__ >= 912
import GHC.Tc.Types.CtLoc (ctLocSpan)
#else
import GHC.Tc.Types.Constraint (ctLocSpan)
#endif

import qualified GHC.HsToCore.Docs as GHC
import qualified GHC.Types.Unique.Map as GHC
import qualified GHC.Hs.Doc as GHC
import qualified GHC.Iface.Load as GHC (loadInterfaceForName)
import qualified GHC.Tc.Utils.TcType as GHC (tyCoFVsOfType, mkPhiTy)
import qualified GHC.Tc.Solver as GHC (simplifyTop, simplifyInfer, captureTopConstraints, InferMode(..))
import qualified GHC.Tc.Solver.Monad as GHC (zonkTcType, runTcSEarlyAbort)
import qualified Data.Aeson as Aeson

import           GHC.Plugin.OllamaHoles.Prompt
import qualified GHC.Plugin.OllamaHoles.Logger as Log
import           GHC.Plugin.OllamaHoles.Candidate

-- | Prompt used to prompt the LLM
promptTemplate :: Text
promptTemplate =
           "Preliminaries:"
        <> "{docs}\n\n"
        <> "--------------------------------------------------------------------\n"
        <> "You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler.\n"
        <> "You are given a hole in a Haskell program, and you need to fill it in.\n"
        <> "The hole is represented by the following JSON encoded information:\n"
        <> "{context}\n\n"
        <> "Provide one or more Haskell expressions that could fill this hole.\n"
        <> "This means coming up with an expression of the correct type that satisfies the constraints.\n"
        <> "Pay special attention to the type of the hole, specifically whether it is a function.\n"
        <> "Make sure you synthesize an expression that matches the type of the hole.\n"
        <> "Output ONLY the raw Haskell expression(s), one per line.\n"
        <> "Do not try to bind the hole variable, e.g. `_b = ...`. Produce only the expression.\n"
        <> "Do not include explanations, introductions, or any surrounding text.\n"
        <> "If you are using a function from scope, make sure to use the qualified name from the list of things in scope.\n"
        <> "Output a maximum of {numexpr} expressions.\n"

-- | Determine which backend to use
getBackend :: Flags -> Backend
getBackend Flags{backend_name = "ollama"} = ollamaBackend
getBackend Flags{backend_name = "gemini"} = geminiBackend
getBackend Flags{backend_name = "openai", ..} = openAICompatibleBackend openai_base_url openai_key_name
getBackend Flags{..} = error $ "unknown backend: " <> T.unpack backend_name

data PluginState = PluginState
  { candidates    :: [HoleFitCandidate]
  , writeLogEvent :: Log.Logger
  }

-- | Ollama plugin for GHC
plugin :: Plugin
plugin =
    defaultPlugin
        { holeFitPlugin = mkHoleFitPluginR
        }

mkHoleFitPluginR
    :: [CommandLineOption] -> Maybe HoleFitPluginR
mkHoleFitPluginR opts =
    Just $
        HoleFitPluginR
            { hfPluginInit = hfPluginInitLLM
            , hfPluginStop = \_ -> return ()
            , hfPluginRun = \ref ->
                    HoleFitPlugin
                        { candPlugin = \_ c -> updTcRef ref (\st -> st { candidates = c }) >> return c
                        , fitPlugin = fitPluginLLM opts ref
                        }
            }

hfPluginInitLLM :: TcM (TcRef PluginState)
hfPluginInitLLM = do
    logger <- liftIO Log.initLogger
    newTcRef $ PluginState
        { candidates    = []
        , writeLogEvent = logger
        }

pluginName :: Text
pluginName = "Ollama Plugin"

fitPluginLLM
    :: [CommandLineOption]
    -> TcRef PluginState
    -> TypedHole
    -> [HoleFit]
    -> GHC.IOEnv (Env TcGblEnv TcLclEnv) [HoleFit]
fitPluginLLM opts ref hole fits = do
    PluginState cands logger <- readTcRef ref
    let flags@Flags{..} = parseFlags opts
    dflags <- getDynFlags
    gbl_env <- getGblEnv
    let backend = getBackend flags
    available_models <- liftIO $ listModels backend
    liftIO $ when debug $ T.putStrLn $ "Running " <> pluginName <> " with flags:"
    liftIO $ when debug $ print flags

    case available_models of
        Nothing ->
            error $ T.unpack pluginName <> ": No models available, check your configuration"
        Just models -> do
            unless (model_name `elem` models) $
                error $ T.unpack pluginName
                        <> ": Model "
                        <> T.unpack model_name
                        <> " not found. "
                        <> ( if backend_name == "ollama"
                                then "Use `ollama pull` to download the model, or "
                                else ""
                            )
                        <> "specify another model using "
                        <> "`-fplugin-opt=GHC.Plugin.OllamaHoles:model=<model_name>`\n"
                        <> "Availble models: \n"
                        <> T.unpack (T.unlines models)
            liftIO $ when debug $ T.putStrLn $ pluginName <> ": Hole Found"
            let promptContext = getPromptContext hole fits gbl_env cands dflags
            case th_hole hole of
                Just h -> do
                    docs <- if include_docs then getDocs cands else return ""

                    let promptContext' = T.unpack $ maybe "" encodePromptContext promptContext
                    let prompt' =
                            replacePlaceholders
                                promptTemplate
                                [ ("{numexpr}", show num_expr)
                                , ("{docs}", docs)
                                , ("{context}", promptContext')
                                ]
                    liftIO $ when debug $ do T.putStrLn $ pluginName <> " Prompt:\n```\n" <> prompt' <> "\n```"
                    res <- liftIO $ generateFits backend prompt' model_name model_options
                    case res of
                        Right rsp ->
                            extractHoleFitsFromResponse dflags prompt' rsp logger hole h debug
                        Left err -> do
                            liftIO $
                                when debug $
                                    T.putStrLn $
                                        pluginName <> " failed to generate a response.\n" <> T.pack err
                            -- Return the original fits without modification
                            return fits
                Nothing -> return fits

extractHoleFitsFromResponse
    :: DynFlags -> Log.Prompt -> Log.Response -> Log.Logger
    -> TypedHole -> Hole -> Bool -> TcM [HoleFit]
extractHoleFitsFromResponse dflags prompt' rsp logger hole h debug = do
    let rawLines      = preProcess (T.lines rsp) -- raw candidate list
    let surfaceUnique = L.nub rawLines           -- remove syntactic duplicates

    liftIO $ when debug $ do
        putStrLn "--- raw candidates ---"
        mapM_ T.putStrLn rawLines
        putStrLn ""
        putStrLn "--- syntactic uniques ---"
        mapM_ T.putStrLn surfaceUnique
        putStrLn ""

    let parseCtx  = mkParseCtx dflags
    let renameCtx = mkRenameCtx debug
    let checkCtx  = mkCheckCtx debug hole
    let prepCtx   = mkPrepCtx dflags (holeArity h)

    -- prepare candidates for semantic deduplication
    preparedE <- traverse
        (pipelineCandidate parseCtx renameCtx checkCtx prepCtx)
        surfaceUnique

    -- check that we reach the same conclusion as
    -- the original fit check
    when debug $
      regressionCheck hole surfaceUnique preparedE

    let failures = [ (src, err) | (src, Left err) <- zip surfaceUnique preparedE ]
    let prepared = [ pc | Right pc <- preparedE ]
    let deduped  = dedupePreparedCandidates prepared
    let fits'    = map (RawHoleFit . text . T.unpack . prSource) deduped

    liftIO $ when debug $ do
        when (not $ null failures) $ do
            putStrLn "--- candidate validation failures ---"
            mapM_ (putStrLn . uncurry renderCandidateError) failures
            putStrLn ""
        putStrLn "--- prepared for semantic-ish deduplication ---"
        forM_ prepared $ \pc -> do
            putStrLn ("source: " <> T.unpack (prSource pc))
            putStrLn ("rank:   " <> show (prRank pc))
            putStrLn ("key:    " <> show (prNormKey pc))
            putStrLn ""
        putStrLn "--- semantic-ish uniques ---"
        mapM_ (putStrLn . T.unpack . prSource) deduped
        putStrLn ""

    liftIO $ Log.writeLogEvent logger $ Log.mkLogEvent
        prompt' rsp (length rawLines) (length deduped) (length prepared)

    return fits'

holeArity :: Hole -> Int
holeArity = length . fst . splitFunTys . hole_ty

-- | This is meant to be a temporary regression check. We changed how
-- parsing/checking is done; this check asserts that the new method
-- agrees with @verifyHoleFit@.
regressionCheck
  :: TypedHole -> [Text] -> [Either a b] -> TcM ()
regressionCheck hole surfaceUnique preparedE = do
    -- temporary regression check: old validator vs new staged pipeline
    agreement <- for surfaceUnique $ \src -> do
        oldOk <- verifyHoleFit False hole src
        let newOk = case lookup src (zip surfaceUnique preparedE) of
                Just (Right _) -> True
                _              -> False
        pure (src, oldOk, newOk)

    let disagreements =
          [ (src, oldOk, newOk)
          | (src, oldOk, newOk) <- agreement
          , oldOk /= newOk
          ]

    liftIO $ when (not $ null disagreements) $ do
        putStrLn "--- validation disagreements (old verifyHoleFit vs new pipeline) ---"
        forM_ disagreements $ \(src, oldOk, newOk) -> do
            putStrLn ("candidate: " <> T.unpack src)
            putStrLn ("old: " <> show oldOk)
            putStrLn ("new: " <> show newOk)
            putStrLn ""

-- | Parse an expression in the current context
parseInContext :: Text -> TcM (Either String (LHsExpr GhcPs))
parseInContext fit = do
    dflags <- getDynFlags
    let parsed =
            GHC.unP (GHC.parseExpression >>= \p -> GHC.runPV $ GHC.unECP p) $
                GHC.initParserState
                    (GHC.initParserOpts dflags)
                    (GHC.stringToStringBuffer (T.unpack fit))
                    (GHC.mkRealSrcLoc (mkFastString "<hole-fit-validation>") 1 1)
    case parsed of
      GHC.PFailed st -> return $ Left (showPprUnsafe $ GHC.getPsErrorMessages st)
      GHC.POk _ (p_e :: LHsExpr GhcPs) -> return $ Right p_e

-- | Check that the hole fit matches the type of the hole
verifyHoleFit :: Bool -> TypedHole -> Text -> TcM Bool
verifyHoleFit _ hole fit | Just h <- th_hole hole = discardErrs $ do
    -- Instaniate a new IORef session with the current HscEnv.
    parsed <- parseInContext fit
    case parsed of
        Left _ -> do
            return False
        Right p_e -> do
            -- If parsing was successful, we try renaming the expression
            (rn_e, _) <- GHC.rnLExpr p_e
            ifErrsM (return False) $ do
              -- Finally, we infer the type of the expression
              (does_fit, _) <-
                  GHC.withoutUnification (GHC.tyCoFVsOfType $ hole_ty h) $ do
                      -- Make sure to capture the constraints and include those when checking the fit
                      -- based on tcRnExpr, but in the TcM so that we get the right references,
                      -- without zonking and passing the constraints on to the hole.
                      ((tc_lvl, expr_ty), wanteds) <-
                         GHC.captureTopConstraints $
                          GHC.pushTcLevelM $ GHC.tcInferSigma False rn_e
                      fresh <- GHC.newName (mkVarOcc "hf-fit")
                      ((qtvs, dicts, _, _), residual) <-
                        GHC.captureConstraints $
                          GHC.simplifyInfer tc_lvl GHC.NoRestrictions
                                            []    {- No sig vars -}
                                            [(fresh, expr_ty)]
                                            wanteds
                      let r_ty = mkInfForAllTys qtvs $ GHC.mkPhiTy (map idType dicts) expr_ty
                      _ <- GHC.simplifyTop residual
                      zonked <- GHC.runTcSEarlyAbort $ GHC.zonkTcType r_ty
                      GHC.tcCheckHoleFit hole (hole_ty h) zonked
              ifErrsM (return False) (return does_fit)
verifyHoleFit _ _ _ = return False

-- | Preprocess the response to remove empty lines, lines with only spaces, and code blocks
preProcess :: [Text] -> [Text]
preProcess [] = []
-- \| Remove lines between <think> and </think> tags from e.g. deepseek
preProcess (ln : lns)
    | T.isPrefixOf "<think>" ln =
        preProcess (drop 1 $ dropWhile (not . T.isPrefixOf "</think>") lns)
preProcess (ln : lns) | should_drop = preProcess lns
  where
    should_drop :: Bool
    should_drop =
        T.null ln
            || T.all isSpace ln
            || T.isPrefixOf "```" ln
preProcess (ln : lns) = transform ln : preProcess lns
  where
    transform :: Text -> Text
    transform = T.strip

-- | Command line options for the plugin
data Flags = Flags
    { model_name :: Text
    , backend_name :: Text
    , num_expr :: Int
    , debug :: Bool
    , include_docs :: Bool
    , openai_base_url :: Text
    , openai_key_name :: Text
    , model_options :: Maybe Value
    } deriving (Show)

-- | Default flags for the plugin
defaultFlags :: Flags
defaultFlags =
    Flags
        { model_name = "qwen3:latest"
        , backend_name = "ollama"
        , num_expr = 5
        , debug = False
        , include_docs = False
        , openai_base_url = "https://api.openai.com"
        , openai_key_name = "OPENAI_API_KEY"
        , model_options = Nothing
        }

-- | Produce the documentation of all the HolefitCandidates.
getDocs :: [HoleFitCandidate] -> TcM String
getDocs cs = do
    dflags <- getDynFlags
    gbl_env <- getGblEnv
    lcl_docs <-  fmap ( maybe GHC.emptyUniqMap GHC.docs_decls) <$> liftIO $ GHC.extractDocs dflags gbl_env
    all_docs <- allDocs lcl_docs cs
    return $ unlines $ filter (not . null) $ map (mkDoc dflags all_docs) cs
 where
    allDocs docs (IdHFCand _:cs') = allDocs docs cs'
    allDocs docs (GreHFCand gre:cs') | gre_lcl gre = allDocs docs cs'
    allDocs docs (c:cs') = do
        if_docs <- mi_docs <$> GHC.loadInterfaceForName (text "hole-fit docs") (getName c)
        case if_docs of
          Nothing -> allDocs docs cs'
          Just d -> allDocs (GHC.plusUniqMap docs (GHC.docs_decls d)) cs'
    allDocs docs [] = return docs
    mkDoc :: DynFlags -> GHC.UniqMap Name [GHC.HsDoc GhcRn] -> HoleFitCandidate -> String
    mkDoc dflags all_docs hfc | Just rn <- fullyQualified hfc,
                                Just doc <- GHC.lookupUniqMap all_docs (getName hfc) =
       "Documentation for `" <> showSDoc dflags (ppr rn ) <> "`:\n```\n"
       <> processDoc dflags doc
       <> "```"
    mkDoc _ _ _ = ""
    -- We get the first paragraph of the docs to avoid too much context
    processDoc :: DynFlags -> [GHC.HsDoc GhcRn] -> String
    processDoc dflags docs = first_paragraph
      where whole_string = unlines $ map (showSDoc dflags . ppr) docs
            first_paragraph = unlines $ takeWhile (not . null) $ lines whole_string

-- | Parse command line options
parseFlags :: [CommandLineOption] -> Flags
parseFlags = parseFlags' defaultFlags . reverse -- reverse so outside options come first
  where
    parseFlags' :: Flags -> [CommandLineOption] -> Flags
    parseFlags' flags [] = flags
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "model=" (T.pack opt) =
            let model_name = T.drop (T.length "model=") (T.pack opt)
             in parseFlags' flags{model_name = model_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "backend=" (T.pack opt) =
            let backend_name = T.drop (T.length "backend=") (T.pack opt)
             in parseFlags' flags{backend_name = backend_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "openai_base_url=" (T.pack opt) =
            let openai_base_url = T.drop (T.length "openai_base_url=") (T.pack opt)
             in parseFlags' flags{openai_base_url = openai_base_url} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "openai_key_name=" (T.pack opt) =
            let openai_key_name = T.drop (T.length "openai_key_name=") (T.pack opt)
             in parseFlags' flags{openai_key_name = openai_key_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "debug" (T.pack opt) = parseFlags' flags{debug = True} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "include-docs" (T.pack opt) = parseFlags' flags{include_docs = True} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "n=" (T.pack opt) =
            let num_expr = T.unpack $ T.drop (T.length "n=") (T.pack opt)
             in parseFlags' flags{num_expr = read num_expr} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "model-options=" (T.pack opt) =
            let model_options = T.drop (T.length "model-options=") (T.pack opt)
                m_opts = Aeson.eitherDecodeStrictText model_options
            in case m_opts of
                Right o -> parseFlags' flags{model_options = Just o } opts
                Left err -> error $ "Failed to parse model-options: " <> err
    parseFlags' flags _ = flags

-- | Helper function to replace placeholders in a template string
replacePlaceholders :: Text -> [(Text, String)] -> Text
replacePlaceholders = foldl replacePlaceholder
  where
    replacePlaceholder :: Text -> (Text, String) -> Text
    replacePlaceholder str (placeholder, value) = T.replace placeholder (T.pack value) str
