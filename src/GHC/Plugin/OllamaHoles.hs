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
import GHC.Tc.Utils.Monad qualified as GHC

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

import GHC.HsToCore.Docs qualified as GHC
import GHC.Types.Unique.Map qualified as GHC
import GHC.Hs.Doc qualified as GHC
import GHC.Iface.Load qualified as GHC (loadInterfaceForName)
import GHC.Tc.Utils.TcType qualified as GHC (tyCoFVsOfType, mkPhiTy)
import GHC.Tc.Solver qualified as GHC (simplifyTop, simplifyInfer, captureTopConstraints, InferMode(..))
import GHC.Tc.Solver.Monad qualified as GHC (zonkTcType, runTcSEarlyAbort)
import Data.Aeson qualified as Aeson

import GHC.Plugin.OllamaHoles.Options
import GHC.Plugin.OllamaHoles.Prompt
import GHC.Plugin.OllamaHoles.Logger qualified as Log
import GHC.Plugin.OllamaHoles.Candidate
import GHC.Plugin.OllamaHoles.Template



-- | Ollama plugin for GHC
plugin :: Plugin
plugin = defaultPlugin
  { holeFitPlugin = Just . mkHoleFitPluginR
  }

mkHoleFitPluginR
  :: [CommandLineOption] -> HoleFitPluginR
mkHoleFitPluginR opts = HoleFitPluginR
  { hfPluginInit = hfPluginInitLLM opts
  , hfPluginStop = \_ -> return ()
  , hfPluginRun = \ref -> HoleFitPlugin
    { candPlugin = \_ cs -> updTcRef ref (setCandidates cs) >> return cs
    , fitPlugin = fitPluginLLM ref
    }
  }

-- State
--------

data PluginState = PluginState
  { candidates     :: [HoleFitCandidate]
  , writeLogEvent  :: Log.Logger
  , templateSpec   :: TemplateSpec
  , parsedTemplate :: Template
  , commandOptions :: Flags
  }

setCandidates :: [HoleFitCandidate] -> PluginState -> PluginState
setCandidates cs st = st { candidates = cs }

-- | Initialize the plugin state
hfPluginInitLLM :: [CommandLineOption] -> TcM (TcRef PluginState)
hfPluginInitLLM opts = do
  let optParseResult = parseCommandLineOptions defaultFlags opts
  flags <- case optParseResult of
    Left err -> error $ "error parsing options: " <> show err
    Right (flags, unk) -> do
        when (not $ null unk) $ do
            error $ "unknown options: " <> show unk
        pure flags
  spec <- case mkTemplateSpec flags of
    Left err -> error $ "Template spec error: " <> show err
    Right ok -> pure ok
  logger <- liftIO $ Log.initLogger (log_mode flags) (log_dir flags)
  template <- liftIO $ do
    raw <- loadTemplate spec
    case raw of
      Left err -> error $ "template parse error: " <> show err
      Right ok -> pure ok
  newTcRef $ PluginState
    { candidates     = []
    , writeLogEvent  = logger
    , templateSpec   = spec
    , parsedTemplate = template
    , commandOptions = flags
    }



-- | Determine which backend to use
getBackend :: Flags -> Backend
getBackend Flags{backend_name = "ollama"} = ollamaBackend
getBackend Flags{backend_name = "gemini"} = geminiBackend
getBackend Flags{backend_name = "openai", ..} = openAICompatibleBackend openai_base_url openai_key_name
getBackend Flags{..} = error $ "unknown backend: " <> T.unpack backend_name



pluginName :: Text
pluginName = "Ollama Plugin"

fitPluginLLM
    :: TcRef PluginState
    -> TypedHole
    -> [HoleFit]
    -> GHC.IOEnv (Env TcGblEnv TcLclEnv) [HoleFit]
fitPluginLLM ref hole fits = do
    PluginState cands logger templateSpec template flags <- readTcRef ref
    dflags <- getDynFlags
    let Flags{..} = flags
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

            -- Prepare the prompt
            gbl_env <- getGblEnv
            let promptContext = getPromptContext hole fits gbl_env cands dflags
            let promptContext'' = maybe "" encodePromptContext promptContext
            docs <- if include_docs then getDocs cands else return ""
            let env = mkTemplateEnv
                    [ ("context" , promptContext'')
                    , ("docs"    , T.pack docs)
                    , ("numexpr" , T.pack (show num_expr))
                    , ("backend" , backend_name)
                    , ("model"   , model_name)
                    , ("guidance", maybe "" (mconcat . pcGuidance) promptContext)
                    ]
            prompt'' <- case expandTemplate env template of
                Left err -> error $ "Template substitution error: " <> show err
                Right ok -> pure ok

            case th_hole hole of
                Just h -> do
                    liftIO $ when debug $ do T.putStrLn $ "NEW PROMPT:\n\n" <> prompt'' <> "\n\n"
                    res <- liftIO $ generateFits backend prompt'' model_name model_options
                    case res of
                        Right rsp ->
                            extractHoleFitsFromResponse dflags prompt'' rsp logger hole h debug
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



-- | Helper function to interpret a @TemplateSpec@ from the flags.
mkTemplateSpec :: Flags -> Either TemplateError TemplateSpec
mkTemplateSpec Flags{..} = do
    let mkSpec source = TemplateSpec
            { tsSearchDir = template_search_dir
            , tsSource = source
            }
    case (template_path, template_name) of
        (Just fp, _) -> Right $ mkSpec $ TemplateFile fp
        (_, Just nm) -> fmap (mkSpec . NamedTemplate) $ parseTemplateName nm
        _            -> Right $ mkSpec $ DefaultTemplate

