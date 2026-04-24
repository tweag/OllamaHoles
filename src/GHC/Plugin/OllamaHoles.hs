{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | The Ollama plugin for GHC
module GHC.Plugin.OllamaHoles where

import Control.Monad (unless, when, forM_, (>=>))
import Control.Monad.Except (ExceptT, runExceptT, MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except ()
import Data.Aeson (Value)
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
  { hfPluginInit =
      -- Initialize the plugin (this may fail).
      hfPluginInitLLM opts :: TcM (TcRef (Either PluginError PluginState))
  , hfPluginStop = \_ -> return ()
  , hfPluginRun = \ref -> HoleFitPlugin
    { candPlugin = \_ cs -> updTcRef ref (fmap (setCandidates cs)) >> return cs
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

data PluginError
  = OptionParseError OptError
  | UnknownOptionError [Token]
  | TemplateSpecError TemplateError
  | TemplateParseError TemplateError
  | TemplateSubError TemplateError
  | NoModelsAvailable
  | ModelNotFound Text [Text] Text
  | TypedHoleNotFound TypedHole
  | ResponseFailed Text
  -- deriving (Eq, Show)

renderPluginError :: PluginError -> Text
renderPluginError err = case err of
  ModelNotFound modelName models backendName -> mconcat
    [ pluginName, ": Model ", modelName, " not found. "
    , if backendName == "ollama"
        then "Use `ollama pull` to download the model, or " else ""
    , "specify another model using "
    , "`-fplugin-opt=GHC.Plugin.OllamaHoles:model=<model_name>`\n"
    , "Availble models: \n", T.unlines models
    ]
  _ -> mconcat
    [ pluginName, ": ERROR - " --, T.pack (show err)
    ]

setCandidates :: [HoleFitCandidate] -> PluginState -> PluginState
setCandidates cs st = st { candidates = cs }



hfPluginInitLLM
  :: [CommandLineOption]
  -> TcM (TcRef (Either PluginError PluginState))
hfPluginInitLLM =
  (liftIO . runExceptT . tryPluginInitLLM) >=> newTcRef

-- | Initialize the plugin state
tryPluginInitLLM
  :: [CommandLineOption] -> ExceptT PluginError IO PluginState
tryPluginInitLLM opts = do
  flags <- case parseCommandLineOptions defaultFlags opts of
    Right (fs, []) -> pure fs
    Right (_, unk) -> throwError $ UnknownOptionError unk
    Left err       -> throwError $ OptionParseError err
  spec <- onLeftIO TemplateSpecError $ pure $ mkTemplateSpec flags
  logger <- liftIO $ Log.initLogger (log_mode flags) (log_dir flags)
  template <- onLeftIO TemplateParseError $ loadTemplate spec
  pure $ PluginState
    { candidates     = []
    , writeLogEvent  = logger
    , templateSpec   = spec
    , parsedTemplate = template
    , commandOptions = flags
    }

onLeftIO :: (MonadIO m) => (e1 -> e2) -> IO (Either e1 a) -> ExceptT e2 m a
onLeftIO f act = liftIO act >>= either (throwError . f) pure



-- | Determine which backend to use
getBackend :: Flags -> Backend
getBackend Flags{backend_name = "ollama"} = ollamaBackend
getBackend Flags{backend_name = "gemini"} = geminiBackend
getBackend Flags{backend_name = "openai", ..} = openAICompatibleBackend openai_base_url openai_key_name
getBackend Flags{..} = error $ "unknown backend: " <> T.unpack backend_name



pluginName :: Text
pluginName = "Ollama Plugin"

fitPluginLLM
    :: TcRef (Either PluginError PluginState)
    -> TypedHole
    -> [HoleFit]
    -> GHC.IOEnv (Env TcGblEnv TcLclEnv) [HoleFit]
fitPluginLLM ref hole fits = do
    refResult <- readTcRef ref
    case refResult of
        Left err -> do
            liftIO $ putStrLn $ "Initializing OllamaHoles failed with the following error: " -- <> show err
            pure fits
        Right (PluginState cands logger templateSpec template flags) -> do
            dflags <- getDynFlags
            let Flags{..} = flags
            let backend = getBackend flags
            liftIO $ when debug $ T.putStrLn $ "Running " <> pluginName <> " with flags:"
            liftIO $ when debug $ print flags

            crashOnError $ usingModel model_name backend backend_name

            liftIO $ when debug $ T.putStrLn $ pluginName <> ": Hole Found"

            h <- crashOnError $ atHole hole
            prompt <- crashOnError $ prepareHoleFitPrompt dflags flags hole fits cands template
            liftIO $ when debug $ do T.putStrLn $ "NEW PROMPT:\n\n" <> prompt <> "\n\n"
            rsp <- crashOnError $ submitRequest backend prompt model_name model_options
            extractHoleFitsFromResponse dflags prompt rsp logger hole h debug

-- | Ensure that the specified model exists.
usingModel :: Text -> Backend -> Text -> ExceptT PluginError TcM ()
usingModel modelName backend backendName = do
  available_models <- liftIO $ listModels backend
  case available_models of
    Nothing -> throwError NoModelsAvailable
    Just models -> unless (modelName `elem` models) $
      throwError $ ModelNotFound modelName models backendName

-- | Build a prompt for the LLM from context.
prepareHoleFitPrompt
  :: DynFlags -> Flags -> TypedHole
  -> [HoleFit]          -- Known hole fits provided by GHC
  -> [HoleFitCandidate] -- Not yet checked; used for getting guidance
  -> Template -> ExceptT PluginError TcM Text
prepareHoleFitPrompt dflags flags hole fits cands template = do
  gbl_env <- lift getGblEnv
  docs <- lift $ if include_docs flags then getDocs cands else return ""
  onLeftIO TemplateSubError $ pure $ expandTemplateWith template $ mkTemplateEnv
    [ ("backend" , backend_name flags)
    , ("model"   , model_name flags)
    , ("numexpr" , T.pack (show $ num_expr flags))
    , ("docs"    , T.pack docs)
    , ("context" , maybe "" encodePromptContext $
                        getPromptContext hole fits gbl_env cands dflags)
    ]

atHole
  :: TypedHole -> ExceptT PluginError TcM Hole
atHole hole = case th_hole hole of
  Just ok -> pure ok
  Nothing -> throwError $ TypedHoleNotFound hole

-- Temporary
crashOnError :: (Monad m) => ExceptT e m a -> m a
crashOnError act = do
    result <- runExceptT act
    case result of
        Left err -> error $ "ERROR: " -- <> show err
        Right ok -> pure ok

submitRequest
  :: Backend -> Log.Prompt -> Text -> Maybe Value
  -> ExceptT PluginError TcM Text
submitRequest backend prompt modelName modelOptions = do
  res <- liftIO $ generateFits backend prompt modelName modelOptions
  case res of
    Right rsp -> pure rsp
    Left err -> throwError $ ResponseFailed $ T.pack err

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

    -- prepare candidates for semantic deduplication
    preparedE <- traverse
        (pipelineCandidate (mkParseCtx dflags) (mkRenameCtx debug)
          (mkCheckCtx debug hole) (mkPrepCtx dflags (holeArity h)))
        surfaceUnique

    -- check that we reach the same conclusion as
    -- the original fit check
    when debug $ regressionCheck hole surfaceUnique preparedE

    let failures = [ (src, err) | (src, Left err) <- zip surfaceUnique preparedE ]
    let prepared = [ pc | Right pc <- preparedE ]
    let deduped  = dedupePreparedCandidates prepared
    let holeFits = map (RawHoleFit . text . T.unpack . prSource) deduped

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

    return holeFits

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

