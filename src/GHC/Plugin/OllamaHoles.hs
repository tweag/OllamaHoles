{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

-- | The Ollama plugin for GHC
module GHC.Plugin.OllamaHoles where

import Control.Monad (unless, when, forM_, (>=>))
import Control.Monad.Except (ExceptT, runExceptT, MonadError(..), liftEither)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
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
import GHC.Plugin.OllamaHoles.Trigger



-- | Ollama plugin for GHC
plugin :: Plugin
plugin = defaultPlugin
  { holeFitPlugin = Just . mkHoleFitPluginR
  }

pluginName :: Text
pluginName = "Ollama Plugin"

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

setCandidates :: [HoleFitCandidate] -> PluginState -> PluginState
setCandidates cs st = st { candidates = cs }

data PluginError
  = OptionParseError OptError
  | UnknownOptionError [Token]
  | TemplateSpecError TemplateError
  | TemplateParseError TemplateError
  | TemplateSubError TemplateError
  | NoModelsAvailable
  | ModelNotFound Text [Text] BackendSlug
  | TypedHoleNotFound TypedHole
  | ResponseFailed Text
  | HoleMissingTriggerName
  | HoleNameDoesNotMatchPolicy Text

isSilentError :: PluginError -> Bool
isSilentError = \case
  OptionParseError           _ -> True -- \
  UnknownOptionError         _ -> True --  | These are initialization errors, and
  TemplateSpecError          _ -> True --  | are printed by printRenderedError.
  TemplateParseError         _ -> True -- /
  TypedHoleNotFound          _ -> True -- This just means there are no typed holes.
  HoleNameDoesNotMatchPolicy _ -> True -- The hole exists but doesn't match the trigger.
  _                            -> False

renderPluginError :: PluginError -> Text
renderPluginError = \case
  OptionParseError err ->
    "option parse error: " <> T.pack (show err)

  UnknownOptionError toks ->
    "unrecognized plugin option(s): "
      <> T.intercalate ", " (map renderToken toks)

  TemplateSpecError err ->
    "template specification error: " <> T.pack (show err)

  TemplateParseError err ->
    "template load/parse error: " <> T.pack (show err)

  TemplateSubError err ->
    "template substitution error: " <> T.pack (show err)

  NoModelsAvailable ->
    "no models available; check your backend configuration"

  ModelNotFound modelName models backendName -> mconcat
    [ "model "
    , modelName
    , " not found for backend "
    , renderBackendSlug backendName
    , ". "
    , if backendName == Ollama
        then "Use `ollama pull` to download the model, or "
        else ""
    , "specify another model using "
    , "`-fplugin-opt=GHC.Plugin.OllamaHoles:model=`.\n"
    , "Available models:\n"
    , T.unlines models
    ]

  TypedHoleNotFound _ ->
    "could not locate the typed hole in the current context"

  ResponseFailed msg ->
    "backend request failed: " <> msg

  HoleNameDoesNotMatchPolicy holeName ->
    "skipping " <> holeName <> " because it does not match the configured trigger policy"
  where
    renderToken :: Token -> Text
    renderToken = \case
      BooleanToken key -> key
      ValueToken key val -> key <> "=" <> val



-- Initialize
-------------

hfPluginInitLLM
  :: [CommandLineOption]
  -> TcM (TcRef (Either PluginError PluginState))
hfPluginInitLLM =
  (liftIO . runExceptT . tryPluginInitLLM)
    >=> printRenderedError >=> newTcRef

-- | Initialize the plugin state
tryPluginInitLLM
  :: [CommandLineOption] -> ExceptT PluginError IO PluginState
tryPluginInitLLM opts = do
  flags <- case parseCommandLineOptions defaultFlags opts of
    Right (fs, []) -> pure fs
    Right (_, unk) -> throwError $ UnknownOptionError unk
    Left err       -> throwError $ OptionParseError err
  spec <- liftEitherIO TemplateSpecError $ pure $ mkTemplateSpec flags
  logger <- liftIO $ Log.initLogger (log_mode flags) (log_dir flags)
  template <- liftEitherIO TemplateParseError $ loadTemplate spec
  pure $ PluginState
    { candidates     = []
    , writeLogEvent  = logger
    , templateSpec   = spec
    , parsedTemplate = template
    , commandOptions = flags
    }



-- Fit Holes
------------

fitPluginLLM
  :: TcRef (Either PluginError PluginState)
  -> TypedHole
  -> [HoleFit] -- Known hole fits from GHC
  -> TcM [HoleFit]
fitPluginLLM ref hole fits = do
  result <- runExceptT $ tryFitPluginLLM ref hole fits
  case result of
    Right ok -> pure ok
    Left err -> do
      unless (isSilentError err) $ liftIO $
        T.putStrLn $ pluginName <> ": " <> renderPluginError err
      pure fits

tryFitPluginLLM
  :: TcRef (Either PluginError PluginState)
  -> TypedHole
  -> [HoleFit] -- Known hole fits from GHC
  -> ExceptT PluginError TcM [HoleFit]
tryFitPluginLLM ref typedHole fits = do
  st <- readTcRef ref >>= liftEither
  debugMsg st $ "running with flags\n" <> T.pack (show $ commandOptions st)

  withTypedHole typedHole $ \hole -> do
    let tpol = trigger_policy $ commandOptions st
        holeName = holeTriggerName hole

    -- Does this hole match the trigger?
    unless (shouldTriggerHole tpol holeName) $
      throwError (HoleNameDoesNotMatchPolicy holeName)
    debugMsg st "Hole Found"

    -- Does the specified model exist?
    checkModel st

    prompt <- prepareHoleFitPrompt st typedHole fits
    debugMsg st $ "prompt:\n" <> prompt

    rsp <- submitRequest st prompt
    lift $ extractHoleFitsFromResponse st prompt rsp typedHole hole

holeTriggerName :: Hole -> Text
holeTriggerName =
  T.pack . occNameString . rdrNameOcc . hole_occ

-- | Ensure that the specified model exists.
checkModel :: PluginState -> ExceptT PluginError TcM ()
checkModel st = do
  let flags = commandOptions st
  let backend = getBackend flags
  available_models <- liftIO $ listModels backend
  case available_models of
    Nothing -> throwError NoModelsAvailable
    Just models -> unless (model_name flags `elem` models) $
      throwError $ ModelNotFound (model_name flags) models (backend_name flags)

-- | Build a prompt for the LLM from context.
prepareHoleFitPrompt
  :: PluginState -> TypedHole
  -> [HoleFit]          -- Known hole fits provided by GHC
  -> ExceptT PluginError TcM Text
prepareHoleFitPrompt st hole fits = do
  let flags = commandOptions st
  gbl_env <- lift getGblEnv
  dflags <- lift getDynFlags
  docs <- lift $ if include_docs flags
    then getDocs (candidates st) else return ""
  liftEitherIO TemplateSubError $ pure $
    expandTemplateWith (parsedTemplate st) $ mkTemplateEnv
      [ ("backend" , renderBackendSlug $ backend_name flags)
      , ("model"   , model_name flags)
      , ("numexpr" , T.pack (show $ num_expr flags))
      , ("docs"    , T.pack docs)
      , ("context" , maybe "" encodePromptContext $
                      getPromptContext hole fits gbl_env (candidates st) dflags)
      ]

withTypedHole
  :: TypedHole -> (Hole -> ExceptT PluginError TcM a)
  -> ExceptT PluginError TcM a
withTypedHole hole act = case th_hole hole of
  Nothing -> throwError $ TypedHoleNotFound hole
  Just ok -> act ok

submitRequest
  :: PluginState -> Log.Prompt
  -> ExceptT PluginError TcM Text
submitRequest st prompt = do
  let flags = commandOptions st
  let backend = getBackend flags
  res <- liftIO $ generateFits
    backend prompt (model_name flags) (model_options flags)
  case res of
    Right rsp -> pure rsp
    Left err -> throwError $ ResponseFailed $ T.pack err

extractHoleFitsFromResponse
  :: PluginState -> Log.Prompt -> Log.Response
  -> TypedHole -> Hole -> TcM [HoleFit]
extractHoleFitsFromResponse st prompt rsp hole h = do
  let logger = writeLogEvent st
  let debugMode = debug (commandOptions st)
  dflags <- getDynFlags

  -- Get the raw list of candidates
  let rawLines = preProcess (T.lines rsp)
  when (not $ null rawLines) $ do
    let numRaw = T.pack $ show $ length rawLines
    debugMsg st $ "--- raw candidates (" <> numRaw <> ") ---\n"
      <> T.unlines rawLines

  -- Remove syntactic duplicates
  let surfaceUnique = L.nub rawLines
  when (not $ null surfaceUnique) $ do
    let numSurf = T.pack $ show $ length surfaceUnique
    debugMsg st $ "--- syntactic uniques (" <> numSurf <> ") ---\n"
      <> T.unlines surfaceUnique

  -- prepare candidates for semantic deduplication
  preparedE <- for surfaceUnique
    (pipelineCandidate (mkParseCtx dflags) (mkRenameCtx debugMode)
      (mkCheckCtx debugMode hole) (mkPrepCtx dflags (holeArity h)))

  -- check that we reach the same conclusion as
  -- the original fit check
  regressionCheck hole surfaceUnique preparedE

  -- Report any validation errors
  let failures = [ (src, err) | (src, Left err) <- zip surfaceUnique preparedE ]
  when (not $ null failures) $ do
    let numFail = T.pack $ show $ length failures
    debugMsg st $ "--- candidate validation failures (" <> numFail <> ") ---\n"
      <> T.unlines (fmap (uncurry renderCandidateError) failures)

  -- Normalize expressions for deduplication
  let prepared = [ pc | Right pc <- preparedE ]
  when (not $ null prepared) $ do
    let numPrep = T.pack $ show $ length prepared
    debugMsg st $ "--- prepared for semantic-ish deduplication (" <> numPrep <> ") ---\n"
      <> T.intercalate "\n" (flip foldMap prepared $ \pc -> (:[]) $ T.unlines
        [ "source: " <> prSource pc
        , "rank:   " <> T.pack (show $ prRank pc)
        , "key:    " <> T.pack (show $ prNormKey pc)
        ])

  -- Deduplicate expressions
  let deduped  = dedupePreparedCandidates prepared
  when (not $ null deduped) $ do
    let numUniq = T.pack $ show $ length deduped
    debugMsg st $ "--- semantic-ish uniques (" <> numUniq <> ") ---\n"
      <> T.unlines (fmap prSource deduped)

  liftIO $ Log.writeLogEvent logger $ Log.mkLogEvent
      prompt rsp (length rawLines) (length deduped) (length prepared)

  let holeFits = map (RawHoleFit . text . T.unpack . prSource) deduped
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

-- | Determine which backend to use
getBackend :: Flags -> Backend
getBackend flags = case backend_name flags of
    Gemini -> geminiBackend
    Ollama -> ollamaBackend
    OpenAI -> openAICompatibleBackend (openai_base_url flags) (openai_key_name flags)



-- Utils
--------

debugMsg :: (MonadIO m) => PluginState -> Text -> m ()
debugMsg st txt = liftIO $ when (debug $ commandOptions st) $
  T.putStrLn $ pluginName <> ": " <> txt

liftEitherIO :: (MonadIO m) => (e1 -> e2) -> IO (Either e1 a) -> ExceptT e2 m a
liftEitherIO f act = liftIO act >>= either (throwError . f) pure

printRenderedError
  :: (MonadIO m) => Either PluginError u -> m (Either PluginError u)
printRenderedError x = case x of
  Right u -> pure (Right u)
  Left err -> do
    liftIO $ T.putStrLn $ renderPluginError err
    pure (Left err)
