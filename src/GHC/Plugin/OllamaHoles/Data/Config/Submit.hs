module GHC.Plugin.OllamaHoles.Data.Config.Submit
  ( submitPromptForProfile
  ) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Prompt

import GHC.Plugin.OllamaHoles.Data.Service.Types
import GHC.Plugin.OllamaHoles.Data.Profile.Types
import GHC.Plugin.OllamaHoles.Data.Profile.Error
import GHC.Plugin.OllamaHoles.Data.Config.Types



submitPromptForProfile
  :: MonadIO m
  => FancyConfig
  -> (Service -> ServiceProf -> PromptContext -> m Text)
  -> PromptContext
  -> ProfileName
  -> ExceptT ProfileSubmitError m [ProfileResponse]
submitPromptForProfile fancy renderPrompt ctx rootName =
  go rootName
  where
    go name = case fancy of
      _ -> case M.lookup name (cfgProfiles fancy) of
        Nothing ->
          throwError (SubmitUnknownProfile name)

        Just profile ->
          case profKind profile of
            ProfService serviceProf ->
              submitOne name serviceProf

            ProfFanout fanoutProf ->
              fmap concat $
                traverse go (profProfiles fanoutProf)

    submitOne profileName serviceProf = do
      service <- case fancy of
        _ -> case M.lookup (profService serviceProf) (cfgServices fancy) of
          Nothing ->
            throwError
              (SubmitUnknownService profileName (profService serviceProf))

          Just service ->
            pure service

      prompt <-
        lift (renderPrompt service serviceProf ctx)

      let backend =
            configureBackend (svcConfig service)

      result <-
        liftIO $
          generateFits
            backend
            prompt
            (unModelName (profModel serviceProf))
            (profModelOptions serviceProf)

      case result of
        Left err -> throwError $ SubmitBackendFailed
          profileName (profService serviceProf) (T.pack err)
        Right response -> pure
          [ ProfileResponse
            { prProfileName = profileName
            , prServiceName = profService serviceProf
            , prPrompt = prompt
            , prResponse = response
            }
          ]

