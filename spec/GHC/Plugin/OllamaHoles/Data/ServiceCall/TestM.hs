module GHC.Plugin.OllamaHoles.Data.ServiceCall.TestM
  ( TestM
  , runTestM
  , ServiceCallTestEnv(..)
  , serviceCallOps
  , listModelsFromEnv
  , unusedPromptContext
  , svcA
  , svcB
  , emptyOverrides
  , svcOverlay
  , profA
  , profB
  , profOverlay

    -- Indexed fixtures
  , indexedProfileName
  , indexedServiceName
  , indexedModelName
  , indexedService
  , indexedServiceProf
  , indexedServiceProfile
  , indexedPromptResponse
  ) where

import Control.Monad.Except
import Data.Functor.Identity
import Data.Map qualified as M
import Data.Text qualified as T

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Prompt
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Types
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.Trigger



type TestM =
  Identity

runTestM :: TestM a -> a
runTestM =
  runIdentity



data ServiceCallTestEnv = ServiceCallTestEnv
  { testOllamaModels :: Maybe [ModelName]
  , testOpenAIModels :: Maybe [ModelName]
  , testResponses :: M.Map ServiceName PromptResponse
  }



listModelsFromEnv
  :: ServiceCallTestEnv -> Service -> TestM (Maybe [ModelName])
listModelsFromEnv env service =
  pure $ case svcConfig service of
    SvcOllama{} -> testOllamaModels env
    SvcOpenAI{} -> testOpenAIModels env



serviceCallOps
  :: ServiceCallTestEnv -> ServiceCallOps TestM
serviceCallOps env = ServiceCallOps
  { opsListModels =
      listModelsFromEnv env

  , opsGetServiceCallTemplate = \_path _call ->
      pure unusedTemplate

  , opsSubmitServiceCall = \_request call -> do
      let serviceName =
            svcName (callService call)

      case M.lookup serviceName (testResponses env) of
        Just response ->
          pure response

        Nothing ->
          throwError $
            ServiceCallError $
              "missing fake response for service: "
                <> show serviceName
  }



unusedTemplate :: Template
unusedTemplate =
  error "unused test template"



unusedPromptContext :: PromptContext
unusedPromptContext =
  error "unused test prompt context"



emptyOverrides :: OverrideConfig
emptyOverrides =
  OverrideConfig
    { overrideModelName = Nothing
    , overrideTemplate = Nothing
    , overrideModelOptions = Nothing
    , overrideNumExpr = Nothing
    , overrideIncludeDocs = Nothing
    }

svcA :: Service
svcA =
  Service
    { svcName = ServiceName "svc-a"
    , svcConfig = SvcOllama (OllamaConfig Nothing)
    }

svcB :: Service
svcB =
  Service
    { svcName = ServiceName "svc-b"
    , svcConfig = SvcOllama (OllamaConfig Nothing)
    }

svcOverlay :: Service
svcOverlay =
  Service
    { svcName = ServiceName "svc-overlay"
    , svcConfig = SvcOllama (OllamaConfig Nothing)
    }

profA :: ServiceProf
profA =
  ServiceProf
    { profService = ServiceName "svc-a"
    , profModel = ModelName "model-a"
    , profTemplate = Nothing
    , profModelOptions = Nothing
    , profNumExpr = Just 5
    , profIncludeDocs = Just False
    }

profB :: ServiceProf
profB =
  ServiceProf
    { profService = ServiceName "svc-b"
    , profModel = ModelName "model-b"
    , profTemplate = Nothing
    , profModelOptions = Nothing
    , profNumExpr = Just 5
    , profIncludeDocs = Just False
    }

profOverlay :: ServiceProf
profOverlay =
  ServiceProf
    { profService = ServiceName "svc-overlay"
    , profModel = ModelName "model-overlay"
    , profTemplate = Nothing
    , profModelOptions = Nothing
    , profNumExpr = Just 5
    , profIncludeDocs = Just False
    }



indexedProfileName :: Int -> ProfileName
indexedProfileName i =
  ProfileName ("p" <> T.pack (show i))

indexedServiceName :: Int -> ServiceName
indexedServiceName i =
  ServiceName ("svc" <> T.pack (show i))

indexedModelName :: Int -> ModelName
indexedModelName i =
  ModelName ("model" <> T.pack (show i))

indexedService :: Int -> Service
indexedService i =
  Service
    { svcName = indexedServiceName i
    , svcConfig = SvcOllama (OllamaConfig Nothing)
    }

indexedServiceProf :: Int -> ServiceProf
indexedServiceProf i =
  ServiceProf
    { profService = indexedServiceName i
    , profModel = indexedModelName i
    , profTemplate = Nothing
    , profModelOptions = Nothing
    , profNumExpr = Just 5
    , profIncludeDocs = Just False
    }

indexedServiceProfile :: Int -> Profile
indexedServiceProfile i =
  Profile
    { profName = indexedProfileName i
    , profTrigger = TriggerNone
    , profKind = ProfService (indexedServiceProf i)
    }

indexedPromptResponse :: Int -> PromptResponse
indexedPromptResponse i =
  PromptResponse ("response-" <> T.pack (show i))
