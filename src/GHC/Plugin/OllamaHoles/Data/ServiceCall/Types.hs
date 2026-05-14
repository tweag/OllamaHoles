module GHC.Plugin.OllamaHoles.Data.ServiceCall.Types where

import Control.Monad.Except (ExceptT)
import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Prompt
import GHC.Plugin.OllamaHoles.Data.Config
import GHC.Plugin.OllamaHoles.Data.Template
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Error



data ServiceCall = ServiceCall
  { callService :: Service
  , callProfile :: ServiceProf
  } deriving (Eq, Show)

data PromptRequest = PromptRequest
  { requestContext  :: PromptContext
  , requestTemplate :: Template
  } deriving (Eq, Show)

data PromptResponse = PromptResponse
  { unPromptResponse :: Text
  } deriving (Eq, Show)

-- | Type modeling service calls where the service was
-- able to list the available models and the specified
-- model shown to exist. Warnings correspond to services
-- where either of those conditions does not hold.
data CheckedServiceCalls = CheckedServiceCalls
  { checkedAccepted :: [ServiceCall]
  , checkedWarnings :: [ModelSelectionWarning]
  } deriving (Eq, Show)

data ServiceCallResponses = ServiceCallResponses
  { serviceCallResponses :: [PromptResponse]
  , serviceCallWarnings  :: [ModelSelectionWarning]
  }

data ServiceCallOps m = ServiceCallOps
  { opsListModels
      :: Service -> m (Maybe [ModelName])
  , opsSubmitServiceCall
      :: PromptRequest -> ServiceCall
      -> ExceptT ServiceCallError m PromptResponse
  , opsGetServiceCallTemplate
      :: FilePath -> ServiceCall
      -> ExceptT ServiceCallError m Template
  }
