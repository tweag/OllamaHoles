module GHC.Plugin.OllamaHoles.Data.ServiceCall.Types where

import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Prompt
import GHC.Plugin.OllamaHoles.Template
import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile



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
