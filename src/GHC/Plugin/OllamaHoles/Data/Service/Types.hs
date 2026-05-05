module GHC.Plugin.OllamaHoles.Data.Service.Types where

import GHC.Generics (Generic)
import Data.String (IsString(..))
import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Backend (BackendConfig)



newtype ServiceName = ServiceName Text
  deriving (Eq, Ord, Show)

instance IsString ServiceName where
  fromString = ServiceName . fromString



-- | A @Service@ accepts prompts and returns responses.
-- It has a name and a configuration specifying how to
-- communicate with it.
data Service = Service
  { svcName   :: ServiceName
  , svcConfig :: BackendConfig
  } deriving (Eq, Show, Generic)
