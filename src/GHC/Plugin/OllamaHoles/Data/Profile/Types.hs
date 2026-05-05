{-# LANGUAGE DeriveGeneric #-}

module GHC.Plugin.OllamaHoles.Data.Profile.Types where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)

import GHC.Plugin.OllamaHoles.Template (TemplateSource)
import GHC.Plugin.OllamaHoles.Data.Trigger.Types
import GHC.Plugin.OllamaHoles.Data.Trigger.Error
import GHC.Plugin.OllamaHoles.Data.Service (Service, ServiceName)



newtype ProfileName = ProfileName
  { unProfileName :: Text
  } deriving (Eq, Ord, Show)

instance IsString ProfileName where
  fromString = ProfileName . fromString


newtype ModelName = ModelName
  { unModelName :: Text
  } deriving (Eq, Ord, Show)

instance IsString ModelName where
  fromString = ModelName . fromString



-- | A @Profile@ consists of details about how
-- to construct a prompt for a service.
data Profile = Profile
  { profName    :: ProfileName
  , profKind    :: ProfileKind
  , profTrigger :: TriggerPolicy
  } deriving (Eq, Show, Generic)

data ProfileKind
  = ProfService ServiceProf
  | ProfFanout  FanoutProf
  deriving (Eq, Show, Generic)

data ServiceProf = ServiceProf
  { profService      :: ServiceName
  , profModel        :: ModelName
  , profTemplate     :: Maybe TemplateSource
  , profModelOptions :: Maybe Value
  , profNumExpr      :: Maybe Int
  , profIncludeDocs  :: Maybe Bool
  } deriving (Eq, Show, Generic)

data FanoutProf = FanoutProf
  { profProfiles :: NonEmpty ProfileName
  } deriving (Eq, Show, Generic)



data RoutedProfile = RoutedProfile
  { routedProfile :: Profile
  , routedMatch   :: TriggerMatch
  } deriving (Eq, Show)



data ProfileResponse = ProfileResponse
  { prProfileName :: ProfileName
  , prServiceName :: ServiceName
  , prPrompt      :: Text
  , prResponse    :: Text
  } deriving (Eq, Show)
