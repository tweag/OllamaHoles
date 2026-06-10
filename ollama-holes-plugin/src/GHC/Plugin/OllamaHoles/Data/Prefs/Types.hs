module GHC.Plugin.OllamaHoles.Data.Prefs.Types where

import Data.Text (Text)
import GHC.Generics (Generic)

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile
import GHC.Plugin.OllamaHoles.Data.Template



-- | @Preferences@ is a raw representation of the user's
-- config file. Internal references have not yet been
-- resolved. The configuration includes
--   * services:  backends and how to communicate with them
--   * profiles:  what to do with a typed hole
--   * templates: what to send to the backend
data Preferences = Preferences
  { prefServices  :: [Service]
  , prefProfiles  :: [Profile]
  , prefTemplates :: [(TemplateName, Template)]
  } deriving (Eq, Show, Generic)
