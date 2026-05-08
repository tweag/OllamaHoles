module GHC.Plugin.OllamaHoles.Data.Prefs.Types where

import GHC.Generics (Generic)

import GHC.Plugin.OllamaHoles.Data.Service
import GHC.Plugin.OllamaHoles.Data.Profile



-- | @Preferences@ is a raw representation of the user's
-- config file. Internal references have not yet been
-- resolved. The configuration includes
--   * services: backends and how to communicate with them
--   * profiles: what to do with a typed hole
data Preferences = Preferences
  { prefServices :: [Service]
  , prefProfiles :: [Profile]
  } deriving (Eq, Show, Generic)
