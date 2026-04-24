-- | The Backend data type
module GHC.Plugin.OllamaHoles.Backend.Common (Backend(..)) where

import Data.Text (Text)
import Data.Aeson (Value)

-- | Backend to use.
data Backend = Backend
    { listModels :: IO (Maybe [Text])
    , generateFits :: Text -> Text -> Maybe Value -> IO (Either String Text)
    }
