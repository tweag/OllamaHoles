module GHC.Plugin.OllamaHoles.Data.Template.Types.Internal
  ( TemplateName(unTemplateName)
  , unsafeCreateRawTemplateName
  ) where

import Data.Text (Text)



newtype TemplateName = TemplateName
  { unTemplateName :: Text
  } deriving (Eq, Show)

-- For tests
unsafeCreateRawTemplateName :: Text -> TemplateName
unsafeCreateRawTemplateName = TemplateName
