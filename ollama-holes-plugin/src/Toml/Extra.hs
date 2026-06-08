module Toml.Extra where

import Data.Text (Text)
import Data.Text qualified as T
import Toml qualified as Toml
import Toml.Schema qualified as Toml

tomlValidateText
  :: (Text -> Either e a) -> (e -> Text)
  -> Toml.Value' l -> Toml.Matcher l a
tomlValidateText parse renderErr value = do
  raw <- Toml.fromValue value
  either (fail . T.unpack . renderErr)
    pure (parse raw)
