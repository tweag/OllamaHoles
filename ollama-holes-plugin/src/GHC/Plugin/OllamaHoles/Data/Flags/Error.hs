module GHC.Plugin.OllamaHoles.Data.Flags.Error
  ( FlagError(..)
  ) where

import GHC.Driver.Plugins (CommandLineOption)
import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Data.Trigger.Error (TriggerPolicyError)
import GHC.Plugin.OllamaHoles.Data.Template.Error (TemplateError)

import GHC.Plugin.OllamaHoles.Data.Flags.Types (FlagName)

data FlagError
  = EmptyFlag
  | MalformedFlag CommandLineOption
  | MissingValue FlagName
  | UnexpectedValue FlagName Text
  | EmptyValue FlagName
  | InvalidInt FlagName Text
  | InvalidJson FlagName Text String
  | InvalidEnum FlagName Text [Text]
  | InvalidBackend Text
  | InvalidTriggerPolicy Text TriggerPolicyError
  | InvalidTemplateNameFlag TemplateError Text
  deriving (Eq, Show)
