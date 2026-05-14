module GHC.Plugin.OllamaHoles.Data.Spec (tests) where

import Test.Tasty (TestTree, testGroup)

import GHC.Plugin.OllamaHoles.Data.Flags.Spec qualified as FlagsSpec
import GHC.Plugin.OllamaHoles.Data.Trigger.Spec qualified as TriggerSpec
import GHC.Plugin.OllamaHoles.Data.Template.Spec qualified as TemplateSpec
import GHC.Plugin.OllamaHoles.Data.Service.Spec qualified as ServiceSpec
import GHC.Plugin.OllamaHoles.Data.Profile.Spec qualified as ProfileSpec
import GHC.Plugin.OllamaHoles.Data.Prefs.Spec qualified as PrefsSpec
import GHC.Plugin.OllamaHoles.Data.ServiceCall.Spec qualified as ServiceCallSpec



tests :: TestTree
tests = testGroup "GHC.Plugin.OllamaHoles.Data"
  [ FlagsSpec.tests
  , TriggerSpec.tests
  , TemplateSpec.tests
  , ServiceSpec.tests
  , ProfileSpec.tests
  , PrefsSpec.tests
  , ServiceCallSpec.tests
  ]
