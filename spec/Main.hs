module Main where

import Test.Tasty
import qualified GHC.Plugin.OllamaHoles.Candidate.Spec as CandidateSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Normalize.Spec as NormalizeSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Rewrite.Spec as RewriteSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Compat.Spec as CompatSpec
import qualified GHC.Plugin.OllamaHoles.Options.Spec as OptionsSpec
import qualified GHC.Plugin.OllamaHoles.Logger.Spec as LoggerSpec
import qualified GHC.Plugin.OllamaHoles.Config.Spec as ConfigSpec
import qualified GHC.Plugin.OllamaHoles.Config.Trigger.Spec as ConfigTriggerSpec
import qualified GHC.Plugin.OllamaHoles.Data.ServiceCall.Route.Spec as ServiceCallRouteSpec
import qualified GHC.Plugin.OllamaHoles.Data.Config.Build.Spec as BuildSpec
import qualified GHC.Plugin.OllamaHoles.Data.Spec as DataSpec

main :: IO ()
main = defaultMain $
    testGroup "ollama-holes"
        [ CandidateSpec.tests
        , NormalizeSpec.tests
        , RewriteSpec.tests
        , CompatSpec.tests
        , OptionsSpec.tests
        , LoggerSpec.tests
        , ConfigSpec.tests
        , ConfigTriggerSpec.tests
        , ServiceCallRouteSpec.tests
        , BuildSpec.tests
        , DataSpec.tests
        ]