module Main where

import Test.Tasty
import qualified GHC.Plugin.OllamaHoles.Candidate.Spec as CandidateSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Normalize.Spec as NormalizeSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Rewrite.Spec as RewriteSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Compat.Spec as CompatSpec
import qualified GHC.Plugin.OllamaHoles.Logger.Spec as LoggerSpec
import qualified GHC.Plugin.OllamaHoles.Data.Config.Build.Spec as BuildSpec
import qualified GHC.Plugin.OllamaHoles.Data.Spec as DataSpec

main :: IO ()
main = defaultMain $
    testGroup "ollama-holes"
        [ CandidateSpec.tests
        , NormalizeSpec.tests
        , RewriteSpec.tests
        , CompatSpec.tests
        , LoggerSpec.tests
        , BuildSpec.tests
        , DataSpec.tests
        ]