module Main where

import Test.Tasty
import qualified GHC.Plugin.OllamaHoles.Candidate.Spec as CandidateSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Normalize.Spec as NormalizeSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Compat.Spec as CompatSpec

main :: IO ()
main = defaultMain $
    testGroup "ollama-holes"
        [ CandidateSpec.tests
        , NormalizeSpec.tests
        , CompatSpec.tests
        ]