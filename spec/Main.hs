module Main where

import Test.Tasty
import qualified GHC.Plugin.OllamaHoles.Candidate.Spec as CandidateSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Normalize.Spec as NormalizeSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Rewrite.Spec as RewriteSpec
import qualified GHC.Plugin.OllamaHoles.Candidate.Compat.Spec as CompatSpec
import qualified GHC.Plugin.OllamaHoles.Template.Parse.Spec as ParseSpec
import qualified GHC.Plugin.OllamaHoles.Template.Load.Spec as LoadSpec
import qualified GHC.Plugin.OllamaHoles.Template.Expand.Spec as ExpandSpec
import qualified GHC.Plugin.OllamaHoles.Options.Spec as OptionsSpec
import qualified GHC.Plugin.OllamaHoles.Logger.Spec as LoggerSpec

main :: IO ()
main = defaultMain $
    testGroup "ollama-holes"
        [ CandidateSpec.tests
        , NormalizeSpec.tests
        , RewriteSpec.tests
        , CompatSpec.tests
        , ParseSpec.tests
        , LoadSpec.tests
        , ExpandSpec.tests
        , OptionsSpec.tests
        , LoggerSpec.tests
        ]