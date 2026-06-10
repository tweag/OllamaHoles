module CabalDemo where

-- Demonstrates enabling the plugin project-wide with cabal.
-- See ollama-holes-cabal-demo in ollama-holes-demo.cabal

-- This should route to the "default" profile.
demoDefault :: Maybe Int -> Int
demoDefault mx = _llm mx
