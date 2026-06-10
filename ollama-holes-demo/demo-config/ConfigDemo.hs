{-# OPTIONS_GHC -fplugin=GHC.Plugin.OllamaHoles #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:config=demo-config/ollama-holes.toml #-}

module ConfigDemo where

-- This should route to the "default" profile.
demoDefault :: Maybe Int -> Int
demoDefault mx =
  _llm mx

-- This should route to the "alt" profile.
demoAlt :: Maybe Int -> Int
demoAlt mx =
  _alt mx

-- This should fanout to both "default" and "alt".
demoBoth :: Maybe Int -> Int
demoBoth mx =
  _both mx

-- This should not hit the plugin backend because no trigger matches.
demoNoRoute :: Maybe Int -> Int
demoNoRoute mx =
  _plain mx