{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ollama_holes_plugin (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ollama_holes_plugin"
version :: Version
version = Version [0,1,6,0] []

synopsis :: String
synopsis = "A typed-hole plugin that uses LLMs to generate valid hole-fits"
copyright :: String
copyright = "2025 (c) Matthias Pall Gissurarson  "
homepage :: String
homepage = "https://github.com/Tritlo/OllamaHoles"
