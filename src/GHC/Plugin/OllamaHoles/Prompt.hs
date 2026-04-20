{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Plugin.OllamaHoles.Prompt
    ( PromptContext(..)
    , encodePromptContext
    , getPromptContext
    , Guidance
    ) where

import Data.Aeson
import Data.Aeson.Encoding qualified as Enc
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import GHC.Generics (Generic)
import GHC.Plugins hiding ((<>))
import GHC.Tc.Types (TcGblEnv(..), ImportAvails(..))
import GHC.Tc.Types.Constraint (Hole(..))



#if __GLASGOW_HASKELL__ >= 912
import GHC.Tc.Types.CtLoc (ctLocSpan)
import qualified Data.Map as Map
#else
import GHC.Tc.Types.Constraint (ctLocSpan)
#endif



data PromptContext = PromptContext
    { pcHoleVariable :: T.Text
    , pcHoleType     :: T.Text
    , pcModule       :: T.Text
    , pcLocation     :: T.Text
    , pcImports      :: T.Text
    , pcConstraints  :: T.Text
    , pcKnownFits    :: T.Text
    , pcGuidance     :: Guidance
    } deriving (Eq, Show, Generic)

type Guidance = String

-- Manually encoding so we have control over key order
encodePromptContext :: PromptContext -> String
encodePromptContext ctx = LT.unpack $ LT.decodeUtf8 $ Enc.encodingToLazyByteString $ pairs $
    "hole_variable"  .= pcHoleVariable ctx
    <> "hole_type"   .= pcHoleType ctx
    <> "module"      .= pcModule ctx
    <> "location"    .= pcLocation ctx
    <> "imports"     .= pcImports ctx
    <> "constraints" .= pcConstraints ctx
    <> "known_fits"  .= pcKnownFits ctx
    <> "guidance"    .= pcGuidance ctx

getPromptContext
    :: TypedHole -> [HoleFit] -> TcGblEnv
    -> DynFlags -> Guidance -> Maybe PromptContext
getPromptContext hole fits env dflags guidance = do
    h <- th_hole hole
    let pcHoleVariable = T.pack $ occNameString $ occName $ hole_occ h
    let pcHoleType = T.pack $ showSDoc dflags $ ppr $ hole_ty h
    let pcModule = T.pack $ moduleNameString $ moduleName $ tcg_mod env
    let pcLocation = T.pack $ showSDoc dflags (ppr $ ctLocSpan . hole_loc <$> th_hole hole)
    let pcConstraints = T.pack $ showSDoc dflags $ ppr $ th_relevant_cts hole
    let pcKnownFits = T.pack $ showSDoc dflags $ ppr fits
    let pcGuidance = guidance

#if __GLASGOW_HASKELL__ >= 912
    let pcImports = T.pack $ showSDoc dflags (ppr $ Map.keys $ imp_mods $ tcg_imports env)
#else
    let pcImports = T.pack $ showSDoc dflags (ppr $ moduleEnvKeys $ imp_mods $ tcg_imports env)
#endif

    pure $ PromptContext
      { pcHoleVariable, pcHoleType, pcModule, pcLocation
      , pcImports, pcConstraints, pcKnownFits, pcGuidance
      }