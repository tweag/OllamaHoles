module GHC.Plugin.OllamaHoles.Data.Template.Load
  ( loadTemplate
  ) where

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import GHC.Plugin.OllamaHoles.Data.Template.Types
import GHC.Plugin.OllamaHoles.Data.Template.Error
import GHC.Plugin.OllamaHoles.Data.Template.Parse



loadTemplate :: TemplateSpec -> IO (Either TemplateError Template)
loadTemplate spec = do
    let TemplateSpec
          { tsSearchDir = searchDir
          , tsTmplMap = templateMap
          , tsSource = source
          } = spec
    case source of
        DefaultTemplate ->
            pure (parseTemplate defaultTemplateText)

        TemplateFile path -> do
            exists <- doesFileExist path
            if exists
                then fmap parseTemplate $ T.readFile path
                else pure $ Left $ TemplateFileNotFound path

        NamedTemplate name -> case M.lookup name templateMap of
            Just template -> pure $ Right template
            Nothing -> pure $ Left $
                UnknownTemplateName name
