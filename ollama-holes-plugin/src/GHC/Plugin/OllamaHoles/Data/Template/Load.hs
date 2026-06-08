module GHC.Plugin.OllamaHoles.Data.Template.Load
  ( loadTemplate
  ) where

import Data.Bifunctor (first)
import Control.Monad.Except
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>), isAbsolute)

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
          let resolvedPath = resolveTemplateFilePath searchDir path
          exists <- doesFileExist resolvedPath
          if exists
            then do
              body <- T.readFile resolvedPath
              pure $ parseTemplate body
            else pure $ Left $ TemplateFileNotFound resolvedPath

        NamedTemplate name -> case M.lookup name templateMap of
            Just template -> pure $ Right template
            Nothing -> pure $ Left $
                UnknownTemplateName name

resolveTemplateFilePath :: FilePath -> FilePath -> FilePath
resolveTemplateFilePath searchDir path
  | isAbsolute path = path
  | otherwise = searchDir </> path