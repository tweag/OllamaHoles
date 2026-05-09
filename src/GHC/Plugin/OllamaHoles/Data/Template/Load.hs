module GHC.Plugin.OllamaHoles.Data.Template.Load
  ( loadTemplate
  ) where

import Data.Text (Text)
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

        NamedTemplate name' -> do
            let name = unTemplateName name'
            if T.any (not . nameSafeChar) name || T.null name
                then pure (Left $ InvalidTemplateName name)
                else do
                    let path = searchDir </> T.unpack name <> ".txt"
                    exists <- doesFileExist path
                    if exists
                        then fmap parseTemplate $ T.readFile path
                        else pure (Left (UnknownTemplateName searchDir name))
