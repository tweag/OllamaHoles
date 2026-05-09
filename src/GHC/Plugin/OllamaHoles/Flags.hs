module GHC.Plugin.OllamaHoles.Flags where

import GHC.Plugin.OllamaHoles.Data.Flags
import GHC.Plugin.OllamaHoles.Template

-- | Helper function to interpret a @TemplateSpec@ from the flags.
mkTemplateSpec :: Flags -> Either TemplateError TemplateSpec
mkTemplateSpec flags = do
    let mkSpec source = TemplateSpec
            { tsSearchDir = maybe "." id $ template_search_dir flags
            , tsSource = source
            }
    case (template_path flags, template_name flags) of
        (Just fp, _) -> Right $ mkSpec $ TemplateFile fp
        (_, Just nm) -> Right $ mkSpec $ NamedTemplate nm
        _            -> Right $ mkSpec $ DefaultTemplate
