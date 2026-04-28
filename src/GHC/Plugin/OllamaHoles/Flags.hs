module GHC.Plugin.OllamaHoles.Flags where

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Options
import GHC.Plugin.OllamaHoles.Template

-- | Helper function to interpret a @TemplateSpec@ from the flags.
mkTemplateSpec :: Flags -> Either TemplateError TemplateSpec
mkTemplateSpec flags = do
    let mkSpec source = TemplateSpec
            { tsSearchDir = template_search_dir flags
            , tsSource = source
            }
    case (template_path flags, template_name flags) of
        (Just fp, _) -> Right $ mkSpec $ TemplateFile fp
        (_, Just nm) -> fmap (mkSpec . NamedTemplate) $ parseTemplateName nm
        _            -> Right $ mkSpec $ DefaultTemplate

-- | Determine which backend to use
getBackend :: Flags -> Backend
getBackend flags = case backend_name flags of
    Gemini -> geminiBackend
    Ollama -> ollamaBackend
    OpenAI -> openAICompatibleBackend (openai_base_url flags) (openai_key_name flags)
