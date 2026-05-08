module GHC.Plugin.OllamaHoles.Flags where

import GHC.Plugin.OllamaHoles.Backend
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

-- | Determine which backend to use
getBackend :: Flags -> Backend
getBackend flags = case backend_name flags of
    Nothing     -> ollamaBackend $ OllamaConfig Nothing
    Just Gemini -> geminiBackend $ GeminiConfig "GEMINI_API_KEY"
    Just Ollama -> ollamaBackend $ OllamaConfig Nothing
    Just OpenAI -> openAICompatibleBackend $ OpenAIConfig (maybe "https://api.openai.com" id $ openai_base_url flags) (maybe "OPENAI_API_KEY" id $ openai_key_name flags)
