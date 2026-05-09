module GHC.Plugin.OllamaHoles.Data.Template.Expand
  ( expandTemplate
  , mkTemplateEnv
  ) where

import Data.Map qualified as M
import Data.Text (Text)

import GHC.Plugin.OllamaHoles.Data.Template.Types
import GHC.Plugin.OllamaHoles.Data.Template.Error



mkTemplateEnv :: [(Text, Text)] -> TemplateEnv
mkTemplateEnv = TemplateEnv . M.fromList .
    fmap (\(k,v) -> (Placeholder k, v))

lookupPlaceholder
    :: TemplateEnv -> Placeholder -> Either Placeholder Text
lookupPlaceholder (TemplateEnv m) name =
    maybe (Left name) Right (M.lookup name m)

expandTemplate
  :: Template -> TemplateEnv -> Either TemplateError Text
expandTemplate (Template exprs) env = do
    collectEithers UnknownPlaceholders mconcat $
        fmap (expandTemplateExpr env) exprs
    where
        expandTemplateExpr
            :: TemplateEnv -> TemplateExpr -> Either Placeholder Text
        expandTemplateExpr env expr = case expr of
            TemplateChunk txt -> Right txt
            TemplateVar var -> lookupPlaceholder env var

collectEithers
    :: forall a b u v. ([a] -> u) -> ([b] -> v) -> [Either a b] -> Either u v
collectEithers f g = go [] []
    where
        go :: [a] -> [b] -> [Either a b] -> Either u v
        go as bs xs = case xs of
            [] -> if null as
                then Right $ g $ reverse bs
                else Left  $ f $ reverse as
            x:rest -> case x of
                Left  a -> go (a:as) bs rest
                Right b -> go as (b:bs) rest

