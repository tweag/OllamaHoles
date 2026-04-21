{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Plugin.OllamaHoles.Candidate.Rewrite
    ( NormExpr(..)
    , RewriteRule(..)
    , RewriteStep(..)
    , RewriteMeasure(..)
    , defaultRewriteRules
    , composeAppRule
    , dollarAppRule
    , idAppRule
    , etaReduce1Rule
    , normalizeNormExpr
    , normalizeNormExprWith
    , canonicalizeNormExpr
    , rewriteMeasure
    , usesBound
    , applyMany
    ) where

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Rule representation
--------------------------------------------------------------------------------

data NormExpr
    = NFree Text
    | NBound Int
    | NApp NormExpr [NormExpr]
    | NLam Int NormExpr
    | NLit Text
    | NSectionL NormExpr NormExpr -- left operand, operator
    | NSectionR NormExpr NormExpr -- operator, right operand
    | NOther Text
    deriving (Eq, Ord, Show)

data RewriteRule = RewriteRule
    { rrName :: Text
    , rrStep :: NormExpr -> Maybe NormExpr
    }

data RewriteStep = RewriteStep
    { rsRule   :: Text
    , rsBefore :: NormExpr
    , rsAfter  :: NormExpr
    } deriving (Eq, Show)

-- | A lexicographic measure used to guarantee termination.
--
-- Rewrites are only accepted if they strictly decrease this measure.
--
-- The first component counts "administrative" operators that we want to
-- eliminate during normalization, such as (.) and ($).
-- The second component is the overall tree size.
data RewriteMeasure = RewriteMeasure
    { rmAdministrativeOps :: Int
    , rmNodeCount         :: Int
    } deriving (Eq, Ord, Show)

rewriteMeasure :: NormExpr -> RewriteMeasure
rewriteMeasure expr = RewriteMeasure
    { rmAdministrativeOps = countAdministrativeOps expr
    , rmNodeCount         = exprSize expr
    }

--------------------------------------------------------------------------------
-- Default rule set
--------------------------------------------------------------------------------

defaultRewriteRules :: [RewriteRule]
defaultRewriteRules =
    [ composeAppRule
    , dollarAppRule
    , idAppRule
    , etaReduce1Rule
    ]

-- | (f . g) x y z  ==>  f (g x) y z
composeAppRule :: RewriteRule
composeAppRule = RewriteRule
    { rrName = "compose-app"
    , rrStep = \case
        NApp (NFree ".") (f : g : x : rest) ->
            Just (applyMany f (NApp g [x] : rest))
        _ ->
            Nothing
    }

-- | ($) f x y z  ==>  f x y z
dollarAppRule :: RewriteRule
dollarAppRule = RewriteRule
    { rrName = "dollar-app"
    , rrStep = \case
        NApp (NFree "$") (f : x : rest) ->
            Just (applyMany f (x : rest))
        _ ->
            Nothing
    }

-- | id x y z  ==>  x y z
idAppRule :: RewriteRule
idAppRule = RewriteRule
    { rrName = "id-app"
    , rrStep = \case
        NApp (NFree "id") (x : rest) ->
            Just (applyMany x rest)
        _ ->
            Nothing
    }

-- | \x -> f x  ==>  f
--
-- Only when the bound variable does not occur in f.
etaReduce1Rule :: RewriteRule
etaReduce1Rule = RewriteRule
    { rrName = "eta-reduce-1"
    , rrStep = \case
        NLam 1 (NApp f [NBound 0]) | not (usesBound 0 f) ->
            Just f
        _ ->
            Nothing
    }

--------------------------------------------------------------------------------
-- Fixed-point rewriting
--------------------------------------------------------------------------------

normalizeNormExpr :: NormExpr -> (NormExpr, [RewriteStep])
normalizeNormExpr = normalizeNormExprWith defaultRewriteRules

normalizeNormExprWith :: [RewriteRule] -> NormExpr -> (NormExpr, [RewriteStep])
normalizeNormExprWith rules = go
  where
    go :: NormExpr -> (NormExpr, [RewriteStep])
    go expr0 =
        let (expr1, childSteps) = rewriteChildren rules expr0
        in case firstDecreasingRewrite rules expr1 of
            Nothing ->
                (expr1, childSteps)
            Just step ->
                let (expr2, laterSteps) = go (rsAfter step)
                in (expr2, childSteps <> [step] <> laterSteps)

canonicalizeNormExpr :: NormExpr -> NormExpr
canonicalizeNormExpr = \case
  NApp f xs ->
    applyMany
      (canonicalizeNormExpr f)
      (map canonicalizeNormExpr xs)

  NLam n body ->
    NLam n (canonicalizeNormExpr body)

  NSectionL x op ->
    NSectionL (canonicalizeNormExpr x) (canonicalizeNormExpr op)

  NSectionR op y ->
    NSectionR (canonicalizeNormExpr op) (canonicalizeNormExpr y)

  other ->
    other

--------------------------------------------------------------------------------
-- Internals
--------------------------------------------------------------------------------

rewriteChildren :: [RewriteRule] -> NormExpr -> (NormExpr, [RewriteStep])
rewriteChildren rules = \case
    NApp f xs ->
        let (f',  sf)  = normalizeNormExprWith rules f
            (xs', sxs) = normalizeList rules xs
        in (applyMany f' xs', sf <> sxs)

    NLam n body ->
        let (body', sb) = normalizeNormExprWith rules body
        in (NLam n body', sb)

    other ->
        (other, [])

normalizeList :: [RewriteRule] -> [NormExpr] -> ([NormExpr], [RewriteStep])
normalizeList rules = foldr step ([], [])
  where
    step x (xs, logs) =
        let (x', logs') = normalizeNormExprWith rules x
        in (x' : xs, logs' <> logs)

firstDecreasingRewrite :: [RewriteRule] -> NormExpr -> Maybe RewriteStep
firstDecreasingRewrite rules before = do
    RewriteRule{..} <- findApplicable rules
    after <- rrStep before
    let mb = rewriteMeasure before
        ma = rewriteMeasure after
    if ma < mb
        then Just RewriteStep
            { rsRule   = rrName
            , rsBefore = before
            , rsAfter  = after
            }
        else Nothing
    where
        findApplicable :: [RewriteRule] -> Maybe RewriteRule
        findApplicable = find
            (\RewriteRule{..} ->
                maybe False (\a -> rewriteMeasure a < rewriteMeasure before) (rrStep before))

applyMany :: NormExpr -> [NormExpr] -> NormExpr
applyMany f                []         = f
applyMany (NApp g xs)      ys         = NApp g (xs <> ys)
applyMany (NSectionL x op) (a : rest) = applyMany (NApp op [x, a]) rest
applyMany (NSectionR op y) (a : rest) = applyMany (NApp op [a, y]) rest
applyMany f                ys         = NApp f ys

usesBound :: Int -> NormExpr -> Bool
usesBound k = \case
    NBound i   -> i == k
    NApp f xs  -> usesBound k f || any (usesBound k) xs
    NLam n bod -> usesBound (k + n) bod
    _          -> False

countAdministrativeOps :: NormExpr -> Int
countAdministrativeOps = \case
    NFree "."   -> 1
    NFree "$"   -> 1
    NApp f xs   -> countAdministrativeOps f + sum (map countAdministrativeOps xs)
    NLam _ bod  -> countAdministrativeOps bod
    _           -> 0

exprSize :: NormExpr -> Int
exprSize = \case
    NFree _    -> 1
    NBound _   -> 1
    NLit _     -> 1
    NOther _   -> 1
    NApp f xs  -> 1 + exprSize f + sum (map exprSize xs)
    NLam _ bod -> 1 + exprSize bod
    NSectionL x op -> 1 + exprSize x + exprSize op
    NSectionR op y -> 1 + exprSize op + exprSize y