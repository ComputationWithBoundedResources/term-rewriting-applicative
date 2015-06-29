-- | Rewrite Rules in applicative form

module Data.Rewriting.Applicative.Rule (
  -- * Type
  ARule
  , Rule (..)
  -- * specific for rules in applicative form
  , rule
  , funs
  , funsDL
  , mapRule
  , mapSides

  -- * re-exported from 'Data.Rewriting.Rule'
  , prettyRule
  , vars
  , varsDL
  , left
  , right
  , rename
  , both  
  , isLinear
  , isLeftLinear
  , isRightLinear
  , isGround
  , isLeftGround
  , isRightGround
  , isErasing
  , isCreating
  , isDuplicating
  , isCollapsing
  , isExpanding
  , isValid
  , isInstanceOf
  , isVariantOf
  ) where

import qualified Data.Rewriting.Applicative.Term as T
import qualified Data.Rewriting.Rule as R

import Data.Rewriting.Rule.Type
import Data.Rewriting.Rule.Pretty (prettyRule)
import Data.Rewriting.Rule.Ops hiding (funsDL, funs)

-- | applicative rule, over signature @'Data.Rewriting.Applicative.Term.Asym' f@ that contains beside @f@ a dedicated, binary application symbol
type ARule f v = R.Rule (T.ASym f) v

rule :: T.ATerm f v -> T.ATerm f v -> ARule f v
rule = R.Rule

funs :: ARule f v -> [f]
funs = flip funsDL []

funsDL :: ARule f v -> [f] -> [f]
funsDL r = T.funsDL (lhs r) . T.funsDL (rhs r)

mapSides :: (T.Term f v -> T.Term f' v') -> R.Rule f v -> R.Rule f' v'
mapSides f r = R.Rule{ R.lhs = f (R.lhs r), R.rhs = f (R.rhs r) }

mapRule :: (f -> f') -> (v -> v') -> ARule f v -> ARule f' v'
mapRule f v = mapSides (T.amap f v)

