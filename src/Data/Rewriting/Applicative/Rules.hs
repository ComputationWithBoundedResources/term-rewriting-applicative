-- | Applicative Rewrite Systems 

module Data.Rewriting.Applicative.Rules (
  module Data.Rewriting.Rules
  , funs
  , funsDL
  , mapRules
  ) where

import Data.Rewriting.Rules hiding (funs,funsDL)
import qualified Data.Rewriting.Applicative.Rule as R


funs :: [R.ARule f v] -> [f]
funs = flip funsDL []

funsDL :: [R.ARule f v] -> [f] -> [f]
funsDL rs fs = foldr R.funsDL fs rs

mapRules :: (f -> f') -> (v -> v') -> [R.ARule f v] -> [R.ARule f' v']
mapRules f v = map (R.mapRule f v)
