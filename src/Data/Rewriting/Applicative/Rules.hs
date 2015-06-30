-- | Applicative Rewrite Systems 

module Data.Rewriting.Applicative.Rules (
  module Data.Rewriting.Rules
  , funs
  , funsDL
  , amap
  ) where

import Prelude hiding (map)
import qualified Prelude as P
import Data.Rewriting.Rules hiding (funs,funsDL)
import qualified Data.Rewriting.Applicative.Rule as R


funs :: [R.ARule f v] -> [f]
funs = flip funsDL []

funsDL :: [R.ARule f v] -> [f] -> [f]
funsDL rs fs = foldr R.funsDL fs rs

amap :: (f -> f') -> (v -> v') -> [R.ARule f v] -> [R.ARule f' v']
amap f v = P.map (R.amap f v)
