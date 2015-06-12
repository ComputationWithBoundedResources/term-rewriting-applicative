-- | Applicative Rewrite Systems 

module Data.Rewriting.Applicative.Rules (
  module Data.Rewriting.Rules
  , funs
  , funsDL
  ) where

import Data.Rewriting.Rules hiding (funs,funsDL)
import qualified Data.Rewriting.Applicative.Rule as R


funs :: [R.ARule f v] -> [f]
funs = flip funsDL []

funsDL :: [R.ARule f v] -> [f] -> [f]
funsDL rs fs = foldr R.funsDL fs rs
