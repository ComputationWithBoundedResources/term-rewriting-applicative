-- | Applicative Rewrite Systems 

module Data.Rewriting.Applicative.Rules (
  module Data.Rewriting.Rules
  , funs
  , funsDL
  , amap
  , applicativeArity
  ) where

import Prelude hiding (map)
import qualified Prelude as P
import qualified Data.Map as Map
import Data.Rewriting.Rules hiding (funs,funsDL)
import qualified Data.Rewriting.Applicative.Term as T
import qualified Data.Rewriting.Applicative.Rule as R


funs :: [R.ARule f v] -> [f]
funs = flip funsDL []

funsDL :: [R.ARule f v] -> [f] -> [f]
funsDL rs fs = foldr R.funsDL fs rs

amap :: (f -> f') -> (v -> v') -> [R.ARule f v] -> [R.ARule f' v']
amap f v = P.map (R.amap f v)

applicativeArity :: Ord f => [R.ARule f v] -> f -> Int
applicativeArity rs = \ f -> Map.findWithDefault 0 f m
  where
    m = Map.fromListWith max (aa [] terms)
   
    aa l [] = l
    aa l ((T.aterm -> T.TVar _ ):ts) = aa l ts    
    aa l ((T.aterm -> (T.TFun _ ts')):ts) = aa l (ts'++ts)
    aa l ((T.aformM -> Just (T.atermM -> Just (T.TFun f thd),tas)):ts) = 
      aa ((f,length tas) : l) (thd ++ tas ++ ts)
    aa l ((T.aformM -> Just (_,tas)):ts) = aa l (tas ++ ts)
    aa l (_:ts) = aa l ts
    terms = concatMap (\ (R.Rule l r) -> [l,r]) rs
