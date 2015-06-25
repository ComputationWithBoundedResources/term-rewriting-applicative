module Data.Rewriting.Applicative.Problem (
  fromFile
  , module P
) where

import qualified Data.Rewriting.Problem as P hiding (parseFileIO, parseIO, fromFile)
import qualified Data.Rewriting.Problem.Parse as PP
import Data.Rewriting.Applicative.Term
import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Rule as R

type Problem f v = P.Problem (ASym f) v

fromFile :: FilePath -> String -> IO (Either P.ProblemParseError (Problem String String))
fromFile fp appsym = 
  fmap (mapProblem toASym id) <$> PP.fromFile fp 
  where toASym f | f == appsym = App
                 | otherwise = Sym f


mapProblem :: (f -> f') -> (v -> v') -> P.Problem f v -> P.Problem f' v'
mapProblem ffun fvar prob = 
   P.Problem { P.startTerms = P.startTerms prob 
             , P.strategy = P.strategy prob
             , P.theory = map (mapTheory ffun fvar) <$> P.theory prob 
             , P.rules = mapRulesPair ffun fvar (P.rules prob)
             , P.variables = map fvar (P.variables prob)
             , P.symbols = map ffun (P.symbols prob)
             , P.comment = P.comment prob}


mapTheory :: (f -> f') -> (v -> v') -> P.Theory f v -> P.Theory f' v'
mapTheory ffun _ (P.SymbolProperty p fs) = P.SymbolProperty p (map ffun fs)
mapTheory ffun fvar (P.Equations rs) = P.Equations (map (mapRule ffun fvar) rs)

mapRule :: (f -> f') -> (v -> v') -> R.Rule f v -> R.Rule f' v'
mapRule ffun fvar rl = R.Rule (modify (R.lhs rl)) (modify (R.rhs rl)) where
    modify = T.map ffun fvar


mapRulesPair :: (f -> f') -> (v -> v') -> P.RulesPair f v -> P.RulesPair f' v'
mapRulesPair ffun fvar rp = 
    P.RulesPair { P.strictRules = modify (P.strictRules rp)
                , P.weakRules = modify (P.weakRules rp)}
        where modify = map (mapRule ffun fvar)
