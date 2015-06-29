module Data.Rewriting.Applicative.Problem (
  fromFile
  , module P
) where

import Data.Rewriting.Problem as P hiding (parseFileIO, parseIO, fromFile, Problem)
import qualified Data.Rewriting.Problem.Parse as PP
import qualified Data.Rewriting.Problem.Type as PT
import Data.Rewriting.Applicative.Term
import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Rule as R

type Problem f v = PT.Problem (ASym f) v

fromFile :: FilePath -> (String -> Bool) -> IO (Either P.ProblemParseError (Problem String String))
fromFile fp isApp =
  fmap (mapProblem toASym id) <$> PP.fromFile fp 
  where toASym f | isApp f = App
                 | otherwise = Sym f


mapProblem :: (f -> f') -> (v -> v') -> PT.Problem f v -> PT.Problem f' v'
mapProblem ffun fvar prob = 
   PT.Problem { PT.startTerms = PT.startTerms prob 
              , PT.strategy = PT.strategy prob
              , PT.theory = map (mapTheory ffun fvar) <$> PT.theory prob 
              , PT.rules = mapRulesPair ffun fvar (PT.rules prob)
              , PT.variables = map fvar (PT.variables prob)
              , PT.symbols = map ffun (PT.symbols prob)
              , PT.comment = PT.comment prob}

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
