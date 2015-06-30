module Data.Rewriting.Applicative.Problem (
  fromFile
  , module P
) where

import Data.Rewriting.Problem as P hiding (parseFileIO, parseIO, fromFile, Problem)
import qualified Data.Rewriting.Problem.Parse as PP
import qualified Data.Rewriting.Problem.Type as PT
import Data.Rewriting.Applicative.Term

type Problem f v = PT.Problem (ASym f) v

fromFile :: FilePath -> (String -> Bool) -> IO (Either P.ProblemParseError (Problem String String))
fromFile fp isApp =
  fmap (P.map toASym id) <$> PP.fromFile fp 
  where toASym f | isApp f = App
                 | otherwise = Sym f


