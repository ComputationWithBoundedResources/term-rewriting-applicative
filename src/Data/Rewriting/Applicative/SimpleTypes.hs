-- | Simple types for applicative rewrite systems.

module Data.Rewriting.Applicative.SimpleTypes(
         -- * Simple Types
       Type (..)
       , TypeDecl (..)
       , (:::) (..)
       , Signature
       , Env
       , lookupEnv
       , envToMap
       , envFromMap
       , extendEnv
         -- * Typed Terms
       , STTerm
       , withType
       , unType
       , typeOf
         -- ** Constructors
       , app
       , fun
       , var
         -- * Simply Typed ATRS
       , STRule (..)
       , STAtrs (..)
       , fromATRS
       )
       where

import qualified Data.Rewriting.Term as T
import qualified Data.Rewriting.Applicative.Term as AT
import qualified Data.Rewriting.Rule as R
import qualified Data.Rewriting.Rules as RS
import qualified Data.Rewriting.Substitution as S
import qualified Data.Rewriting.Substitution.Type as ST
import           Data.Rewriting.Substitution.Unify (unify)

import           Data.Rewriting.Applicative.Term (ASym (..), ATerm, atermM, aterm, AView (..))

import           Data.Rewriting.Applicative.Rule (ARule, mapSides)
import           Data.List (nub)
import           Data.Maybe (fromJust, isNothing)
import qualified Data.Map as Map
import           Control.Monad.RWS
import           Control.Monad.Except (MonadError, throwError)
import qualified Control.Monad.State as State

import           Control.Arrow (first)
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Type = Type :~> Type | BT Int deriving (Eq, Ord, Show)

data e ::: t = e ::: t

type STTerm f v = ATerm (f ::: TypeDecl) (v ::: Type)

data TypeDecl = TypeDecl { inputTypes :: [Type], outputType :: Type }
              deriving (Eq, Ord, Show)
                      
type Signature f = Map.Map f TypeDecl

newtype Env v = Env (Map.Map v Type)

-- typed term constructors

fun :: f -> Type -> [STTerm f v] -> STTerm f v
fun f tp ts = AT.fun (f ::: td) ts where
  td = TypeDecl { inputTypes = typeOf `map` ts, outputType = tp }

var :: v -> Type -> STTerm f v
var v tp = AT.var (v ::: tp)

app :: STTerm f v -> STTerm f v -> Maybe (STTerm f v)
app t1 t2 = 
  case (typeOf t1, typeOf t2) of
    (tp1 :~> _, tp1') | tp1 == tp1' -> return (AT.app t1 t2)
    _ -> Nothing

-- functions

lookupEnv :: Ord v => v -> Env v -> Maybe Type
lookupEnv v (Env m) = Map.lookup v m

extendEnv :: Ord v => Env v -> v -> Type -> Env v
extendEnv (Env m) v t = Env (Map.insert v t m)

envToMap :: Env v -> Map.Map v Type
envToMap (Env m) = m

envFromMap :: Map.Map v Type -> Env v
envFromMap = Env


withType :: (Ord v, Ord f) => Env v -> Signature f -> ATerm f v -> Maybe (STTerm f v)
withType env sig t =
  case atermM t of
    Just (TVar v) ->
      var v <$> lookupEnv v env
    Just (TFun f ts) -> do
      td <- Map.lookup f sig
      tts <- withType env sig `mapM` ts
      guard (inputTypes td == typeOf `map` tts)
      return (fun f (outputType td) tts)
    Just (t1 :@ t2) -> do
      tt1 <- withType env sig t1
      tt2 <- withType env sig t2
      app tt1 tt2
    _ -> Nothing
   

unType :: ATerm (f ::: tp) (v ::: tp') -> ATerm f v
unType = AT.amap dropAnnot dropAnnot where
  dropAnnot (e ::: _) = e

typeOf :: STTerm f v -> Type
typeOf (aterm -> TVar (_ ::: tp)) = tp
typeOf (aterm -> TFun (f ::: td) _) = outputType td
typeOf (aterm -> t1 :@ _) = 
  case typeOf t1 of
    _ :~> tp -> tp
    _ -> error "typeOf: Ill-typed simply-typed applicative term"

data STRule f v = STRule { ruleEnv     :: Env v
                         , untypedRule :: ARule f v
                         , typedRule   :: ARule (f ::: TypeDecl) (v ::: Type)
                         , ruleType    :: Type }

data STAtrs f v = STAtrs { rules :: [STRule f v]
                         , signature :: Signature f} 
    
--------------------------------------------------------------------------------
-- type inference
--------------------------------------------------------------------------------

-- unification

data TSym = TApp | TBase Int deriving (Eq, Ord, Show)

data TVar f = TFresh Int | TIn f Int | TOut f deriving (Eq, Ord, Show)
type TExp f = T.Term TSym (TVar f)

type UP f = [(TExp f, TExp f)]
type TAssign f = ST.Subst TSym (TVar f)
-- inference monad

newtype TInferM f a = 
  TInferM { runTInfer :: RWST () (UP f) Int (Either String) a }
  deriving (Applicative, Functor, Monad, MonadWriter (UP f), MonadState Int, MonadError String)

(=~) :: TExp f -> TExp f -> TInferM f ()
a =~ b = tell [(a,b)]

freshVar :: TInferM f (TVar f)
freshVar = modify succ >> (TFresh <$> get)

inType :: f -> Int -> TExp f
inType f i = T.Var (TIn f i)

outType :: f -> TExp f
outType f = T.Var (TOut f)

evalTInferM :: TInferM f a -> Either String (a, UP f)
evalTInferM m = evalRWST (runTInfer m) () 0


solveUP :: (Ord f) => UP f -> Either String (TAssign f)
solveUP [] = return (ST.fromMap Map.empty)
solveUP ((c1,c2):cs) = do
  u <- solveUP cs
  u1 <- maybe (throwError "non-unifiable") return (S.apply u c1 `unify` S.apply u c2)
  return (S.compose u1 u)


expToType :: (Ord f, MonadState (Map.Map (TVar f) Type, Int) m) => TAssign f -> TExp f -> m Type
expToType assign (T.Var v) = do
  (env, fresh) <- State.get
  case Map.lookup v env of
   Nothing ->
     case Map.lookup v (ST.toMap assign) of
      mt | isNothing mt || mt == Just (T.Var v) -> do
             State.put (Map.insert v (BT fresh) env, fresh+1)
             return (BT fresh)
      mt -> do
        tp <- expToType assign (fromJust mt)
        State.modify (first (Map.insert v tp))
        return tp
   Just tp -> return tp
expToType _ (T.Fun (TBase bt) _) = return (BT bt)
expToType assign (T.Fun TApp [t1,t2]) = do
  tp1 <- expToType assign t1
  tp2 <- expToType assign t2
  return (tp1 :~> tp2)
expToType _ _ = error "expToType: TApp supplied with wrong number of arguments"

mkEnv  :: (Ord f, Ord v, MonadState (Map.Map (TVar f) Type, Int) m) => TAssign f -> Map.Map v (TVar f) -> m (Env v)
mkEnv assign e =
  Env <$> Map.fromList <$>
  mapM (\ (v,t) -> do { tp <- expToType assign (T.Var t); return (v,tp) })
  (Map.toList e)

mkSignature  :: (Ord f, MonadState (Map.Map (TVar f) Type, Int) m) => TAssign f -> [(f,Int)] -> m (Signature f)
mkSignature assign =
  foldM (\ sig (f,ar) -> do
            ins <- mapM (expToType assign . inType f) [0..ar-1]
            out <- expToType assign (outType f)
            return (Map.insert f (TypeDecl ins out) sig))       
  Map.empty


fromATRS :: (Ord v, Ord f, Eq v) => [ARule f v] -> Either String (STAtrs f v)
fromATRS rs = do
  (erl,up) <- evalTInferM (typeRule `mapM` rs)
  assign <- solveUP up
  return $ flip State.evalState (Map.empty, 0::Int) $ do
    sig <- mkSignature assign fs 
    trs <- forM erl $ \ (rl, e) -> do
      env <- mkEnv assign e
      let trl = mapSides (fromJust . withType env sig) rl
      return STRule { ruleEnv = env
                    , untypedRule = rl
                    , typedRule = trl
                    , ruleType = typeOf (R.lhs trl)}
    return STAtrs { signature = sig, rules = trs }
  where
    fs = nub [ (f,ar) | (Sym f,ar) <- RS.funs (map (mapSides T.withArity) rs)]    
        
    typeRule rl@(R.Rule lhs rhs) = do
      let vs = nub (T.vars lhs)
      e <- foldM (\e v -> Map.insert v <$> freshVar <*> return e) Map.empty vs
      tp <- T.Var <$> freshVar
      e |- (lhs, tp)
      e |- (rhs, tp)
      return (rl,e)

    e |- (aterm -> TVar v, a) = T.Var (fromJust (Map.lookup v e)) =~ a
    e |- (aterm -> t1 :@ t2, a) = do
      b <- T.Var <$> freshVar
      e |- (t1, T.Fun TApp [b,a])
      e |- (t2, b)
    e |- (aterm -> TFun f ts, a) = do
      outType f =~ a
      mapM_ (\(i,ti) -> e |- (ti, inType f i)) (zip [0..] ts)
    _ |- _ = throwError "non-applicative term trs given"  

       
-- pretty printing

instance PP.Pretty Type where
  pretty = pp False
    where
      pp _ (BT bt) = PP.text (names !! bt)
        where names = (:[]) `map` ['A'..'Z'] ++ [show i | i <- [(1::Int)..]]
      pp paren (ta :~> tb) = encl (pp True ta PP.<+> PP.text "->" PP.<+> pp False tb)
        where encl d | paren = PP.lparen PP.<+> d PP.<+> PP.rparen
                     | otherwise = d
  
instance PP.Pretty TypeDecl where
  pretty td
    | null (inputTypes td) = PP.pretty (outputType td)
    | otherwise = PP.encloseSep PP.lbracket PP.rbracket PP.comma (map PP.pretty (inputTypes td))
                  PP.<+> PP.text "->" PP.<+> PP.pretty (outputType td)
      
instance (PP.Pretty f) => PP.Pretty (Signature f) where
  pretty sig = PP.vcat [ PP.pretty f PP.<+> PP.text "::" PP.<+> PP.pretty td | (f,td) <- Map.toList sig]


instance PP.Pretty v => PP.Pretty (Env v) where
  pretty (Env m)
    | Map.null m = PP.empty
    | otherwise = PP.hcat $ PP.punctuate (PP.text ", ")
      [ PP.pretty v PP.<+> PP.text ":" PP.<+> PP.pretty s | (v,s) <- Map.toList m ]
      
instance (PP.Pretty f, PP.Pretty v) => PP.Pretty (STRule f v) where
  pretty rl = PP.pretty (ruleEnv rl)
    PP.<+> PP.text "⊢"
    PP.</> PP.hang 2 (rule (untypedRule rl) PP.<+> PP.text ":" PP.<+> PP.pretty (ruleType rl))
    where
      rule (R.Rule l r) = AT.prettyATerm l PP.<+> PP.text "->" PP.</> AT.prettyATerm r
      
instance (PP.Pretty f, PP.Pretty v) => PP.Pretty (STAtrs f v) where
  pretty sttrs =
    PP.vcat [ PP.pretty r | r <- rules sttrs]
    PP.<$> PP.text ""
    PP.<$> PP.hang 2 (PP.text "where"
                      PP.<$> PP.pretty (signature sttrs))
