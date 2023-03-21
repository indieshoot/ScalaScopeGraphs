{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module AlgJ where

import Data.List
import qualified Data.Map as Map
import Data.Term

import Free
import Free.Logic.Exists
import Free.Logic.Equals
import Free.Error

import Syntax
import Data.Foldable
import System.IO.Unsafe (unsafePerformIO)

type Ctx = [(String, Scheme)]

instantiate :: (Functor f, Exists Ty < f) => Scheme -> Free f Ty
instantiate (Scheme vars t) = do
  subst <- mapM
    (\i -> do y <- exists; return (i,y))
    vars
  return $ substsIn subst t

generalize :: Ctx -> Ty -> Scheme
generalize ctx t =
  let s_fvs = fv t
      ctx_fvs = concatMap (sfv . snd) ctx
      gens = s_fvs \\ ctx_fvs
   in Scheme gens t

tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f )
     => MLy -> Ctx -> Free f Ty
tc (Num _) _ = return numT
tc (Plus e1 e2) ctx = do
  t1 <- tc e1 ctx
  equals t1 numT
  t2 <- tc e2 ctx
  equals t2 numT
  return numT
tc (Ident x) ctx = do
  case lookup x ctx of
    (Just s) -> instantiate s
    Nothing  -> err $ "Variable " ++ x ++ " not found."
tc (App e1 e2) ctx = do
  t1 <- tc e1 ctx
  t2 <- tc e2 ctx
  t' <- exists
  equals t1 $ funT t2 t'
  return t'
tc (Abs x e) ctx = do
  t  <- exists
  t' <- tc e $ (x, Scheme [] t) : ctx
  return $ funT t t'
tc (Let x e1 e2) ctx = do
  t <- tc e1 ctx
  let s  = generalize ctx t
  tc e2 ((x, s): ctx)

-- Running the type checker
runTC :: MLy -> Either String (UMap Int, Scheme)
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hExists) 1
        $ flip (handle_ hEquals) Map.empty
          ( tc e [] :: Free ( Equals Ty 
                            + Exists Ty
                            + Error String
                            + Nop) 
                            Ty
          )
  in case x of
    Left s -> Left s
    Right (Left (UnificationError t1 t2)) -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (t, u)) -> Right (u, generalize [] $ explicate u t)
