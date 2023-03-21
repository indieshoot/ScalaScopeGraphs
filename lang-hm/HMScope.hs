{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module HMScope where

--import Debug.Trace

import Free

import Data.Regex
import Data.Term
import Data.List

import Free.Error
import Free.Logic.Exists
import Free.Logic.Equals
import Free.Logic.Generalize
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)

import qualified Data.Map as Map
import Syntax

-- Labels & declarations
data Label = P | D deriving (Eq, Show)
data Decl = Decl String Ty deriving (Eq, Show)

projTy (Decl _ t) = t

-- scope graph lib configuration/convenience
wildcard = Atom P `Pipe` Atom D

edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl


-- Type checker for an MLy language
tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Generalize [Int] Ty < f
      , Error String < f
      , Scope Sc Label Decl < f )
   => MLy -> Sc -> Ty -> Free f ()
tc (Num _) _ t = equals t numT
tc (Plus e1 e2) sc t = do
  equals t numT
  tc e1 sc numT
  tc e2 sc numT
tc (Abs x b) sc t = do
  s <- exists
  t' <- exists
  sc' <- new
  edge sc' P sc
  sink sc' D (Decl x s)
  tc b sc' t'
  equals t (funT s t')
tc (Ident x) sc t = do
  ds <- query
          sc
          (Star (Atom P) `Dot` Atom D)
          (\ p1 p2 -> lenRPath p1 < lenRPath p2)
          (\ (Decl y _) -> x == y)
  if length ds == 1
    then do
      dt <- instantiate @[Int] (projTy (head ds))
      equals t dt
    else if null ds
         then err $ "Query failed: unbound identifier " ++ x
         else err $ "Query yielded ambiguous binders for " ++ x
tc (App f a) sc t = do
  s <- exists
  tc f sc (funT s t)
  tc a sc s
tc (Let x e body) sc t = do
  s <- exists
  tc e sc s
  st <- inspect s
  ds <- query
          sc
          (Star wildcard `Dot` Atom D)
          (\ p1 p2 -> lenRPath p1 < lenRPath p2)
          (\ (_ :: Decl) -> True)
  st' <- generalize (concatMap (\ (Decl _ t) -> fv t) ds) st
  sc' <- new
  edge sc' P sc
  sink sc' D (Decl x st')
  tc body sc' t


-- Running the type checker
runTC :: MLy -> Either String (Graph Label Decl, Ty)
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hScope) emptyGraph
        $ flip (handle_ hEquals) Map.empty
        $ flip (handle_ hExists) 0
        $ handle (hGeneralize fv schemeT genT)
        (do t <- exists
            tc e 0 t
        :: Free ( Generalize [Int] Ty
                + Exists Ty
                + Equals Ty
                + Scope Sc Label Decl
                + Error String
                + Nop )
                () )
  in case x of
    Left s                                   -> Left s
    Right (Left (UnificationError t1 t2), _) -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right (_, u), sg)                  -> 
      let t' = inspectVar u 0
          vs = fv t'
       in Right (sg, schemeT vs t')
  where
    genT t = do
      t <- inspect t
      case t of
        Term "∀" ts -> 
            let gens = init ts
                t' = last ts 
            in do
              substs <- mapM (\case
                                Const i -> do y <- exists; return (i,y)
                                _       -> err "Bad quantifier" 
                             )
                        gens
              return $ substsIn substs t'
        _ -> return t

