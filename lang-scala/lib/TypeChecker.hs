{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TypeChecker where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import ScSyntax
import Free.Logic.Exists
import Free.Logic.Equals
import Debug.Trace
import qualified Data.Term as T


----------------------------
-- Scope Graph Parameters --
----------------------------

data Label
  = P -- Lexical Parent Label
  | I -- Import Label
  | MOD -- Module Label
  | D -- Variable Label
  deriving (Show, Eq)

data Decl
  = Decl String Ty   -- Variable declaration
  | Module String Sc -- Module declaration
  deriving (Eq)


instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t
  show (Module x s) = "module " ++ x ++ " @ " ++ show s

projTy :: Decl -> Ty
projTy (Decl _ t) = t
projTy (Module _ _) = error "Cannot project a module"

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom D

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Path order based on Ministatix priorities.
pStatix :: PathOrder Label Decl
pStatix p1 p2 = label == LT || label == EQ
  where
    label = pStatixHelper p1 p2

pStatixHelper :: ResolvedPath Label Decl -> ResolvedPath Label Decl -> Ordering
pStatixHelper (ResolvedPath p1 _ _) (ResolvedPath p2 _ _) = comparePaths (extractPath p1) (extractPath p2)
  where
    comparePaths [] [] = EQ
    comparePaths (_:_) [] = GT
    comparePaths [] (_:_) = LT
    comparePaths (x:xs) (y:ys) = case compareLabel x y of
      Just r -> r
      Nothing -> comparePaths xs ys
    compareLabel MOD P = Just LT
    compareLabel P MOD = Just GT
    compareLabel MOD I = Just LT
    compareLabel I MOD = Just GT
    compareLabel D P = Just LT
    compareLabel P D = Just GT
    compareLabel D I = Just LT
    compareLabel I D = Just GT
    compareLabel I P = Just LT
    compareLabel P I = Just GT
    compareLabel _ _ = Nothing
    extractPath (Start _) = []
    extractPath (Step p l _) = extractPath p ++ [l]

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
matchDecl x (Module x' _) = x == x'

------------------
-- Type Checker --
------------------

-- Function to type check scala expressions
tcScExp :: (Functor f
      -- , Exists Ty < f, Equals Ty < f
      , Error String < f, Scope Sc Label Decl < f) => ScExp -> Sc -> Free f Ty
tcScExp (ScLit lit) _ = case lit of 
    ScInt _ -> return intT
    ScBool _ -> return boolT
tcScExp (ScId (ScIdentifier x)) s = do
  ds <- query s re pShortest (matchDecl x) <&> map projTy 
  case ds of
    []  -> err "No matching declarations found"
    [t] -> return t
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC
tcScExp (ScBinOp l op r) s = case op of 
    ScAdd -> tcBinOp l r intT intT s
    ScMinus -> tcBinOp l r intT intT s
    ScMult -> tcBinOp l r intT intT s
    ScDiv -> tcBinOp l r intT intT s
    ScEquals -> tcBinOp l r intT boolT s
    ScLessThan -> tcBinOp l r intT boolT s
tcScExp (ScIf cond thenBranch elseBranch) s = do
  ifBool <- tcScExp cond s
  trueBranch <- tcScExp thenBranch s
  falseBranch <- tcScExp elseBranch s
  if ifBool == boolT then
    if trueBranch == falseBranch then return trueBranch else err "Branches need the same output type."
  else err "There needs to be a boolean condition."
tcScExp (ScFun (ScParam str strType) body) s = do
  let newTy = toTy strType
  s' <- new
  edge s' P s
  sink s' D $ Decl str newTy
  t' <- tcScExp body s'
  return $ funT newTy t'
tcScExp (ScApp func app) s = do
  f' <- tcScExp func s
  a' <- tcScExp app s
  case f' of
    (T.Term "->" [from, to]) | from == a' -> return to
    (T.Term "->" _) -> err "Arguments do not match."
    _ -> err "Not function."
tcScExp (ScObj s) _ = return $ objT s

tcBinOp :: (Functor f, 
              -- Exists Ty < f, Equals Ty < f, 
              Error String < f, Scope Sc Label Decl < f) => ScExp -> ScExp -> Ty -> Ty -> Sc -> Free f Ty
tcBinOp l r inp out s = do
  tcL <- tcScExp l s
  tcR <- tcScExp r s
  if tcL == inp && tcR == inp then
    return out
  else
    err "Error when type checking a binary operator."

-- Tie it all together
runTC :: ScExp -> Either String (Ty, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tcScExp e 0) emptyGraph


