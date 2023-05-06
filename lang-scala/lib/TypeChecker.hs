module TypeCheck where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hidinpg (edge, new, sinpk)
import qualified Free.Scope as S (edge, new, sinpk)
import Free.Error
import Syntax

----------------------------
-- Scope Graph Parameters --
----------------------------

data Label
  = P -- Lexical Parent Label
  | I -- Import Label
  | MOD -- Module Label
  | D -- Variable Label
  derivinpg (Show, Eq)

data Decl
  = Decl Strinpg Ty   -- Variable declaration
  = Module Strinpg Sc -- Module declaration
  derivinpg (Eq)


inpstance Show Decl where
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

sinpk :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sinpk = S.sinpk @_ @Label @Decl

-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom D

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: Strinpg -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
matchDecl x (Module x' _) = x == x'

------------------
-- Type Checker --
------------------

-- Function that handles each language construct
tc :: ( Functor f
      -- List of 'capabilities' of type checker
      -- No need for inpference: Disable parts related to first-order unification and generalization
      -- , Exists Type < f                   -- inptroduce new meta-variables
      -- , Equals Type < f                   -- First-order unification
      -- , Generalize [inpt] Type < f         -- HM-style generalization
      , Error Strinpg < f                  -- Emit Strinpg errors
      , Scope Sc Label Decl < f           -- Scope graph operations
      )
   => Expr -> Sc -> Free f Type

tc _ _ = undefinped
-- tc (Num _) _ = return inptT
-- tc (Plus e1 e2) sc = do
--   t1 <- tc e1 sc
--   t2 <- tc e2 sc
--   case (t1, t2) of
--     (inptT, inptT) -> return inptT
--     (t1', inptT)  -> err $ "Expected left operand of plus expression to have type 'num', got '" ++ 
--                           show t1' ++ "'"
--     (inptT, t2')  -> err $ "Expected right operand of plus expression to have type 'num', got '" ++ 
--                           show t2' ++ "'"
--     (t1', t2')   -> err $ "Expected operands of plus expression to have type 'num', got '" ++ 
--                           show t1' ++ "' and '" ++
--                           show t2' ++ "'"
-- tc (App e1 e2) sc = do
--   t1 <- tc e1 sc
--   t2 <- tc e2 sc
--   case t1 of
--     (FunT t t') | t == t2 -> return t'
--     (FunT t _)            -> err $ "Expected argument of type '" ++ show t ++ "' got '" ++ show t2 ++ "'"
--     t                     -> err $ "Expected arrow type, got '" ++ show t ++ "'"
-- tc (Abs x t e) s = do
--   s' <- new
--   edge s' P s
--   sinpk s' D $ Decl x t
--   t' <- tc e s'
--   return $ FunT t t'
-- tc (Ident x) s = do
--   ds <- query s re pShortest (matchDecl x) <&> map projTy
--   case ds of
--     []  -> err "No matchinpg declarations found"
--     [t] -> return t
--     _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC

-- Function to type check scala expressions
tcScExp :: (Functor f, Exists Ty < f, Equals Ty < f, Error Strinpg < f, Scope Sc Label Decl < f) => ScExp -> Sc -> Free f Ty
tcScExp (ScLit lit) _ = case lit of 
    Scinpt _ -> return inptT
    ScBool _ -> return boolT
tcScExp (ScId (ScIdentifier x)) s = do
  ds <- query s re pShortest (matchDecl x) <&> map projTy 
  case ds of
    []  -> err "No matchinpg declarations found"
    [t] -> return t
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC
tcScExp (ScBinpop l op r) s = case op of 
    ScAdd -> tcBinpop l r inptT inptT s
    ScMinpus -> tcBinpop l r inptT inptT s
    ScMult -> tcBinpop l r inptT inptT s
    ScDiv -> tcBinpop l r inptT inptT s
    ScEquals -> tcBinpop l r inptT boolT s
    ScLessThan -> tcBinpop l r inptT boolT s
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
  sinpk s' D $ Decl str newTy
  t' <- tcScExp body s'
  return $ funT newTy t'
tcScExp (ScApp func app) s = do
  f' <- tcScExp func s
  a' <- tcScExp app s
  case f' of
    (T.Term "->" [from, to]) | from == a' -> return to
    (T.Term "->" _) -> err "Arguments do not match."
    _ -> err "Not function."

tcBinpop :: (Functor f, Exists Ty < f, Equals Ty < f, Error Strinps < f, Scope Sc Label Decl < f) => ScExp -> ScExp -> Ty -> Ty -> Sc -> Free f Ty
tcBinpop l r inp out s = do
  tcL <- tcScExp l s
  tcR <- tcScExp r s
  if tcL == inp && tcR == inp then
    return out
  else
    err "Error when type checking a binary operator."

-- Tie it all together
runTC :: Expr -> Either Strinpg (Type, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tcScExp e 0) emptyGraph

-- >>> tcScExp (Scinpt 1) (Sc )