module TypeCheck where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Syntax

----------------------------
-- Scope Graph Parameters --
----------------------------

data Label
  = P -- Lexical Parent Label
  | D -- Declaration
  deriving (Show, Eq)

data Decl
  = Decl String Type   -- Variable declaration
  deriving (Eq)


instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t

projTy :: Decl -> Type
projTy (Decl _ t) = t

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

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'

------------------
-- Type Checker --
------------------

-- Function that handles each language construct
tc :: ( Functor f
      -- List of 'capabilities' of type checker
      -- No need for inference: Disable parts related to first-order unification and generalization
      -- , Exists Type < f                   -- Introduce new meta-variables
      -- , Equals Type < f                   -- First-order unification
      -- , Generalize [Int] Type < f         -- HM-style generalization
      , Error String < f                  -- Emit String errors
      , Scope Sc Label Decl < f           -- Scope graph operations
      )
   => Expr -> Sc -> Free f Type

tc (Num _) _ = return NumT
tc (Plus e1 e2) sc = do
  t1 <- tc e1 sc
  t2 <- tc e2 sc
  case (t1, t2) of
    (NumT, NumT) -> return NumT
    (t1', NumT)  -> err $ "Expected left operand of plus expression to have type 'num', got '" ++ 
                          show t1' ++ "'"
    (NumT, t2')  -> err $ "Expected right operand of plus expression to have type 'num', got '" ++ 
                          show t2' ++ "'"
    (t1', t2')   -> err $ "Expected operands of plus expression to have type 'num', got '" ++ 
                          show t1' ++ "' and '" ++
                          show t2' ++ "'"
tc (App e1 e2) sc = do
  t1 <- tc e1 sc
  t2 <- tc e2 sc
  case t1 of
    (FunT t t') | t == t2 -> return t'
    (FunT t _)            -> err $ "Expected argument of type '" ++ show t ++ "' got '" ++ show t2 ++ "'"
    t                     -> err $ "Expected arrow type, got '" ++ show t ++ "'"
tc (Abs x t e) s = do
  s' <- new
  edge s' P s
  sink s' D $ Decl x t
  t' <- tc e s'
  return $ FunT t t'
tc (Ident x) s = do
  ds <- query s re pShortest (matchDecl x) <&> map projTy
  case ds of
    []  -> err "No matching declarations found"
    [t] -> return t
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC


-- Tie it all together
runTC :: Expr -> Either String (Type, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tc e 0) emptyGraph
