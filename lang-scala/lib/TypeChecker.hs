{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TypeChecker where

import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import ScSyntax
import Debug.Trace


----------------------------
-- Scope Graph Parameters --
----------------------------

data Label
  = P -- Lexical Parent Label
  | ExplicitI -- Explicit Import Label
  | WildcardI -- Wildcard Import Label
  | VAR -- Variable Label
  | OBJ -- Object label
  deriving (Show, Eq)

data Decl
  = Decl String Type   -- Variable declaration
  | ExplicitImp String Sc -- Explicit Import declaration
  | WildcardImp String Sc -- Wildcard Import declaration
  | ObjD String Sc -- Object declaration
  deriving (Eq)


instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t
  show (ExplicitImp x s) = "Explicit Import" ++ x ++ " @ " ++ show s
  show (WildcardImp x s) = "Wildcard Import" ++ x ++ " @ " ++ show s
  show (ObjD x s) = "Object" ++ x ++ "@" ++ show s

projTy :: Decl -> Type
projTy (Decl _ t) = t
projTy (ObjD _ _) = error "Cannot project an object"
projTy (ExplicitImp _ _) = error "Cannot project an import."
projTy (WildcardImp _ _) = error "Cannot project an import."

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom VAR

-- Regular expression for variable declarations
re' :: RE Label
re' = Dot (Star (Pipe (Atom P) (Atom OBJ))) (Atom VAR)


-- Expression for Explicit imports
reExplicit :: RE Label
reExplicit = Dot (Star $ Atom P) $ (Star $ Atom ExplicitI)

-- Expression for Wildcard imports
reWildcard :: RE Label
reWildcard = Dot (Star $ Atom P) $ (Star $ Atom WildcardI)

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
matchDecl x (ExplicitImp x' _) = x == x'
matchDecl x (WildcardImp x' _) = x == x'
matchDecl x (ObjD x' _) = x == x'

------------------
-- Type Checker --
------------------

-- type check the scala program.
tcAll :: ( Functor f, Error String < f, Scope Sc Label Decl < f)
   => ScProg -> Sc -> Free f ()
tcAll p s = do
  -- Phase 1: allocate scopes
  mapM_ (`buildSG` s) p

  -- Phase 2: type check declarations
  mapM_ (`tcScDecl` s) p

  -- Function to type check Scala expressions
tcScExp ::
  (Functor f, Error String < f, Scope Sc Label Decl < f) =>
  ScExp ->
  Sc ->
  Free f Type
tcScExp (ScNum _) _ = return NumT
tcScExp (ScBool _) _ = return BoolT
tcScExp (ScId x) s = do
  ds <- query s re pShortest (matchDecl x) <&> map projTy
  case ds of
    [] -> err "No matching declarations found"
    [t] -> return t
    _ -> err "BUG: Multiple declarations found" -- cannot happen for STLC
tcScExp (ScPlus l r) s = tcBinOp l r NumT NumT s
tcScExp (ScIf cond thenBranch elseBranch) s = do
  ifBool <- tcScExp cond s
  trueBranch <- tcScExp thenBranch s
  falseBranch <- tcScExp elseBranch s
  if ifBool == BoolT then
    if trueBranch == falseBranch then return trueBranch else err "Branches need the same output type."
  else
    err "There needs to be a boolean condition."
tcScExp (ScFun (ScParam str strType) body) s = do
  let newTy = strType
  s' <- new
  edge s' P s
  sink s' VAR $ Decl str newTy
  t' <- tcScExp body s'
  return $ FunT newTy t'
tcScExp (ScApp func app) s = do
  f' <- tcScExp func s
  a' <- tcScExp app s
  case f' of
    FunT t t' | t == a' -> return t'
    FunT t _ -> err $ "Expected argument of type '" ++ show t ++ "' got '" ++ show a' ++ "'"
    _ -> err "Not a function."


-- type check binary operators
tcBinOp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScExp -> ScExp -> Type -> Type -> Sc -> Free f Type
tcBinOp l r inp out s = do
  tcL <- tcScExp l s
  tcR <- tcScExp r s
  if tcL == inp && tcR == inp then
    return out
  else
    err "Error when type checking a binary operator."

-- type check declarations
tcScDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f Type
tcScDecl (ScVal (ScParam name t) expr) s = do
  x <- query s re pShortest (matchDecl name)
  let xLength = length x
  trace ("\nLength of query: " ++ show xLength) $ return ()  -- Print the length
  -- Check if the variable exists or if it declrared more than once.
  if length x < 1  then err "The variable is not defined." 
  else if length x > 1 then err "The variable is defined more than once."
  else do
    t' <- tcScExp expr s -- need to check
    if t == t' then return (ValT name) else err "Type missmatch in val."
tcScDecl (ScDef name t expr) s = do
  x <- query s re pShortest (matchDecl name)
  if length x /= 1  then err "variable is defined more than once." 
  else do
    t' <- tcScExp expr s -- need to check
    if t == t' then return (ValT name) else err "Type missmatch in definition."
    -- return t
tcScDecl (ScObject name defs) s = do
  -- type check declarations 
  mapM_ (`tcScDecl` s) defs
  return (ObjT name)
tcScDecl (ScExplicitImport imp sImp) s = do
    ds <- query s re pShortest (matchDecl imp) <&> map projTy
    case ds of
        [] -> err $ "Imported module not found: " ++ imp
        [t] -> return t
        _ -> err "Multiple matching imports found" 
tcScDecl (ScWildcardImport wildcard sImp) s = do
    ds <- query s re pShortest (matchDecl wildcard) <&> map projTy
    case ds of
        [] -> err "No matching wildcard imports found"
        [t] -> return t
        _ -> err "Multiple matching wildcard imports found" 


-- Build the scope graph
buildSG :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f ()
buildSG (ScVal (ScParam name t) _) s = do
    let newTy = t
    sink s VAR $ Decl name newTy
buildSG (ScDef name t _) s = do
    let newTy = t
    s' <- new
    edge s' P s
    sink s' VAR $ Decl name newTy
buildSG (ScObject name defs) s = do
    -- add object declarations scope
  sObjDef <- new
  -- add obj declaration to the outer scope
  sink s OBJ $ ObjD name sObjDef
  -- add edge between object scope and outer scope (which is the parent)
  edge sObjDef P s
  -- construct all the associated scopes of the object
  mapM_ (`buildSG` sObjDef) defs
buildSG (ScExplicitImport imp sImp) s = do
   -- Create a new sink in the importing scope
    sink sImp ExplicitI $ ExplicitImp imp sImp
    -- edge sImp ExplicitI s
buildSG (ScWildcardImport wildcard sImp) s = do
   -- Create a new sink in the importing scope
    -- sink sImp VAR $ Import wildcard sImp
    -- edge sImp WildcardI s 
    edge sImp WildcardI s
    sink sImp WildcardI $ WildcardImp wildcard sImp -- Create specialized sink for wildcard imports


-- Tie it all together
runTC :: ScExp -> Either String (Type, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tcScExp e 0) emptyGraph

runTCDecl :: ScDecl -> Either String (Type, Graph Label Decl)
runTCDecl decl = un 
        $ handle hErr 
        $ handle_ hScope (tcScDecl decl 0) emptyGraph

runTCAll :: ScProg -> Either String (Graph Label Decl)
runTCAll prog = 
  let tuple = un 
        $ handle hErr
        $ handle_ hScope (tcAll prog 0
        :: Free ( Scope Sc Label Decl
                + Error String
                + Nop )
                ()
        ) emptyGraph
  in case tuple of 
      Left err -> Left err
      Right ((), sg) -> Right sg

-- tcScExp (ScObj s) _ = return $ ObjT s
    -- case op of 
    -- ScAdd -> tcBinOp l r intT intT s
    -- ScMinus -> tcBinOp l r intT intT s
    -- ScMult -> tcBinOp l r intT intT s
    -- ScDiv -> tcBinOp l r intT intT s
    -- ScEquals -> tcBinOp l r intT boolT s
    -- ScLessThan -> tcBinOp l r intT boolT s

