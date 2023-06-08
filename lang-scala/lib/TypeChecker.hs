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

-- edge labels
data Label
  = P   -- Lexical Parent Label
  | WI  -- Wildcard Import Label
  | EI  -- Explicit Import Label
  | VAR -- Variable Label
  | OBJ -- Object label
  deriving (Show, Eq)

-- sink declarations
data Decl
  = Decl String Type   -- Variable declaration
  | ObjDecl String Sc  -- Object declaration
  deriving (Eq)

instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t
  show (ObjDecl x s) = "Object" ++ x ++ "@" ++ show s

projTy :: Decl -> Type
projTy (Decl _ t) = t
projTy (ObjDecl _ _) = error "Cannot project an object"

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- (P|EI)*VAR
reExplImp :: RE Label
reExplImp = Dot (Star $ Pipe (Atom P) (Atom EI)) $ Atom VAR

-- P*OBJ
reObj :: RE Label
reObj = Dot (Star $ Atom P) $ Atom OBJ

-- Regular expression P*V TODO Set to P*EI?WI?VAR
reImpResVar :: RE Label
reImpResVar = Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom WI)) $ Pipe (Atom VAR) (Atom EI)

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
matchDecl x (ObjDecl x' _) = x == x'

queryObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> String -> Free f (Maybe Sc)
queryObj fromSc toObj = do
    res <- trace ("We try to query for" ++ toObj) query fromSc reObj pShortest $ matchDecl toObj
    case trace ("Query:" ++ show res) res of
      [] -> return Nothing
      [ObjDecl _ g'] -> return $ Just g'
      _ -> err $ "There are multiple occurances of " ++ toObj


------------------
-- Type Checker --
------------------

  -- Function to type check Scala expressions
tcScExp ::
  (Functor f, Error String < f, Scope Sc Label Decl < f) =>
  ScExp ->
  Sc ->
  Free f Type
tcScExp (ScNum _) _ = return NumT
tcScExp (ScBool _) _ = return BoolT
tcScExp (ScId x) s = do
  ds <- query s reImpResVar pShortest (matchDecl x) <&> map projTy
  case trace ("The resulted queried expression is: " ++ show ds) ds of
    [] -> err "No matching declarations found - expression"
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
tcScDecl (ScVal (ScParam _ t) expr) s = do
    -- sink s VAR $ Decl name t
    t' <- trace ("We type check the expr:" ++ show expr) tcScExp expr s
    if t == t' then trace ("The resulted type checking is: " ++ show t') return t' else err "Type missmatch in val."
tcScDecl (ScDef name t expr) s = do
    t' <- trace ("We type check the expr" ++ show expr ++ " in method: " ++ name ) tcScExp expr s
    if t == t' then trace ("The resulted type checking of method is: " ++ show t') return t' else err "Type missmatch in def."
tcScDecl (ScObject name _ defs) s = do
    types <- mapM (`tcScDecl` s) defs
    return (ObjT name types) 


--------------------------- FIRST IDEA --------------------------------------------
-- MAIN PHASES

-- Step 1: Declare objects
-- Step 2: Resolve and declare wildcard imports
--       - Draw WI edges for Wildcard Imports.
-- Step 3: Declare variables
-- Step 4: Resolve and declare named imports
--       - Copy imported names from Explicit Imports.
-- Step 5: Type-check expressions

-- Step 1: allocate objects scopes
step1 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [Sc]
step1 p s = mapM (`scopeObj` s) p

scopeObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f Sc
scopeObj (ScObject name _ _) s = do 
      -- Create new scope for the object
      sObjDef <- new
      -- Add edge between object scope and parent scope.
      edge sObjDef P s
      -- Add object declaration
      sink s OBJ $ ObjDecl name sObjDef
      return sObjDef


-- Step 2: add wildcard import edges to scope graph 
step2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step2 = mapM_ impResAbs

impResAbs :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impResAbs (ScObject _ imps _, s) = mapM_ (`wildImpt` s) imps -- Import individually.

wildImpt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Imp -> Sc -> Free f ()
wildImpt (ScEImp _ _) _ = return ()
wildImpt (ScWImp objName) s = do 
      impSc <- queryObj s objName
      case trace ("Query wildcard import:" ++ show impSc) impSc of
        Just s' -> do
          -- Draw an edge from s to the imported s'.
          trace ("Drawing edge from:" ++ show s ++ "to: " ++ show s') edge s WI s' 
        _ -> err $ "Object " ++ objName ++ "'does not exist."


-- Step 3: Variable declaration phase
step3 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step3 = mapM_ varDecl

varDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
varDecl (ScObject _ _ defs, s) = mapM_ (`declareVar` s) defs

declareVar :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f ()
declareVar (ScVal (ScParam name t) _) s = sink s VAR $ Decl name t
declareVar (ScDef name t _) s = do
    s' <- new
    edge s' P s
    sink s' VAR $ Decl name t
declareVar (ScObject name _ _) s = do 
    -- Create new scope for the object
    sObjDef <- new
    -- Add edge between object scope and parent scope.
    edge sObjDef P s
    -- Add object declaration
    sink s OBJ $ ObjDecl name sObjDef


-- Step 4: copy imported names to the importing scopes 
step4 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step4 = mapM_ impResExp

impResExp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impResExp (ScObject _ imps _, s) = mapM_ (`explImpt` s) imps -- Import individually.

explImpt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Imp -> Sc -> Free f ()
explImpt (ScEImp objName varName) s = do
        impSc <- queryObj s objName
        case trace ("Query explicit object:" ++ show impSc) impSc of
          Just s' -> do
              ds' <- trace ("We try to query for explicit var:" ++ varName) query s' reExplImp pShortest (matchDecl varName) <&> map projTy
              case trace ("Query explicit var:" ++ show ds') ds' of
                [] -> err "No matching declarations found - explicit import"
                [t] -> do
                -- copy name in our scope
                  sink s EI $ Decl varName t
                _ -> err "BUG: Multiple declarations found - import res"
          Nothing -> err "Import scope not found" 
explImpt (ScWImp _) _ = return ()


-- Step 5: type check
step5 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [Type]
step5 p s = do
  -- allocate object scopes
  objSc <- step1 p s
  let scopedProg = zip p objSc
  -- wildcard imports
  _ <- step2 scopedProg
  -- Variable declaration phase
  _ <- step3 scopedProg
  -- explicit imports
  _ <- step4 scopedProg
  -- Type check the rest
  concat <$> mapM tcObj scopedProg
  where
    tcObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Type]
    tcObj (ScObject _ _ defs, g) = mapM (`tcScDecl` g) defs
  

----------------------------------------RUN METHODS---------------------------------------------------

-- Tie it all together

runTCPhased :: ScProg -> Either String ([Type], Graph Label Decl)
runTCPhased p = un
        $ handle hErr
        $ handle_ hScope (step5 p 0) emptyGraph
    
runTC :: ScExp -> Either String (Type, Graph Label Decl)
runTC e = un
        $ handle hErr
        $ handle_ hScope (tcScExp e 0) emptyGraph

-- tcScExp (ScObj s) _ = return $ ObjT s
    -- case op of 
    -- ScAdd -> tcBinOp l r intT intT s
    -- ScMinus -> tcBinOp l r intT intT s
    -- ScMult -> tcBinOp l r intT intT s
    -- ScDiv -> tcBinOp l r intT intT s
    -- ScEquals -> tcBinOp l r intT boolT s
    -- ScLessThan -> tcBinOp l r intT boolT s

    -- runTCDecl :: ScDecl -> Either String (Type, Graph Label Decl)
-- runTCDecl decl = un 
--         $ handle hErr 
--         $ handle_ hScope (tcScDecl decl 0) emptyGraph

