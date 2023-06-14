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
import Data.Maybe
import Data.List
import Data.Data (typeOf)

----------------------------
-- Scope Graph Parameters --
----------------------------

-- edge labels
data Label
  = P   -- Lexical Parent Label
  | WI  -- Wildcard Import Label
  | EI  -- Explicit Import Label
  | VAR -- Variable Label
  | TY  -- Type Label
  | OBJ -- Object Label
  | DEF -- Definition Label
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

-- P*VAR
reExplImp :: RE Label
reExplImp =  Atom VAR

reTy :: RE Label
reTy = Dot (Star $ Atom P) $ Pipe (Atom TY) (Atom EI)

-- Regular expression P*WI?EIVAR
reImpResVar :: RE Label
reImpResVar = Dot (Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom DEF)) (Pipe Empty $ Atom WI)) $ Pipe (Atom EI) (Atom VAR)

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
matchDecl x (ObjDecl x' _) = x == x'

-- P*OBJ
reObj :: RE Label
reObj = Dot (Star $ Atom P) $ Star $ Atom OBJ

queryObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> String -> Free f (Maybe Sc)
queryObj fromSc toObj = do
    res <- trace ("We try to query for OBJECT: " ++ toObj ++ " from scope: " ++ show fromSc) query fromSc reObj pShortest $ matchDecl toObj
    case trace ("Queried object is: " ++ show res) res of
      [] -> return Nothing
      [ObjDecl _ g'] -> return $ Just g'
      _ -> err $ "There are multiple occurances of " ++ toObj


------------------
-- Type Checker --
------------------

-- Function to type check Scala expressions
tcScExp ::
  (Functor f, Error String < f, Scope Sc Label Decl < f) => ScExp -> Sc -> Free f Type
tcScExp (ScNum _) _ = return NumT
tcScExp (ScBool _) _ = return BoolT
tcScExp (ScId x) s = do
  ds <- trace ("We query VAL: " ++ x ++ " from scope: " ++ show s) query s reImpResVar pShortest (matchDecl x) <&> map projTy
  case trace ("The resulted queried expression is: " ++ show ds) ds of
    [] -> err "No matching declarations found - expression"
    [t] -> return t
    _ -> err "BUG: Multiple declarations found" -- cannot happen for STLC
tcScExp (ScBinOp l op r) s = do
      case op of 
        ScAdd -> tcBinOp l r NumT NumT s
        ScMinus -> tcBinOp l r NumT NumT s
        ScMult -> tcBinOp l r NumT NumT s
        ScDiv -> tcBinOp l r NumT NumT s
        ScEquals -> tcBinOp l r NumT BoolT s
        ScLessThan -> tcBinOp l r NumT BoolT s
tcScExp (ScIf cond thenBranch elseBranch) s = do
  ifBool <- tcScExp cond s
  trueBranch <- tcScExp thenBranch s
  falseBranch <- tcScExp elseBranch s
  if ifBool == BoolT then
    if trueBranch == falseBranch then return trueBranch else err "Branches need the same output type."
  else
    err "There needs to be a boolean condition."
tcScExp (ScApp func args) s = do
  f' <- tcScExp func s
  a' <- mapM (`tcScExp` s) args
  case f' of
    FunT argTypes retType ->
      if argTypes == a'
        then return retType
        else err $ "Expected arguments of types '" ++ show argTypes ++ "', got '" ++ show (map typeOf a') ++ "'"
    _ -> err "Not a function."
tcScExp (ScQRef objs varName) s = do 
  impSc <- queryObjChain s objs
  case trace ("Query explicit object:" ++ show impSc) impSc of
    Just s' -> do
      ds' <- trace ("We try to query for explicit var:" ++ varName) query s' reExplImp pShortest (matchDecl varName) <&> map projTy
      case ds' of
        [] -> err "No matching declarations found - explicit import"
        [t] -> return t
        _ -> err "Multiple matching declarations found - explicit import"
    Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."


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
tcScDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f [Type]
tcScDecl (ScVal (ScParam _ ty) expr) s = do
    case ty of
      (QRefT objs varName) -> do
        impSc <- queryObjChain s objs
        case trace ("Query explicit object:" ++ show impSc) impSc of
          Just s' -> do
            ds' <- trace ("We try to query for TYPE:" ++ varName) query s' reTy pShortest (matchDecl varName) <&> map projTy
            case ds' of
              [] -> err "No matching declarations found - explicit import"
              [t] -> do
                t' <- trace ("We type check the expr:" ++ show expr) tcScExp expr s
                if t == t' then trace ("The resulted type checking is: " ++ show t') return [t'] else err $ "Type-checked type: " ++ show t' ++ " vs. actual type: " ++ show t
              _ -> err "Multiple matching declarations found - explicit import"
          Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."
      (TyRef varName) -> do
            ds' <- trace ("We try to query for TYPE:" ++ varName) query s reTy pShortest (matchDecl varName) <&> map projTy
            case ds' of
              [] -> err "No matching declarations found - type reference"
              [t] -> return [t]
      _ -> do 
       t' <- trace ("We type check the expr:" ++ show expr) tcScExp expr s
       if ty == t' then trace ("The resulted type checking is: " ++ show t') return [t'] else err $ "Type-checked type: " ++ show t' ++ " vs. actual type: " ++ show ty
tcScDecl (ScDef name paramLists t decls expr) s = do
    t' <- trace ("We type check the expr" ++ show expr ++ " in method: " ++ name ++ " with scope: " ++ show s) tcScExp expr s
    mapM_ (`tcScDecl` s) decls
    if t == t' then trace ("The resulted type checking of method is: " ++ show t') return [t'] else err $ "Type missmatch in def with expected: " ++ show t' ++ " vs. got: " ++ show t
tcScDecl (ScObject _ _ defs) s = do
        concat <$> mapM (`tcScDecl` s) defs
tcScDecl (ScType _ ty) s = do
    case ty of
      (QRefT objs varName) -> do
        impSc <- queryObjChain s objs
        case trace ("Query explicit object:" ++ show impSc) impSc of
          Just s' -> do
            ds' <- trace ("We try to query for TYPE:" ++ varName) query s' reTy pShortest (matchDecl varName) <&> map projTy
            case ds' of
              [] -> err "No matching declarations found - explicit import"
              [t] -> return [t]
          Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."
      (TyRef varName) -> do
            ds' <- trace ("We try to query for TYPE:" ++ varName) query s reTy pShortest (matchDecl varName) <&> map projTy
            case ds' of
              [] -> err "No matching declarations found - explicit import"
              [t] -> return [t]
      _ -> return [ty]


--------------------------- FIRST IDEA --------------------------------------------
-- MAIN PHASES

-- Step 1: Declare objects
-- Step 2: Resolve and declare wildcard imports
--       - Draw WI edges for Wildcard Imports.
-- Step 3: Declare variables (Maybe switch step 2 and 3)
-- Step 4: Resolve and declare named imports
--       - Copy imported names from Explicit Imports.
-- Step 5: Type-check expressions

-- Step 1: allocate objects scopes
step1 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [Sc]
step1 p s = catMaybes <$> mapM (`scopeObj` s) p

scopeObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f (Maybe Sc)
scopeObj (ScObject name _ _) s = do 
      -- Create new scope for the object
      sObjDef <- new
      -- Add edge between object scope and parent scope.
      edge sObjDef P s
      -- Add object declaration
      trace ("Drawing declaration for OBJECT: " ++ name ++ " with scope: " ++ show sObjDef) sink s OBJ $ ObjDecl name sObjDef
      -- mapM_ (`scopeObj` sObjDef) defs
      return (Just sObjDef)
scopeObj _ _ = return Nothing


-- Step 2: add wildcard import edges to scope graph 
step2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step2 = mapM_ impResAbs

impResAbs :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impResAbs (ScObject _ imps _, s) = mapM_ (`wildImpt` s) imps -- Import individually.

wildImpt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Imp -> Sc -> Free f ()
wildImpt (ScEImp _ _) _ = return ()
wildImpt (ScWImp objName) s = do 
      impSc <- queryObjChain s objName
      case trace ("Query wildcard import:" ++ show impSc) impSc of
        Just s' -> do
          -- Draw an edge from s to the imported s'.
          trace ("Drawing edge from:" ++ show s ++ "to: " ++ show s') edge s WI s' 
        _ -> err $ "Object " ++ intercalate "." objName ++ " does not exist."

-- Step 3: Variable declaration phase
step3 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f [Sc]
step3 p = concat <$> mapM varDecl p 

varDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Sc]
varDecl (ScObject _ _ defs, s) = mapM (`declareVar` s) defs

declareVar :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f Sc
declareVar (ScVal (ScParam name t) _) s = do
        trace ("Drawing declaration for VAR: " ++ name ++ " with scope: " ++ show s) sink s VAR $ Decl name t
        return s
declareVar (ScDef name params returnT decls _) s = do
    s' <- new
    edge s' P s
    edge s DEF s'
    trace ("Drawing declaration for VAR: " ++ name ++ " with scope: " ++ show s) sink s VAR $ Decl name returnT
    mapM_ (\(ScParam str ty) -> sink s' VAR $ Decl str ty) (concat params)
    mapM_ (`declareVar` s') decls
    return s'
declareVar (ScObject name _ defs) s = do
      sObjDef <- new
      -- Add edge between object scope and parent scope.
      edge sObjDef P s
      -- Add object declaration
      trace ("Drawing declaration for OBJECT: " ++ name ++ " with scope: " ++ show sObjDef) sink s OBJ $ ObjDecl name sObjDef
      mapM_ (`declareVar` sObjDef) defs
      return s
declareVar (ScType name ty) s = do
    trace ("Drawing declaration for TYPE: " ++ name ++ " with scope: " ++ show s) sink s TY $ Decl name ty
    return s

-- Step 4: copy imported names to the importing scopes 
step4 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step4 = mapM_ impResExp

impResExp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impResExp (ScObject _ imps _, s) = mapM_ (`explImpt` s) imps

explImpt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Imp -> Sc -> Free f ()
explImpt (ScEImp objName varName) s = do
  impSc <- queryObjChain s objName
  case trace ("Query explicit object:" ++ show impSc) impSc of
    Just s' -> do
      ds' <- trace ("We try to query for explicit var:" ++ varName) query s' reExplImp pShortest (matchDecl varName) <&> map projTy
      case ds' of
        [] -> err "No matching declarations found - explicit import"
        [t] -> sink s EI $ Decl varName t
        _ -> err "Multiple matching declarations found - explicit import"
    Nothing -> err $ "Object " ++ intercalate "." objName ++ " does not exist."
explImpt (ScWImp _) _ = return ()


queryObjChain :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> [ObjName] -> Free f (Maybe Sc)
queryObjChain s [] = return (Just s)
queryObjChain s (objName : rest) = do
  impSc <- trace ("We try to query for child object: " ++ objName ++ " with scope: " ++ show s) queryObj s objName
  case impSc of
    Just s' -> trace ("Child scope is: " ++ show s') queryObjChain s' rest
    Nothing -> return Nothing

-- Step 5: type check
step5 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [Type]
step5 p s = do
  -- allocate object scopes
  objSc <- step1 p s
  let scopedProg = zip p objSc
  -- Variable declaration phase
  otherSc <- step3 scopedProg
  let scopedProg' = trace ("The scopes are: " ++ show otherSc) zip p otherSc
  let scopedProg'' = trace ("The scopes are: " ++ show otherSc) zip p (reverse otherSc)
  -- wildcard imports
  _ <- step2 scopedProg'
  -- explicit imports
  _ <- step4 scopedProg'
  -- Type check the rest
  if length p == 1 then concat <$> trace ("The final scope array: " ++ show scopedProg') mapM tcObj scopedProg'' -- we want to type check with the most inner scope
                   else concat <$> trace ("The final scope array: " ++ show scopedProg') mapM tcObj scopedProg'
  where
    tcObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Type]
    tcObj (ScObject _ _ defs, g) = concat <$> mapM (`tcScDecl` g) defs
  

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


