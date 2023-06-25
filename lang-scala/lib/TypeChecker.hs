{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TypeChecker where

import Data.Functor ( (<&>) )
import Data.Regex ( RE(Atom, Empty, Pipe, Dot, Star) )
import Data.Maybe ( catMaybes )
import Data.List ( intercalate )
import Data.Data (typeOf)
import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error ( err, hErr, Error )
import ScSyntax


----------------------------
-- Scope Graph Parameters --
----------------------------

-- edge labels
data Label
  = P   -- Lexical Parent Label
  | WI  -- Wildcard Import Label
  | EI  -- Explicit Import Label
  | VAL -- Variable Label
  | TY  -- Type Label
  | OBJ -- Object Label
  | DEF -- Definition Label
  deriving (Show, Eq)

-- sink declarations
data Decl
  = Decl    String Type  -- Variable declaration
  | ObjDecl String Sc    -- Object declaration
  deriving (Eq)

instance Show Decl where
  show (Decl x t)    = x ++ " : " ++ show t
  show (ObjDecl x s) = "Object" ++ x ++ "@" ++ show s

projTy :: Decl -> Type
projTy (Decl _ t)    = t
projTy (ObjDecl _ _) = error "Cannot project an object."

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Explicit Import Resolution: (VAL|TY)
reExplImp :: RE Label
reExplImp =  Pipe (Atom VAL) (Atom TY)

-- Type Resolution: (P*WI?)(EI|TY))
reTy :: RE Label
reTy = Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom WI)) $ Pipe (Atom EI) (Atom TY) 

-- Variable Resolution: P*DEF*WI?(EI|VAL)
reImpResVar :: RE Label
reImpResVar = Dot (Dot (Dot (Star $ Atom P) (Star $ Atom DEF)) (Pipe Empty $ Atom WI)) $ Pipe (Atom EI) (Atom VAL)

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _)    = x == x'
matchDecl x (ObjDecl x' _) = x == x'

-- Object Resolution: P*OBJ
reObj :: RE Label
reObj = Dot (Star $ Atom P) $ Atom OBJ

-- Query for object scope from given scope, with given object name
queryObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> String -> Free f (Maybe Sc)
queryObj fromSc objName = do
    res <- query fromSc reObj pShortest $ matchDecl objName
    case res of
      [] -> return Nothing
      [ObjDecl _ g'] -> return $ Just g'
      _ -> err $ "There are multiple occurances of " ++ objName


------------------
-- Type Checker --
------------------

-- Type check expressions from the Scala subset
tcScExp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScExp -> Sc -> Free f Type
tcScExp (ScNum _)  _    = return NumT
tcScExp (ScBool _) _    = return BoolT
tcScExp (ScId x)   s    = do
  ds <- query s reImpResVar pShortest (matchDecl x) <&> map projTy
  case ds of
    []  -> err $ "No matching declarations found for the expression " ++ show x
    [t] -> return t
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC

tcScExp (ScBinOp l op r) s = do
  case op of 
    ScAdd    -> tcBinOp l r NumT NumT s
    ScMinus  -> tcBinOp l r NumT NumT s
    ScMult   -> tcBinOp l r NumT NumT s
    ScDiv    -> tcBinOp l r NumT NumT s
    ScEquals -> tcBinOp l r NumT BoolT s

tcScExp (ScIf cond thenBranch elseBranch) s = do
  ifBool <- tcScExp cond s
  trueBranch <- tcScExp thenBranch s
  falseBranch <- tcScExp elseBranch s
  if ifBool == BoolT
    then if trueBranch == falseBranch
           then return trueBranch
           else err "Branches need the same output type."
    else err "There needs to be a boolean condition."

tcScExp (ScApp func args) s = do
  f' <- tcScExp func s
  a' <- concat <$> mapM (mapM (`tcScExp` s)) args
  case f' of
    FunT argTypes retType ->
      if argTypes == a'
        then return retType
        else err $ "Expected arguments of types '" ++ show argTypes ++ "', got '" ++ show (map typeOf a') ++ "'"
    t -> return t

tcScExp (ScQRef objs varName) s = do 
  impSc <- queryObjChain s objs
  case impSc of
    Just s' -> do
      ds' <- query s' reExplImp pShortest (matchDecl varName) <&> map projTy
      case ds' of
        []  -> err $ "No matching declarations found for variable " ++ show varName
        [t] -> return t
        _   -> err "Multiple matching declarations found - explicit import"
    Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."

tcScExp ScUnit _ = return Unit

-- Type check binary operators
tcBinOp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScExp -> ScExp -> Type -> Type -> Sc -> Free f Type
tcBinOp l r inp out s = do
  tcL <- tcScExp l s
  tcR <- tcScExp r s
  if tcL == inp && tcR == inp then
    return out
  else
    err "Type mismatch in binary operator."


-- Type check declarations from Scala subset
tcScDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f [Type]
tcScDecl (ScVal (ScParam _ ty) expr) s = do
  case ty of
    (QRefT objs varName) -> do
      -- Resolve qualified reference
      impSc <- queryObjChain s objs
      case impSc of
        Just s' -> do
          -- Query for matching declaration in the imported scope
          ds' <- query s' reTy pShortest (matchDecl varName) <&> map projTy
          case ds' of
            []  -> err $ "No matching declarations found for variable " ++ show varName
            [t] -> do
              -- Type check the expression
              t' <- tcScExp expr s
              if t == t'
                then return [t']
                else err $ "Type-checked type: " ++ show t' ++ " vs. actual type: " ++ show t
            _   -> err "Multiple matching declarations found."
        Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."
    (TyRef varName) -> do
      -- Query for matching declaration in the current scope
      ds' <- query s reTy pShortest (matchDecl varName) <&> map projTy
      case ds' of
        []  -> err $ "No matching declarations found for variable " ++ show varName
        [t] -> do
          -- Type check the expression
          t' <- tcScExp expr s
          if t' == t
            then return [t]
            else err "The types do not match."
        _   -> err "Ambiguous reference."
    _ -> do
      -- Type check the expression with the given type
      t' <- tcScExp expr s
      if ty == t'
        then return [t']
        else err $ "Type-checked type: " ++ show t' ++ " vs. actual type: " ++ show ty

tcScDecl (ScDef _ _ t (Body decls expr)) s = do
  -- Type check the body expression
  t' <- tcScExp expr s
  -- Type check the nested declarations
  mapM_ (`tcScDecl` s) decls
  if t == t'
    then return [t']
    else err $ "Type mismatch in def with expected: " ++ show t ++ " vs. got: " ++ show t'

tcScDecl (ScObject _ defs) s = do
  -- Type check each nested declaration in the object
  concat <$> mapM (`tcScDecl` s) defs

tcScDecl (ScType _ ty) s = do
  case ty of
    (QRefT objs varName) -> do
      -- Resolve qualified reference
      impSc <- queryObjChain s objs
      case impSc of
        Just s' -> do
          -- Query for matching declaration in the imported scope
          ds' <- query s' reTy pShortest (matchDecl varName) <&> map projTy
          case ds' of
            []  -> err $ "No matching declarations found for variable " ++ show varName
            [t] -> return [t]
        Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."
    (TyRef varName) -> do
        -- Query for matching declaration
        ds' <- query s reTy pShortest (matchDecl varName) <&> map projTy
        case ds' of
          []  -> err $ "No matching declarations found for variable " ++ show varName
          [t] -> return [t]
          _   -> err "Ambiguous reference."
    _ -> return [ty]

tcScDecl _ _ = return []

----------------------
-- Phased Algorithm --
----------------------

-- MAIN PHASES
-- Step 1: Declare parent objects
-- Step 2: Declare variables
-- Step 3: Resolve imports
--       - Draw WI edges for Wildcard Imports.
--       - Copy imported names from Explicit Imports.
-- Step 4: Type-check expressions


-- Step 1: allocate objects scopes
step1 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [Sc]
step1 p s = catMaybes <$> mapM (`scopeObj` s) p

scopeObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f (Maybe Sc)
scopeObj (ScObject name  _) s = do 
      -- Create new scope for the object
      sObjDef <- new
      -- Add edge between object scope and parent scope.
      edge sObjDef P s
      -- Add object declaration
      sink s OBJ $ ObjDecl name sObjDef
      -- Return object scope
      return (Just sObjDef)
scopeObj _ _ = return Nothing


-- Step 2: Variable declaration phase
step2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f [Sc]
step2 p = concat <$> mapM varDecl p

-- Variable declaration phase for a single statement
varDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Sc]
varDecl (ScObject _ defs, s) = catMaybes <$> mapM (`declareVar` s) defs
varDecl _ = return []

-- Declare a variable in the given scope
declareVar :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f (Maybe Sc)
declareVar (ScVal (ScParam name t) _) s = do
  -- Add value sink declaration
  sink s VAL $ Decl name t
  return (Just s)

declareVar (ScDef name params returnT (Body decls _)) s = do
  s' <- new
  -- draw Parent edge
  edge s' P s
  -- draw Definition edge
  edge s DEF s'
  -- Add definition sink declaration
  sink s VAL $ Decl name returnT
  -- Declare function parameters in the function scope
  mapM_ (\(ScParam str ty) -> sink s' VAL $ Decl str ty) (concat params)
  -- Recursively declare variables in the function body
  mapM_ (`declareVar` s') decls
  return (Just s')

declareVar (ScObject name defs) s = do
  sObjDef <- new
  -- Add edge between object scope and parent scope
  edge sObjDef P s
  -- Add object declaration
  sink s OBJ $ ObjDecl name sObjDef
  -- Recursively declare variables in the object
  mapM_ (`declareVar` sObjDef) defs
  return (Just s)

declareVar (ScType name ty) s = do
  -- Add type sink declaration
  sink s TY $ Decl name ty
  return (Just s)

declareVar (ScImp (ScEImp objNames varNames)) _ = do
  -- Check for ambiguous import names
  if objNames == varNames
    then err "Ambiguous Names."
    else return Nothing

declareVar _ _ = return Nothing


-- Step 3: Import resolution
step3 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step3 = mapM_ impResExp

-- Import resolution for a single statement
impResExp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impResExp (ScObject _ defs, s) = mapM_ (`explImpt` s) defs
impResExp _ = return ()

-- Explicit import resolution for a single declaration
explImpt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f ()
explImpt (ScImp (ScEImp objName varNames)) s = do
  -- Query for the required object scope
  impSc <- queryObjChain s objName
  case impSc of
    Just s' -> do
      -- Query for the variable(s) in the obtained object scope
      ds' <- query s' reExplImp pShortest (matchDeclList varNames) <&> map projTy
      case ds' of
        [] -> err $ "No matching declarations found for " ++ show varNames
        -- Add a sink declaration for each variable imported variable
        ts -> mapM_ (drawSink s) (zip varNames ts)
    Nothing -> err $ "Object " ++ intercalate "." objName ++ " does not exist."

explImpt (ScImp (ScWImp objName)) s = do
  impSc <- queryObjChain s objName
  case impSc of
    Just s' -> do
      -- Draw an edge from s to the imported s'.
      edge s WI s'
    _ -> err $ "Object " ++ intercalate "." objName ++ " does not exist."

explImpt _ _ = return ()

-- Draw an edge with an imported declaration in the given scope
drawSink :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> (String, Type) -> Free f ()
drawSink s (varName, t) = sink s EI (Decl varName t)

-- Check if a declaration matches a list of variable names
matchDeclList :: [String] -> Decl -> Bool
matchDeclList varNames (Decl name _) = name `elem` varNames

-- Query the object chain to find the scope of an object
queryObjChain :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> [ObjName] -> Free f (Maybe Sc)
queryObjChain s [] = return (Just s)
queryObjChain s (objName : rest) = do
  impSc <- queryObj s objName
  case impSc of
    Just s' -> queryObjChain s' rest
    Nothing -> return Nothing


-- Step 4: type check
step4 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [Type]
step4 p s = do
  -- Allocate parent object scopes
  objSc <- step1 p s
  let scopedProg = zip p objSc
  
  -- Declare variables
  otherSc <- step2 scopedProg
  let scopedProg'  = zip p otherSc
  let scopedProg'' = zip p (reverse otherSc)

  -- Resolve imports
  _ <- if null otherSc
  then step3 (scopedProg' ++ scopedProg)
  else step3 scopedProg'

  -- Type check the program
  if length p == 1 then concat <$> mapM tcObj scopedProg'' -- type check most inner scope
                   else concat <$> mapM tcObj scopedProg'
  where
    tcObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Type]
    tcObj (ScObject _ defs, g) = concat <$> mapM (`tcScDecl` g) defs
    tcObj _                    = return []
  

-------------------------
-- Tie It All Together --
-------------------------

runTCPhased :: ScProg -> Either String ([Type], Graph Label Decl)
runTCPhased p = un
        $ handle hErr
        $ handle_ hScope (step4 p 0) emptyGraph
    


