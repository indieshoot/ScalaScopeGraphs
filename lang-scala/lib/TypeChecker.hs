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
  | VAL -- Variable Label
  | TY  -- Type Label
  | OBJ -- Object Label
  | DEF -- Definition Label
  | B   -- Block Imp label
  deriving (Show, Eq)

-- sink declarations
data Decl
  = Decl String Type   -- Variable declaration
  | ObjDecl String Sc  -- Object declaration
  | ChangeableDecl String Type Sc
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
reExplImp =  Pipe (Atom VAL) (Atom TY)

reTy :: RE Label
reTy = Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom WI)) $ Pipe (Atom EI) (Atom TY) 

-- (`B*(`P`B*)*(`I|`W)?`VAL)
-- (`DEF*(`P`DEF*)*(`W?)(`VAL|`EI))

-- Regular expression P*WI?EIVAR
reImpResVar :: RE Label
reImpResVar = Dot (Dot (Pipe (Atom WI) (Star $ Atom P)) (Star $ Atom DEF)) $ Pipe (Atom EI) (Atom VAL)
  -- Dot (Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom DEF)) (Pipe Empty $ Atom WI)) $ Pipe (Atom EI) (Atom VAR)


-- Make test pass
reImpResVar2 :: RE Label
reImpResVar2 = Dot (Dot (Pipe (Atom WI) (Atom P)) (Pipe Empty $ Atom DEF)) $ Pipe (Atom EI) (Atom VAL)

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
tcScExp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScExp -> Sc -> Free f Type
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
  a' <- concat <$> mapM (\argGroup -> mapM (`tcScExp` s) argGroup) args
  case f' of
    FunT argTypes retType ->
      if argTypes == a'
        then return retType
        else err $ "Expected arguments of types '" ++ show argTypes ++ "', got '" ++ show (map typeOf a') ++ "'"
    t -> return t
-- tcScExp (ScApp func args) s = do
--   f' <- tcScExp func s
--   a' <- mapM (`tcScExp` s) (concat <$> args)
--   case f' of
--     FunT argTypes retType ->
--       if argTypes == a'
--         then return retType
--         else err $ "Expected arguments of types '" ++ show argTypes ++ "', got '" ++ show (map typeOf a') ++ "'"
--     t -> return t
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
tcScExp ScUnit _ = return Unit


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
              [] -> err "No matching declarations found - qualified reference"
              [t] -> do
                t' <- trace ("We type check the expr:" ++ show expr) tcScExp expr s
                if t == t' then trace ("The resulted type checking is: " ++ show t') return [t'] else err $ "Type-checked type: " ++ show t' ++ " vs. actual type: " ++ show t
              _ -> err "Multiple matching declarations found - explicit import"
          Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."
      (TyRef varName) -> do
            ds' <- trace ("We try to query for TYPE:" ++ varName ++ "with scope: " ++ show s) query s reTy pShortest (matchDecl varName) <&> map projTy
            case trace ("TYPE REF IS: " ++ show ds') ds' of
              [] -> err "No matching declarations found - type reference"
              [t] -> do 
                t' <- trace ("We type check the expr:" ++ show expr) tcScExp expr s
                if t' == t then return [t] else err "The types do not match."
              _  -> err "Ambiguous reference."
      _ -> do 
       t' <- trace ("We type check the expr:" ++ show expr) tcScExp expr s
       if ty == t' then return [t'] else err $ "Type-checked type: " ++ show t' ++ " vs. actual type: " ++ show ty
tcScDecl (ScDef name _ t (Body decls expr)) s = do
    t' <- trace ("We type check the expr" ++ show expr ++ " in method: " ++ name ++ " with scope: " ++ show s) tcScExp expr s
    mapM_ (`tcScDecl` s) decls
    if t == t' then return [t'] else err $ "Type missmatch in def with expected: " ++ show t ++ " vs. got: " ++ show t'
tcScDecl (ScObject _  defs) s = do
        concat <$> mapM (`tcScDecl` s) defs
tcScDecl (ScType _ ty) s = do
    case ty of
      (QRefT objs varName) -> do
        impSc <- queryObjChain s objs
        case trace ("Query explicit object:" ++ show impSc) impSc of
          Just s' -> do
            ds' <- trace ("We try to query for TYPE:" ++ varName) query s' reTy pShortest (matchDecl varName) <&> map projTy
            case trace ("TYPE IS: " ++ show ds') ds' of
              [] -> err "No matching declarations found - qualified reference"
              [t] -> return [t]
          Nothing -> err $ "Object " ++ intercalate "." objs ++ " does not exist."
      (TyRef varName) -> do
            ds' <- trace ("We try to query for TYPE:" ++ varName ++ "with scope: " ++ show s) query s reTy pShortest (matchDecl varName) <&> map projTy
            case ds' of
              [] -> err "No matching declarations found - explicit import"
              [t] -> do 
                -- sink s TY $ Decl name t
                return [t]
              _  -> err "Ambiguous reference."
      _ -> return [ty]
tcScDecl _ _ = return []


--------------------------- FIRST IDEA --------------------------------------------

stepImpSc :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f [Sc]
stepImpSc p = concat <$> mapM impDecl p 

impDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Sc]
impDecl (ScObject _ defs, s) =  catMaybes <$> mapM (`declareImp` s) defs

declareImp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f (Maybe Sc)
declareImp (ScImp (ScEImp _ _)) s = do
            -- Create new scope for the object
      sImp <- new
      -- Add edge between object scope and parent scope.
      edge sImp B s
      return (Just sImp)
declareImp _ _ = return Nothing


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
scopeObj (ScObject name  _) s = do 
      -- Create new scope for the object
      sObjDef <- new
      -- Add edge between object scope and parent scope.
      edge sObjDef P s
      -- Add object declaration
      trace ("Drawing declaration for OBJECT: " ++ name ++ " with scope: " ++ show sObjDef) sink s OBJ $ ObjDecl name sObjDef
      -- mapM_ (`scopeObj` sObjDef) defs
      return (Just sObjDef)
scopeObj _ _ = return Nothing


-- stmt-ok(s_mod, s, stmt, s') :- stmt match

--  // definitions
--  { DF2TM(annot, modif, def) ->
--      def-ok(s_mod, s, def)
--      , s == s'

--  // imports
--  | IM2TM(imp) ->
--      new s'
--      , s' -[ `B ]-> s
--      , import-ok(s', s, imp)

--  }.


-- Step 2: add wildcard import edges to scope graph 
step2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step2 = mapM_ impResAbs

impResAbs :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impResAbs (ScObject _ defs, s) = mapM_ (`wildImpt` s) defs -- Import individually.
impResAbs _ = return ()

wildImpt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f ()
-- wildImpt (ScImp (ScEImp _ _) _ = return ()
wildImpt (ScImp (ScWImp objName)) s = do 
      impSc <- queryObjChain s objName
      case trace ("Query wildcard import:" ++ show impSc) impSc of
        Just s' -> do
          -- Draw an edge from s to the imported s'.
          trace ("Drawing edge from:" ++ show s ++ "to: " ++ show s') edge s WI s' 
        _ -> err $ "Object " ++ intercalate "." objName ++ " does not exist."
wildImpt _ _ = return ()

-- Step 3: Variable declaration phase
step3 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f [Sc]
step3 p = concat <$> mapM varDecl p 

varDecl :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Sc]
varDecl (ScObject _ defs, s) =  catMaybes <$> mapM (`declareVar` s) defs
varDecl _ = return []

declareVar :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f (Maybe Sc)
declareVar (ScVal (ScParam name t) _) s = do
        trace ("Drawing declaration for VAR: " ++ name ++ " with scope: " ++ show s) sink s VAL $ Decl name t
        return (Just s)
declareVar (ScDef name params returnT (Body decls _)) s = do
    s' <- new
    edge s' P s
    edge s DEF s'
    trace ("Drawing declaration for VAR: " ++ name ++ " with scope: " ++ show s) sink s VAL $ Decl name returnT
    mapM_ (\(ScParam str ty) -> sink s' VAL $ Decl str ty) (concat params)
    mapM_ (`declareVar` s') decls
    -- mapM_ (`declareVar` sc) innerDecls
    return (Just s')
declareVar (ScObject name defs) s = do
      sObjDef <- new
      -- Add edge between object scope and parent scope.
      edge sObjDef P s
      -- Add object declaration
      trace ("Drawing declaration for OBJECT: " ++ name ++ " with scope: " ++ show sObjDef) sink s OBJ $ ObjDecl name sObjDef
      mapM_ (`declareVar` sObjDef) defs
      return (Just s)
declareVar (ScType name ty) s = do
    trace ("Drawing declaration for TYPE: " ++ name ++ " with scope: " ++ show s) sink s TY $ Decl name ty
    return (Just s)
-- declareVar (ScImp (ScEImp _ _)) s  = 
  -- do 
  --     sImp <- new
  --     edge sImp B s
  --     -- mapM_ (`declareVar` sImp) defs
  --     return (Just sImp)
declareVar (ScImp (ScEImp objNames varNames)) _ = do 
    if objNames == varNames then err "Ambiguous Names." else return Nothing
declareVar _ _ = return Nothing

-- Step 4: copy imported names to the importing scopes 
step4 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step4  =  mapM_ impResExp 

impResExp :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impResExp (ScObject _ defs, s) =  mapM_ (`explImpt` s) defs
impResExp _ = return ()

explImpt :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f ()
explImpt (ScImp (ScEImp objName varNames)) s = do
    impSc <- queryObjChain s objName
    case impSc of
      Just s' -> do
        ds' <- query s' reExplImp pShortest (matchDeclList varNames) <&> map projTy
        case ds' of
          [] -> err "No matching declarations found - explicit import"
          ts -> 
          --sImp <- new
          --edge s F sImp 
            mapM_ (drawSink s) (zip varNames ts)
          --return (Just sImp)
      Nothing -> err $ "Object " ++ intercalate "." objName ++ " does not exist."
explImpt _ _ = return ()

drawSink :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> (String, Type) -> Free f ()
drawSink s (varName, t) = sink s EI (Decl varName t)

matchDeclList :: [String] -> Decl -> Bool
matchDeclList varNames (Decl name _) = name `elem` varNames

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
  
  -- scImp <- stepImpSc scopedProg
  -- let scopedProgImp = zip p scImp

  -- Variable declaration phase
  otherSc <- step3 scopedProg
  let scopedProg' = trace ("The scopes are: " ++ show otherSc) zip p otherSc
  let scopedProg'' = trace ("The scopes are: " ++ show otherSc) zip p (reverse otherSc)

  -- wildcard imports
  _ <- step2 scopedProg
  
  -- explicit imports
  _ <- step4 scopedProg'
  -- let scopedProg''' = zip p finalScopes
  -- Type check the rest
  if length p == 1 then concat <$> trace ("The final scope array: " ++ show scopedProg') mapM tcObj scopedProg'' -- we want to type check with the most inner scope
                   else concat <$> trace ("The final scope array: " ++ show scopedProg') mapM tcObj scopedProg'
  where
    tcObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Type]
    tcObj (ScObject _ defs, g) = concat <$> mapM (`tcScDecl` g) defs
    tcObj _ = return []
  

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


