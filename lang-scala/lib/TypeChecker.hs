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
import Data.Maybe (catMaybes)


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
  | ObjDecl String Sc -- Object declaration
  -- | ExplicitImp String Sc -- Explicit Import declaration
  deriving (Eq)

instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t
  -- show (ExplicitImp x s) = "Explicit Import" ++ x ++ " @ " ++ show s
  show (ObjDecl x s) = "Object" ++ x ++ "@" ++ show s

projTy :: Decl -> Type
projTy (Decl _ t) = t
projTy (ObjDecl _ _) = error "Cannot project an object"
-- projTy (ExplicitImp _ _) = error "Cannot project an import."

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression P*VAR
re :: RE Label
re = Dot (Star $ Atom P) $ Atom VAR

-- Import resolution (P*EI?WI?OBJ)
reImpRes :: RE Label
reImpRes = Dot (Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom EI)) (Pipe Empty $ Atom WI)) $ Atom OBJ

-- P*WI?VAR
reImpVar :: RE Label
reImpVar = Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom WI)) $ Atom VAR

-- Regular expression P*WI?OBJ
reObj :: RE Label
reObj = Dot (Dot (Star $ Atom P) (Pipe Empty $ Atom WI)) $ Atom OBJ

-- Regular expression for variable declarations
re' :: RE Label
re' = Dot (Star (Pipe (Atom P) (Atom OBJ))) (Atom VAR)

re'' :: RE Label
re'' = Dot (Star $ Atom P) $ Atom OBJ

-- Expression for Wildcard imports
reWildcard :: RE Label
reWildcard = Dot (Star $ Atom P) $ (Star $ Atom WI)

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
-- matchDecl x (ExplicitImp x' _) = x == x'
matchDecl x (ObjDecl x' _) = x == x'

queryObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => Sc -> String -> Free f (Maybe Sc)
queryObj fromSc toObj = do
    res <- trace ("We try to quert for" ++ toObj) query fromSc reImpRes pShortest $ matchDecl toObj
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
  -- x <- query s re pShortest (matchDecl name)
  -- let xLength = length x
  -- trace ("\nLength of query: " ++ show xLength) $ return ()  -- Print the length
  -- -- Check if the variable exists or if it declrared more than once.
  -- if length x < 1  then err "The variable is not defined." 
  -- else if length x > 1 then err "The variable is defined more than once."
  -- else do
    t' <- tcScExp expr s -- need to check
    -- Add the declaration to the current scope. Do NOT start a new scope.
    sink s VAR $ Decl name t
    if t == t' then return (ValT name) else err "Type missmatch in val."
tcScDecl (ScDef name t expr) s = do
  x <- query s re pShortest (matchDecl name)
  if length x /= 1  then err "variable is defined more than once." 
  else do
    t' <- tcScExp expr s -- need to check
    if t == t' then return (ValT name) else err "Type missmatch in definition."
    return t

-- I am not if I still need to deal with objects here

-- tcScDecl (ScObject name defs) s = do
--   -- type check declarations 
--   mapM_ (`tcScDecl` s) defs
--   return (ObjT name)

--------------------------- FIRST IDEA --------------------------------------------
-- MAIN PHASES

-- Step 1: Create all object scopes
-- Step 2: Deal with imports. 
--          - Copy imported names from Explicit Imports.
--          - Draw WI edges for Wildcard Imports.
-- Step 3: Declare all variables.
-- Step 4: Type-checking all initialization expressions

scopeObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f Sc
scopeObj (ScObject name _) s = do 
      -- Create new scope for the object
      sObjDef <- new
      -- Add edge between object scope and parent scope.
      edge sObjDef P s
      -- Add object declaration
      sink s OBJ $ ObjDecl name sObjDef
      return sObjDef
   
impRes :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f ()
impRes (ScImport (ScEImp objName varName), s) = do 
        impSc <- queryObj s objName
        case impSc of
          Just s' -> do
              ds' <- query s' re pShortest (matchDecl varName) <&> map projTy
              case ds' of
                [] -> err "No matching declarations found"
                [t] -> do
                -- copy name in our scope
                  sink s EI $ Decl varName t
                _ -> err "BUG: Multiple declarations found - import res"
          Nothing -> err "Import scope not found" 
impRes (ScImport (ScWImp objName), s) = do 
      impSc <- queryObj s objName
      case impSc of
        Just s' -> do
          -- Draw an edge from s to the imported s'.
          edge s WI s' 
        _ -> err $ "Object " ++ objName ++ "'does not exist."
impRes _ = err "Not an import."

-- Step 1: allocate objects scopes
step1 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [Sc]
step1 p s = mapM (`scopeObj` s) p

-- Step 2: add import edges and copy imported names to object graph. 
step2 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg' -> Free f ()
step2 = mapM_ impRes

-- Step 3: type check
step3 :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f [[Type]]
step3 p s = do
  objSc <- step1 p s
  let scopedProg = zip p objSc
  _ <- step2 scopedProg
  -- Type check the rest
  mapM tcObj scopedProg
  where
    tcObj :: (Functor f, Error String < f, Scope Sc Label Decl < f) => (ScDecl, Sc) -> Free f [Type]
    tcObj (ScObject _ es, g) = mapM (`tcScDecl` g) es
  

runTCPhased :: ScProg -> Either String ([[Type]], Graph Label Decl)
runTCPhased p = un
        $ handle hErr
        $ handle_ hScope (step3 p 0) emptyGraph


-----------------------------------------SECOND IDEA--------------------------------------------------------

-- Build the scope graph
buildSG :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScDecl -> Sc -> Free f ()
-- values
buildSG (ScVal (ScParam name t) _) s = do
    sink s VAR $ Decl name t

-- definitions
buildSG (ScDef name t _) s = do
    s' <- new
    edge s' P s
    sink s' VAR $ Decl name t

-- objects
buildSG (ScObject name defs) s = do
  -- Create new scope for the object
  sObjDef <- new
  -- Add edge between object scope and outer scope.
  edge sObjDef P s
  -- Link object to its parent scope.
  sink s OBJ $ ObjDecl name sObjDef
  -- Recursively create the scopes for the childrent within the object.
  mapM_ (`buildSG` sObjDef) defs


-- type check the Scala program.
tcAll :: ( Functor f, Error String < f, Scope Sc Label Decl < f)
   => ScProg -> Sc -> Free f ()
tcAll p s = do
  -- Phase 1: allocate scopes
  mapM_ (`buildSG` s) p

  -- Phase 2: type check declarations
  mapM_ (`tcScDecl` s) p


------------------------------------THIRD IDEA------------------------------------------------------

-- Scope Graph Construction
-- Object Hierarchal Structure - keeps track of the hierarchical order of objects in a Scala program
-- Example of possible structure: objects can contain inner objects, imports and declarations.

-- 	1) Create object hierarchy
-- 	2) Import resolution (P*EI?WI?OBJ)
-- 	3) Add declarations
--  4) Type check decl bodies, mostly based on (1)


-- Build object hierarchy
-- Need to return the scope graph for further processing -> subprogram 
objHierarchy :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ObjStructure -> Sc -> Free f ObjScope
objHierarchy (SubProg objName i rest defs) s = do
  -- New object scope
  s' <- new
  edge s' P s
  -- Create object declaration
  sink s OBJ $ ObjDecl objName s'
  -- Recursive case
  rest' <- mapM (`objHierarchy` s') rest
  -- Return the subprogram with the scope.
  return $ SubProgSc objName i rest' defs s'


-- method for import resolution here

-- Create all declarations.
addSinks :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ObjScope -> Free f [(ScExp, Sc)]
addSinks (SubProgSc _ _ rest defs s) = do
  x  <- mapM (tcDef s) defs
  xs <- concat <$> mapM addSinks rest
  return $ x ++ xs
  where
    tcDef s (ScVal (ScParam name t) expr) = do
      tcScExp expr s 
      sink s VAR $ Decl name t
      return (expr, s)
    tcDef s (ScDef name t expr)  = do
      tcScExp expr s
      s' <- new
      edge s' P s
      sink s' VAR $ Decl name t
      return (expr, s)

-- runTcObjects :: (Functor f, Error String < f, Scope Sc Label Decl < f) => ScProg -> Sc -> Free f ()
-- runTcObjects e g = do
--   -- subProgs <- createObjHierarchy
--   -- defs <- addSinks subProgs
--   mapM_ (\(g, e) -> tcScExp e g ) defs


----------------------------------------RUN METHODS---------------------------------------------------

-- Tie it all together

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

