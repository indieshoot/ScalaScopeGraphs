{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ScSyntax where
import Free.Scope (Sc)

data Type
  = NumT
  | BoolT
  | ValT String
  | FunT Type Type
  | ObjT String
  | Unit        -- unit type for void methods
  | ImpT String -- Import type
  deriving Eq

instance Show Type where
  show (ValT i) = "α" ++ show i
  show NumT = "num"
  show BoolT = "bool"
  show (ObjT str) = "Object " ++ str  
  show (FunT ti to) = "(" ++ show ti ++ " -> " ++ show to ++ ")"
  show _ = "undefined"


-- Inspiration: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html

type ScProg = [ScDecl]
type ScProg' = [(ScDecl, Sc)]

-- ScDecl is an algebraic data type that can take on one of the forms:
data ScDecl
  = ScVal ScParam ScExp 
  | ScDef String Type ScExp
  | ScObject String [ScDecl] -- object MyObj { ... }
  | ScImport Imp
  deriving (Eq, Show)

data ScParam = ScParam String Type deriving (Eq, Show)

type ObjName = String
type VarName = String

data Imp 
  = ScEImp ObjName VarName
  | ScWImp ObjName
  deriving (Eq, Show)

-- Given an import string, extract the imported names
extractImportedNames :: String -> [String]
extractImportedNames importStr = wordsDot (=='.') (drop 1 importStr) -- Assumes importStr is of the form "import A.B.C.x"

wordsDot     :: (Char -> Bool) -> String -> [String]
wordsDot p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsDot p s''
                            where (w, s'') = break p s'



-- Subprograms in Scala examples are made of object trees.
data ObjStructure
    = SubProg ObjName [Imp] [ObjStructure] [ScDecl]
    deriving (Eq, Show)

data ObjScope
  = SubProgSc ObjName [Imp] [ObjScope] [ScDecl] Sc
  deriving (Eq, Show)

type ObjectRef = (String, Sc)

data ScExp 
  = ScId String
  | ScNum Int
  | ScBool Bool 
  | ScPlus ScExp ScExp
  | ScIf ScExp ScExp ScExp
  | ScFun ScParam ScExp    -- [ScParam] later - ScFun String [ScParam] [ScExp]
  | ScApp ScExp ScExp      -- [ScExp] later
  deriving (Eq, Show)


example :: ScExp
example = ScNum 1
 

-- data ScIdent
--   = ScIdentifier String
--   -- | ScINested ScModule String
--   deriving (Eq, Show)

-- data ScLiteral
--   = ScInt Int
--   | ScBool Bool
--   | ScString String
--   deriving (Eq, Show)

-- | ScBinOp ScExp ScOp ScExp
-- data ScOp
--   = ScAdd
--   | ScMinus
--   | ScMult
--   | ScDiv
--   | ScEquals
--   | ScLessThan
--   deriving (Eq, Show)


-- INFERENCE -- 

-- data ScType
--   = ScNum
--   | ScBoolean
--   | ScStr
--   | ScFn ScType ScType
--   | ScObjTy String
--   deriving (Eq, Show)

-- toTy :: ScType -> Ty
-- toTy ScNum = intT
-- toTy ScBoolean = boolT
-- toTy ScStr = Term "String" [] -- defined in the Scala standard library
-- toTy (ScFn f t) = funT (toTy f) (toTy t)
-- toTy (ScObjTy s) = objT s

-- intT :: Term c
-- intT = Term "Int" []
-- boolT :: Term c
-- boolT = Term "Bool" []
-- funT :: Term c -> Term c -> Term c
-- funT f t = Term "->" [f, t]
-- objT :: String -> Ty
-- objT s = Term ("MyObject_" ++ s) []
