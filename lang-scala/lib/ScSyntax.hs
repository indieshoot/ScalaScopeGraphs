{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ScSyntax where

data Type
  = NumT
  | BoolT
  | ValT String
  -- | VarT String
  | FunT Type Type
  | ObjT String
  deriving Eq

instance Show Type where
  show (ValT i) = "α" ++ show i
  -- show (VarT i) = "α" ++ show i
  show NumT = "num"
  show BoolT = "bool"
  show (ObjT str) = "Object " ++ str  
  show (FunT ti to) = "(" ++ show ti ++ " -> " ++ show to ++ ")"
  show _ = "undefined"


-- Inspiration: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
type ScProg = [ScDecl]

-- ScDecl is an algebraic data type that can take on one of three forms:
data ScDecl
  = ScVal ScParam ScExp 
  | ScDef String Type ScExp
  | ScObject String [ScDecl] -- object MyObj { ... }
  -- | ScImport String -- nested later?
  -- | ScTrait String [ScParam] [ScDecl] -- similar to interfaces in Java
  deriving (Eq, Show)

data ScParam = ScParam String Type deriving (Eq, Show)

data ScExp 
  = ScId String
  | ScNum Int
  | ScBool Bool 
  | ScPlus ScExp ScExp
  | ScIf ScExp ScExp ScExp
  | ScFun ScParam ScExp -- [ScParam] later - ScFun String [ScParam] [ScExp]
  | ScApp ScExp ScExp -- [ScExp] later
  -- | ScObj String -- MyObj
  -- | ScRecord [(String, ScExp)] -- record creation = case class in Scala
  deriving (Eq, Show)
 

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

example :: ScExp
example = ScNum 1


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
