{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module ScSyntax where

import Data.Term

type Ty = Term Int

instance Show Ty where
  show (Const i) = "α" ++ show i
  show (Var i) = "α" ++ show i
  show (Term "->" [t1, t2]) = show t1 ++ " -> " ++ show t2
  show (Term "Num" []) = "Num"
  show (Term "Bool" []) = "Boolean"
  show _ = "undefined"

intT :: Term c
intT = Term "Int" []
boolT :: Term c
boolT = Term "Bool" []
funT :: Term c -> Term c -> Term c
funT f t = Term "->" [f, t]


-- Inspiration: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
type ScProg = [ScDecl]

-- ScDecl is an algebraic data type that can take on one of three forms:
data ScDecl
  = ScMod String [ScDecl]
  | ScImport ScModule
  | ScDef String ScExp -- [ScParam]  - def add(x: Int, y: Int): Int = x + y
  | ScObj String [ScDecl]
  -- | ScTrait String [ScParam] [ScDecl] -- similar to interfaces in Java
  deriving (Eq, Show)

data ScParam = ScParam String ScType deriving (Eq, Show)

data ScModule
  = ScModName String
  | ScModNested ScModule String
  deriving (Eq, Show)

data ScExp 
  = ScId ScIdent
  | ScLit ScLiteral
  | ScBinOp ScExp ScOp ScExp
  | ScIf ScExp ScExp ScExp
  | ScFun ScParam ScExp -- [ScParam] later
  | ScApp ScExp ScExp -- [ScExp] later
  -- | ScRecord [(String, ScExp)] -- record creation = case class in Scala
  deriving (Eq, Show)
  --   | LetRec (String, ScExp) ScExp

data ScIdent
  = ScIdentifier String
  -- | ScINested ScModule String
  deriving (Eq, Show)

data ScLiteral
  = ScInt Int
  | ScBool Bool
  | ScString String
  deriving (Eq, Show)

data ScOp
  = ScAdd
  | ScMinus
  | ScMult
  | ScDiv
  | ScEquals
  | ScLessThan
  deriving (Eq, Show)

-- data ScFBind = ScFBind String ScExp deriving (Eq, Show)

data ScType
  = ScNum
  | ScBoolean
  | ScStr
  | ScFn ScType ScType
  deriving (Eq, Show)

toTy :: ScType -> Ty
toTy ScNum = intT
toTy ScBoolean = boolT
toTy ScStr = Term "String" [] -- defined in the Scala standard library
toTy (ScFn f t) = funT (toTy f) (toTy t)


example :: ScExp
example = ScLit (ScInt 1)