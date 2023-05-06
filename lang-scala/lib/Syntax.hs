{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Data.Term

type Ty = Term Int

instance Show Ty where
  show (Const i) = "α" ++ show i
  show (Var i) = "α" ++ show i
  show (Term "->" [t1, t2]) = show t1 ++ " -> " ++ show t2
  show (Term "Int" []) = "Int"
  show (Term "Bool" []) = "Boolean"
  show _ = "undefined"

intT :: Term c
intT = Term "Int" []
boolT :: Term c
boolT = Term "Bool" []
funT :: Term c -> Term c -> Term c
funT f t = Term "->" [f, t]


-- Inspiration: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
type ScProg = ScProg [ScDecl]

-- ScDecl is an algebraic data type that can take on one of three forms:
data ScDecl
  = ScMod String [ScDecl]
  | ScImport ScModule
  | ScVal ScPat ScExp -- val x = 2 + 3
  | ScDef String [ScParam] ScExp -- def add(x: Int, y: Int): Int = x + y
  -- | ScTrait String [ScParam] [ScDecl] -- similar to interfaces in Java
  deriving (Eq, Show)

data ScParam = ScParam String ScType deriving (Eq, Show)

data ScModule
  = ScModName String
  | ScModNested ScModule String
  deriving (Eq, Show)

data ScExp 
  = ScId ScIdent
  | ScLiteral ScLiteral
  | ScBinOp ScExp ScOp ScExp
  | ScIf ScExp ScExp ScExp
  | ScFun [ScParam] ScExp
  | ScApp ScExp [ScExp]
  | ScLet ScPat ScExp ScExp
  | ScMatch ScExp [(ScPat, ScExp)]
  -- | ScRecord [(String, ScExp)] -- record creation = case class in Scala
  deriving (Eq, Show)
  --   | App ScExp ScExp
  -- | LetRec (String, ScExp) ScExp

data ScIdent
  = ScILiteral String
  | ScINested ScModule String
  deriving (Eq, Show)

data ScLiteral
  = ScInt Integer
  | ScBool Bool
  | ScString String
  deriving (Eq, Show)

data ScPat -- Scala Pattern for pattern matching
  = ScVarPat String
  | ScLitPat ScLiteral
  | ScTuplePat [ScPat]
  | ScConsPat ScPat ScPat
  -- | ScRecordPat [(String, ScPat)] -- pattern matching against records
  deriving (Eq, Show)

data ScOp
  = ScAdd
  | ScMinus
  | ScMult
  | ScDiv
  | ScEquals
  | ScLessThan
  deriving (Eq, Show)

  -- Literal           ::=  [‘-’] integerLiteral
  --                     |  [‘-’] floatingPointLiteral
  --                     |  booleanLiteral
  --                     |  characterLiteral
  --                     |  stringLiteral
  --                     |  interpolatedString
  --                     |  symbolLiteral
  --                     |  ‘null’

-- data ScFBind = ScFBind String ScExp deriving (Eq, Show)

data ScType
  = ScInt
  | ScBool
  | ScString
  | ScFn ScType ScType
  deriving (Eq, Show)

toTy :: ScType -> Ty
toTy ScInt = intT
toTy ScBool = boolT
toTy ScString = Term "String" [] -- defined in the Scala standard library
toTy (ScFn f t) = funT (toTy f) (toTy t)


-- data Expr
--   = Num Int
--   | Tru
--   | Fls
--   | Plus Expr Expr
--   | Conditional Expr Expr Expr
--   | Nil Type
--   | Cons Expr Expr
--   | Head Expr
--   | Tail Expr
--   | Tuple [Expr]
--   | Index Int Expr
--   | Let (String, Type, Expr) Expr
--   | App Expr Expr
--   | Ident String
--   | Abs String Type Expr
--   deriving (Eq, Show)
