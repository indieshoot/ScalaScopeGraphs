{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ScSyntax where
import Free.Scope (Sc)
import Data.List

data Type
  = NumT
  | BoolT
  | FunT [Type] Type
  | QRefT [ObjName] VarName
  | TyRef String
  | Unit -- unit type for void methods
  deriving Eq

instance Show Type where
  show NumT = "num"
  show BoolT = "bool"
  show (FunT ti to) = "(" ++ show ti ++ " -> " ++ show to ++ ")"
  show (QRefT objs varName) = intercalate "." objs ++ "." ++ varName
  show _ = "undefined"


-- Inspiration: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
type ScProg = [ScDecl]
type ScProg' = [(ScDecl, Sc)]

-- ScDecl is an algebraic data type that can take on one of the forms:
data ScDecl
  = ScVal ScParam ScExp 
  | ScType String Type
  | ScDef String [[ScParam]] RetTy [ScDecl] ScExp -- we address the multi-clause/multi-params 
  | ScObject String [Imp] [ScDecl] 
  deriving (Eq, Show)

data ScParam = ScParam String Type deriving (Eq, Show)

type RetTy = Type
type ObjName = String
type VarName = String

data Imp 
  = ScEImp [ObjName] VarName
  | ScWImp [ObjName]
  deriving (Eq, Show)

data ScExp 
  = ScId String
  | ScNum Int
  | ScBool Bool 
  | ScBinOp ScExp ScOp ScExp
  | ScIf ScExp ScExp ScExp   
  | ScApp ScExp [ScExp]   
  | ScQRef [ObjName] VarName  -- qualified references
  deriving (Eq, Show)

data ScOp
  = ScAdd
  | ScMinus
  | ScMult
  | ScDiv
  | ScEquals
  | ScLessThan
  deriving (Eq, Show)


example :: ScExp
example = ScNum 1
 


