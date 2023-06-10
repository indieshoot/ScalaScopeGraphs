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
  | ObjT String [Type]
  | Unit        -- unit type for void methods
  | ImpT -- Import type
  deriving Eq

instance Show Type where
  show (ValT i) = "α" ++ show i
  show NumT = "num"
  show BoolT = "bool"
  show (ObjT str t) = "Object " ++ str ++ " " ++ show t 
  show (FunT ti to) = "(" ++ show ti ++ " -> " ++ show to ++ ")"
  show _ = "undefined"


-- Inspiration: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html
type ScProg = [ScDecl]
type ScProg' = [(ScDecl, Sc)]

-- ScDecl is an algebraic data type that can take on one of the forms:
data ScDecl
  = ScVal ScParam ScExp 
  | ScDef String Type ScExp
  | ScObject String [Imp] [ScDecl] 
  deriving (Eq, Show)

data ScParam = ScParam String Type deriving (Eq, Show)

type ObjName = String
type VarName = String

data Imp 
  = ScEImp [ObjName] VarName
  | ScWImp ObjName
  deriving (Eq, Show)

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
