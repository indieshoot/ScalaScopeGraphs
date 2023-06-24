{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module ScSyntax where
import Free.Scope (Sc)
import Data.List ( intercalate )

-- Inspiration for the subset: https://www.scala-lang.org/files/archive/spec/2.13/13-syntax-summary.html

--------------------
-- Type Data Type --
--------------------
data Type
  = NumT                    -- Numeral type
  | BoolT                   -- Boolean type
  | FunT  [Type] Type       -- Function type
  | QRefT [ObjName] VarName -- Qualified type reference (e.g. x : A.B.T = ...)
  | TyRef String            -- Type reference (e.g. x : T = ...)
  | Unit                    -- Unit type for void methods
  deriving Eq

-- custom Show instance for Type 
instance Show Type where
  show NumT         = "num"
  show BoolT        = "bool"
  show Unit         = "unit"
  show (FunT fr to) = "(" ++ show fr ++ " -> " ++ show to ++ ")"
  show (QRefT ob v) = intercalate "." ob ++ "." ++ v
  show (TyRef name) = name
  show _            = "undefined"

--------------------
-- Scala Program  --
--------------------
type ScProg  = [ScDecl]        -- list of declarations
type ScProg' = [(ScDecl, Sc)]  -- zipped list of declarations with the appropriate scope

----------------------------
-- Declaration Data Type  --
----------------------------
data ScDecl
  = ScObject String [ScDecl]               -- Scala object 
  | ScImp Imp                              -- Scala import
  | ScVal ScParam ScExp                    -- Scala val
  | ScType String Type                     -- Scala type alias
  | ScDef String [[ScParam]] RetTy DefBody -- Scala def
  deriving (Eq, Show)

-- Type aliases for better readability
type RetTy   = Type    -- return type
type ObjName = String  -- object name
type VarName = String  -- variable name

data ScParam = ScParam String Type deriving (Eq, Show)  -- Scala parameter (e.g. x : Int)
data DefBody = Body [ScDecl] ScExp  deriving (Eq, Show) -- Def body data type

------------------------------------
-- Explicit and Wildcard Imports  --
------------------------------------
data Imp 
  = ScEImp [ObjName] [VarName]  -- Scala explicit imports (e.g. import A.x)
  | ScWImp [ObjName]            -- Scala wildcard imports (e.g. import A._)
  deriving (Eq, Show)


--------------------------
-- Expression Data Type --
--------------------------
data ScExp 
  = ScId String               -- identifiers 
  | ScNum Int                 -- numerals
  | ScBool Bool               -- booleans
  | ScBinOp ScExp ScOp ScExp  -- binary operators
  | ScIf ScExp ScExp ScExp    -- if else blocks
  | ScApp ScExp [[ScExp]]     -- function application
  | ScQRef [ObjName] VarName  -- qualified references
  | ScUnit                    -- void return
  deriving (Eq, Show)

----------------------------
-- Scala Subset Operators --
----------------------------
data ScOp
  = ScAdd    -- (+)
  | ScMinus  -- (-)
  | ScMult   -- (*)
  | ScDiv    -- (/)
  | ScEquals -- (==)
  deriving (Eq, Show)

 


