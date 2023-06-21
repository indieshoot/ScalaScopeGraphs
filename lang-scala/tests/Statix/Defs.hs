module Statix.Defs where

import Test.HUnit

import Data.Either (isRight)
import TypeChecker (Label, Decl, runTC, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)


runTCFailD :: ScProg -> IO String
runTCFailD p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPhD :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPhD = either assertFailure return . runTCPhased

-- object A {
--   def A: Int = A;
-- };

testSameNameDef :: IO ()
testSameNameDef = do
  t <- runTCPhD [ScObject "x" 
                    [
                      ScDef "x"  [] NumT (Body [] (ScId "x") )
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT] $ fst t 

-- object A {
--   def f: Int = 42;
--   def f: Int = 21;
-- };

testDuplicateVal :: IO ()
testDuplicateVal = do
  t <- runTCFailD [ScObject "A" 
                    [
                      ScDef "f" [] NumT (Body [] (ScNum 42) ), 
                      ScDef "f" [] NumT (Body [] (ScNum 21) )
                    ] 
               ]
  assertEqual "Incorrect types" "Error: there is already a declaration f : num at label VAL" t

-- object O {
--   type I = Int;
--   type I = Int;
-- };

testDuplicateType :: IO ()
testDuplicateType = do
  t <- runTCFailD [ScObject "O" 
                    [
                      ScType "I" NumT, 
                      ScType "I" NumT
                    ] 
               ]
  assertEqual "Incorrect types" "Error: there is already a declaration I : num at label TY" t

-- object A {
--   def f: Int = g;
--   def g: Int = 42;
-- };

testForwardRef :: IO ()
testForwardRef = do
  t <- runTCPhD [ScObject "A" 
                    [
                      ScDef "f" [] NumT (Body [] (ScId "g") ), 
                      ScDef "g" [] NumT (Body [] (ScNum 3) )
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 


-- object O {
--   def f(x: Int)(y : Boolean): Boolean = {
--     val x2: Int = x;
--     y
--   };
-- };

testMultipleParamClauses :: IO ()
testMultipleParamClauses = do
  t <- runTCPhD [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "x" NumT], [ScParam "y" BoolT]] BoolT 
                            (Body [ScVal (ScParam "x2" NumT) (ScId "x")]  
                            (ScId "y") )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 

-- object O {
--   def f(x: Int, y: Int)(z : Boolean, m: Int): Boolean = {
--     val x2: Int = x;
--     val y2: Int = y;
--     val z2: Boolean = z;
--     val m2: Int = m;
--     z
--   };
-- };

testMultipleParamClausesMultipleParams :: IO ()
testMultipleParamClausesMultipleParams = do
  t <- runTCPhD [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "x" NumT, ScParam "y" NumT], [ScParam "z" BoolT, ScParam "m" NumT]] BoolT 
                            (Body [ScVal (ScParam "x2" NumT) (ScId "x")
                                    , ScVal (ScParam "y2" NumT) (ScId "y")
                                    , ScVal (ScParam "z2" BoolT) (ScId "z")
                                    , ScVal (ScParam "m2" NumT) (ScId "m") ]  
                            (ScId "z") )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 

-- object O {
--   def f(x: Int, y: Int)(z : Boolean, m: Int): Boolean = {
--     m
--   };
-- };

testMultipleParamClausesFail :: IO ()
testMultipleParamClausesFail = do
  t <- runTCFailD [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "x" NumT, ScParam "y" NumT], [ScParam "z" BoolT, ScParam "m" NumT]] BoolT 
                            (Body []  
                            (ScId "m") )
                    ]
                 ]
  assertEqual "Incorrect types" "Type missmatch in def with expected: bool vs. got: num" t 

-- object A {
--   def f: Int = g;
--   def g: Int = f;
-- };

testMutualDefs :: IO ()
testMutualDefs = do
  t <- runTCPhD [ScObject "A" 
                    [
                      ScDef "f" [] NumT (Body [] (ScId "g") ), 
                      ScDef "g" [] NumT (Body [] (ScId "f") )
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- nested-val-defs:
-- object O {
--   val x : Int = 42;
--   object P {
--     val x : Int = 43;
--   };
-- };

testNestedObj :: IO ()
testNestedObj = do
  t <- runTCPhD [ScObject "O" 
                    [
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScObject "P" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 43)
                        ]
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   type x = Int;
--   val x: Int = 3;
-- };

testTypeAndValOverlap :: IO ()
testTypeAndValOverlap = do
  t <- runTCPhD [ScObject "A" 
                    [ 
                      ScType "x" NumT, 
                      ScVal (ScParam "x" NumT) (ScNum 3)
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 


-- object A {
--   def f: Int = f;
-- };

testRecDefs :: IO ()
testRecDefs = do
  t <- runTCPhD [ScObject "A" 
                    [
                      ScDef "f" [] NumT (Body [] (ScId "f") ) 
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT] $ fst t 

-- object O {
--   object A {
--     type x = Int;
--     val x: Int = 3;
--   };
--   import A.x;

--   val y : x = x;
-- };

testTypeAndValOverlapImp :: IO ()
testTypeAndValOverlapImp = do
  t <- runTCPhD [ScObject "O" 
                  [ScObject "A" 
                    [ 
                      ScType "x" NumT, 
                      ScVal (ScParam "x" NumT) (ScNum 3)
                    ],
                  ScImp (ScEImp ["A"] ["x"]),
                  ScVal (ScParam "y" (TyRef "x")) (ScId "x")
                 ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT, NumT] $ fst t 

-- object O {
--   type A = Int;
-- };

testTypeDecl :: IO ()
testTypeDecl = do
  t <- runTCPhD [ScObject "O" 
                    [ 
                      ScType "A" NumT
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT] $ fst t 

-- test fails since the type takes a type on the rhs
-- object O {
--   val x : Int = 42;
--   type X = x;
-- };

-- test fails since val takes an expression on the rhs
-- object O {
--   type X = Int;
--   val x : Int = X;
-- };

-- object O {
--   type J = Int;
--   object P {
--     val x : I = 42;
--   };
-- };

testTypeNoFound :: IO ()
testTypeNoFound = do
  t <- runTCFailD [ScObject "O" 
                    [
                      ScType "J" NumT,
                      ScObject "P" 
                        [
                          ScVal (ScParam "x" (TyRef "I")) (ScNum 43)
                        ]
                    ] 
               ]
  assertEqual "Incorrect types" "No matching declarations found - type reference" t 

-- object O {
--   type I = Int;
--   object P {
--     val x : I = 42;
--   };
-- };

testTypeFound :: IO ()
testTypeFound = do
  t <- runTCPhD [ScObject "O" 
                    [
                      ScType "I" NumT,
                      ScObject "P" 
                        [
                          ScVal (ScParam "x" (TyRef "I")) (ScNum 43)
                        ]
                    ] 
               ]
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 
