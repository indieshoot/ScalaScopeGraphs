module Statix.Statements where

import Test.HUnit

import TypeChecker (Label, Decl, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax


runTCFailS :: ScProg -> IO String
runTCFailS p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPhS :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPhS = either assertFailure return . runTCPhased

-- object O {
--   type I = Int;
--   object P {
--     val x : I = 42;
--   };
-- };

testNestedObjTy :: IO ()
testNestedObjTy  = do
  t <- runTCPhS [ScObject "O" 
                    [
                      ScType "I" NumT,
                      ScObject "P" 
                        [
                          ScVal (ScParam "x" (TyRef "I")) (ScNum 42)
                        ]
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 





-- object O {
--   def f: Int = {
--     g
--   };
  
--   def g: Int = 42;
-- };

testBodyOuterAccess :: IO ()
testBodyOuterAccess = do
  t <- runTCPhS [ScObject "O" 
                    [ 
                      ScDef "f" [] NumT (Body []
                            (ScId "g") ),
                      ScDef "g" [] NumT (Body []
                            (ScNum 42) )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 


-- object N {
--   val x : Boolean = true;
-- };
-- object O {
--   import N._;
--   def f: Boolean = {
--     x
--   };
-- };

testBodyOuterAccessImp :: IO ()
testBodyOuterAccessImp = do
  t <- runTCPhS [ScObject "N" 
                    [ 
                      ScVal (ScParam "x" BoolT) (ScBool True)
                    ], 
                ScObject "O" 
                    [ 
                      ScImp (ScWImp ["N"]),
                      ScDef "f" [] BoolT (Body []
                            (ScId "x"))
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT, BoolT] $ fst t

-- object N {
--   val x : Boolean = true;
-- };
-- object O {
--   import N._;
--   def f: Int = {
--     x
--   };
-- };

testBodyOuterAccessImpFail :: IO ()
testBodyOuterAccessImpFail = do
  t <- runTCFailS [ScObject "N" 
                    [ 
                      ScVal (ScParam "x" BoolT) (ScBool True)
                    ], 
                ScObject "O" 
                    [ 
                      ScImp (ScWImp ["N"]),
                      ScDef "f" [] NumT (Body []
                            (ScId "x"))
                    ]
                 ]
  assertEqual "Incorrect types"  "Type missmatch in def with expected: num vs. got: bool" t

-- object O {
--   def f: Int = {
--     42;
--   };
-- };

testExprStatement :: IO ()
testExprStatement = do
  t <- runTCPhS [ScObject "O" 
                    [ 
                      ScDef "f" [] NumT (Body []
                            (ScNum 42) )
                 ]
                ] 
  print $ snd t
  assertEqual "Incorrect types" [NumT] $ fst t 

-- object O {
--   def f: Unit = {
--     val x: Int = 42;
--     val x: Int = 43;
--   };
-- };

testBlockNoShadow :: IO ()
testBlockNoShadow = do
  t <- runTCFailS [ScObject "O" 
                    [ 
                      ScDef "f" [] Unit (Body [
                        ScVal (ScParam "x" NumT) (ScNum 42),
                        ScVal (ScParam "x" NumT) (ScNum 43)
                      ]
                     ScUnit )
                 ]
                ] 
  assertEqual "Incorrect types" "Error: there is already a declaration x : num at label VAL" t

-- object O {
--   def f: Int = {
--     true
--   };
-- };

testExprStatementFail :: IO ()
testExprStatementFail = do
  t <- runTCFailS [ScObject "O" 
                    [ 
                      ScDef "f" [] NumT (Body []
                            (ScBool True) )
                 ]
                ] 
  assertEqual "Incorrect types" "Type missmatch in def with expected: num vs. got: bool" t 

-- object O {
--   def f: Boolean = {
--     val x: Int = 42;
--     val y: Boolean = true;
--     val z: Int = x;
--     y
--   };
-- };

testMExprStatement :: IO ()
testMExprStatement = do
  t <- runTCPhS [ScObject "O" 
                    [ 
                      ScDef "f" [] BoolT (Body [
                        ScVal (ScParam "x" NumT) (ScNum 42),
                        ScVal (ScParam "y" BoolT) (ScBool True),
                        ScVal (ScParam "z" NumT) (ScId "x")
                      ]
                            (ScId "y") )
                 ]
                ] 
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 


testTypeAliaChain :: IO ()
testTypeAliaChain = do
  t <- runTCPhS [ScObject "A" 
                    [ 
                      ScType "X" NumT,
                      ScType "Y" (TyRef "X")
                      -- ScType "Z" (TyRef "Y"),
                      -- ScVal (ScParam "x" (TyRef "Z")) (ScNum 42)
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t