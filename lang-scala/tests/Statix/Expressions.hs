module Statix.Expressions where

import Test.HUnit

import TypeChecker (Label, Decl, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax

runTCFailE :: ScProg -> IO String
runTCFailE p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPhE :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPhE = either assertFailure return . runTCPhased

-- object O {
--   def f(g : Int => Boolean): Int => Boolean = g;
-- };

testFunctionValRef :: IO ()
testFunctionValRef = do
  t <- runTCPhE [ScObject "A" 
                    [
                      ScDef "f" [[ScParam "g" (FunT [NumT] BoolT)]] (FunT [NumT] BoolT) 
                          (Body []  (ScId "g") )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [FunT [NumT] BoolT] $ fst t 

-- object O {
--   def f(g : Int => Boolean)(a : Int): Boolean = g(true);
-- };

testFunctionCallNo :: IO ()
testFunctionCallNo = do
  t <- runTCFailE [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "g" (FunT [NumT] BoolT)], [ScParam "a" NumT]] BoolT (Body []
                            (ScApp (ScId "g") [[ScBool True]]) )
                    ]
                 ]
  assertEqual "Incorrect types" "Expected arguments of types '[num]', got '[Type]'" t 

-- object O {
--   def f(g : Int => Boolean)(a : Int): Boolean = g(a);
-- };

testFunctionCall :: IO ()
testFunctionCall = do
  t <- runTCPhE [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "g" (FunT [NumT] BoolT)], [ScParam "a" NumT]] BoolT (Body []
                            (ScApp (ScId "g") [[ScId "a"]]) )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 

-- object O {
--   def f(g : Int => Boolean): Int => Boolean = f;
-- };

testFunctionRef :: IO ()
testFunctionRef = do
  t <- runTCFailE [ScObject "O" 
                    [ 
                      ScDef "f" [[ScParam "g" (FunT [NumT] BoolT)]] (FunT [NumT] BoolT) (Body []
                            (ScApp (ScId "f") []) )
                    ]
                 ]
  assertEqual "Incorrect types" "Expected arguments of types '[num]', got '[]'" t 

-- object O {
--   def f(x: Int)(y: Boolean): Boolean = y;
--   def g: Boolean = f(42)(true); // currying correct
-- };

-- object O {
--   def f(x: Int, y: Boolean): Boolean = y;
--   def g: Boolean = f(42, true); // currying correct
-- };

testCurry :: IO ()
testCurry = do
  t <- runTCPhE [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "x" NumT], [ScParam "y" BoolT]] BoolT (Body []
                            (ScId "y") ),
                      ScDef "g" [] BoolT (Body []
                            (ScApp (ScId "f") [[ScNum 42], [ScBool True]]) )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT, BoolT] $ fst t 

-- object O {
--   def f(x : Int): Int = x;
--   def g: Int = f(42);
-- };

testSingleCurry :: IO ()
testSingleCurry = do
  t <- runTCPhE [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "x" NumT]] NumT (Body []
                            (ScId "x") ),
                      ScDef "g" [] NumT (Body []
                            (ScApp (ScId "f") [[ScNum 42]]) )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object O {
--   def f(x : Int): Int = x;
--   def g: Boolean = f(42);
-- };

testSingleCurryFail :: IO ()
testSingleCurryFail = do
  t <- runTCFailE [ScObject "A" 
                    [ 
                      ScDef "f" [[ScParam "x" NumT]] NumT (Body []
                            (ScId "x") ),
                      ScDef "g" [] BoolT (Body []
                            (ScApp (ScId "f") [[ScNum 42]]) )
                    ]
                 ]
  assertEqual "Incorrect types" "Type missmatch in def with expected: bool vs. got: num" t 

-- object O {
--   def f: Int = x;
--   def g: Int = f(); // no automatic conversion
-- };

testZeroCurryFail :: IO ()
testZeroCurryFail = do
  t <- runTCFailE [ScObject "O" 
                    [ 
                      ScDef "f" [] NumT (Body []
                            (ScId "x") ),
                      ScDef "g" [] NumT (Body []
                            (ScApp (ScId "f") []) )
                    ]
                 ]
  assertEqual "Incorrect types" "No matching declarations found - expression" t 