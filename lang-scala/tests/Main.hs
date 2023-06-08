module Main where

import Test.HUnit

import Data.Either (isRight)
import ATermParser
import TypeChecker (Label, Decl, runTC, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)


runTCTest :: ScExp -> IO (Type, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runTCFail :: ScExp -> IO String
runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e


runTCPh :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPh = either assertFailure return . runTCPhased


-- Define your test cases like the following
test1 :: IO ()
test1 = do
  t <- runTCTest $ ScNum 1
  assertEqual "Incorrect type: not a number" NumT $ fst t

test2 :: IO ()
test2 = do
  t <- runTCTest $ ScBool True
  assertEqual "Incorrect type: not a boolean" BoolT $ fst t

-- object A {
--   val x : Int = 3
-- }
-- object B {
--   import A.x;
--   val y : Int = x
--   }

testEImp :: IO ()
testEImp = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 3)
                      -- ScVal (ScParam "x" NumT) (ScId "y")
                    ] , 
                ScObject "B" [ScEImp "A" "x"]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   val y : Bool = True
--   val x : Int = 42
-- }
-- object B {
--     import A._;
--     val y : Int = x
--   }

testWImp :: IO ()
testWImp = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScVal (ScParam "y" BoolT) (ScBool True) , 
                      ScVal (ScParam "x" NumT) (ScNum 5) 
                    ] , 
                ScObject "B" [ScWImp "A"]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT, NumT, NumT] $ fst t 


-- object A {
--   import B._;
--   val x : Int = y
-- }
-- object B {
--     import A.x;
--     val y : Int = x
--   }

testDoubleImports :: IO ()
testDoubleImports = do
  t <- runTCPh [ScObject "A" [ScWImp "B"]
                    [ 
                      ScVal (ScParam "x" NumT) (ScId "y") 
                    ] , 
                ScObject "B" [ScEImp "A" "x"]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 


  -- object A {
  --   val x : Int = 21
  -- }
  
  -- object B {
  --   val x : Int = 42
  -- }

  -- object C {
  --   import B._
  --   import A.x

  --   val y : Int = x -- queries to A == 21
  -- }

testNameClash :: IO ()
testNameClash = do
  t <- runTCPh [ScObject "A" []
                    [
                      ScVal (ScParam "x" NumT) (ScNum 21) 
                    ] , 
                ScObject "B" []
                    [ 
                      ScVal (ScParam "x" BoolT) (ScBool True)
                    ] ,
                ScObject "C" [ScWImp "B", ScEImp "A" "x"]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, BoolT, NumT] $ fst t 


tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "test1" ~: test1 
    , "test2" ~: test2 
    , "testEImp" ~: testEImp
    , "testWImp" ~: testWImp
    , "testDoubleImport" ~: testDoubleImports
    , "testNameClash" ~: testNameClash
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess