module Main where

import Test.HUnit

import Data.Either (isRight)
import ATermParser
import TypeChecker (Label, Decl, runTC, runTCAll, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)


runTCTest :: ScExp -> IO (Type, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runTCFail :: ScExp -> IO String
runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e

runTCTestProg :: ScProg -> IO (Graph Label Decl) 
runTCTestProg = either assertFailure return . runTCAll

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
  t <- runTCPh [ScObject "A" 
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 3)
                      -- ScVal (ScParam "x" NumT) (ScId "y")
                    ] , 
                ScObject "B" 
                    [ ScImport (ScEImp "A" "x"),
                      ScVal (ScParam "y" NumT) (ScId "X")] 
                 ]
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
  t <- runTCPh [ScObject "A" 
                    [ 
                      -- ScVal (ScParam "y" BoolT) (ScBool True) , 
                      ScVal (ScParam "x" NumT) (ScNum 5) 
                    ] , 
                ScObject "B" 
                    [ ScImport (ScWImp "A"),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  assertEqual "Incorrect types" [BoolT, NumT, NumT] $ fst t 


-- object A {
--   import B._;
--   val x : Int = 5
-- }
-- object B {
--     import A.x;
--     val y : Int = x
--   }

testDoubleImports :: IO ()
testDoubleImports = do
  t <- runTCPh [ScObject "A" 
                    [ ScImport (ScWImp "B"),
                      ScVal (ScParam "x" NumT) (ScNum 5) 
                    ] , 
                ScObject "B" 
                    [ ScImport (ScEImp "A" "x"),  
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
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
  t <- runTCPh [ScObject "A" 
                    [
                      ScVal (ScParam "x" NumT) (ScNum 21) 
                    ] , 
                ScObject "B" 
                    [ 
                      ScVal (ScParam "y" NumT) (ScNum 42)
                    ] ,
                ScObject "C" 
                    [ ScImport (ScWImp "B"), 
                      ScImport (ScEImp "A" "x"),  
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
               ]
  assertEqual "Incorrect types" [NumT, NumT, NumT] $ fst t 



  
testTypeCheck :: String -> Bool -> Either String (Graph Label Decl) -> IO ()
testTypeCheck message expected res = do
  trace "Intentional behaviour" $ print' $! res
  assertEqual message expected $ isRight res
  
print' :: Either String (Graph Label Decl) -> IO ()
print' (Right g) = print g
print' (Left e) = putStrLn $ "Received error message: " ++ e

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "test1" ~: test1 
    , "test2" ~: test2 
    , "testEImp" ~: testEImp
    -- , "testWImp" ~: testWImp
    -- , "testDoubleImport" ~: testDoubleImports
    -- , "testNameClash" ~: testNameClash
    ]


-- object MyObj {
--   val x : Int = 2

--   def m(y : Int) : Int = {
--     y + 2
--   }
-- }

test3 :: IO ()
test3 = do
  testTypeCheck "Type checks" True $! runTCAll [ScObject "MyObj" [ ScVal (ScParam "x" NumT) (ScNum 2) ]]

test4 :: IO ()
test4 = do
  testTypeCheck "Type checks" True $! runTCAll [ ScVal (ScParam "x" NumT) (ScNum 2) ]

test5 :: IO ()
test5 = do
  testTypeCheck "Type checks" False $! runTCAll [ScObject "MyObj" [ ScVal (ScParam "x" NumT) (ScNum 2), 
    ScVal (ScParam "x" NumT) (ScNum 3)  ]]


-- object A {
--   val x : Int = 5
--   object B {
--     val y : Int = x
--   }
-- }

test6 :: IO ()
test6 = do
  testTypeCheck "Type checks" True $! runTCAll [ScObject "A" 
                                                [ ScVal (ScParam "x" NumT) (ScNum 5), 
                                                  ScObject "B" 
                                                    [ ScVal (ScParam "y" NumT) (ScId "x")] 
                                                ]
                                               ]


main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess

