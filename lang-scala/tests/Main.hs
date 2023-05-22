module Main where

import Test.HUnit

import Data.Either (isRight)
import ATermParser
import TypeChecker (runTC,  Label, Decl, runTCDecl)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)


-- runTCTest :: ScProg -> IO (Ty, Graph Label Decl) 
-- runTCTest = either assertFailure return . runTC

runTCTest :: ScExp -> IO (Type, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runTCFail :: ScExp -> IO String
runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e

runTCTestObj :: ScDecl -> IO (Type, Graph Label Decl) 
runTCTestObj = either assertFailure return . runTCDecl
-- Define your test cases like the following
test1 :: IO ()
test1 = do
  t <- runTCTest $ ScNum 1
  assertEqual "Incorrect type: not a number" NumT $ fst t

test2 :: IO ()
test2 = do
  t <- runTCTest $ ScBool True
  assertEqual "Incorrect type: not a boolean" BoolT $ fst t


-- object MyObj {
--   val x : Int = 2

--   def m(y : Int) : Int = {
--     y + 2
--   }
-- }


test3 :: IO ()
test3 = do
  t <- runTCTestObj $ ScObject "MyObj" [ ScVal (ScParam "x" NumT) (ScNum 2) ]
  assertEqual "Incorrect type" (ObjT "MyObj") $ fst t
  

test4 :: IO ()
test4 = do
  t <- runTCTestObj $ ScObject "MyObj"
    [ ScVal (ScParam "x" NumT) (ScNum 42) , 
    ScDef "m" (FunT NumT NumT) (ScFun (ScParam "y" NumT) (ScPlus (ScId "y") (ScNum 2)))]
  assertEqual "Incorrect type" (ObjT "MyObj") $ fst t

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "test1" ~: test1 
    , "test2" ~: test2 
    , "test3" ~: test3 
    , "test4" ~: test4]
    -- , "Parsing tests" ~: parser ]

parser :: Test
parser = TestList
  [ "./aterm-res/scala/simple/empty.aterm" ~: testP1 
  , "./aterm-res/scala/simple/missing-def.no.aterm" ~: testP2 
  , "./aterm-res/scala/simple/param-shadows-def.aterm" ~: testP3 
  , "./aterm-res/scala/simple/param-shadows-def.no.aterm" ~: testP4 
  , "./aterm-res/scala/simple/rec-defs.aterm" ~: testP5 
  , "./aterm-res/scala/simple/seq-defs.aterm" ~: testP6
  , "./aterm-res/scala/simple/type-mismatch.no.aterm" ~: testP7 ]

testP1 :: IO ()
testP1 = runParseTest "./aterm-res/scala/empty.aterm"

testP2 :: IO ()
testP2 = runParseTest "./aterm-res/scala/simple/missing-def.no.aterm"

testP3 :: IO ()
testP3 = runParseTest "./aterm-res/scala/simple/param-shadows-def.aterm"

testP4 :: IO ()
testP4 = runParseTest "./aterm-res/scala/simple/param-shadows-def.no.aterm"

testP5 :: IO ()
testP5 = runParseTest "./aterm-res/scala/simple/rec-defs.aterm"

testP6 :: IO ()
testP6 = runParseTest "./aterm-res/scala/simple/seq-defs.aterm"

testP7 :: IO ()
testP7 = runParseTest "./aterm-res/scala/simple/type-mismatch.no.aterm"


runParseTest :: String -> IO ()
runParseTest s = do
  p <- parse s
  print p
  assertEqual "It parses" (isRight p) True


main :: IO ()
-- main = do
--     result <- runTestTT tests
--     print result
--     if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess
