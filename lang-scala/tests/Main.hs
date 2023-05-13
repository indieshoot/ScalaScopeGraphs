module Main where

import Test.HUnit

import Data.Either (isRight)
import ATermParser
import TypeChecker (runTC,  Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)


-- runTCTest :: ScProg -> IO (Ty, Graph Label Decl) 
-- runTCTest = either assertFailure return . runTC

runTCTest :: ScExp -> IO (Ty, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

-- Define your test cases like the following
test1 :: IO ()
test1 = do
  t <- runTCTest $ ScLit (ScInt 2)
  assertEqual "Incorrect type" intT $ fst t

test2 :: IO ()
test2 = do
  t <- runTCTest $ ScLit (ScBool True)
  assertEqual "Incorrect type" boolT $ fst t

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "test1" ~: test1 
    , "test2" ~: test2 
      , "Parsing tests" ~: parser ]

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
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess


--