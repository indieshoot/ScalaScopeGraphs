module Main where

import Test.HUnit

import Data.Either (isRight)
import ATermParser
import TypeChecker (runTC,  Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)

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
    , "test2" ~: test2]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess


-- module Main where

-- import Test.HUnit

-- import Data.Either (isRight)
-- import ATermParser
-- import TypeChecker (runTC,  Label, Decl)
-- import qualified System.Exit as Exit
-- import Free.Scope (Graph)
-- import Syntax
-- import Debug.Trace (trace)

-- runTCTest :: ScProg -> IO (Ty, Graph Label Decl) 
-- runTCTest = either assertFailure return . runTC

-- runTCFail :: ScProg -> IO String
-- runTCFail e = either return (const $ assertFailure "Expected exception, got none") $ runTC e

-- -- Define your test cases like the following
-- test1 :: IO ()
-- test1 = do
--   assertEqual "Incorrect type" 1 1

-- tests :: Test
-- tests = TestList
--   [ "Trial and error tests" ~: trialAndError
--   -- , "End to end tests" ~: e2e
--   , "Parsing tests" ~: parser
--   , "Basis tests" ~: basis ]

-- trialAndError :: Test
-- trialAndError = TestList []

-- parser :: Test
-- parser = TestList
--   [ "./aterm-res/scala/simple/empty.aterm" ~: testP1
--   , "./aterm-res/scala/simple/missing-def.no.aterm" ~: testP2
--   , "./aterm-res/scala/simple/param-shadows-def.aterm" ~: testP3
--   , "./aterm-res/scala/simple/param-shadows-def.no.aterm" ~: testP4
--   , "./aterm-res/scala/simple/rec-defs.aterm" ~: testP5
--   , "./aterm-res/scala/simple/rec-function-def.aterm" ~: testP6
--   , "./aterm-res/scala/simple/rec-function-letrec.aterm" ~: testP7
--   , "./aterm-res/scala/simple/seq-defs.aterm" ~: testP8
--   , "./aterm-res/scala/simple/type-mismatch.no.aterm" ~: testP9 ]

-- -- e2e :: Test
-- -- e2e = TestList
-- --   [ "./aterm-res/scala/empty.aterm" ~: testE2E1
-- --   , "./aterm-res/scala/simple/missing-def.no.aterm" ~: testE2E2
-- --   , "./aterm-res/scala/simple/param-shadows-def.aterm" ~: testE2E3
-- --   , "./aterm-res/scala/simple/param-shadows-def.no.aterm" ~: testE2E4
-- --   , "./aterm-res/scala/simple/rec-defs.aterm" ~: testE2E5
-- --   , "./aterm-res/scala/simple/rec-function-def.aterm" ~: testE2E6
-- --   , "./aterm-res/scala/simple/rec-function-letrec.aterm" ~: testE2E7
-- --   , "./aterm-res/scala/simple/seq-defs.aterm" ~: testE2E8
-- --   , "./aterm-res/scala/simple/type-mismatch.no.aterm" ~: testE2E9 ]

-- basis :: Test
-- basis = TestList
--   [ "./aterm-res/scala/simple/missing-def.no.aterm" ~: testB1
--   , "./aterm-res/scala/simple/param-shadows-def.aterm" ~: testB2
--   , "./aterm-res/scala/simple/param-shadows-def.no.aterm" ~: testB3
--   , "./aterm-res/scala/simple/rec-defs.aterm" ~: testB4
--   , "./aterm-res/scala/simple/rec-function-def.aterm" ~: testB5
--   , "./aterm-res/scala/simple/rec-function-letrec.aterm" ~: testB6
--   , "./aterm-res/scala/simple/seq-defs.aterm" ~: testB7
--   , "./aterm-res/scala/simple/type-mismatch.no.aterm" ~: testB8 ]

-- testP1 :: IO ()
-- testP1 = runParseTest "./aterm-res/scala/empty.aterm"

-- testP2 :: IO ()
-- testP2 = runParseTest "./aterm-res/scala/simple/missing-def.no.aterm"

-- testP3 :: IO ()
-- testP3 = runParseTest "./aterm-res/scala/simple/param-shadows-def.aterm"

-- testP4 :: IO ()
-- testP4 = runParseTest "./aterm-res/scala/simple/param-shadows-def.no.aterm"

-- testP5 :: IO ()
-- testP5 = runParseTest "./aterm-res/scala/simple/rec-defs.aterm"

-- testP6 :: IO ()
-- testP6 = runParseTest "./aterm-res/scala/simple/rec-function-def.aterm"

-- testP7 :: IO ()
-- testP7 = runParseTest "./aterm-res/scala/simple/rec-function-letrec.aterm"

-- testP8 :: IO ()
-- testP8 = runParseTest "./aterm-res/scala/simple/seq-defs.aterm"

-- testP9 :: IO ()
-- testP9 = runParseTest "./aterm-res/scala/simple/type-mismatch.no.aterm"

-- -- testE2E1 :: IO ()
-- -- testE2E1 = runE2ETest "./aterm-res/scala/empty.aterm" runTCTest

-- -- testE2E2 :: IO ()
-- -- testE2E2 = runE2ETest "./aterm-res/scala/simple/missing-def.no.aterm" runTCFail

-- -- testE2E3 :: IO ()
-- -- testE2E3 = runE2ETest "./aterm-res/scala/simple/param-shadows-def.aterm" runTCTest

-- -- testE2E4 :: IO ()
-- -- testE2E4 = runE2ETest "./aterm-res/scala/simple/param-shadows-def.no.aterm" runTCFail

-- -- testE2E5 :: IO ()
-- -- testE2E5 = runE2ETest "./aterm-res/scala/simple/rec-defs.aterm" runTCTest

-- -- testE2E6 :: IO ()
-- -- testE2E6 = runE2ETest "./aterm-res/scala/simple/rec-function-def.aterm" runTCTest

-- -- testE2E7 :: IO ()
-- -- testE2E7 = runE2ETest "./aterm-res/scala/simple/rec-function-letrec.aterm" runTCTest

-- -- testE2E8 :: IO ()
-- -- testE2E8 = runE2ETest "./aterm-res/scala/simple/seq-defs.aterm" runTCTest

-- -- testE2E9 :: IO ()
-- -- testE2E9 = runE2ETest "./aterm-res/scala/simple/type-mismatch.no.aterm" runTCFail

-- testB1 :: IO ()
-- testB1 = runBasisTest "./aterm-res/scala/simple/missing-def.no.aterm" False

-- testB2 :: IO ()
-- testB2 = runBasisTest "./aterm-res/scala/simple/param-shadows-def.aterm" True

-- testB3 :: IO ()
-- testB3 = runBasisTest "./aterm-res/scala/simple/param-shadows-def.no.aterm" False

-- testB4 :: IO ()
-- testB4 = runBasisTest "./aterm-res/scala/simple/rec-defs.aterm" True

-- testB5 :: IO ()
-- testB5 = runBasisTest "./aterm-res/scala/simple/rec-function-def.aterm" True

-- testB6 :: IO ()
-- testB6 = runBasisTest "./aterm-res/scala/simple/rec-function-letrec.aterm" True

-- testB7 :: IO ()
-- testB7 = runBasisTest "./aterm-res/scala/simple/seq-defs.aterm" True

-- testB8 :: IO ()
-- testB8 = runBasisTest "./aterm-res/scala/simple/type-mismatch.no.aterm" False


-- runParseTest :: String -> IO ()
-- runParseTest s = do
--   p <- parse s
--   print p
--   assertEqual "It parses" (isRight p) True

-- runBasisTest :: String -> Bool -> IO ()
-- runBasisTest s v = do
--   p <- parse s
--   case p of
--     Left _ -> assertFailure "Failed to parse"
--     Right p -> intentionalBehaviour "Type checks" v $! runTC p

-- intentionalBehaviour :: String -> Bool -> Either String (Graph Label Decl) -> IO ()
-- intentionalBehaviour message expected res = do
--   trace "Inentional behaviour" $ print' $! res
--   assertEqual message expected $ isRight res

-- runE2ETest :: String -> (ScProg -> IO a) -> IO ()
-- runE2ETest s runner = do
--   p <- parse s
--   case p of
--     Left _ -> assertFailure "Failed to parse"
--     Right m -> do
--       _ <- runner m
--       return ()

-- print' :: Either String (Graph Label Decl) -> IO ()
-- print' (Right g) = print g
-- print' (Left e) = putStrLn $ "Received error message: " ++ e

-- main :: IO ()
-- main = do
--     result <- runTestTT tests
--     print result
--     if failures result > 0 then Exit.exitFailure else Exit.exitSuccess