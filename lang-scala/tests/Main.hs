module Main where

import Test.HUnit

import Data.Either (isRight)
import TypeChecker (Label, Decl, runTC, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)


runTCTest :: ScExp -> IO (Type, Graph Label Decl) 
runTCTest = either assertFailure return . runTC

runTCFail :: ScProg -> IO String
runTCFail p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p


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
                ScObject "B" [ScEImp ["A"] "x"]
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
                ScObject "B" [ScWImp ["A"]]
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
  t <- runTCPh [ScObject "A" [ScWImp ["B"]]
                    [ 
                      ScVal (ScParam "x" NumT) (ScId "y") 
                    ] , 
                ScObject "B" [ScEImp ["A"] "x"]
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
                ScObject "C" [ScEImp ["A"] "x", ScWImp ["B"]]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, BoolT, NumT] $ fst t 

-- object A {
--   def f: Int = g;
--   def g: Int = f;
-- };

testMutualDefs :: IO ()
testMutualDefs = do
  t <- runTCPh [ScObject "A" []
                    [
                      ScDef "f" [] NumT [] (ScId "g"), 
                      ScDef "g" [] NumT [] (ScId "f")
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- recursive-defs: pass
-- object A {
--   def f: Int = f;
-- };

testRecDefs :: IO ()
testRecDefs = do
  t <- runTCPh [ScObject "A" []
                    [
                      ScDef "x" [] NumT [] (ScId "x") 
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT] $ fst t 

-- nested-val-defs:
-- object O {
--   val x : Int = 42;
--   object P {
--     val x : Int = 43;
--   };
-- };

testNestedObj :: IO ()
testNestedObj = do
  t <- runTCPh [ScObject "O" []
                    [
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScObject "P" []
                        [
                          ScVal (ScParam "x" NumT) (ScNum 43)
                        ]
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   def A: Int = A;
-- };

testSameNameDef :: IO ()
testSameNameDef = do
  t <- runTCPh [ScObject "x" []
                    [
                      ScDef "x"  [] NumT [] (ScId "x") 
                      -- ScVal (ScParam "x" NumT) (ScId "x")
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
  t <- runTCFail [ScObject "A" []
                    [
                      ScVal (ScParam "x" NumT) (ScNum 3), 
                      ScVal (ScParam "x" NumT) (ScNum 4)
                    ] 
               ]
  assertEqual "Incorrect types" "Error: there is already a declaration x : num at label VAR" t

-- object A {
--   def f: Int = g;
--   def g: Int = 42;
-- };

testForwardRef :: IO ()
testForwardRef = do
  t <- runTCPh [ScObject "A" []
                    [
                      ScDef "f" [] NumT [] (ScId "g"), 
                      ScDef "g" [] NumT [] (ScNum 3)
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- qualified reference to value: make pass
-- object A {
--   object B {
--     val x : Int = 42;
--   };
-- };

-- object O {
--   val x : Int = A.B.x;
-- };

testQualifiedRef :: IO ()
testQualifiedRef = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                    ] , 
                ScObject "O" []
                    [ 
                      ScVal (ScParam "y" NumT) (ScQRef ["A", "B"] "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   object B {
--     type T = Int;
--   };
-- };

-- object O {
--   val x : A.B.T = 42;
-- };

testQualifiedRefTy :: IO ()
testQualifiedRefTy = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScType "T" NumT
                        ]
                    ] , 
                ScObject "O" []
                    [ 
                      ScVal (ScParam "x" (QRefT ["A", "B"] "T")) (ScNum 42)
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   object B {
--     val x : Int = 42;
--   };
-- };

-- object O {
--   import A.B.x;
--   val y : Int = x;
-- };


testMExplImp :: IO ()
testMExplImp = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                    ] , 
                ScObject "O" [ScEImp ["A", "B"] "x"]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

testMWImp :: IO ()
testMWImp= do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                    ] , 
                ScObject "O" [ScWImp ["A", "B"]]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

testDeepExplRef :: IO ()
testDeepExplRef = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScObject "C" []
                        [
                          ScObject "D" []
                        [
                          ScObject "E" []
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                        ]
                        ]
                        ]
                    ] , 
                ScObject "O" [ScEImp ["A", "B", "C", "D", "E"] "x"]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   object B {
--     type X = Int;
--   };
-- };

-- object O {
--   type Y = A.B.X;
-- };

testQualifiedRefTy2 :: IO ()
testQualifiedRefTy2 = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScType "T" NumT
                        ]
                    ] , 
                ScObject "O" []
                    [ 
                      ScType "Y" (QRefT ["A", "B"] "T")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object O {
--   val x: Int = 42;
--   def f: Boolean = {
--     val x: Boolean = true;
--     x
--   };
-- };

testBlockShadow :: IO ()
testBlockShadow = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScDef "f" [] BoolT [ScVal (ScParam "x" BoolT) (ScBool True)]  (ScId "x")
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, BoolT] $ fst t 

-- object O {
--   def f(x: Int)(y : Boolean): Boolean = {
--     val x2: Int = x;
--     y
--   };
-- };
testMultipleParamClauses :: IO ()
testMultipleParamClauses = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScDef "f" [[ScParam "x" NumT], [ScParam "y" BoolT]] BoolT 
                            [ScVal (ScParam "x2" NumT) (ScId "x")]  
                            (ScId "y")
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 

-- object A {
--   type x = Int;
--   val x: Int = 3;
-- };

testTypeAndValOverlap :: IO ()
testTypeAndValOverlap = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScType "x" NumT, 
                      ScVal (ScParam "x" NumT) (ScNum 3)
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

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
  t <- runTCPh [ScObject "O" [ScEImp ["A"] "x"] 
                  [ScObject "A" []
                    [ 
                      ScType "x" NumT, 
                      ScVal (ScParam "x" NumT) (ScNum 3)
                    ],
                  ScVal (ScParam "y" (TyRef "x")) (ScId "x")
                 ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT, NumT] $ fst t 

-- object O {
--   type I = Int;
--   object P {
--     val x : I = 42;
--   };
-- };

testNestedObjTy :: IO ()
testNestedObjTy  = do
  t <- runTCPh [ScObject "O" []
                    [
                      ScType "I" NumT,
                      ScObject "P" []
                        [
                          ScVal (ScParam "x" (TyRef "I")) (ScNum 43)
                        ]
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object O {
--   def f(g : Int => Boolean): Int => Boolean = g;
-- };

testFunctionValRef :: IO ()
testFunctionValRef = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScDef "f" [[ScParam "g" (FunT [NumT] BoolT)]] (FunT [NumT] BoolT) []
                            (ScId "g")
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [FunT [NumT] BoolT] $ fst t 

-- object O {
--   def f(g : Int => Boolean)(a : Int): Boolean = g(a);
-- };

testFunctionCall :: IO ()
testFunctionCall = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScDef "f" [[ScParam "g" (FunT [NumT] BoolT)], [ScParam "a" NumT]] BoolT []
                            (ScApp (ScId "g") [ScId "a"])
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 

-- this should fail since we try to access x from C, but it is in B.
testDeepExplRefFail :: IO ()
testDeepExplRefFail = do
  t <- runTCFail [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42),
                          ScObject "C" []
                        [
                        ]
                        ]
                    ] , 
                ScObject "O" [ScEImp ["A", "B", "C"] "x"]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  assertEqual "Incorrect types" "No matching declarations found - explicit import" t 

-- object O {
--   def f(x: Int)(y: Boolean): Boolean = y;
--   def g: Boolean = f(42)(true); // currying correct
-- };

testCurry :: IO ()
testCurry = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScDef "f" [[ScParam "x" NumT], [ScParam "y" BoolT]] BoolT []
                            (ScId "y"),
                      ScDef "g" [] BoolT []
                            (ScApp (ScId "f") [ScNum 42, ScBool True])
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 

-- object O {
--   def f(x : Int): Int = x;
--   def g: Int = f(42);
-- };

testSingleCurry :: IO ()
testSingleCurry = do
  t <- runTCPh [ScObject "A" []
                    [ 
                      ScDef "f" [[ScParam "x" NumT]] NumT []
                            (ScId "x"),
                      ScDef "g" [] NumT []
                            (ScApp (ScId "f") [ScNum 42])
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT] $ fst t 


-- object O1 {
--   type T = Boolean;
--   object O2 {
--     type T = Int;
--     object O3 {
--       object O4 {
--         object O5 {
--           val x : T = 42;
--         };
--       };
--     };
--   };
-- };

-- object N {
--   import O1.O2.O3.O4.O5._;
--   val y : Int = x;
-- };

testDeepWildRef :: IO ()
testDeepWildRef = do
  t <- runTCFail [ScObject "A" []
                    [ 
                      ScObject "B" []
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42),
                          ScObject "C" []
                        [
                        ]
                        ]
                    ] , 
                ScObject "O" [ScWImp ["A", "B", "C"]]
                    [ 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  assertEqual "Incorrect types" "No matching declarations found - explicit import" t 

tests :: Test
tests = TestList
    [ "test1" ~: test1 
    , "test2" ~: test2 
    -- , "testNestedWildcard" ~: testDeepWildRef
    --, "testCurry" ~: testCurry
    -- , "testSingleCurry" ~: testSingleCurry
    , "testEImp" ~: testEImp
    , "testWImp" ~: testWImp
    , "testDoubleImport" ~: testDoubleImports
    , "testNameClash" ~: testNameClash
    , "testMutualDefs" ~: testMutualDefs
    , "testRecursiveDefs" ~: testRecDefs
    , "testNestedObjects" ~: testNestedObj
    , "testAllSameName" ~: testSameNameDef
    , "testDuplicateValue" ~: testDuplicateVal
    , "testForwardReference"  ~: testForwardRef
    , "testMultipleExplicitImp" ~: testMExplImp
    , "testMultipleWildcardImp" ~: testMWImp
    , "testDeepReference" ~: testDeepExplRef
    , "testQualifiedReference" ~: testQualifiedRef
    , "testQualifiedReferenceType" ~: testQualifiedRefTy
    , "testQualifiedReferenceType2" ~: testQualifiedRefTy2
    , "testMultipleParamClauses" ~: testBlockShadow
    , "testMultipleParams" ~: testMultipleParamClauses
    , "testTypeAndValOverlap" ~: testTypeAndValOverlap
    , "testTypeAndValOverlapImp" ~: testTypeAndValOverlapImp
    , "testNestedTy" ~: testNestedObjTy
    , "testFunctionType" ~: testFunctionValRef
    , "testFunctionCall" ~: testFunctionCall
    , "testDeepReferenceFail" ~: testDeepExplRefFail
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess