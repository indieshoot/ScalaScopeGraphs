module Statix.References where

import Test.HUnit ( assertEqual, assertFailure )
import TypeChecker (Label, Decl, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax

runTCFailR :: ScProg -> IO String
runTCFailR p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPhR :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPhR = either assertFailure return . runTCPhased


-- object A {
--   object C {
--     type Y = Int;
--   };
-- };
--   object B {
--     type X = Int;
--   };

-- object O {
--   type Y = A.B.C.Y;
-- };

testPathNoLexical :: IO ()
testPathNoLexical = do
  t <- runTCFailR [ScObject "A" 
                    [ 
                      ScObject "C" 
                        [
                          ScType "Y" NumT
                        ]
                    ] , 
                ScObject "B" [
                        ScType "X" NumT
                ],
                ScObject "O" 
                    [ 
                       ScVal (ScParam "x" (QRefT ["A", "B", "C"] "Y")) (ScNum 42)
                    ] 
                 ]
  assertEqual "Incorrect types" "Object A.B.C does not exist."  t 

-- object A {
--   object C {
--     type Y = Int;
--   };
--   object B {
--     import C._;
--   };
-- };

-- object O {
--   type Y = A.B.Y;
-- };

testPathNoTransitive :: IO ()
testPathNoTransitive = do
  t <- runTCFailR [ScObject "A" 
                    [ 
                      ScObject "C" 
                        [
                          ScType "Y" NumT
                        ],
                    ScObject "B" [
                    ScImp (ScWImp ["C"])
                ]
                    ] , 

                ScObject "O" 
                    [ 
                       ScType "Y" (QRefT ["A", "B"] "Y")
                    ] 
                 ]
  assertEqual "Incorrect types" "No matching declarations found for variable \"Y\""  t 

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
  t <- runTCPhR [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScType "T" NumT
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScVal (ScParam "x" (QRefT ["A", "B"] "T")) (ScNum 42)
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
  t <- runTCPhR [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScType "T" NumT
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScType "Y" (QRefT ["A", "B"] "T")
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
--   val x : Int = A.B.x;
-- };

testQualifiedRef :: IO ()
testQualifiedRef = do
  t <- runTCPhR [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScVal (ScParam "y" NumT) (ScQRef ["A", "B"] "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   def f: Int = g;
-- };

testUnbound:: IO ()
testUnbound = do
  t <- runTCFailR [ScObject "A" 
                    [ 
                        ScDef "f" [] NumT (Body [] (ScId "g"))
                    ] 
                 ]
  assertEqual "Incorrect types" "No matching declarations found for the expression \"g\"" t 