module Statix.Imports where

import Test.HUnit ( assertEqual, assertFailure )
import TypeChecker (Label, Decl, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax

runTCFailI :: ScProg -> IO String
runTCFailI p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPhI :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPhI = either assertFailure return . runTCPhased


-- object O1 {
--   type T = Boolean;
--   object O2 {
--     type T = Int;
--     object O3 {
--       object O4 {
--         val x : T = 42;
--         object O5 {};
--       };
--     };
--   };
-- };

-- object N {
--   import O1.O2.O3.O4.O5.x;
--   val y : Int = x;
-- };

testDeepExplRefFail :: IO ()
testDeepExplRefFail = do
  t <- runTCFailI [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42),
                          ScObject "C" 
                        [
                        ]
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScImp (ScEImp ["A", "B", "C"] ["x"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  assertEqual "Incorrect types" "No matching declarations found for [\"x\"]" t 


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
--   import O1.O2.O3.O4.O5.x;
--   val y : Int = x;
-- };

testDeepExplRef :: IO ()
testDeepExplRef = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScObject "C" 
                        [
                          ScObject "D" 
                        [
                          ScObject "E" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                        ]
                        ]
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScImp (ScEImp ["A", "B", "C", "D", "E"] ["x"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 


-- object O1 {
--   type T = Boolean;
--   object O2 {
--     type T = Int;
--     object O3 {
--       object O4 {
--             val x : T = 42;
--         object O5 {
--         };
--       };
--     };
--   };
-- };

-- object N {
--   import O1.O2.O3.O4.O5._;
--   val y : Int = x;
-- };

testDeepWildRefFail :: IO ()
testDeepWildRefFail = do
  t <- runTCFailI [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42),
                          ScObject "C" 
                        [
                        ]
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScImp (ScWImp ["A", "B", "C"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                ]     
  assertEqual "Incorrect types" "No matching declarations found for [\"x\"]" t 

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
--   import O1.O2.O3.O4.O5.x;
--   val y : Int = x;
-- };

testDeepWildRef :: IO ()
testDeepWildRef = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScObject "C" 
                        [
                          ScObject "D" 
                        [
                          ScObject "E" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                        ]
                        ]
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScImp (ScWImp ["A", "B", "C", "D", "E"]),
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

-- object C {
--   import A.B;
--   import B._;
--   val y : X = 42;
-- };

testImpFromImprotedObj :: IO ()
testImpFromImprotedObj = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                            ScType "X" NumT
                        ]
                    ] , 
                ScObject "C" 
                    [ 
                      ScImp (ScWImp ["A", "B"]),
                      ScImp (ScWImp ["A", "B"]), 
                      ScVal (ScParam "y" (TyRef "X")) (ScNum 42)
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   val x : Int = 3
-- }
-- object B {
--   import A.x;
--   val y : Int = x
--   }

testEImp :: IO ()
testEImp = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 3)
                    ] , 
                ScObject "B" 
                    [ 
                      ScImp (ScEImp ["A"] ["x"]),
                      ScVal (ScParam "y" NumT) (ScId "x")] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- -- object A {
-- --   val y : Bool = True
-- --   val x : Int = 42
-- -- }
-- -- object B {
-- --     import A._;
-- --     val y : Int = x
-- --   }

testWImp :: IO ()
testWImp = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScVal (ScParam "y" BoolT) (ScBool True) , 
                      ScVal (ScParam "x" NumT) (ScNum 5) 
                    ] , 
                ScObject "B" 
                    [ 
                      ScImp (ScWImp ["A"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT, NumT, NumT] $ fst t 

-- object A {
--   import B._;
-- };

testImpUnbound :: IO ()
testImpUnbound = do
  t <- runTCFailI [ScObject "A" 
                    [ 
                        ScImp (ScWImp ["B"])
                    ] 
                ]     
  assertEqual "Incorrect types" "Object B does not exist." t 

-- object A {
--   import A._;
-- };

testSelf :: IO ()
testSelf = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                        ScImp (ScWImp ["A"])
                    ]
                ]
  print $ snd t
  assertEqual "Incorrect types" [] $ fst t 

-- object A {
--   object B {
--     import A._;
--   };
-- };

testSuperSelf :: IO ()
testSuperSelf = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScObject "B"  [
                        ScImp (ScWImp ["A"])
                      ]
                    ]
                ]
  print $ snd t
  assertEqual "Incorrect types" [] $ fst t 

-- object O {
--   object N {
--     type T = Int;
--     type S = Int;
--     type R = Int;
--   };

--   import N.{T, S};
--   val y : R = 42;
-- };

testMultipleImportsFail :: IO ()
testMultipleImportsFail = do
  t <- runTCFailI [ScObject "O" 
                    [ 
                      ScObject "N" 
                      [   
                        ScType "T" NumT,
                        ScType "S" NumT,
                        ScType "R" NumT
                      ],
                    ScImp (ScEImp ["O", "N"] ["T", "S"]), 
                    ScVal (ScParam "x" (TyRef "R")) (ScNum 42)
                    ]
               ]
  assertEqual "Incorrect types" "No matching declarations found for variable \"R\"" t


testMultipleImports :: IO ()
testMultipleImports = do
  t <- runTCPhI [ScObject "O" 
                    [ 
                      ScObject "N" 
                      [   
                        ScType "T" NumT,
                        ScType "S" NumT,
                        ScType "R" NumT
                      ],
                    ScImp (ScEImp ["O", "N"] ["T", "S"]), 
                    ScVal (ScParam "x" (TyRef "T")) (ScNum 42) ,
                    ScVal (ScParam "y" (TyRef "S")) (ScNum 42) 
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT, NumT, NumT, NumT] $ fst t

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
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScImp (ScWImp ["B"]),
                      ScVal (ScParam "x" NumT) (ScId "y") 
                    ] , 
                ScObject "B" 
                    [ 
                      ScImp (ScEImp ["A"] ["x"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- this fails because of syntax
-- object A {
--   object B {
--     val x : Int = 42;
--   };
-- };

-- object C {
--   import A.B;
--   val y : Int = x;
-- };


-- object A {
--   object B {
--     val x : Int = 42;
--   };
-- };

-- object C {
--   import A._;
--   val y : Int = x;
-- };

testWImpInvisible :: IO ()
testWImpInvisible = do
  t <- runTCFailI [ScObject "A" 
                    [ 
                      ScObject "B" 
                      [   
                        ScVal (ScParam "x" NumT) (ScNum 42)
                      ]
                    ],
                ScObject "C"  
                    [
                        ScImp (ScWImp ["A"]), 
                        ScVal (ScParam "y" NumT) (ScId "x")
                    ]
               ]
  assertEqual "Incorrect types" "No matching declarations found for the expression \"x\"" t

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
  t <- runTCPhI [ScObject "A" 
                    [
                      ScVal (ScParam "x" NumT) (ScNum 21) 
                    ] , 
                ScObject "B" 
                    [ 
                      ScVal (ScParam "x" BoolT) (ScBool True)
                    ] ,
                ScObject "C" 
                    [ 
                      ScImp (ScEImp ["A"] ["x"]),
                      ScImp (ScWImp ["B"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, BoolT, NumT] $ fst t 

-- object O {
--   object N {
--     type T = Int;
--   };
--   object M {
--     import N.T;
--   };
--   import M.T;
--   val x : T = 42;
-- };

testWImpNotTransitive :: IO ()
testWImpNotTransitive = do
  t <- runTCFailI [ScObject "O" 
                    [ 
                      ScObject "N" 
                        [
                          ScType "T" NumT
                        ], 
                      ScObject "M" 
                        [
                          ScImp (ScWImp ["N"])
                        ],
                        ScImp (ScWImp ["M"]),
                        ScVal (ScParam "x" (TyRef "T")) (ScNum 42)
                    ] 
                 ]
  assertEqual "Incorrect types" "No matching declarations found for variable \"T\"" t

testEImpNotTransitive :: IO ()
testEImpNotTransitive = do
  t <- runTCFailI [ScObject "O" 
                    [ 
                      ScObject "N" 
                        [
                          ScType "T" NumT
                        ], 
                      ScObject "M" 
                        [
                          ScImp (ScWImp ["N"])
                        ],
                        ScImp (ScEImp ["M"] ["T"]),
                        ScVal (ScParam "x" (TyRef "T")) (ScNum 42)
                    ] 
                 ]
  assertEqual "Incorrect types" "No matching declarations found for [\"T\"]" t

-- object A {
--   import B._;
--   val y : X = 42;
-- };

-- object B {
--   type X = Int;
-- };

testWImpForward :: IO ()
testWImpForward = do
  t <- runTCPhI [ScObject "A" 
                    [ 
                        ScImp (ScWImp ["B"]),
                        ScVal (ScParam "y" (TyRef "X")) (ScNum 42)
                    ] , 
                ScObject "B" 
                    [ 
                      ScType "X" NumT
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
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScImp (ScEImp ["A", "B"] ["x"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

testMWImp :: IO ()
testMWImp= do
  t <- runTCPhI [ScObject "A" 
                    [ 
                      ScObject "B" 
                        [
                          ScVal (ScParam "x" NumT) (ScNum 42)
                        ]
                    ] , 
                ScObject "O" 
                    [ 
                      ScImp (ScWImp ["A", "B"]), 
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 