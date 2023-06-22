module Statix.Precedence where

import Test.HUnit

import TypeChecker (Label (TY), Decl, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax


runTCFailP :: ScProg -> IO String
runTCFailP p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPhP :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPhP = either assertFailure return . runTCPhased

-- object O {
--   val x: Int = 42;
--   def f: Int = {
--     val x: Boolean = true;
--     x
--   };
-- };

testBlockShadowFail :: IO ()
testBlockShadowFail = do
  t <- runTCFailP [ScObject "A" 
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScDef "f" [] NumT 
                      (Body [ScVal (ScParam "x" BoolT) (ScBool True)]  (ScId "x") )
                    ]
                 ]
  assertEqual "Incorrect types" "Type missmatch in def with expected: num vs. got: bool" t 

-- object O {
--   val x: Int = 42;
--   def f: Boolean = {
--     val x: Boolean = true;
--     x
--   };
-- };

testBlockShadow :: IO ()
testBlockShadow = do
  t <- runTCPhP [ScObject "A" 
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScDef "f" [] BoolT 
                      (Body [ScVal (ScParam "x" BoolT) (ScBool True)]  (ScId "x") )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, BoolT] $ fst t 

-- object O1 {
--   type T = Int;
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

testDeepType :: IO ()
testDeepType = do
  t <- runTCPhP [ScObject "O1" 
                    [ 
                      ScType "T" NumT,
                      ScObject "O2" 
                        [
                          ScType "T" NumT,
                          ScObject "O3" 
                        [
                          ScObject "O4" 
                        [
                          ScObject "O5" 
                        [
                          ScVal (ScParam "x" (TyRef "T")) (ScNum 42)
                        ]
                        ]
                        ]
                        ]
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT, NumT] $ fst t 

-- object I {
-- };
-- object O1 {
--   type T = Boolean;
--   object O2 {
--     type T = Int;
--     object O3 {
--       import I._;
--       object O4 {
--         object O5 {
--           val x : T = 42;
--         };
--       };
--     };
--   };
-- };

testDeepTypeImp :: IO ()
testDeepTypeImp = do
  t <- runTCPhP [ ScObject "I" [

                ],
                ScObject "O1" 
                    [ 
                      ScType "T" NumT,
                      ScObject "O2" 
                        [
                          ScType "T" NumT,
                          ScObject "O3" 
                        [
                          ScImp (ScWImp ["I"]),
                          ScObject "O4" 
                        [
                          ScObject "O5" 
                        [
                          ScVal (ScParam "x" (TyRef "T")) (ScNum 42)
                        ]
                        ]
                        ]
                        ]
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT, NumT] $ fst t 


-- object O {
--   def f: Int = {
--     val x: Boolean = true;
--     { 
--       val x: Int = 42;
--       x
--     }
--   };
-- };

testInnerBlockShadowsOuter :: IO ()
testInnerBlockShadowsOuter = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScDef "f" [] BoolT 
                      (Body [ScVal (ScParam "x" BoolT) (ScBool True)]  (ScId "x") )
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, BoolT] $ fst t 

-- object O {
--   def f(x: Int)(x : Boolean): Boolean = x;
-- };

testInnerParam :: IO ()
testInnerParam = do
  t <- runTCFailP [ScObject "O" 
                    [ 
                      ScDef "f" [[ScParam "x" NumT], [ScParam "x" BoolT]] BoolT 
                      (Body []  (ScId "x") )
                    ]
                 ]
  assertEqual "Incorrect types" "BUG: Multiple declarations found" t 

-- object A {
--   object A { val x : Int = 42; };
--   import A._;
--   val y : Int = x;
-- };

testInnerShadowsSelf:: IO ()
testInnerShadowsSelf = do
  t <- runTCPhP [ScObject "A" 
                    [ 
                      ScObject "A" [
                        ScVal (ScParam "x" NumT) (ScNum 42)
                      ],
                      ScImp (ScWImp ["A"]),
                      ScVal (ScParam "y" NumT) (ScId "x")
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object O {
--   type I = Boolean;
--   object P {
--     type I = Int;
--     val i : I = 42;
--   };
-- };

testInnerTypeShadowsOuter:: IO ()
testInnerTypeShadowsOuter = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScType "I" NumT,
                      ScObject "P" [
                        ScType "I" NumT,
                        ScVal (ScParam "i" (TyRef "I")) (ScNum 42)
                      ]
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT, NumT] $ fst t 

-- object O {
--   type Int = Boolean;
--   val x : Int = true;
-- };

testDeclShadowsPredef :: IO ()
testDeclShadowsPredef = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScType "NumT" BoolT,
                      ScVal (ScParam "i" (TyRef "NumT")) (ScBool True)
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT, BoolT] $ fst t 

-- object O {
--   val x : Int = 42;
--   val y : Int = x;
--   val x : Boolean = true;
-- };

testValAmbiguity :: IO ()
testValAmbiguity = do
  t <- runTCFailP [ScObject "O" 
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScVal (ScParam "y" NumT) (ScId "x"),
                      ScVal (ScParam "x" BoolT) (ScBool True)
                    ]
                 ]
  assertEqual "Incorrect types" "BUG: Multiple declarations found" t 

-- object O {
--   object N {
--     type I = Int;
--   };
--   import N.I;
--   object M {
--     type J = I;
--   };
-- };

testMixImport :: IO ()
testMixImport = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "N" [
                        ScType "I" NumT
                      ],
                      ScImp (ScEImp ["N"] ["I"]),
                      ScObject "M" [
                        ScType "J" (TyRef "I")
                      ]
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object O {
--   object N {
--     val x : Int = 42;
--   };

--   import N.x;
--   object I {
--     import N._;
--     val y : Int = x;
--   };
-- };

testMultiplePathsSameValue :: IO ()
testMultiplePathsSameValue = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "N" [
                        ScVal (ScParam "x" NumT) (ScNum 42)
                      ],
                      ScImp (ScEImp ["N"] ["x"]),
                      ScObject "I" [
                        ScImp (ScWImp ["N"]),
                        ScVal (ScParam "y" NumT) (ScId "x")
                      ]
                    ]
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object A {
--   object I {
--     val f: Boolean = true;
--   };

--   object B {
--     import I.f;
--     val g: Int = f;
--   };

--   val f: Int = 42;
-- };

testOuterPrecedesInnerImp :: IO ()
testOuterPrecedesInnerImp = do
  t <- runTCPhP [ScObject "A" 
                    [ 
                      ScObject "I" [
                        ScVal (ScParam "f" BoolT) (ScBool True)
                      ],
                      ScObject "B" [
                        ScImp (ScEImp ["N"] ["f"]),
                        ScVal (ScParam "g" NumT) (ScId "f")
                      ],
                      ScVal (ScParam "f" NumT) (ScNum 42)
                    ]
                 ]
  assertEqual "Incorrect types" [BoolT, NumT, NumT] $ fst t 

-- object A {
--   object B {
--     val g: Int = f;
--   };

--   val f: Int = 42;
-- };

testOuterLocalDef :: IO ()
testOuterLocalDef = do
  t <- runTCPhP [ScObject "A" 
                    [ 
                      ScObject "I" [
                        ScVal (ScParam "g" NumT) (ScId "f")
                      ],
                      ScVal (ScParam "f" NumT) (ScNum 42)
                    ]
                 ]
  assertEqual "Incorrect types" [NumT, NumT] $ fst t 

-- object O {
--   val x : Int = 42;
--   def f(x : Boolean): Int = x;
-- };

testParamShadowsOuterFail :: IO ()
testParamShadowsOuterFail = do
  t <- runTCFailP [ScObject "O" 
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScDef "f" [[ScParam "x" BoolT]] NumT 
                      (Body []  (ScId "x") )
                    ]
                 ]
  assertEqual "Incorrect types" "Type missmatch in def with expected: num vs. got: bool" t

-- object O {
--   val x : Int = 42;
--   def f(x : Boolean): Boolean = x;
-- }; 

testParamShadowsOuter :: IO ()
testParamShadowsOuter = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScVal (ScParam "x" NumT) (ScNum 42),
                      ScDef "f" [[ScParam "x" BoolT]] BoolT 
                      (Body []  (ScId "x") )
                    ]
                 ]
  assertEqual "Incorrect types" [NumT, BoolT] $ fst t

-- object O {
--   def f(f : Boolean): Boolean = f;
-- };

testParamShadowsSelf :: IO ()
testParamShadowsSelf = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScDef "f" [[ScParam "f" BoolT]] BoolT 
                      (Body []  (ScId "f") )
                    ]
                 ]
  assertEqual "Incorrect types" [BoolT] $ fst t

-- object O {
--   object N {
--     type A = Boolean;
--   };

--   object I {
--     import N._;
--     import N._;
--     val x : A = true;
--   };
-- };

testDoubleWImp :: IO ()
testDoubleWImp = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "N" 
                      [   
                        ScType "A" BoolT
                      ]
                    ],
                ScObject "C" 
                    [
                        ScImp (ScWImp ["O", "N"]), 
                        ScImp (ScWImp ["O", "N"]), 
                        ScVal (ScParam "x" (TyRef "A")) (ScBool True) 
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [BoolT, BoolT] $ fst t

-- object O {
--   object M {
--     type A = Int;
--   };
--   object N {
--     type A = Boolean;
--   };

--   object I {
--     import N.A;
--     import M.A;
--     val x : A = 3;
--   };
-- };

testEImpNoShadow :: IO ()
testEImpNoShadow = do
  t <- runTCFailP [ScObject "O" 
                    [ 
                      ScObject "M" 
                      [   
                        ScType "A" NumT
                      ], 
                      ScObject "N" 
                      [   
                        ScType "A" BoolT
                      ]
                    ],
                ScObject "I" 
                    [
                        ScImp (ScEImp ["O", "N"] ["A"]) ,
                        ScImp (ScEImp ["O", "M"] ["A"]), 
                        ScVal (ScParam "x" (TyRef "A")) (ScNum 3) 
                    ]
               ]
  assertEqual "Incorrect types" "Ambiguous reference." t

-- object O {
--   object M {
--     type A = Int;
--   };
--   object N {
--     type A = Boolean;
--   };

--   object P {
--     import N._;
--     import M.A;
--     val x : A = 3;
--   };
-- };

testEImpShadow :: IO ()
testEImpShadow = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "M" 
                      [   
                        ScType "A" NumT
                      ], 
                      ScObject "N" 
                      [   
                        ScType "A" BoolT
                      ]
                    ],
                ScObject "P" 
                    [
                        ScImp (ScWImp ["O", "N"]) ,
                        ScImp (ScEImp ["O", "M"] ["A"]), 
                        ScVal (ScParam "x" (TyRef "A")) (ScNum 3) 
                    ]
               ]
  assertEqual "Incorrect types" [NumT, BoolT, NumT] $ fst t

-- object A {
--   object A {
--   };
-- };

-- object C {
--   import A.A;
--   import A._;
-- };

testAmbiguousName :: IO ()
testAmbiguousName = do
  t <- runTCFailP [ScObject "A" 
                    [ 
                      ScObject "A" []
                    ],
                  ScObject "C" [
                    ScImp (ScEImp ["A"] ["A"]),
                    ScImp (ScWImp ["A"])
                  ]
                 ]
  assertEqual "Incorrect types" "Ambiguous Names." t

-- object O {
--   object N {
--     type I = Int;
--   };

--   import N.I;
--   val x : I = 42;
-- };

testSpecificImpResolves :: IO ()
testSpecificImpResolves = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "N" 
                      [   
                        ScType "I" NumT
                      ],
                        ScImp (ScEImp ["N"] ["I"]), 
                        ScVal (ScParam "x" (TyRef "I")) (ScNum 42) 
                    ]
               ]
  assertEqual "Incorrect types" [NumT, NumT] $ fst t


-- object O {
--   object M {
--     type A = Int;
--   };
--   object N {
--     type A = Boolean;
--   };

--   import N.A;
--   object I {
--     import M.A;
--     val x : A = 3;
--   };
-- };

testEShadowsInnerE :: IO ()
testEShadowsInnerE = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "M" 
                      [   
                        ScType "A" NumT
                      ],
                    ScObject "N" [
                      ScType "A" BoolT
                    ]
                    ],
                    ScImp (ScWImp ["N"]),
                    ScObject "I" 
                    [
                      ScImp (ScEImp ["M"] ["A"]),
                      ScVal (ScParam "x" (TyRef "A")) (ScNum 3) 
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, BoolT] $ fst t

-- object O {
--   object M {
--     type A = Int;
--   };
--   object N {
--     type A = Boolean;
--   };

--   object I {
--     import N._;
--     import M._;
--     val x : A = 3;
--   };
-- };

testWNoShadow :: IO ()
testWNoShadow = do
  t <- runTCFailP [ScObject "O" 
                    [ 
                      ScObject "M" 
                      [   
                        ScType "A" NumT
                      ],
                    ScObject "N" [
                      ScType "A" BoolT
                    ],
                    ScObject "I" 
                    [
                        ScImp (ScWImp ["O", "N"]), 
                        ScImp (ScWImp ["O", "M"]), 
                        ScVal (ScParam "x" (TyRef "A")) (ScBool True) 
                    ]
                    ]
               ]
  assertEqual "Incorrect types" "No matching declarations found - type reference" t

-- object O {
--   object M {
--     type A = Int;
--   };
--   object N {
--     type A = Boolean;
--   };

--   import M.A;
--   import N._;
--   val x : A = 3;
-- };

testWNoShadowE :: IO ()
testWNoShadowE = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "M" 
                      [   
                        ScType "A" NumT
                      ],
                    ScObject "N" [
                      ScType "A" BoolT
                    ],
                    ScImp (ScEImp ["O", "M"] ["A"]), 
                    ScImp (ScWImp ["O", "N"]), 
                    ScVal (ScParam "x" (TyRef "A")) (ScNum 3) 
                    ]
               ]
  assertEqual "Incorrect types" [NumT, BoolT, NumT] $ fst t

-- object O {
--   object M {
--     type A = Int;
--   };
--   object N {
--     type A = Boolean;
--   };

--   import M.A;
--   object I {
--     import N._;
--     val x : A = true;
--   };
-- };

testWNoShadowOuter :: IO ()
testWNoShadowOuter = do
  t <- runTCFailP [ScObject "O" 
                    [ 
                      ScObject "M" 
                      [   
                        ScType "A" NumT
                      ],
                    ScObject "N" [
                      ScType "A" BoolT
                    ],
                    ScObject "I" [
                      ScImp (ScEImp ["O", "M"] ["A"]), 
                      ScImp (ScWImp ["O", "N"]), 
                      ScVal (ScParam "x" (TyRef "A")) (ScNum 3) 
                    ]
                    ]
               ]
  assertEqual "Incorrect types" "No matching declarations found - type reference"  t

-- object O {
--   object N {
--     type I = Int;
--   };

--   import N._;
--   val x : I = 42;
-- };

testWImpResolves :: IO ()
testWImpResolves = do
  t <- runTCPhP [ScObject "O" 
                    [ 
                      ScObject "N" 
                      [   
                        ScType "I" NumT
                      ],
                        ScImp (ScWImp ["N"]), 
                        ScVal (ScParam "x" (TyRef "I")) (ScNum 42) 
                    ]
               ]
  assertEqual "Incorrect types" [NumT, NumT] $ fst t

-- object O {
--   object M {
--     type A = Int;
--   };
--   object N {
--     type A = Boolean;
--   };

--   import M._;
--   object I {
--     import N._;
--     val x : A = true;
--   };
-- };

testWShadowW :: IO ()
testWShadowW = do
  t <- runTCFailP [ScObject "O" 
                    [ 
                      ScObject "M" 
                      [   
                        ScType "A" NumT
                      ],
                    ScObject "N" [
                      ScType "A" BoolT
                    ],
                    ScImp (ScWImp ["O", "M"]), 
                    ScObject "I" [
                      ScImp (ScWImp ["O", "N"]), 
                      ScVal (ScParam "x" (TyRef "A")) (ScNum 3) 
                    ]
                    ]
               ]
  assertEqual "Incorrect types" "No matching declarations found - type reference"  t