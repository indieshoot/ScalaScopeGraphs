module Statix.Comprehensive where
  
import Test.HUnit ( assertEqual, assertFailure )
import TypeChecker (Label, Decl, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax


runTCFailC :: ScProg -> IO String
runTCFailC p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPhC :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPhC = either assertFailure return . runTCPhased


-- object a {
--   object b {
--     def f(): Unit = {};
--   };
-- };

-- object c {
--   import a._;
--   import b.f;
--   def g(): Unit = { f() };
-- };

testPaperExample :: IO ()
testPaperExample = do
  t <- runTCPhC [ScObject "A" 
                    [ 
                      ScObject "B" 
                      [   
                        ScDef "f" [] Unit (Body [] ScUnit)
                      ]
                    ],
                ScObject "C"  
                    [
                        ScImp (ScWImp ["A"]), 
                        ScImp (ScEImp ["A", "B"] ["f"]),
                        ScDef "g" [] Unit (Body [] (ScApp (ScId "f") []))
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [Unit, Unit] $ fst t


-- object a {
--   object b {
--     def f(): Unit = {};
--   };
-- };

-- object c {
--   import a._;
--   def g(): Unit = {
--     // imports are sequenced, so f is not in scope here
--     f();
--   };
--   import b.f;
-- };


testPaperExFail :: IO ()
testPaperExFail = do
  t <- runTCPhC [ScObject "A" 
                    [ 
                      ScObject "B" 
                      [   
                        ScDef "f" [] Unit (Body [] ScUnit)
                      ]
                    ],
                ScObject "C"  
                    [
                        ScImp (ScWImp ["A"]), 
                        ScDef "g" [] Unit (Body [] (ScApp (ScId "f") [])),
                        ScImp (ScEImp ["A", "B"] ["f"])
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [] $ fst t

  -- object o {
-- def f:Int = g;
-- import n._;
-- def g:Int = h
-- }
-- object n {
-- import o.f
-- def h:Int = f;
-- }

testPaper :: IO ()
testPaper = do
  t <- runTCPhC [ScObject "O" 
                    [ 
                      ScDef "f" [] NumT (Body [] (ScId "g")),
                      ScImp (ScWImp ["N"]),
                      ScDef "g" [] NumT (Body [] (ScId "h"))
                    ] , 
                ScObject "N" 
                    [ 
                      ScImp (ScEImp ["O"] ["f"]),
                      ScDef "h" [] NumT (Body [] (ScId "f"))
                    ] 
                 ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT, NumT] $ fst t 



