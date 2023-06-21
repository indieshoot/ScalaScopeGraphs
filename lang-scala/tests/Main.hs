module Main where

import Test.HUnit

import Data.Either (isRight)
import TypeChecker (Label, Decl, runTC, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax
import Debug.Trace (trace)
import Statix.Comprehensive
import Statix.Defs
import Statix.Expressions
import Statix.Imports
import Statix.Precedence
import Statix.References
import Statix.Statements

runTCFail :: ScProg -> IO String
runTCFail p = either return (const $ assertFailure "Expected exception, got none") $ runTCPhased p

runTCPh :: ScProg -> IO ([Type], Graph Label Decl) 
runTCPh = either assertFailure return . runTCPhased


testTypeAliaChain :: IO ()
testTypeAliaChain = do
  t <- runTCPh [ScObject "A" 
                    [ 
                      ScType "X" NumT,
                      ScType "Y" (TyRef "X")
                      -- ScType "Z" (TyRef "Y"),
                      -- ScVal (ScParam "x" (TyRef "Z")) (ScNum 42)
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t

-- object A {
--   def f(x: Int): Int = 42;
--   val f: Int = 42;
-- };

testPassUnsuported :: IO ()
testPassUnsuported = do
  t <- runTCPh [ScObject "A" 
                    [ 
                      ScVal (ScParam "f" NumT) (ScNum 42),
                      ScDef "f" [[ScParam "x" NumT]] NumT (Body [] (ScNum 42))
                    ]
               ]
  print $ snd t
  assertEqual "Incorrect types" [NumT, NumT] $ fst t


tests :: Test
tests = TestList
    [  
    -- , "testTypeAliasChain" ~: testTypeAliaChain
    -- "testPaperFail" ~: testPaperExFail

    -- comprehensive
     "testPaperEx" ~: testPaperExample

    -- defs
    , "testTypeAndValOverlapImp" ~: testTypeAndValOverlapImp
    , "testTypeNotFound" ~: testTypeNoFound
    , "testTypeFound" ~: testTypeFound
    , "testMutualDefs" ~: testMutualDefs
    ,  "testRecursiveDefs" ~: testRecDefs
    , "testNestedObjects" ~: testNestedObj
    , "testAllSameName" ~: testSameNameDef
    , "testDuplicateDefinition" ~: testDuplicateVal
    , "testDuplicateType" ~: testDuplicateType
    , "testForwardReference"  ~: testForwardRef
    ,  "testMultipleParams" ~: testMultipleParamClauses
    ,  "testMultipleParamsMultipleParams" ~: testMultipleParamClausesMultipleParams
    ,  "testMultipleParamsNo" ~: testMultipleParamClausesFail
    , "testTypeAndValOverlap" ~: testTypeAndValOverlap
    , "testTypeDeclaration" ~: testTypeDecl

    -- expressions
    , "testCurry" ~: testCurry
    , "testSingleCurry" ~: testSingleCurry
    , "testSingleCurryNo" ~: testSingleCurryFail
    , "testFunctionType" ~: testFunctionValRef
    , "testFunctionCall" ~: testFunctionCall
    , "testFunctionCallFail" ~: testFunctionCallNo
    , "testFunctionCallRef" ~: testFunctionRef
    , "testZeroCurryFail" ~: testZeroCurryFail

    -- imports

    , "testNestedWildcard" ~: testDeepWildRef
    , "testDeepReference" ~: testDeepExplRef
    , "testDeepReferenceFail" ~: testDeepExplRefFail
    , "testEImp" ~: testEImp
    , "testWImp" ~: testWImp
    , "testImpObj" ~: testImpFromImprotedObj
    , "testImpUnboundName" ~: testImpUnbound
    , "testSelfImp" ~: testSelf
    , "testSelfImpSuper" ~: testSuperSelf
    , "testMultipleImports" ~: testMultipleImports
    , "testMultipleImportsFail" ~: testMultipleImportsFail
    , "testMutualImport" ~: testDoubleImports
    , "testWInvisible" ~: testWImpInvisible
    , "testNameClash" ~: testNameClash
    , "testImpNotTransitive" ~: testWImpNotTransitive
    , "testImpNotTransitive" ~: testEImpNotTransitive
    , "testMultipleExplicitImp" ~: testMExplImp
    , "testMultipleWildcardImp" ~: testMWImp
    , "testWImpForward" ~: testWImpForward
    -- , "testNestedWildcardFail" ~: testDeepWildRefFail

    -- precedence
    , "testMultipleParamClauses" ~: testBlockShadow
    , "testMultipleParamClauses" ~: testBlockShadowFail
    , "testDeepType" ~: testDeepType
    , "testDeepType" ~: testDeepTypeImp
    , "testInnerParamDouble" ~: testInnerParam
    , "testInnerShadowsSelf" ~: testInnerShadowsSelf
    , "testInnerTypeShadowsOuter" ~: testInnerTypeShadowsOuter
    , "testDeclShadowsPredef" ~: testDeclShadowsPredef
    , "testValAmbiguity" ~: testValAmbiguity
    , "testMixImport" ~: testMixImport
    , "testMultiplePaths" ~: testMultiplePathsSameValue
    , "testOuterPrecedsImp" ~: testOuterPrecedesInnerImp
    , "testOuterLocalDef" ~: testOuterLocalDef
    , "testParamShadowsOuter" ~: testParamShadowsOuter
    , "testParamShadowsOuter" ~: testParamShadowsOuterFail
    , "testParamShadowsSelf" ~: testParamShadowsSelf
    , "testDoubleWildcardImp" ~: testDoubleWImp
    , "testDoubleEImpNoShadow" ~: testEImpNoShadow
    , "testDoubleEImpShadow" ~: testEImpShadow
    , "testAmbiguousName" ~: testAmbiguousName
    , "testSpecificImpResolves" ~: testSpecificImpResolves
    , "testEShadowsInnerE" ~: testEShadowsInnerE
    , "testWNoShadow" ~: testWNoShadow
    , "testWNoShadowE" ~: testWNoShadowE
    , "testWNoShadowO" ~: testWNoShadowOuter
    , "testWImpResolves" ~: testWImpResolves
    -- , "testWImpShadowW" ~: testWShadowW
    -- , "testInnerShadowsOuter" ~: testInnerBlockShadowsOuter


    -- references
    -- "testPaperFail" ~: testPaperExFail
    , "testPathNoLex" ~: testPathNoLexical
    , "testPathNoTransitive" ~: testPathNoTransitive
    , "testQualifiedReference" ~: testQualifiedRef
    , "testQualifiedReferenceType" ~: testQualifiedRefTy
    , "testQualifiedReferenceType2" ~: testQualifiedRefTy2
    , "testUnbound" ~: testUnbound
    
    -- statements 
    , "testNestedTy" ~: testNestedObjTy
    , "testBodyOuterAccess" ~: testBodyOuterAccess
    , "testBodyOuterAccess" ~: testBodyOuterAccessImp
    , "testBodyOuterAccess" ~: testBodyOuterAccessImpFail
    , "testExprStatement" ~: testExprStatement
    , "testLaterBlockNoShadow" ~: testBlockNoShadow
    , "testExprStatementNo" ~: testExprStatementFail
    , "testMExpr" ~: testMExprStatement
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess