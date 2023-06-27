module Main where

import Test.HUnit
    ( (~:), runTestTT, Counts(failures, errors), Test(TestList) )
    
import TypeChecker (Label, Decl, runTCPhased)
import qualified System.Exit as Exit
import Free.Scope (Graph)
import ScSyntax

import Statix.Comprehensive
import Statix.Defs
import Statix.Expressions
import Statix.Imports
import Statix.Precedence
import Statix.References
import Statix.Statements


----------------------------
-- Mini-Statix Test Suite --
----------------------------

-- The original test suite can be found at: https://github.com/MetaBorgCube/scala.mstx/tree/master/tests

-- Test suite comprising of 87 passing tests. 
tests :: Test
tests = TestList
    [  
    -- Comprehensive
     "testPaperEx" ~: testPaperExample
    , "testPaper2" ~: testPaper

    -- Defs
    , "testTypeAndValOverlapImp" ~: testTypeAndValOverlapImp
    , "testTypeNotFound" ~: testTypeNoFound
    , "testTypeFound" ~: testTypeFound
    , "testMutualDefs" ~: testMutualDefs
    , "testRecursiveDefs" ~: testRecDefs
    , "testNestedObjects" ~: testNestedObj
    , "testAllSameName" ~: testSameNameDef
    , "testDuplicateDefinition" ~: testDuplicateVal
    , "testDuplicateType" ~: testDuplicateType
    , "testForwardReference"  ~: testForwardRef
    , "testMultipleParams" ~: testMultipleParamClauses
    , "testMultipleParams" ~: testMultipleParamClausesMultipleParams
    , "testMultipleParamsFail" ~: testMultipleParamClausesFail
    , "testTypeAndValOverlap" ~: testTypeAndValOverlap
    , "testTypeDeclaration" ~: testTypeDecl

    -- Expressions
    , "testCurry" ~: testCurry
    , "testSingleCurry" ~: testSingleCurry
    , "testSingleCurryFail" ~: testSingleCurryFail
    , "testFunctionType" ~: testFunctionValRef
    , "testFunctionCall" ~: testFunctionCall
    , "testFunctionCallFail" ~: testFunctionCallNo
    , "testFunctionCallRef" ~: testFunctionRef
    , "testZeroCurryFail" ~: testZeroCurryFail

    -- Imports
    , "testNestedWildcardImp" ~: testDeepWildRef
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

    -- Precedence
    , "testMultipleParamClauses" ~: testBlockShadow
    , "testMultipleParamClauses" ~: testBlockShadowFail
    , "testDeepType" ~: testDeepType
    , "testDeepTypeImp" ~: testDeepTypeImp
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

    -- References
    , "testPathNoLex" ~: testPathNoLexical
    , "testPathNoTransitive" ~: testPathNoTransitive
    , "testQualifiedReference" ~: testQualifiedRef
    , "testQualifiedReferenceType" ~: testQualifiedRefTy
    , "testQualifiedReferenceType2" ~: testQualifiedRefTy2
    , "testUnbound" ~: testUnbound
    
    -- Statements 
    , "testNestedTy" ~: testNestedObjTy
    , "testBodyOuterAccess" ~: testBodyOuterAccess
    , "testBodyOuterAccess" ~: testBodyOuterAccessImp
    , "testBodyOuterAccess" ~: testBodyOuterAccessImpFail
    , "testExprStatement" ~: testExprStatement
    , "testLaterBlockNoShadow" ~: testBlockNoShadow
    , "testExprStatementFail" ~: testExprStatementFail
    , "testMExpr" ~: testMExprStatement
    ]


-- In addition, there are 3 tests that present the right behavior through the structure of our Syntax:

-- 1) test fails since the type takes a type on the rhs
-- object O {
--   val x : Int = 42;
--   type X = x;
-- };

-- 2) test fails since val takes an expression on the rhs
-- object O {
--   type X = Int;
--   val x : Int = X;
-- };

-- object O {
--   type J = Int;
--   object P {
--     val x : I = 42;
--   };
-- };


-- 3) 
-- object A {
--   object B {
--     val x : Int = 42;
--   };
-- };

-- object C {
--   import A.B;
--   val y : Int = x;
-- };


main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess