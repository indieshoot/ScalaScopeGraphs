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

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess