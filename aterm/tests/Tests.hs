module Tests where

import ATerms.ATerm
import ATerms.Parser
import Data.Either
import Test.HUnit

assertParseSucceeds :: ATerm -> String -> IO ()
assertParseSucceeds exp act =
  either
    assertFailure
    (assertEqual "Parse result not equal to expected term." exp)
    (parse act)

testParseString :: Test
testParseString = TestCase $ assertParseSucceeds (AStr "Hello World!") "\"Hello World!\""

testParseNil :: Test
testParseNil = TestCase $ assertParseSucceeds ANil "[]"

testParseFuncNoArgs :: Test
testParseFuncNoArgs = TestCase $ assertParseSucceeds (AFunc "C" []) "C()"

testParseFuncNested :: Test
testParseFuncNested = TestCase $ assertParseSucceeds (AFunc "C" [AFunc "D" [], AFunc "E" [AFunc "F" []]]) "C(D(), E(F()))"

testParseZeroTuple :: Test
testParseZeroTuple = TestCase $ assertParseSucceeds (ATuple []) "()"

testParseTuple :: Test
testParseTuple = TestCase $ assertParseSucceeds (ATuple [AFunc "A" [], ANil, ATuple []]) "(A(), [], ())"

testParseConc :: Test
testParseConc = TestCase $ assertParseSucceeds (ACons (AFunc "C" []) ANil) "[C()]"

testParseConc2 :: Test
testParseConc2 = TestCase $ assertParseSucceeds (ACons (AFunc "C" []) (ACons (AFunc "D" [ACons (AFunc "C" []) (ACons ANil ANil)]) ANil)) "[C(), D([C(), []])]"

tests :: [Test]
tests =
    [ "testParseString" ~: testParseString,
      "testParseNil" ~: testParseNil,
      "testParseFuncNoArgs" ~: testParseFuncNoArgs,
      "testParseFuncNested" ~: testParseFuncNested,
      "testParseZeroTuple"  ~: testParseZeroTuple,
      "testParseTuple"      ~: testParseTuple,
      "testParseConc"       ~: testParseConc,
      "testParseConc2"      ~: testParseConc2
    ]
