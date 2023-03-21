module Unification where

import Data.Map
import Data.Term
import Free.Logic.Equals
import Test.HUnit (assertFailure, assertEqual, Test (TestList), (~:))

emptyUMap :: UMap Int 
emptyUMap = empty

assertUnificationSucceed :: (Eq c, Show c) => Term c -> Term c -> UMap c -> IO (UMap c)
assertUnificationSucceed t1 t2 u = either
  (\(UnificationError t1 t2) -> assertFailure $ "Cannot unify " ++ show t1 ++ " and " ++ show t2)
  return
  (unify t1 t2 u)

assertTermsEqual :: (Eq c, Show c) => Term c -> Term c -> UMap c -> IO ()
assertTermsEqual t1 t2 u = assertEqual "Terms not equal" (explicate u t1) (explicate u t2)

testUnifyVars :: IO ()
testUnifyVars = do
  let t1 = Var 1
  let t2 = Var 2
  u <- assertUnificationSucceed @Int t1 t2 empty
  assertTermsEqual t1 t2 u

testUnifyVarToTerm :: IO ()
testUnifyVarToTerm = do
  let t1 = Var 1
  let t2 = Term "C" [Const 1]
  u <- assertUnificationSucceed t1 t2 empty
  assertTermsEqual t1 t2 u

testUnifyVarToConst :: IO ()
testUnifyVarToConst = do
  let t1 = Var 1
  let t2 = Const 1
  u <- assertUnificationSucceed t1 t2 empty
  assertTermsEqual t1 t2 u



tests :: Test
tests = TestList
  [ "testUnifyVars"             ~: testUnifyVars 
  , "testUnifyVarToConst"       ~: testUnifyVarToConst
  , "testUnifyVarToTerm"        ~: testUnifyVarToTerm ]