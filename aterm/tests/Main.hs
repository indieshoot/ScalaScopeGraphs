module Main where

import qualified System.Exit as Exit

import Test.HUnit

import Tests
import Integration

main :: IO ()
main = do
    itest <- testCases
    result <- runTestTT $ TestList $ tests ++ itest
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess