module Main where

import Tests
import qualified System.Exit as Exit

import Test.HUnit

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess