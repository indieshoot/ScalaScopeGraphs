module Integration where

import ATerms.Parser

import System.IO

import System.Directory
import System.Directory.Recursive

import Test.HUnit
import Data.List (isSuffixOf)

testFile2Case :: FilePath -> IO Test
testFile2Case path = do
    text <- readFile path
    putStrLn $ "Added test case: " ++ path
    result <- either assertFailure (const $ return True) (parse text)
    return $ path ~: assertBool "" result

testCases :: IO [Test]
testCases = do 
    cases <- getFilesRecursive "aterm-res"
    let cases' = filter isAterm cases
    mapM_ putStrLn cases'
    mapM testFile2Case cases'

isAterm :: FilePath -> Bool
isAterm = isSuffixOf ".aterm"
