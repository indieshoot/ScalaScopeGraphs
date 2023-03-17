module Main where

import Syntax
import qualified HMScope as S

main :: IO ()
main = do
    print $ S.runTC example