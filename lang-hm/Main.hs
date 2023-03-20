module Main where

import Syntax
import qualified HMScope as S
import qualified AlgJ as J

example :: MLy
example = Let "g"
              (Abs "y"
                (Let "f"
                  (Abs "x"
                  $ Ident "y")
                  (Let "_" (App (Ident "f") (Num 0)) (Ident "f"))))
              (Ident "g")

main :: IO ()
main = do
    print $ J.runTC example
    print $ S.runTC example