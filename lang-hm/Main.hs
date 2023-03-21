module Main where

import Syntax
import qualified HMScope as S
import qualified AlgJ as J

example :: MLy
example = Plus 
           (App 
            (Abs "f"
              (Let "y" 
                (App (Ident "f") $ Num 10)
                (App (Ident "f") $ Ident "y"))
            ) 
            (Abs "x" 
              (Plus (Ident "x") (Ident "x")))
           )
          (Num 2)

main :: IO ()
main = do
    print $ snd <$> J.runTC example
    print $ snd <$> S.runTC example