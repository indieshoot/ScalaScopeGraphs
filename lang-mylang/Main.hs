module Main where
import TypeCheck (runTC)
import Syntax (example)

main :: IO ()
main = do
    print $ runTC example
