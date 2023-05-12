module Main where
import TypeChecker (runTC)
import ScSyntax (example)

main :: IO ()
main = do
    print $ runTC example