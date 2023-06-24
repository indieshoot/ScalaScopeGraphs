module Main where
import TypeChecker (runTCPhased)

main :: IO ()
main = do
    print $ runTCPhased []
