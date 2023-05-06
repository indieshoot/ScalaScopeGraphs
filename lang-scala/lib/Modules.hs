module Modules where

import Syntax
import Free.Scope
import Data.List

data ModTree
  = Anon [LModule] [ModTree] [LDecl]
  | Named String [LModule] [ModTree] [LDecl]
  deriving (Eq, Show)

data AnnotatedModTree
  = AAnon Sc [LModule] [AnnotatedModTree] [LDecl]
  | ANamed Sc String [LModule] [AnnotatedModTree] [LDecl]
  deriving (Eq, Show)

type ModWrapped = (String, [LDecl])

createModuleTree :: LProg -> ModTree
createModuleTree xs = let (is, ms, ls) = extract xs in Anon is (map traverseModule ms) ls

createModuleHops :: LModule -> [String]
createModuleHops (LMLiteral s) = [s]
createModuleHops (LMNested r s) = createModuleHops r ++ [s]

isEverythingImported :: AnnotatedModTree -> Bool
isEverythingImported (AAnon _ imports children _) = null imports && all isEverythingImported children
isEverythingImported (ANamed _ _ imports children _) = null imports && all isEverythingImported children

traceUnimported :: AnnotatedModTree -> [String]
traceUnimported (AAnon _ imports children _) = map (pr Nothing) imports ++ concatMap traceUnimported children
traceUnimported (ANamed _ n imports children _) = map (pr $ Just n) imports ++ concatMap traceUnimported children

pr :: Maybe String -> LModule -> String
pr q s = par q ++ intercalate "." (createModuleHops s)
  where
    par Nothing = ""
    par (Just s) = s ++ ": "

traverseModule :: ModWrapped -> ModTree
traverseModule (name, xs) = let (is, ms, ls) = extract xs in Named name is (map traverseModule ms) ls

extract :: [LDecl] -> ([LModule], [ModWrapped], [LDecl])
extract = extract' ([], [], [])

extract' :: ([LModule], [ModWrapped], [LDecl]) -> [LDecl] -> ([LModule], [ModWrapped], [LDecl])
extract' s [] = s
extract' (is, ms, ls) ((LImport x):xs) = extract' (x:is, ms, ls) xs
extract' (is, ms, ls) ((LMod n ds):xs) = extract' (is, (n, ds) : ms, ls) xs
extract' (is, ms, ls) (l:xs) = extract' (is, ms, l : ls) xs