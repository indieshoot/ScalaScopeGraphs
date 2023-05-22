module ATermParser where

import ATerms.ATerm
import qualified ATerms.Parser as P
import ScSyntax

-- code inspiration from https://github.com/Arraying/ScopeGraphs/blob/imports/lang-lmr/lib/AParser.hs

parse :: String -> IO (Either String ScProg)
parse s = do
  -- A bit messy because we are working with IO here.
  prog <- parseATerm s
  return $ prog >>= mapper
  where
    mapper :: ATerm -> Either String [ScDecl]
    mapper (AFunc "Program" [_ignored, decls]) = mapM parseDecl $ toList decls
    mapper t = Left $ "Unknown top level statement: " ++ show t


parseDecl :: ATerm -> Either String ScDecl
-- parseDecl (AFunc "Module" [AStr name, decls]) = do
--   decls' <- mapM parseDecl $ toList decls
--   return $ ScMod name decls'    
parseDecl (AFunc "Def" (b:_)) = do
  (p, q) <- parseBind b
  t <- parseType b
  return $ ScVal (ScParam p t) q
  where
    parseBind (AFunc "DefBind" [AStr n, e]) = do
      e' <- parseExp e
      return (n, e')
    parseBind t = Left $ "Unknown bind: " ++ show t
parseDecl t = Left $ "Unknown declaration: " ++ show t


parseExp :: ATerm -> Either String ScExp
parseExp (AFunc "Int" [AStr v]) = return $ ScNum (read v :: Int)
parseExp (AFunc "True" []) = return $ (ScBool True)
parseExp (AFunc "False" []) = return $ (ScBool False)
parseExp (AFunc "Var" [v]) = do
  v' <- parseVar v
  return $ v'
parseExp (AFunc "If" [c, t, f]) = do
  c' <- parseExp c
  t' <- parseExp t
  f' <- parseExp f
  return $ ScIf c' t' f'
parseExp (AFunc "Fun" [AFunc "ArgDecl" [AStr n, a], b]) = do
  a' <- parseType a
  b' <- parseExp b
  return $ ScFun (ScParam n a') b'
parseExp (AFunc "App" [fn, a]) = do
  fn' <- parseExp fn
  a' <- parseExp a
  return $ ScApp fn' a'
parseExp (AFunc bin [l, r]) = do
  l' <- parseExp l
  r' <- parseExp r
  case bin of
    "Add" -> return $ ScPlus l' r'
    -- "Sub" -> return $ ScBinOp l' ScMinus r'
    -- "Mul" -> return $ ScBinOp l' ScMult r'
    -- "Eq" -> return $ ScBinOp l' ScEquals r'
    _ -> Left $ "Unknown binop: " ++ bin 
parseExp t = Left $ "Unknown construct: " ++ show t

parseType :: ATerm -> Either String Type
parseType (AFunc "TInt" []) = return NumT
parseType (AFunc "TBool" []) = return BoolT
parseType t = Left $ "Unknown type: " ++ show t


parseVar :: ATerm -> Either String ScExp
parseVar (AFunc "VarRef" [AStr n]) = return $ ScId n
parseVar t = Left $ "Unknown variable: " ++ show t

toList :: ATerm -> [ATerm]
toList ANil = []
toList (ACons h t) = h : toList t
toList x = [x]


-- Parses the ATerm of a file.
parseATerm :: String -> IO (Either String ATerm)
parseATerm path = P.parse <$> readFile path