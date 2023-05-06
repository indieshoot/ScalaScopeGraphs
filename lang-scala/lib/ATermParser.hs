module AParser where

import ATerms.ATerm
import qualified ATerms.Parser as P
import Syntax

parse :: String -> IO (Either String LProg)
parse s = do
  -- A bit messy because we are working with IO here.
  prog <- parseATerm s
  return $ prog >>= mapper
  where
    mapper :: ATerm -> Either String [LDecl]
    mapper (AFunc "Program" [_ignored, decls]) = mapM parseDecl $ listify decls
    mapper t = Left $ "Unknown top level statement: " ++ show t

-- Currently unsupported:
-- * Records
parseDecl :: ATerm -> Either String LDecl
parseDecl (AFunc "Module" [AStr name, decls]) = do
  decls' <- mapM parseDecl $ listify decls
  return $ LMod name decls'    
parseDecl (AFunc "Import" [n]) = do
  n' <- parseMod n
  return $ LImport n'
parseDecl (AFunc "Def" bs) = do
  bs' <- mapM parseBind bs
  return $ LDef bs'
  where
    parseBind (AFunc "DefBind" [AStr n, e]) = do
      e' <- parseExp e
      return (n, e')
    parseBind t = Left $ "Unknown bind: " ++ show t
parseDecl t = Left $ "Unknown declaration: " ++ show t

-- Currently unsupported:
-- * LetRec (multiple bindings)
-- * New
-- * With
-- * Ref
parseExp :: ATerm -> Either String LExp
parseExp (AFunc "Int" [AStr v]) = return $ Num (read v :: Int)
parseExp (AFunc "True" []) = return Tru
parseExp (AFunc "False" []) = return Fls
parseExp (AFunc "Var" [v]) = do
  v' <- parseVar v
  return $ Id v'
parseExp (AFunc "If" [c, t, f]) = do
  c' <- parseExp c
  t' <- parseExp t
  f' <- parseExp f
  return $ If c' t' f'
parseExp (AFunc "Fun" [AFunc "ArgDecl" [AStr n, a], b]) = do
  a' <- parseType a
  b' <- parseExp b
  return $ Fn (n, a') b'
parseExp (AFunc "App" [fn, a]) = do
  fn' <- parseExp fn
  a' <- parseExp a
  return $ App fn' a'
parseExp (AFunc "LetRec" [ACons (AFunc "DefBind" [AStr n, e]) _, b]) = do
  e' <- parseExp e
  b' <- parseExp b
  return $ LetRec (n, e') b'
parseExp (AFunc bin [l, r]) = do
  l' <- parseExp l
  r' <- parseExp r
  case bin of
    "Add" -> return $ Plus l' r'
    "Sub" -> return $ Minus l' r'
    "Mul" -> return $ Mult l' r'
    "Eq" -> return $ Eql l' r'
    _ -> Left $ "Unknown binop: " ++ bin 
parseExp t = Left $ "Unknown construct: " ++ show t

parseType :: ATerm -> Either String LType
parseType (AFunc "TInt" []) = return LInt
parseType (AFunc "TBool" []) = return LBool
parseType t = Left $ "Unknown type: " ++ show t

parseMod :: ATerm -> Either String LModule
parseMod (AFunc "ModRef" [AStr n]) = return $ LMLiteral n
parseMod (AFunc "ModQRef" [m, AStr n]) = do
  m' <- parseMod m
  return $ LMNested m' n
parseMod t = Left $ "Unknown module: " ++ show t

parseVar :: ATerm -> Either String LIdent
parseVar (AFunc "VarRef" [AStr n]) = return $ LILiteral n
parseVar (AFunc "VarQRef" [m, AStr n]) = do
  m' <- parseMod m
  return $ LINested m' n
parseVar t = Left $ "Unknown variable: " ++ show t

listify :: ATerm -> [ATerm]
listify ANil = []
listify (ACons h t) = h : listify t
listify x = [x]

-- Parses the ATerm of a file.
parseATerm :: String -> IO (Either String ATerm)
parseATerm path = P.parse <$> readFile path