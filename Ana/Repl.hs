module Repl where

import Ana
import Parser

import System.IO           (hFlush, stdout)
import qualified Data.Map.Lazy as M

-- top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Ana REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl M.empty

-- stores variables from let expressions at runtime
-- terms are stored in left 
-- types are stored in right
type Environment = M.Map String (Either AnaTerm T)

-- REPL loop, takes input reduces and prints result, or exits out
repl :: Environment -> IO ()
repl env = do
  putStr ">   "
  hFlush stdout
  s <- getLine
  if Prelude.null s
  then putStrLn "Goodbye."
  else do
    if head s == '\''
    then case apply term (tail s) of
      (x:_) -> do
        if null $ snd x
        then case typeof' x' of
          Just _ -> mapM_ putStrLn . prependReductions x' $ reductions x'
          _ -> cannotType $ tail s
        else cannotParse s
          where x' = formatTerm (fst x) env
      _ -> cannotParse s
    else if head s == 't'
    then case apply term $ tail s of
      (x:_) -> do
        if null $ snd x
        then case typeof' $ fst x of
          Just y -> print y
          _ -> cannotType $ tail s
        else cannotParse s
      _ -> cannotParse s
    else case apply (pLet +++ pTypeLet +++ pTerm) s of
      [(("", Left t),"")] -> do      -- reducing a term
        case typeof' t' of
          Just _ -> putStrLn . prependTerm t' $ reduce t'
          _ -> cannotType $ show t'
          where t' = formatTerm t env
      [((v,Left t),"")] -> do       -- let expression
        case typeof' t' of
          Just _ -> do
            putStrLn $ "Saved term: " ++ show t'
            repl $ M.insert v (Left t') env
          _ -> cannotType s
          where t' = formatTerm t env
      [((v,Right t),"")] -> do       -- let expression
        putStrLn $ "Saved type: " ++ show t'
        repl $ M.insert v (Right t') env
        where t' = formatType t env
      _ -> cannotParse s
    repl env

-- takes a term and context and substitutes env terms
-- all free occurrences in the term
formatTerm :: AnaTerm -> Environment -> AnaTerm
formatTerm t1 env = foldl
  (\t (v,t2) -> case t2 of
    Left t2 ->
      if v `elem` vars t
      then substitute t (Var v, t2)
      else t
    Right t2 ->
      if v `elem` typeVarsInTerm t
      then tSubUnder t (TVar v, t2)
      else t) t1 $ M.assocs env

-- takes a type and context and substitutes env terms
-- all free occurrences in the term
formatType :: T -> Environment -> T
formatType t1 env = foldl
  (\t (v,t2) -> case t2 of
    Right t2 -> if v `elem` typeVars t
      then typeSub t (TVar v, t2)
      else t
    _ -> t) t1 $ M.assocs env

--function prepends ~> arrows or prints existing term if no reds occur
prependReductions :: AnaTerm -> [AnaTerm] -> [String]
prependReductions x xs = if null xs then ["=   "++show x] else
  map (\x -> "~>  " ++ show x) xs

--function prepends reduction ops for one multi-step reduction
prependTerm :: AnaTerm -> AnaTerm -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y

cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s

cannotType :: String -> IO ()
cannotType s = putStrLn $ (++) "Cannot Type Term: " s
