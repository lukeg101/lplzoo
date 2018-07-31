module Repl where

import ULC
import Parser

import System.IO           (hFlush, stdout)
import qualified Data.Map.Lazy as M 

-- top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Untyped \x03bb-calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl M.empty

-- stores variables from let expressions at runtime
type Environment = M.Map String Term

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
      (x:xs) -> do
        if (null $ snd x)
        then mapM_ putStrLn . prependReductions (fst x) . reductions $ fst x
        else cannotParse s
      _ -> cannotParse s
    else case apply (pLet +++ pTerm) s of
      [(("",t),"")] -> do      -- reducing a term
        putStrLn . prependTerm t' $ reduce t'
        where t' = formatTerm t env 
      [((v,t),"")] -> do       -- let expression
        putStrLn $ "Saved: " ++ show t'
        repl $ M.insert v t' env
        where t' = formatTerm t env
      _ -> cannotParse s    
  repl env


-- takes a term and context and substitutes env terms
-- all free occurrences in the term
formatTerm :: Term -> Environment -> Term
formatTerm t1 env = foldl 
  (\t (v,t2) -> 
    if elem v (vars t1) 
    then substitute t (Var v, t2) 
    else t) t1 $ M.assocs env

--function prepends ~> arrows or prints existing term if no reds occur
prependReductions :: Term -> [Term] -> [String]
prependReductions x xs = if null xs then ["=   "++show x] else 
  map (\x -> "~>  " ++ show x) xs

--function prepends reduction ops for one multi-step reduction
prependTerm :: Term -> Term -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y


--error message for parse failure
cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s

