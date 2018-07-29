module Repl where

import ULC
import Parser

import System.IO           (hFlush, stdout)

-- top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Untyped \x03bb-calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl []

-- stores variables from let expressions at runtime
type Context = [(String,Term)]

-- REPL loop, takes input reduces and prints result, or exits out
repl :: Context -> IO ()
repl ctx = do
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
        where t' = formatTerm t ctx 
      [((v,t),"")] -> do       -- let expression
        putStrLn $ "Saved: " ++ show t
        repl ((v,t):ctx)
      _ -> cannotParse s    
  repl ctx

-- takes a term and context and substitutes context terms
-- all free occurrences in the term
formatTerm :: Term -> Context -> Term
formatTerm t1 [] = t1
formatTerm t1 ((v,t2):xs) = if elem v vs 
  then formatTerm (substitute t1 (Var v, t2)) xs
  else formatTerm t1 xs
    where vs = vars t1

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

