module Repl where

import ULC
import Parser

import System.IO           (hFlush, stdout)

-- top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Untyped \x03bb-calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl

-- REPL loop, takes input reduces and prints result, or exits out
repl :: IO ()
repl = do
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
    else case apply term s of
      (x:xs) -> do
        if (null $ snd x)
        then putStrLn . prependTerm (fst x) $ reduce $ fst x
        else cannotParse s
      _ -> cannotParse s
    repl

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

