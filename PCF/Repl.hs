module Repl where

import PCF
import Parser

import System.IO           (hFlush, stdout)
import Control.Monad       (guard)

-- top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the PCF REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl []

-- stores variables from let expressions at runtime
type Environment = [(String,PCFTerm)]

-- REPL loop, takes input reduces and prints result, or exits out
repl :: Environment -> IO ()
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
        then case typeof' x' of
          Just y -> mapM_ putStrLn . prependReductions x' $ reductions x'
          _ -> cannotType $ tail s
        else cannotParse s
          where x' = formatTerm (fst x) ctx
      _ -> cannotParse s
    else if head s == 't'
    then case apply term $ tail s of
      (x:xs) -> do
        if (null $ snd x)
        then case typeof' $ fst x of
          Just y -> print y
          _ -> cannotType $ tail s
        else cannotParse s
      _ -> cannotParse s
    else case apply (pLet +++ pTerm) s of
      [(("",t),"")] -> do      -- reducing a term
        case typeof' t' of
          Just y -> putStrLn . prependTerm t' $ reduce t'
          _ -> cannotType s
          where t' = formatTerm t ctx 
      [((v,t),"")] -> do       -- let expression
        case typeof' t of
          Just y -> do 
            putStrLn $ "Saved: " ++ show t
            repl ((v,t):ctx)
          _ -> cannotType s
      _ -> cannotParse s  
    repl ctx

-- takes a term and context and substitutes context terms
-- all free occurrences in the term
formatTerm :: PCFTerm -> Environment -> PCFTerm
formatTerm t1 [] = t1
formatTerm t1 ((v,t2):xs) = if elem v vs 
  then formatTerm (substitute t1 (Var v, t2)) xs
  else formatTerm t1 xs
    where vs = vars t1

--function prepends ~> arrows or prints existing term if no reds occur
prependReductions :: PCFTerm -> [PCFTerm] -> [String]
prependReductions x xs = if null xs then ["=   "++show x] else 
  map (\x -> "~>  " ++ show x) xs

--function prepends reduction ops for one multi-step reduction
prependTerm :: PCFTerm -> PCFTerm -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y

cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s

cannotType :: String -> IO ()
cannotType s = putStrLn $ (++) "Cannot Type Term: " $ s