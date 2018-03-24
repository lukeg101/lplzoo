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
  putStr "> "
  hFlush stdout
  s <- getLine
  if Prelude.null s 
  then putStrLn "Goodbye."
  else do
    if head s == '\'' 
    then case apply term (tail s) of
      (x:xs) -> mapM_ print . reductions $ fst x
      _ -> putStrLn $ "Cannot Parse Term:" ++ s
    else case apply term s of
      (x:xs) -> print . reduce $ fst x
      _ -> putStrLn $ "Cannot Parse Term:" ++ s
    repl
