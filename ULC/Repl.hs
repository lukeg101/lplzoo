module Repl where

import ULC
import Parser

import System.IO           (hFlush, stdout)

replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Untyped \x03bb-calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  s <- getLine
  if Prelude.null s 
  then putStrLn "Goodbye."
  else do
    if head s == '\'' 
    then case apply expr (tail s) of
      (x:xs) -> mapM_ print . reductions $ fst x
      _ -> putStrLn $ "Cannot Parse Term:" ++ s
    else case apply expr s of
      (x:xs) -> print . reduce $ fst x
      _ -> putStrLn $ "Cannot Parse Term:" ++ s
    repl
