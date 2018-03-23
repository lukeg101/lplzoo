module Repl where

import STLC
import Parser

import System.IO           (hFlush, stdout)

-- top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Simply Typed \x03bb-calculus REPL"
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
    then case apply expr (tail s) of
      (x:xs) -> case typeof' $ fst x of
        Just y -> mapM_ print $ reductions (fst x)
        _ -> print $ (++) "Cannot Type Term:" $ show (fst x)
      _ -> print $ "Cannot Parse Term:" ++ s
    else case apply expr s of
      (x:xs) -> case typeof' $ fst x of
        Just y -> print . reduce $ fst x
        _ -> print $ (++) "Cannot Type Term:" $ show (fst x)
      _ -> print $ "Cannot Parse Term:" ++ s
    repl