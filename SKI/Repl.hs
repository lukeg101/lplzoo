module Repl where

import SKI
import Parser

import System.IO           (hFlush, stdout)

-- top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the SKI combinator calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl

-- REPL loop, takes input reduces and prints result, or exits out
repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  s <- getLine
  return ()
  if Prelude.null s 
  then putStrLn "Goodbye."
  else do
    if head s == '\'' 
    then case apply term (tail s) of
      (x:xs) -> do
        if (null $ snd x)
        then mapM_ print . reductions $ fst x
        else cannotParse s
      _ -> cannotParse s
    else case apply term s of
      (x:xs) -> do
        if (null $ snd x)
        then print . reduce $ fst x
        else cannotParse s
      _ -> cannotParse s
    repl

cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s

