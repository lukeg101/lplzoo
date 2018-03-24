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
    then case apply term (tail s) of
      (x:xs) -> case typeof' $ fst x of
        Just y -> mapM_ print $ reductions (fst x)
        _ -> cannotType $ tail s
      _ -> cannotParse s
    else if head s == 't'
    then case apply term $ tail s of
      (x:xs) -> case typeof' $ fst x of
        Just y -> print y
        _ -> cannotType $ tail s
      _ -> cannotParse s
    else case apply term s of
      (x:xs) -> case typeof' $ fst x of
        Just y -> print . reduce $ fst x
        _ -> cannotType s
      _ -> cannotParse s
    repl


cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s

cannotType :: String -> IO ()
cannotType s = putStrLn $ (++) "Cannot Type Term: " $ tail s