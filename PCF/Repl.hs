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
        then case typeof' $ fst x of
          Just y -> mapM_ print $ reductions (fst x)
          _ -> cannotType $ tail s
        else cannotParse s
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
    else case apply term s of
      (x:xs) -> do
        if (null $ snd x)
        then case typeof' $ fst x of
          Just y -> print . reduce $ fst x
          _ -> cannotType s
        else cannotParse s
      _ -> cannotParse s
    repl


cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s

cannotType :: String -> IO ()
cannotType s = putStrLn $ (++) "Cannot Type Term: " $ s