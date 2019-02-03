{-|
Module      : Repl
Description : The Read-Eval-Print-Loop for SKI.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Repl" module provides the Read-eval-print-loop for SKI.
This is the top level interface between the user and the interpreter.
-}
module Repl where

-- SKI imports.
import qualified SKI
import Parser
  (pLet, pTerm, apply, term, (+++))


-- Tool Imports.
import System.IO           (hFlush, stdout)
import qualified Data.Map.Lazy as M 


-- | Top-level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the SKI combinator calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl M.empty


-- | Stores variables from let expressions at runtime
type Environment = M.Map String SKI.SKTerm


-- | REPL loop, takes input reduces and prints result, or exits out.
repl :: Environment -> IO ()
repl env = do putStr ">   "
              hFlush stdout
              s <- getLine
              if null s 
                then putStrLn "Goodbye."
                else do {parseTerm s env; repl env}


-- | Takes a term and context and substitutes env terms
-- into all free occurrences in the term
formatTerm :: SKI.SKTerm -> Environment -> SKI.SKTerm
formatTerm t1 env = foldl 
  (\t (v,t2) -> 
    if v `elem` SKI.vars t1 
      then SKI.substitute t (SKI.Var v, t2) 
      else t) t1 $ M.assocs env


-- | Function prepends ~> arrows or prints existing term if no reds occur
prependReductions :: SKI.SKTerm -> [SKI.SKTerm] -> [String]
prependReductions x xs = if null xs then ["=   "++show x] else 
  map (\x -> "~>  " ++ show x) xs


-- | Helper function to print the reduction steps of a term
printReductions :: String -> IO ()
printReductions s 
  = let prepend x = prependReductions (fst x) . SKI.reductions $ fst x
    in case apply term (tail s) of
      (x:_) -> if null $ snd x
                 then mapM_ putStrLn $ prepend x
                 else cannotParse s
      _     -> cannotParse s

-- | Helper function parses the term as either a Let expression or a raw term.
-- Let expressions update the environment with new variables, using any existing
-- variables in scope. For a raw term, evaluation is attempted after substituting
-- in bound terms from the environment. For SKI termination is not guaranteed.
parseTerm :: String -> Environment -> IO ()
parseTerm s env
  = if head s == '\'' 
      then printReductions s
      else case apply (pLet +++ pTerm) s of
             [(("",t),"")] -> putStrLn . prependTerm t' $ SKI.reduce t'
                              where t' = formatTerm t env 
             [((v,t),"")]  -> do putStrLn $ "Saved: " ++ show t'
                                 repl $ M.insert v t' env
                              where t' = formatTerm t env
             _             -> cannotParse s


-- | Function prepends reduction ops for one multi-step reduction
prependTerm :: SKI.SKTerm -> SKI.SKTerm -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y


-- | Error message for parse failure
cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s

