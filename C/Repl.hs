{-|
Module      : Repl
Description : The Read-Eval-Print-Loop for C.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Repl" module provides the Read-eval-print-loop for C.
This is the top level interface between the user and the interpreter.
-}
module Repl where

-- C Imports.
import C
import Parser
  ((+++), apply, PExpr(PTerm, PAssume), term, pLet, pTerm, pAssume)


-- Tool Imports.
import qualified System.IO     as IO (hFlush, stdout)
import qualified Data.Map.Lazy as M


-- | Top level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the C REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl M.empty M.empty


-- | Stores variables from let expressions at runtime
-- terms are stored in left 
-- types are stored in right
type Environment = M.Map VarName CTerm


-- | REPL loop, takes input reduces and prints result, or exits out
repl :: Environment -> Context -> IO ()
repl env ctx = do putStr ">   "
                  IO.hFlush IO.stdout
                  s <- getLine
                  if Prelude.null s 
                    then putStrLn "Goodbye."
                    else do {parseTerm s env ctx; repl env ctx}



-- | Helper function parses the term as either a Let expression, assume, or a raw term.
-- Let expressions update the environment with new variables, using any existing
-- variables in scope. 
-- Assumes add new symbols with types to the context so we may introduce new types
-- this is needed for dependent typing etc...
-- For a raw term, evaluation is attempted after substituting
-- in bound terms from the environment and typechecking in the context.
parseTerm :: String -> Environment -> Context -> IO ()
parseTerm s env ctx
  | head s == '\'' = printReductions s env ctx
  | head s == 't'  = printType s env ctx
  | otherwise      = case apply (pLet +++ pTerm +++ pAssume) s of
                       [(("", PTerm t),"")]   -> -- reducing a term
                         case typeof t' ctx of
                           Just _ -> putStrLn . prependTerm t' $ reduce t'
                           _      -> cannotType $ show t'
                         where t' = formatTerm t env
                       [((v, PAssume t),"")]  -> -- let expression
                         case typeof t' ctx of
                           Just _ -> do let str = "Saved typing: " ++ v ++ ":" ++show t'
                                        putStrLn str
                                        repl env (M.insert v t' ctx)
                           _      -> cannotType $ show t'
                         where t' = formatTerm t env
                       [((v, PTerm t),"")]    -> -- let expression
                         case typeof t' ctx of
                           Just _ -> do let str = "Saved term: " ++ v ++ " = " ++ show t'
                                        putStrLn str
                                        repl (M.insert v t' env) ctx
                           _      -> cannotType $ show t' 
                         where t' = formatTerm t env
                       _                      -> cannotParse s
                       


-- | Helper function to print the reduction steps of a term
printReductions :: String -> Environment -> Context -> IO ()
printReductions s env ctx
  = let prepend x = prependReductions x $ reductions x
    in case apply term (tail s) of
         (x:_) -> if null $ snd x
                    then case typeof x' ctx of
                      Just _ -> mapM_ putStrLn $ prepend x'
                      _      -> cannotType $ show x'
                    else cannotParse s
          where x' = formatTerm (fst x) env
         _     -> cannotParse s


-- | Helper function to print the type of a term
printType :: String -> Environment -> Context -> IO ()
printType s env ctx
  = case apply term $ tail s of
    (x:_) -> if null $ snd x
               then case typeof x' ctx of
                      Just y -> print $ reduce y
                      _      -> cannotType $ show x'
               else cannotParse s
             where x' = formatTerm (fst x) env
    _     -> cannotParse s


-- | Takes a term and context and substitutes env terms
-- into all free occurrences in the term
formatTerm :: CTerm -> Environment -> CTerm
formatTerm t1 env = foldl 
  (\t (v,t2) ->if v `elem` vars t 
                 then substitute t (Var v, t2) 
                 else t) t1 $ M.assocs env


-- | Function prepends ~> arrows or prints existing term if no reds occur
prependReductions :: CTerm -> [CTerm] -> [String]
prependReductions x xs = if null xs then ["=   "++show x] else 
  map (\x -> "~>  " ++ show x) xs


-- | Function prepends reduction ops for one multi-step reduction
prependTerm :: CTerm -> CTerm -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y


-- | Error message for parse failure
cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s


-- | Error message for typing failure
cannotType :: String -> IO ()
cannotType s = putStrLn $ (++) "Cannot Type Term: " s

