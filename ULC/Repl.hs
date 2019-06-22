{-|
Module      : Repl
Description : The Read-Eval-Print-Loop for ULC.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com
Stability   : stable
Portability : POSIX

The "Repl" module provides the Read-eval-print-loop for ULC.
This is the top level interface between the user and the interpreter.
-}
module Repl where

-- ULC Imports.
import ULC
  (Term(Var), substitute, vars, reductions, reduce)
import Parser
  (apply, term, pLet, pTerm, (+++))


-- Tool Imports.
import qualified Data.Map.Lazy as M

-- Haskeline imports
import Data.List
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import System.Console.Haskeline

-- | Top-level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Untyped \x03bb-calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  evalStateT (runInputT settings repl) M.empty

-- | Stores variables from let expressions at runtime
type Environment = M.Map String ULC.Term
type Interpreter = InputT (StateT Environment IO)

settings :: Settings (StateT Environment IO)
settings = Settings contextCompletion Nothing True

contextCompletion :: CompletionFunc (StateT Environment IO)
contextCompletion = completeWord Nothing " ()\\." completions
  where
    completions :: String -> StateT Environment IO [Completion]
    completions xs = map (\x -> Completion x x True) . filter (xs `isPrefixOf`) . M.keys <$> get

interruptible :: MonadException m => a -> InputT m a -> InputT m a
interruptible d x = handle (\Interrupt -> outputStrLn "interrupted" >> pure d) (withInterrupt x)

-- | REPL loop, takes input reduces and prints result, or exits out.
repl :: Interpreter ()
repl = do ms <- getInputLine ">   "
          case ms of
            Nothing -> repl
            Just "" -> outputStrLn "Goodbye."
            Just s  -> do env  <- lift get
                          env' <- interruptible env (liftIO (parseTerm s env))
                          lift (put env')
                          repl

-- | Takes a term and context and substitutes env terms
-- into all free occurrences in the term
formatTerm :: ULC.Term -> Environment -> ULC.Term
formatTerm t1 env = foldl
  (\t (v,t2) ->
    if v `elem` ULC.vars t1
      then ULC.substitute t (ULC.Var v, t2)
      else t) t1 $ M.assocs env


-- | Function prepends ~> arrows or prints existing term if no reds occur
prependReductions :: ULC.Term -> [ULC.Term] -> [String]
prependReductions x xs = if null xs then ["=   "++show x] else
  map (\x -> "~>  " ++ show x) xs


-- | Function prepends reduction ops for one multi-step reduction
prependTerm :: ULC.Term -> ULC.Term -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y


-- | Error message for parse failure
cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s


-- | Helper function to print the reduction steps of a term
printReductions :: String -> IO ()
printReductions s
  = let prepend x = prependReductions (fst x) . ULC.reductions $ fst x
    in case apply term (tail s) of
         (x:_) -> if null (snd x)
                    then mapM_ putStrLn $ prepend x
                    else cannotParse s
         _     -> cannotParse s


-- | Helper function parses the term as either a Let expression or a raw term.
-- Let expressions update the environment with new variables, using any existing
-- variables in scope. For a raw term, evaluation is attempted after substituting
-- in bound terms from the environment. For ULC termination is not guaranteed.
parseTerm :: String -> Environment -> IO Environment
parseTerm s env
  = if head s == '\''
      then printReductions s >> pure env
      else case apply (pLet +++ pTerm) s of
             [(("",t),"")] -> do let t' = formatTerm t env
                                 putStrLn (prependTerm t' $ reduce t')
                                 pure env
             [((v,t),"")]  -> do let t' = formatTerm t env
                                 putStrLn $ "Saved: " ++ show t'
                                 pure (M.insert v t' env)
             _             -> cannotParse s >> pure env
