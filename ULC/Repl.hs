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
import Parser
  (Command(..), parseReplCommand)


-- Tool Imports.
import           Data.List
import qualified Data.Map as M

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import System.Console.Haskeline
import System.Directory

-- | Top-level repl function
replMain :: IO ()
replMain = do
  putStrLn "Welcome to the Untyped \x03bb-calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  evalStateT (runInputT settings repl) M.empty


-- | Stores variables from let expressions at runtime
type Environment = M.Map String ULC.Term
type Interpreter = InputT (StateT Environment IO)


-- | Settings defining how tab-completion and input history work for the Haskeline session.
settings :: Settings (StateT Environment IO)
settings = Settings contextCompletion Nothing True
  where
    contextCompletion :: CompletionFunc (StateT Environment IO)
    contextCompletion = completeWord Nothing " ()\\." completions

    completions :: String -> StateT Environment IO [Completion]
    completions xs = map (\x -> Completion x x True) . filter (xs `isPrefixOf`) . M.keys <$> get


-- | Marks an InputT action as interruptible, so that pressing Ctrl+C will terminate
-- the action, returning a default value, instead of terminating the program.
interruptible :: MonadException m => a -> InputT m a -> InputT m a
interruptible d x = handle (\Interrupt -> outputStrLn "interrupted" >> pure d) (withInterrupt x)

-- | REPL loop, takes input reduces and prints result, or exits out.
repl :: Interpreter ()
repl = do ms <- getInputLine ">   "
          case ms of
            Nothing -> repl
            Just "" -> outputStrLn "Goodbye."
            Just s  -> do case parseReplCommand s of
                            Nothing -> outputStrLn "Could not parse!"
                            Just  c -> do env <- lift get
                                          interruptible () (handleCommand (formatCommand env c))
                          repl

-- Dispatches a procedure for each type of command.
handleCommand :: Command -> Interpreter ()
handleCommand (T     t) = outputStrLn (prependTerm t (reduce t))
handleCommand (Reds  t) = mapM_ outputStrLn (prependedReductions t)
handleCommand (Load fp) = handleLoad fp
handleCommand (Let v t) = do lift get >>= lift . put . M.insert v t
                             outputStrLn ("Saved term " ++ show t)

-- | Substitutes named terms from the environment into
-- free occurrences of those names within command terms
formatCommand :: Environment -> Command -> Command
formatCommand env c = case c of
                        T     t -> T     (formatTerm t)
                        Reds  t -> Reds  (formatTerm t)
                        Let s t -> Let s (formatTerm t)
                        c       -> c
  where
    formatTerm :: Term -> Term
    formatTerm = flip (M.foldlWithKey (\t1 v t2 -> ULC.substitute t1 (ULC.Var v, t2))) env

-- | Function prepends ~> arrows or prints existing term if no reds occur
prependedReductions :: Term -> [String]
prependedReductions t = let xs = reductions t in if null xs
                                                   then ["=   " ++ show t]
                                                   else map (\x -> "~>  " ++ show x) xs

-- | Function prepends reduction ops for one multi-step reduction
prependTerm :: ULC.Term -> ULC.Term -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y

-- | Helper function to load and run a script of interpreter commands
handleLoad :: FilePath -> Interpreter ()
handleLoad fp = do exists <- liftIO (doesFileExist fp)
                   if exists
                     then do e <- parseCommands <$> liftIO (readFile fp)
                             case e of
                               Left  n  -> outputStrLn ("Could not parse line " ++ show n ++ "!")
                               Right cs -> mapM_ handleCommand cs
                     else outputStrLn "File doesn't exist!"
  where
    parseCommands :: String -> Either Int [Command]
    parseCommands = sequence . map (toEither . fmap parseReplCommand) . filter (not . null . snd) . zip [1..] . lines

    toEither :: (a, Maybe b) -> Either a b
    toEither (a, Nothing) = Left  a
    toEither (_, Just  b) = Right b
