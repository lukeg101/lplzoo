{-|
Module      : Repl
Description : The Read-Eval-Print-Loop for SystemF.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Repl" module provides the Read-eval-print-loop for SystemF.
This is the top level interface between the user and the interpreter.
-}
module Repl where


-- System F Imports.
import SystemF
import Parser


-- Tool Imports.
import qualified System.IO     as IO (hFlush, stdout)
import qualified Data.Map.Lazy as M


-- | Top level repl functionreplMain :: IO ()
replMain = do
  putStrLn "Welcome to the System F REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl M.empty

-- | Stores variables from let expressions at runtime
-- terms are stored in left 
-- types are stored in right
type Environment = M.Map String (Either SFTerm T)


-- | REPL loop, takes input reduces and prints result, or exits out
repl :: Environment -> IO ()
repl env = do putStr ">   "
              IO.hFlush IO.stdout
              s <- getLine
              if null s 
                then putStrLn "Goodbye."
                else do {parseTerm s env; repl env}


-- | Helper function parses the term as either a Let expression or a raw term.
-- Let expressions update the environment with new variables, using any existing
-- variables in scope. For a raw term, evaluation is attempted after substituting
-- in bound terms from the environment. For STLC termination is guaranteed.
parseTerm :: String -> Environment -> IO ()
parseTerm s env
  | head s == '\'' = printReductions s env
  | head s == 't'  = printType s
  | otherwise = case apply (pLet +++ pTypeLet +++ pTerm) s of
                  [(("", Left t),"")] ->   -- reducing a term
                    case typeof' t' of
                      Just _ -> putStrLn . prependTerm t' $ reduce t'
                      _ -> cannotType $ show t'
                      where t' = formatTerm t env 
                  [((v,Left t),"")] ->     -- let expression
                    case typeof' t' of
                      Just _ -> do 
                        putStrLn $ "Saved term: " ++ show t'
                        repl $ M.insert v (Left t') env
                      _ -> cannotType s
                      where t' = formatTerm t env
                  [((v,Right t),"")] -> do       -- let expression
                    putStrLn $ "Saved type: " ++ show t'
                    repl $ M.insert v (Right t') env
                    where t' = formatType t env
                  _ -> cannotParse s 


-- | Helper function to print the reduction steps of a term
printReductions :: String -> Environment -> IO ()
printReductions s env 
  = let prepend x = prependReductions x $ reductions x
    in case apply term (tail s) of
         (x:_) -> if null $ snd x
                    then case typeof' x' of
                      Just _ -> mapM_ putStrLn $ prepend x'
                      _      -> cannotType $ tail s
                    else cannotParse s
          where x' = formatTerm (fst x) env
         _     -> cannotParse s


-- | Helper function to print the type of a term
printType :: String -> IO ()
printType s = case apply term $ tail s of
  (x:_) -> if null $ snd x
             then case typeof' $ fst x of
                    Just y -> print y
                    _      -> cannotType $ tail s
             else cannotParse s
  _     -> cannotParse s


-- | Takes a term and context and substitutes env terms
-- into all free occurrences in the term
formatTerm :: SFTerm -> Environment -> SFTerm
formatTerm t1 env = foldl 
  (\t (v,t2) -> case t2 of 
    Left t2 ->  
      if v `elem` vars t 
      then substitute t (Var v, t2) 
      else t
    Right t2 -> 
      if v `elem` typeVarsInTerm t
      then tSubUnder t (TVar v, t2)
      else t) t1 $ M.assocs env


-- | Takes a type and context and substitutes env terms
-- all free occurrences in the term
formatType :: T -> Environment -> T
formatType t1 env = foldl 
  (\t (v,t2) -> case t2 of 
    Right t2 -> if v `elem` typeVars t1
      then typeSub t (TVar v, t2) 
      else t
    _ -> t) t1 $ M.assocs env


-- | Function prepends ~> arrows or prints existing term if no reds occur
prependReductions :: SFTerm -> [SFTerm] -> [String]
prependReductions x xs = if null xs then ["=   "++show x] else 
  map (\x -> "~>  " ++ show x) xs


-- | Function prepends reduction ops for one multi-step reduction
prependTerm :: SFTerm -> SFTerm -> String
prependTerm x y = if x == y then "=   " ++ show x else "~>* " ++ show y


-- | Error message for parse failure
cannotParse :: String -> IO ()
cannotParse s = putStrLn $ (++) "Cannot Parse Term: " s


-- | Error message for typing failure
cannotType :: String -> IO ()
cannotType s = putStrLn $ (++) "Cannot Type Term: " s