module Parser where

import STLC
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap, guard)
import Data.Char

{-
Implementation based on ideas in Monadic Parser Combinators paper
http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
-}

-- Parser type takes input string and returns a list of possible parses
newtype Parser a = Parser (String -> [(a, String)])

-- Necessary AMP additions for Parser instance
instance Functor Parser where
  fmap = liftM
instance Applicative Parser where
  pure a = Parser (\cs -> [(a,cs)])
  (<*>) = ap

-- Monad instance, generators use the first parser then apply f to the result
instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

-- parser deconstructor
parse (Parser p) = p

-- takes a string and splits on the first char or fails
item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])

-- combines the results of 2 parsers on an input string
-- shortcircuits on the first result returned or fails
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse p cs ++ parse q cs of
  [] -> []
  (x:xs) -> [x])

-- failure parser
zerop = Parser (\cs -> [])

-- parses an element and returns if they satisfy a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zerop}

-- parses chars only
char :: Char -> Parser Char
char c = sat (c ==)

-- parses a string of chars
string :: String -> Parser String
string = mapM char

-- parses 0 or more elements
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

-- parses 1 or more elements
many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a:as)

-- parses 0 or more whitespace
space :: Parser String
space = many (sat isSpace)

-- trims whitespace between an expression
spaces :: Parser a -> Parser a 
spaces p = do
  space
  x <- p
  space
  return x

-- parses a single string
symb :: String -> Parser String
symb = string

-- apply a parser to a string
apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

-- 1 or more digits
nat :: Parser Int
nat = fmap read (many1 (sat isDigit))

-- bracket parses away brackets as you'd expect
bracket :: Parser a -> Parser a
bracket p = do
  symb "("
  x <- p
  symb ")"
  return x

termVar = do
  x <- spaces nat
  return $ Var x

-- type vars are "o" packaged up
typeVar = do
  spaces (symb "O")
  return TVar

-- arrow types are "(X -> Y)" packaged up
typeArr = do
  x <- parseType
  spaces (symb "->")
  y <- parseType
  return $ TArr x y

parseType = typeVar +++ bracket typeArr

-- abstraction allows escaped backslash or lambda
lambdas = ['\x03bb','\\']
lam = do 
  spaces $ identifier lambdas
  x <- nat
  spaces (symb ":")
  t <- typeVar
  spaces (symb ".")
  e <- spaces expr
  return $ Abs x t e

-- app has zero or more spaces
app = do
  e1 <- spaces expr
  e2 <- spaces expr
  return $ App e1 e2

-- expression follows strict BNF form, no bracketing convention
expr = (bracket lam) +++ termVar +++ (bracket  app) 

-- identifies key words
identifier :: [Char] -> Parser Char 
identifier xs = do
  x <- sat (\x -> elem x xs)
  return x
