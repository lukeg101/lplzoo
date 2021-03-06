module Parser where

import Omega
import Control.Monad       (liftM, ap)
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
item = Parser (\x -> case x of
  "" -> []
  (c:cs) -> [(c,cs)])

-- combines the results of 2 parsers on an input string
-- shortcircuits on the first result returned or fails
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse p cs ++ parse q cs of
  [] -> []
  (x:_) -> [x])

-- failure parser
zerop = Parser (const [])

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

space1 :: Parser String
space1 = many1 (sat isSpace)

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

keywords = ["let", "lett", "=", ".", ":", "::", "Nat", "z", "s"] 


-- 1 or more chars
str :: Parser String
str = do
  s <- many1 $ sat isLower
  if s `elem` keywords then zerop else return s

-- 1 or more chars
strT :: Parser String
strT = do
  s <- many1 $ sat (\x -> isUpper x && isAlpha x)
  if s `elem` keywords then zerop else return s

-- left recursion
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where
    rest a = (do
      f <- op
      b <- p
      rest (f a b)) +++ return a

-- bracket parses away brackets as you'd expect
bracket :: Parser a -> Parser a
bracket p = do
  symb "("
  x <- p
  symb ")"
  return x

-- Proper kinds are "*" packaged up
kindVar = do
  symb "*"
  return KVar

-- arrow kinds are kinds surrounded by arrows
kindArr = do
  x <- spaces kindExpr
  symb "=>"
  y <- spaces kindTerm
  return $ KArr x y

-- top level CFG for kinds
kindTerm = kindArr +++ kindExpr

-- second level CFG for kinds
kindExpr = bracket kindTerm +++ kindVar

-- abstraction allows escaped backslash or lambda
typeLam = do
  spaces $ identifier lambdas
  x <- strT
  spaces (symb "::")
  t <- kindTerm
  spaces (symb ".")
  e <- spaces typeTerm
  return $ TAbs x t e

-- arrow type parser
typeArr = do
  x <- spaces typeApp
  symb "->"
  y <- spaces type2
  return $ TArr x y

-- app has zero or more spaces
typeApp = chainl1 type4 $ do
  space1
  return TApp

-- type vars are Strings
typeVar = do TVar <$> strT

-- type vars are "o" packaged up
typeNat = do
  symb "Nat"
  return TNat

-- top level CFG for arrow types are "(X -> Y)" packaged up
typeTerm = type2 +++ typeLam

type2 = typeArr +++ typeApp

-- bottom level of cfg for types
type4 = bracket typeTerm
  +++ typeNat
  +++ typeVar

-- parser for term variables
termVar = do Var <$> str

zero = do
  char 'z'
  return Zero

succ = do
    char 's'
    return Succ

-- abstraction allows escaped backslash or lambda
lambdas = ['\x03bb','\\', 'λ']
lam = do
  spaces $ identifier lambdas
  x <- str
  spaces (symb ":")
  t <- typeTerm
  spaces (symb ".")
  e <- spaces term
  return $ Abs x t e

-- app has zero or more spaces
app = chainl1 expr $ do
  space1
  return App

-- parser for let expressions
pLet = do
  space
  symb "let"
  space1
  v <- str
  spaces $ symb "="
  t <- term
  return (v, Left t)

-- parser for type let expressions
pTypeLet = do
  space
  symb "lett"
  space1
  v <- strT
  spaces $ symb "="
  t <- typeTerm
  return (v,Right t) --right signify type let

pTerm = do
  t <- spaces term
  return ("", Left t)

-- expression follows CFG form with bracketing convention
expr = bracket term +++ termVar
  +++ zero +++ Parser.succ

-- top level of CFG Grammar
term = lam +++ app

-- identifies key words
identifier :: [Char] -> Parser Char
identifier xs = do
  sat (`elem` xs)
