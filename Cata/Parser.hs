module Parser where

import Cata
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

-- 1 or more digits
nat :: Parser Int
nat = fmap read (many1 (sat isDigit))

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

-- type vars are uppercase alphabetical terms packaged up 
typVar = do
  x <- sat (\x -> isUpper x && isAlpha x && not (elem x ['X','M']))
  return $ TVar x

-- units are simply 1
typUnit = do
  spaces $ identifier ['1']
  return $ TUnit

-- mu variables X, TODO Generalise impl to mu variables
typX = do
  spaces (symb "X")
  return X

--arrow type, lowest op precedence
typArr = (do
  x <- typExpr
  spaces (symb "->")
  y <- typTerm
  return $ TArr x y) 

--sum type second lowest
typSum = do
  t1 <- spaces $ typExpr2
  symb "+"
  t2 <- spaces $ typExpr2
  return $ TSum t1 t2

--product type highest op precedence
typProd = do
  t1 <- spaces $ typExpr3
  identifier ['\x00D7', '*']
  t2 <- spaces $ typExpr3
  return $ TProd t1 t2

-- mu type
typMu = do
  identifier ['\x03bc', 'M']
  t1 <- spaces $ bracket typTerm
  return $ TMu t1

-- top level CFG for arrow types are "(X -> Y)" packaged up
typTerm  = typArr +++ typExpr
typExpr  = typSum +++ typExpr2
typExpr2 = typProd +++ typExpr3
typExpr3 = (bracket typTerm) +++ typVar +++ typX +++ typUnit +++ typMu

-- parser for term variables
termVar = do
  x <- nat
  return $ Var x

termUnit = do
  spaces $ symb "()"
  return Unit

termInl = do
  spaces $ symb "inl"
  l1 <- term
  spaces $ symb ":"
  t1 <- typTerm
  return $ App (Inl t1) l1

termInr = do
  spaces $ symb "inr"
  l1 <- term
  spaces $ symb ":"
  t1 <- typTerm
  return $ App (Inl t1) l1

termCase = symb "case" >> return Case

termProd = do
  spaces $ symb "("
  t1 <- term
  spaces $ symb ","
  t2 <- term
  spaces $ symb ")"
  return $ App (App Prod t1) t2

termPrj1 = (string "fst") +++ (string "π1") >> return Prj1  

termPrj2 = (string "snd") +++ (string "π2") >> return Prj2  

termCata = symb "cata" >> return Cata

termIn = do
  spaces $ symb "in"
  l1 <- term
  spaces $ symb ":"
  t1 <- typTerm
  return $ App (In t1) l1


-- abstraction allows escaped backslash or lambda
lambdas = ['\x03bb','\\']
lam = do 
  spaces $ identifier lambdas
  x <- nat
  spaces (symb ":")
  t <- typTerm
  spaces (symb ".")
  e <- spaces term
  return $ Abs x t e

-- app has zero or more spaces
app = chainl1 expr $ do
  space1
  return $ App 

-- expression follows CFG form with bracketing convention
expr = (bracket term) +++ termVar +++ termUnit 
  +++ termInl +++ termInr +++ termCase
  +++ termProd +++ termPrj1 +++ termPrj2
  +++ termCata +++ termIn

-- top level of CFG Gramma
term = app +++ lam

-- identifies key words
identifier :: [Char] -> Parser Char 
identifier xs = do
  x <- sat (\x -> elem x xs)
  return x