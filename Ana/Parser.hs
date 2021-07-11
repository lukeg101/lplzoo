module Parser where

import Ana
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
item = Parser (\cs -> case cs of
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

keywords = ["X", "\x03C0"++"1", "\x03C0"++"2","\x03bc","\x22A4","let", "lett", "=", ".", ":", "out", "ana", "inl", "inr", "case", "()", "fst", "snd", "N"]

-- 1 or more chars
str :: Parser String
str = do
  s <- many1 $ sat isLower
  if s `elem` keywords then zerop else return s

-- 1 or more chars
strT :: Parser String
strT = do
  s <- many1 $ sat (\x -> isUpper x && isAlpha x && notElem x ['X','M'])
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

-- type vars are uppercase alphabetical terms packaged up 
typVar = do
  TVar <$> strT

-- units are simply 1
typUnit = do
  spaces $ identifier ['1','⊤']
  return TUnit

-- mu variables X, TODO Generalise impl to mu variables
typX = do
  spaces (symb "X")
  return X

--arrow type, lowest op precedence
typArr = do
  x <- typExpr
  spaces (symb "->")
  TArr x <$> typTerm

--sum type second lowest
typSum = do
  t1 <- spaces typExpr2
  symb "+"
  t2 <- spaces typExpr2
  return $ TSum t1 t2

--product type highest op precedence
typProd = do
  t1 <- spaces typExpr3
  identifier ['\x00D7', '*']
  t2 <- spaces typExpr3
  return $ TProd t1 t2

-- mu type
typNu = do
  identifier ['\x03BD', 'N']
  t1 <- spaces $ bracket typTerm
  return $ TNu t1

-- top level CFG for arrow types are "(X -> Y)" packaged up
typTerm  = typArr +++ typExpr
typExpr  = typSum +++ typExpr2
typExpr2 = typProd +++ typExpr3
typExpr3 = bracket typTerm +++ typVar +++ typX +++ typUnit +++ typNu

-- parser for term variables
termVar = do
  Var <$> str

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
  return $ App (Inr t1) l1

termCase = symb "case" >> return Case

termProd = do
  spaces $ symb "("
  t1 <- term
  spaces $ symb ","
  t2 <- term
  spaces $ symb ")"
  return $ App (App Prod t1) t2

termPrj1 = string "fst" +++ string "π1" >> return Prj1

termPrj2 = string "snd" +++ string "π2" >> return Prj2

termAna = do
  symb "ana"
  space1
  l1 <- term
  spaces $ symb ":"
  t1 <- typTerm
  return $ App (Ana t1) l1

termOut = symb "out" >> return Out

-- abstraction allows escaped backslash or lambda
lambdas = ['\x03bb','\\']
lam = do
  spaces $ identifier lambdas
  x <- str
  spaces (symb ":")
  t <- typTerm
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
  return (v,Left t) --left signifies terms

-- parser for type let expressions
pTypeLet = do
  space
  symb "lett"
  space1
  v <- strT
  spaces $ symb "="
  t <- typTerm
  return (v,Right t) --right signify type let

pTerm = do
  t <- term
  return ("", Left t)

-- expression follows CFG form with bracketing convention
expr = bracket term +++ termVar
  +++ termInl +++ termInr +++ termCase
  +++ termProd +++ termPrj1 +++ termPrj2
  +++ termAna +++ termOut +++ termUnit

-- top level of CFG Gramma
term = lam +++ app

-- identifies key words
identifier :: [Char] -> Parser Char
identifier xs = do
  sat (`elem` xs)
