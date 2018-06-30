module Parser where

import Sub
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap, guard)
import Data.Char
import Data.List           (nub)

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
  x <- sat (\x -> isUpper x && isAlpha x && (not $ elem x ['1','=']))
  return $ TVar x

typeArr = (do
  x <- typExpr
  spaces (symb "->")
  y <- typTerm
  return $ TArr x y) +++ typExpr

-- units are simply 1
typUnit = do
  spaces $ identifier ['1','⊤']
  return $ TUnit

-- record types are simply tuples of types with  ":"
typRec = do
  symb "{"
  t  <- many1 typRecField
  ts <- many (do {spaces $ symb ","; (x,t) <- typRecField; return (x,t)})
  if ((nub . fst $ unzip (t++ts)) == (fst $ unzip (t++ts)))
  then do -- checks if each record is unique
    symb "}"
    return $ TRec $ t ++ ts
  else zerop

-- parser for the fields in each type record
typRecField = do
  x <- spaces $ nat
  symb ":"
  t <- spaces typTerm
  return (x, t)

-- top level CFG for arrow types are "(X -> Y)" packaged up
typTerm = typeArr

-- second level of CFG for types
typExpr = (bracket typTerm) +++ typVar +++ typUnit +++ typRec

-- parser for term variables
termVar = do
  x <- nat
  return $ Var x

-- unit terms are simply ()
termUnit = do
  symb "()"
  return Unit

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

-- records
termRec = do
  symb "{"
  x  <- many1 termRecField
  xs <- many (do {symb ","; (x,t) <- termRecField; return (x,t)})
  if ((nub . fst $ unzip (x++xs)) == (fst $ unzip (x++xs)))
  then do -- checks if each record is unique
    symb "}"
    return $ Rec $ x ++ xs
  else zerop

--individual record fields
termRecField = do
  x <- spaces nat
  symb "="
  t <- spaces term
  return (x, t)

--projection
termProj = do
  r <- termRec +++ termVar 
  symb "."
  x <- nat
  return $ App r (Proj x)

-- expression follows CFG form with bracketing convention
expr = (bracket term) +++ termProj +++ termRec 
  +++ termVar +++ termUnit

-- top level of CFG Grammar
term = app +++ lam +++ termRec +++ termProj

-- identifies key words
identifier :: [Char] -> Parser Char 
identifier xs = do
  x <- sat (\x -> elem x xs)
  return x
