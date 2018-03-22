module Parser where

import ULC
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap, guard)
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser (\cs -> [(a,cs)])
  (<*>) = ap

instance Monad Parser where
  return = pure
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])


parse (Parser p) = p

item :: Parser Char
item = Parser (\cs -> case cs of
  "" -> []
  (c:cs) -> [(c,cs)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse p cs ++ parse q cs of
  [] -> []
  (x:xs) -> [x])

zerop = Parser (\cs -> [])

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zerop}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string = mapM char

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where
    rest a = do 
      f <- op
      b <- p
      rest (f a b) +++ return a

space :: Parser String
space = many (sat isSpace)

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

-- vars are nats packaged up
var = do
  x <- nat
  return (Var x)

-- abstraction allows escaped backslash or lambda
lambdas = ['\x03bb','\\']
lam = do 
  identifier lambdas
  x <- nat
  symb "."
  e <- expr
  return $ Abs x e

-- app has zero or more spaces
app = do
  e1 <- expr
  space
  e2 <- expr
  return $ App e1 e2

-- expression follows strict BNF form, no bracketing convention
expr = (bracket lam) +++ var +++ (bracket  app) 

-- identifies key words
identifier :: [Char] -> Parser Char 
identifier xs = do
  x <- sat (\x -> elem x xs)
  return x
