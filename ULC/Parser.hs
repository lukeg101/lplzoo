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
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

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
space = many (sat (' ' == ))

token :: Parser a -> Parser a
token p = do 
  a <- p
  return a

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

nat :: Parser Int
nat = do 
  xs <- many1 $ sat (\x -> '0' <= x && x <= '9')
  return $ foldl1 (\m n -> 10 * m + n) [ord x - ord '0' | x <- xs] 

bracket :: Parser a -> Parser a
bracket p = do
  symb "("
  x <- p
  symb ")"
  return x

var = do
  x <- token nat
  return (Var x)

lambdas = ['\x03bb','\\']
lam = do 
  identifier lambdas
  x <- token nat
  symb "."
  e <- atom --should be expr
  return $ Abs x e

app = do
  l1 <- atom
  l2 <- atom 
  return $ App l1 l2

--TODO fix
--expr = atom `chainl1` (return App)

atom = lam +++ var +++ (bracket atom) --nested atom should be expr - fix

identifier :: [Char] -> Parser Char 
identifier xs = token $ do
  x <- sat (\x -> elem x xs)
  return x
