{-|
Module      : Parser
Description : Monadic Parser Combinators for PCF in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Parser" module provides the monadic parser combinators, grammars, and top-level functions needed to parse a human friendly (read whiteboard) version of PCF.
-}
module Parser where

-- PCF Imports.
import qualified PCF

-- Tool Imports.
import qualified Control.Monad       as M (liftM, ap)
import qualified Data.Char           as C

{-
Implementation based on ideas in Monadic Parser Combinators paper
http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
-}

-- | Parser type takes input string and returns a list of possible parses
newtype Parser a = Parser (String -> [(a, String)])


-- | Necessary AMP additions for Parser instance.
instance Functor Parser where
  fmap = M.liftM


-- | Necessary AMP additions for Parser instance.
instance Applicative Parser where
  pure a = Parser (\cs -> [(a,cs)])
  (<*>)  = M.ap


-- | Monad instance, generators use the first parser then apply f to the result
instance Monad Parser where
  return  = pure
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])


-- | Parser deconstructor.
parse (Parser p) = p


-- | Item takes a string and splits on the first char or fails
item :: Parser Char
item = let split cs = case cs of
                        ""     -> []
                        (c:cs) -> [(c,cs)]
       in Parser split


-- | Combines the results of 2 parsers on an input string
-- shortcircuits on the first result returned or fails
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = let apply cs = case parse p cs ++ parse q cs of
                          []    -> []
                          (x:_) -> [x]
          in Parser apply 


-- | Failure parser.
zerop = Parser (const [])


-- | Parses an element and returns if they satisfy a predicate.
sat :: (Char -> Bool) -> Parser Char
sat p = do 
  c <- item
  if p c 
    then return c 
    else zerop


-- | Parses chars only.
char :: Char -> Parser Char
char c = sat (c ==)

-- | Parses a string of chars.
string :: String -> Parser String
string = mapM char


-- | Parses 0 or more elements.
many :: Parser a -> Parser [a]
many p = many1 p +++ return []


-- | Parses 1 or more elements.
many1 :: Parser a -> Parser [a]
many1 p = do
  a  <- p
  as <- many p
  return (a:as)


-- | Parses 0 or more whitespace.
space :: Parser String
space = many (sat C.isSpace)

-- | Parsers 1 or more whitespace.
space1 :: Parser String
space1 = many1 (sat C.isSpace)


-- | Trims whitespace between an expression.
spaces :: Parser a -> Parser a 
spaces p = do
  space
  x <- p
  space
  return x


-- | Parses a single string.
symb :: String -> Parser String
symb = string


-- | Apply a parser to a string.
apply :: Parser a -> String -> [(a,String)]
apply = parse


-- | set of reserved words for PCF
keywords :: [String]
keywords = ["let", "=", ":", "Nat", "z", "s", "p", "if", "Y"] 


-- | 1 or more chars
str :: Parser String
str = do 
  s <- many1 $ sat C.isLower
  if s `elem` keywords 
     then zerop 
     else return s


-- | Left recursion. 
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = let rest a = (do f <- op
                                  b <- p
                                  rest (f a b)) +++ return a
                 in do a <- p
                       rest a 


-- | Parses away brackets as you'd expect.
bracket :: Parser a -> Parser a
bracket p = do
  symb "("
  x <- p
  symb ")"
  return x

--Parser very similar to System T


-- | Top level CFG for arrow types are "(X -> Y)" packaged up
typTerm :: Parser PCF.T
typTerm = (do
  x <- typExpr
  spaces (symb "->")
  PCF.TArr x <$> typTerm) +++ typExpr


-- | type vars are "Nat" packaged up 
typVar :: Parser PCF.T
typVar = do
  symb "Nat"
  return PCF.TNat


-- | Second level of CFG for types
typExpr :: Parser PCF.T
typExpr = bracket typTerm +++ typVar


-- | Parser for term variables
termVar :: Parser PCF.PCFTerm
termVar = PCF.Var <$> str


-- | Abstraction allows escaped backslash or lambda
lambdas :: String
lambdas = ['\x03bb','\\']


-- | Lam parser parses abstractions
lam :: Parser PCF.PCFTerm
lam = do 
  spaces $ identifier lambdas
  x <- str
  spaces (symb ":")
  t <- typTerm
  spaces (symb ".")
  e <- spaces term
  return $ PCF.Abs x t e


-- | App parses application terms, with one or more spaces in between terms.
app :: Parser PCF.PCFTerm
app = chainl1 expr $ do
  space1
  return PCF.App


-- | Parser for let expressions
pLet :: Parser (String, PCF.PCFTerm)
pLet = do
  space
  symb "let"
  space1
  v <- str
  spaces $ symb "="
  t <- term 
  return (v,t)


-- | Parser for regular terms.
pTerm :: Parser (String, PCF.PCFTerm)
pTerm = do
  t <- term 
  return ("", t)


-- | Parser for the zero term
zero :: Parser PCF.PCFTerm
zero = do 
  char 'z'
  return PCF.Zero


-- | Parser for the recursor term
rec :: Parser PCF.PCFTerm
rec = do
  symb "Y"
  return PCF.Y


-- | Parser for the succ term
succ :: Parser PCF.PCFTerm
succ = do
  char 's'
  return  PCF.Succ


-- | Parser for the pred term
pred :: Parser PCF.PCFTerm
pred = do
  char 'p'
  return PCF.Pred


-- | Parser for the if term
if_ :: Parser PCF.PCFTerm
if_ = do
  symb "if"
  return PCF.If

-- | Expression follows CFG form with bracketing convention.
expr :: Parser PCF.PCFTerm
expr = bracket term +++ termVar +++ zero +++ rec
  +++ Parser.pred +++ Parser.succ +++ if_


-- | Top level of CFG Grammar
term :: Parser PCF.PCFTerm
term = lam +++ app


-- | Identifies key words.
identifier :: String -> Parser Char 
identifier xs = sat (`elem` xs)
