{-|
Module      : Parser
Description : Monadic Parser Combinators for C in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Parser" module provides the monadic parser combinators, grammars, and top-level functions needed to parse a human friendly (read whiteboard) version of C.
-}
module Parser where


-- Sol Imports.
import qualified C


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


-- | set of reserved words for C
keywords :: [String]
keywords = ["let", "=", ".", ":", "Pi",
  "(", ")", "\x3a0", "\x03bb", "\x25A1", "*"
  , "assume", "_"]


-- | 1 or more chars
str :: Parser String
str = do
  s <- many1 $ sat C.isAlphaNum
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


-- | Parser for term variables
termVar :: Parser C.CTerm
termVar = C.Var <$> str


-- | Parser for star
star :: Parser C.CTerm
star = do symb "*"
          return $ C.Sort C.SStar


-- | Abstraction allows escaped backslash or lambda
lambdas :: String
lambdas = ['\x03bb','\\']


-- | Lam parser parses abstractions
lam :: Parser C.CTerm
lam = do
  spaces $ identifier lambdas
  x <- str
  spaces (symb ":")
  t <- term
  spaces (symb ".")
  e <- spaces term
  return $ C.Abs x t e


-- | Type-level abstractions
pi :: Parser C.CTerm
pi = do
  spaces (symb "Pi" +++ symb "\x3a0")
  x <- str
  spaces (symb ":")
  ty1 <- term
  spaces (symb ".")
  C.Pi x ty1 <$> term


-- | arrow types are non-dependent pi types
termArr :: Parser C.CTerm
termArr = do
  x <- expr
  spaces (symb "->")
  C.Pi "_" x <$> term -- show inst. prints ->


-- | App parses application terms, with one or more spaces in between terms.
app :: Parser C.CTerm
app = chainl1 expr $ do
  space1
  return C.App


-- | Parsed expressions are either terms or terms in lets
data PExpr
  = PTerm C.CTerm
  | PAssume C.CTerm


-- | Parser for let expressions
pLet :: Parser (String, PExpr)
pLet = do
  space
  symb "let"
  space1
  v <- str
  spaces $ symb "="
  t <- term
  return (v, PTerm t)


-- | Parser for adding things into the context
pAssume :: Parser (String, PExpr)
pAssume = do
  space
  symb "assume"
  space1
  v <- str
  spaces $ symb ":"
  t <- term
  return (v, PAssume t)


-- | Parser for regular terms.
pTerm :: Parser (String, PExpr)
pTerm = do
  t <- spaces term
  return ("", PTerm t)


-- | Expression follows CFG form with bracketing convention.
expr :: Parser C.CTerm
expr = termVar +++ star
  +++ bracket term


-- | Top level of CFG Grammar
term :: Parser C.CTerm
term = lam +++ Parser.pi
  +++ termArr +++ app


-- | Identifies key words.
identifier :: String -> Parser Char
identifier xs = sat (`elem` xs)

