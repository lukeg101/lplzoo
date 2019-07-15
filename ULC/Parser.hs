{-|
Module      : Parser
Description : Monadic Parser Combinators for the untyped lambda calculus in Haskell.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com
Stability   : stable
Portability : POSIX

The "Parser" module provides the monadic parser combinators, grammars, and top-level functions needed to parse a human friendly (read whiteboard) version of the untyped lambda calculus.
-}
module Parser where

-- ULC Imports.
import qualified ULC

-- Tool Imports.
import Data.Char
import Control.Monad

{-
Implementation based on ideas in Monadic Parser Combinators paper
http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf
-}

-- | Parser type takes input string and returns a list of possible parses.
newtype Parser a = Parser (String -> [(a, String)])


-- | Necessary AMP additions for Parser instance.
instance Functor Parser where
  fmap   = liftM


-- | Necessary AMP additions for Parser instance.
instance Applicative Parser where
  pure a = Parser (\cs -> [(a,cs)])
  (<*>)  = ap


-- | Monad instance, generators use the first parser then apply f to the result
instance Monad Parser where
  return  = pure
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])


-- | Parser deconstructor.
parse :: Parser a -> String -> [(a, String)]
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
zerop :: Parser a
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
space = many (sat isSpace)

-- | Parsers 1 or more whitespace.
space1 :: Parser String
space1 = many1 (sat isSpace)


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


-- | Left recursion.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = let rest a = (do f <- op
                                  b <- p
                                  rest (f a b)) +++ return a

                 in do a <- p
                       rest a


-- | Parser 1 or more chars (a string).
str :: Parser String
str = many1 (sat isLower)


-- | Parses away brackets as you'd expect.
bracket :: Parser a -> Parser a
bracket p = do
  symb "("
  x <- p
  symb ")"
  return x


-- | Parser for comments
comment :: Parser ()
comment =  do
  symb ";"
  many (sat isPrint)
  return ()


-- | Type of possible inputs to the REPL
data Command = T    ULC.Term
             | Reds ULC.Term
             | Let  String ULC.Term
             | Load FilePath
             | None  -- needed for comments


-- | Top-level function for parsing a REPL command, failing if the parse is ambiguous or doesn't consume the entire input.
parseReplCommand :: String -> Maybe Command
parseReplCommand s = case parse pCommand s of
                       [(a,"")] -> Just a
                       _        -> Nothing


-- | Parse a command prefixed by a colon, or parse a raw term.
pCommand :: Parser Command
pCommand = spaces (pReds +++ pLet +++ pLoad +++ (T <$> pTerm) +++ pComment)
  where
    pReds :: Parser Command
    pReds = do symb ":reductions"
               space1
               Reds <$> pTerm

    pLet :: Parser Command
    pLet = do symb ":let"
              space1
              v <- str
              spaces $ symb "="
              Let v <$> pTerm

    pLoad :: Parser Command
    pLoad = do symb ":load"
               space1
               fp <- many1 (sat (not . isSpace))
               space
               pure (Load fp)
    pComment :: Parser Command
    pComment = do () <- comment
                  return None



-- | Top-level of CFG Grammar.
pTerm :: Parser ULC.Term
pTerm = lam +++ app


-- | Lam parser parses abstractions
lam :: Parser ULC.Term
lam = do
  spaces $ sat (`elem` "λ\\")
  x <- spaces str
  symb "."
  e <- spaces pTerm
  return $ ULC.Abs x e


-- | App parses application terms, with one or more spaces in between terms.
app :: Parser ULC.Term
app = chainl1 expr $ do
  space1
  return ULC.App


-- | Expression follows CFG form with bracketing convention.
expr :: Parser ULC.Term
expr = bracket pTerm +++ var


-- | Vars are strings packaged up.
var :: Parser ULC.Term
var = ULC.Var <$> str
