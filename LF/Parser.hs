{-|
Module      : Parser
Description : Monadic Parser Combinators for LF in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Parser" module provides the monadic parser combinators, grammars, and top-level functions needed to parse a human friendly (read whiteboard) version of LF.
-}
module Parser where


-- Sol Imports.
import qualified LF


-- Tool Imports.
import qualified Control.Applicative as A (Applicative(..))
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


-- | Parses single digits
digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')


-- | Parsers a natural number
nat :: Parser Int 
nat = do n <- many1 digit
         let maybeInt = read n :: Int
         return maybeInt


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


-- | set of reserved words for LF
keywords :: [String]
keywords = ["let", "lett", "=", ".", ":", "Pi",
  "Nat", "Vec", "(", ")", "\x3a0", "cons", "nil", "succ"] 


-- | 1 or more chars
str :: Parser String
str = do 
  s <- many1 $ sat C.isLower
  if s `elem` keywords 
     then zerop 
     else return s


-- | 1 or more chars
strT :: Parser String
strT = do 
  s1  <- many1 $ sat C.isUpper
  s2 <- many  $ sat C.isAlpha
  let ss = s1 ++ s2
  if ss `elem` keywords 
     then zerop 
     else return ss


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


-- | Type vars are Strings packaged up 
typVar :: Parser LF.T
typVar = LF.TVar <$> strT


-- | Nat type is just Nat
typNat :: Parser LF.T
typNat = symb "Nat" >> return LF.TNat


-- | Parser for type-level terms
typLevelTerm :: Parser LF.T
typLevelTerm = LF.TTerm <$> expr
                  

-- | Type-level abstractions
typPi :: Parser LF.T
typPi = do
  spaces (symb "Pi" +++ symb "\x3a0")
  x <- str
  spaces (symb ":")
  ty1 <- typExpr
  spaces (symb ".")
  ty2 <- typTerm
  return $ LF.TPi x ty1 ty2


-- | Lam parser parses type abstractions
typLam :: Parser LF.T
typLam = do
  spaces $ identifier lambdas
  x <- str
  spaces (symb ":")
  t1 <- typTerm
  spaces (symb ".")
  t2 <- spaces typTerm
  return $ LF.TAbs x t1 t2


-- | arrow types are non-dependent pi types
typArr :: Parser LF.T
typArr = do
  x <- typExpr
  spaces (symb "->")
  y <- typTerm
  return $ LF.TPi "_" x y -- show inst. prints ->


-- | Parser for Vector types
typVec :: Parser LF.T
typVec = symb "Vec" >> return LF.TVec

-- | App has zero or more spaces
typApp :: Parser LF.T
typApp = chainl1 typExpr $ do
  space1
  return LF.TApp


-- | Top level CFG for types
typTerm :: Parser LF.T
typTerm = typPi +++ typLam 
  +++ typArr +++ typApp


-- | Final level of CFG for types
typExpr :: Parser LF.T
typExpr = typVar +++ typNat +++ typVec
  +++ typLevelTerm +++ bracket typTerm 

-- | Parser for term variables
termVar :: Parser LF.LFTerm
termVar = LF.Var <$> str


-- | Parser for term nats
termNat :: Parser LF.LFTerm
termNat = LF.Nat <$> nat


-- | Parser for empty lists
termNil :: Parser LF.LFTerm
termNil = symb "nil" >> return LF.Nil


-- | Parser for list cons
termCons :: Parser LF.LFTerm
termCons = symb "cons" >> return LF.Cons


-- | Parser for the successor term
termSucc :: Parser LF.LFTerm
termSucc = symb "succ" >> return LF.Succ


-- | Abstraction allows escaped backslash or lambda
lambdas :: String
lambdas = ['\x03bb','\\']


-- | Lam parser parses abstractions
lam :: Parser LF.LFTerm
lam = do
  spaces $ identifier lambdas
  x <- str
  spaces (symb ":")
  t <- typTerm
  spaces (symb ".")
  e <- spaces term
  return $ LF.Abs x t e


-- | App parses application terms, with one or more spaces in between terms.
app :: Parser LF.LFTerm
app = chainl1 expr $ do
  space1
  return LF.App 


-- | Parsed expressions are either terms or terms in lets
data PExpr
  = PTerm LF.LFTerm
  | PType LF.T


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


-- | Parser for type let expressions
pTypeLet :: Parser (String, PExpr)
pTypeLet = do
  space
  symb "lett"
  space1
  v <- strT
  spaces $ symb "="
  t <- typTerm 
  return (v, PType t)


-- | Parser for regular terms.
pTerm :: Parser (String, PExpr)
pTerm = do
  t <- term 
  return ("", PTerm t)


-- | Expression follows CFG form with bracketing convention.
expr :: Parser LF.LFTerm
expr = termNat +++ termNil 
  +++ termCons +++ termVar
  +++ termSucc +++ bracket term


-- | Top level of CFG Grammar
term :: Parser LF.LFTerm
term = lam +++ app 


-- | Identifies key words.
identifier :: String -> Parser Char 
identifier xs = sat (`elem` xs)

