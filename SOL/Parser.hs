{-|
Module      : Parser
Description : Monadic Parser Combinators for SOL in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Parser" module provides the monadic parser combinators, grammars, and top-level functions needed to parse a human friendly (read whiteboard) version of SOL.
-}
module Parser where


-- Sol Imports.
import qualified SOL as S


-- Tool Imports.
import qualified Control.Applicative as A (Applicative(..))
import qualified Control.Monad       as M (liftM, ap)
import qualified Data.Char           as C
import qualified Data.List           as L

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


-- | set of reserved words for SOL
keywords :: [String]
keywords = ["let", "lett", "=", ".", ":","L", "[", "]", "P",
  "true", "false", "fst", "snd", "inl", "inr", "case", 
  "\x03C0"++"1", "\x03C0"++"2", "\x00D7", "*", "+",
  "Nat", "Bool", "{", "}", "pack", "as", "E", "(", ")",
  "unpack", "in", "if", "then", "else"] 


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


-- | bracket parses away brackets as you'd expect
sqbracket :: Parser a -> Parser a
sqbracket p = do
  symb "["
  x <- p
  symb "]"
  return x


-- | bracket parses away angle brackets as you'd expect
angbracket :: Parser a -> Parser a
angbracket p = do
  symb "{"
  x <- p
  symb "}"
  return x


-- | Type vars are Strings packaged up 
typVar :: Parser S.T
typVar = S.TVar <$> strT


-- | Nat type is just Nat
typNat :: Parser S.T
typNat = do spaces (symb "Nat")
            return S.TNat


-- | Bool type is just Nat
typBool :: Parser S.T
typBool = do spaces (symb "Bool")
             return S.TBool


-- | Parser Sum types
typSum :: Parser S.T
typSum = do
  t1 <- spaces typExpr2
  symb "+"
  t2 <- spaces typExpr2
  return $ S.TSum t1 t2


-- | Parser for Product type 
typProd :: Parser S.T
typProd = do
  t1 <- spaces typExpr3
  identifier ['\x00D7', '*']
  t2 <- spaces typExpr3
  return $ S.TProd t1 t2


-- | Abstraction allows escaped backslash or lambda types
typeLambdas :: String
typeLambdas = ['\x2200', 'P']


-- | Type-level abstractions
typAbs :: Parser S.T
typAbs = do
  spaces $ identifier typeLambdas
  x <- strT
  spaces (symb ".")
  t <- typTerm
  return $ S.TForall x t


-- | Parser for arrow types are "(X -> Y)" packaged up
typeArr :: Parser S.T
typeArr = do
  x <- typExpr
  spaces (symb "->")
  y <- typTerm
  return $ S.TArr x y


-- | Record types are simply tuples of types with  ":"
typRec :: Parser S.T
typRec = do
  symb "{"
  t  <- typRecField
  ts <- many (do {spaces $ symb ","; (x,t) <- typRecField; return (x,t)})
  if uniqs (t:ts)
    then do symb "}"
            return $ S.TRec $ t:ts
    else zerop
  where uniqs = all (null . tail) . L.group . L.sort . map fst


-- | Parser for the fields in each type record
typRecField :: Parser (String, S.T)
typRecField = do
  x <- spaces str
  symb ":"
  t <- spaces typTerm
  return (x, t)


-- | exists keywords 
exists :: String
exists = ['\x2203', 'E']


-- | Parser for existential types
typExists :: Parser S.T
typExists = do
  identifier exists
  v <- strT
  spaces $ symb "."
  ty <- typTerm
  return $ S.TExists v ty
  

-- | Top level CFG for types are "(X -> Y)" packaged up
typTerm :: Parser S.T
typTerm = typAbs +++ typExists 
    +++ typeArr +++ typExpr


-- | Second level CFG for sums
typExpr :: Parser S.T
typExpr = typSum +++ typExpr2


-- | Third level CFG for products
typExpr2 :: Parser S.T
typExpr2 = typProd +++ typExpr3


-- | Final level of CFG for types
typExpr3 :: Parser S.T
typExpr3 = bracket typTerm +++ typVar
  +++ typNat +++ typBool +++ typRec


-- | Parser for term variables
termVar :: Parser S.SOLTerm
termVar = S.Var <$> str


-- | Parser for term nats
termNat :: Parser S.SOLTerm
termNat = S.Nat <$> nat


-- | Parser for true
termTrue :: Parser S.SOLTerm
termTrue = do spaces $ symb "true"
              return S.BTrue


-- | Parser for true
termFalse :: Parser S.SOLTerm
termFalse = do spaces $ symb "false"
               return S.BFalse


-- | Parser for if expressions
termIf :: Parser S.SOLTerm
termIf = do spaces $ symb "if"
            t1 <- expr
            spaces $ symb "then" 
            t2 <- expr
            spaces $ symb "else"
            t3 <- expr
            return $ S.App (S.App (S.App S.If t1) t2) t3


-- | Parser for records
termRec :: Parser S.SOLTerm
termRec = do
  symb "{"
  x  <- termRecField
  xs <- many (do {symb ","; (x,t) <- termRecField; return (x,t)})
  if uniqs (x:xs) 
    then do symb "}"
            return $ S.Rec (x:xs)
    else zerop
  where uniqs = all (null . tail) . L.group . L.sort . map fst


-- | Parser for individual record fields
termRecField :: Parser (String, S.SOLTerm)
termRecField = do
  x <- spaces str
  symb "="
  t <- spaces term
  return (x, t)


-- | Parser for record projection
termProj :: Parser S.SOLTerm
termProj = do
  r <- termRec +++ termVar 
  symb "."
  x <- str
  return $ S.App r (S.Proj x)


-- | Parser for sums
termIn :: Bool -> Parser S.SOLTerm
termIn b
  = let (s,c) = if b 
                  then ("inl", S.Inl)
                  else ("inr", S.Inr)
    in do spaces $ symb s
          l1 <- term
          spaces $ symb ":"
          t1 <- typTerm
          return $ S.App (c t1) l1


-- | Parser for inl sums
termInl :: Parser S.SOLTerm
termInl = termIn True


-- | Parser for inl sums
termInr :: Parser S.SOLTerm
termInr = termIn False


-- | Parser for case statements
termCase :: Parser S.SOLTerm
termCase = symb "case" >> return S.Case


-- | Parser for products
termProd :: Parser S.SOLTerm
termProd = do
  spaces $ symb "("
  t1 <- term
  spaces $ symb ","
  t2 <- term
  spaces $ symb ")"
  return $ S.App (S.App S.Prod t1) t2


-- | Parser for fst projection
termPrj1 :: Parser S.SOLTerm
termPrj1 = string "fst" 
  +++ string "π1" >> return S.Prj1  


-- | Parser for snd projection
termPrj2 :: Parser S.SOLTerm
termPrj2 = string "snd" 
  +++ string "π2" >> return S.Prj2  


-- | Parser for term-level [TYPES]
termTyp :: Parser S.SOLTerm
termTyp = S.Typ <$> sqbracket (spaces typTerm)


-- | Parser for pack statements
termPack :: Parser S.SOLTerm
termPack
  = let pair = do ty <- spaces typTerm
                  symb ","
                  t <- spaces term 
                  return (t,ty)
    in do spaces $ symb "pack"
          (t1,ty1) <- angbracket pair
          spaces $ symb "as"
          ty2 <- typTerm
          return $ S.Pack ty1 t1 ty2

-- | Parser for unpack statements
termUnpack :: Parser S.SOLTerm
termUnpack
  = let pair = do ty <- spaces strT
                  symb ","
                  t <- spaces str 
                  return (t,ty)
    in do spaces $ symb "unpack"
          (ty1,t1) <- angbracket pair
          spaces $ symb "="
          t2 <- term
          spaces $ symb "in"
          t3 <- term
          return $ S.Unpack ty1 t1 t2 t3

  

-- | Abstraction allows escaped backslash or lambda
lambdas :: String
lambdas = ['\x03bb','\\']


-- | Lam parser parses abstractions
lam :: Parser S.SOLTerm
lam = do
  spaces $ identifier lambdas
  x <- str
  spaces (symb ":")
  t <- typTerm
  spaces (symb ".")
  e <- spaces term
  return $ S.Abs x t e


-- | Second order abstraction for types
lam2 :: Parser S.SOLTerm
lam2 = do
  spaces $ identifier ['\x39b','L']
  x <- strT
  spaces (symb ".")
  e <- spaces term
  return $ S.Forall x e


-- | App parses application terms, with one or more spaces in between terms.
app :: Parser S.SOLTerm
app = chainl1 expr $ do
  space1
  return S.App 


-- | Parser for let expressions
pLet :: Parser (String, Either S.SOLTerm S.T)
pLet = do
  space
  symb "let"
  space1
  v <- str
  spaces $ symb "="
  t <- term 
  return (v, Left t) -- left signifies terms


-- | Parser for type let expressions
pTypeLet :: Parser (String, Either S.SOLTerm S.T)
pTypeLet = do
  space
  symb "lett"
  space1
  v <- strT
  spaces $ symb "="
  t <- typTerm 
  return (v,Right t) --right signify type let


-- | Parser for regular terms.
pTerm :: Parser (String, Either S.SOLTerm S.T)
pTerm = do
  t <- term 
  return ("", Left t)


-- | Expression follows CFG form with bracketing convention.
expr :: Parser S.SOLTerm
expr = termTyp +++ termProj +++ termRec
  +++ termInl +++ termInr +++ termCase
  +++ termProd +++ termPrj1 +++ termPrj2
  +++ termTrue +++ termFalse +++ termIf
  +++ termNat +++ termVar 
  +++ bracket term


-- | Top level of CFG Grammar
term :: Parser S.SOLTerm
term = lam +++ lam2 +++ termPack +++ termUnpack
  +++ app 


-- | Identifies key words.
identifier :: String -> Parser Char 
identifier xs = sat (`elem` xs)

