module ULC where 

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap, guard)
import Data.Char
import System.IO           (hFlush, stdout)

import Data.Set as S
import Data.Map.Lazy as M

--untyped lambda calculus - variables are numbers now as it's easier for renaming
data Term
  = Var Int
  | Abs Int Term
  | App Term Term
  deriving Ord

-- alpha equivalence of lambda terms as Eq instance for Lambda Terms
instance Eq Term where
  a == b = termEquality (a, b) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds to the map and increments the id
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality 
-- if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS
termEquality :: (Term, Term) -> (Map Int Int, Map Int Int) -> Int -> Bool
termEquality (Var x, Var y) (m1, m2) s = case M.lookup x m1 of
  Just a -> case M.lookup y m2 of
    Just b -> a == b
    _ -> False
  _ -> x == y
termEquality (Abs x t1, Abs y t2) (m1, m2) s = 
  termEquality (t1, t2) (m1', m2') (s+1)
  where 
    m1' = M.insert x s m1
    m2' = M.insert y s m2
termEquality (App a1 b1, App a2 b2) c s = 
  termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality _ _ _ = False
  
--Show instance TODO brackets convention
instance Show Term where
  show (Var x)      = show x
  show (App t1 t2)  = '(':show t1 ++ ' ':show t2 ++ ")" 
  show (Abs x t1)   = '(':"\x03bb" ++ show x++ "." ++ show t1 ++ ")"

--bound variables of a term
bound :: Term -> Set Int
bound (Var n)      = S.empty
bound (Abs n t)    = S.insert n $ bound t
bound (App t1 t2)  = S.union (bound t1) (bound t2)

--free variables of a term
free :: Term -> Set Int
free (Var n)      = S.singleton n
free (Abs n t)    = S.delete n (free t)
free (App t1 t2)  = S.union (free t1) (free t2)

--test to see if a term is closed (has no free vars)
closed :: Term -> Bool
closed = S.null . free

--subterms of a term
sub :: Term -> Set Term
sub t@(Var x)      = S.singleton t
sub t@(Abs c t')   = S.insert t $ sub t'
sub t@(App t1 t2)  = S.insert t $ S.union (sub t1) (sub t2)

--element is bound in a term
notfree :: Int -> Term -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: Term -> Set Int
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t1)   = S.insert x $ vars t1

--generates a fresh variable name for a term
newlabel :: Term -> Int
newlabel = (+1) . maximum . vars

--rename t (x,y): renames free occurences of x in t to y
rename :: Term -> (Int, Int) -> Term
rename (Var a) (x,y) = if a == x then Var y else Var a
rename t@(Abs a t') (x,y) = if a == x then t else Abs a $ rename t' (x, y)
rename (App t1 t2) (x,y) = App (rename t1 (x,y)) (rename t2 (x,y))

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: Term -> (Term, Term) -> Term
substitute t1@(Var c1) (Var c2, t2) 
  = if c1 == c2 then t2 else t1 
substitute (App t1 t2) c 
  = App (substitute t1 c) (substitute t2 c)
substitute (Abs y s) c@(Var x, t)
  | y == x = Abs y s
  | y `notfree` t = Abs y $ substitute s c
  | otherwise = Abs z $ substitute (rename s (y,z)) c
  where z = max (newlabel s) (newlabel t)

--eta reduction
eta :: Term -> Maybe Term
eta (Abs x (App t (Var y))) 
  | x == y && x `notfree` t = Just t
  | otherwise = Nothing

--term is normal form if has no subterms of the form (\x.s) t
isNormalForm :: Term -> Bool
isNormalForm = not . any testnf . sub 
  where 
    testnf t = case t of
      (App (Abs _ _) _) -> True
      _ -> False 

--one-step reduction relation 
reduce1 :: Term -> Maybe Term 
reduce1 t@(Var x) = Nothing
reduce1 t@(Abs x s) = case reduce1 s of
  Just s' -> Just $ Abs x s'
  Nothing -> Nothing
reduce1 t@(App (Abs x t1) t2) 
  = Just $ substitute t1 (Var x, t2)  --beta conversion
reduce1 t@(App t1 t2) = case reduce1 t1 of 
  Just t' -> Just $ App t' t2
  _ -> case reduce1 t2 of 
    Just t' -> Just $ App t1 t'
    _ -> Nothing

--multi-step reduction relation - NOT GUARANTEED TO TERMINATE
reduce :: Term -> Term
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: Term -> [Term]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []

--common combinators
i = Abs 1 (Var 1)
true = Abs 1 (Abs 2 (Var 1))
false = Abs 1 (Abs 2 (Var 2))
zero = false
xx = Abs 1 (App (Var 1) (Var 1))
omega = App xx xx
_if = \c t f -> App (App c t) f
_isZero = \n -> _if n false true
_succ = Abs 0 $ Abs 1 $ Abs 2 $ App (Var 1) $ App (App (Var 0) (Var 1)) (Var 2)
appsucc = App _succ

-- function from Haskell Int to Church Numeral
toChurch :: Int -> Term
toChurch n = Abs 0 (Abs 1 (toChurch' n))
  where
    toChurch' 0 = Var 1
    toChurch' n = App (Var 0) (toChurch' (n-1))
 
test1 = App i (Abs 1 (App (Var 1) (Var 1)))
test2 = App (App (Abs 1 (Abs 2 (Var 2))) (Var 2)) (Var 4)
test3 = App (App (toChurch 3) (Abs 0 (App (Var 0) (toChurch 2)))) (toChurch 1)

-- Parsing The ULC

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

-- Simple ULC Repl
main :: IO ()
main = do
  putStrLn "Welcome to the Untyped \x03bb-calculus REPL"
  putStrLn "Type some terms or press Enter to leave."
  repl

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  s <- getLine
  if Prelude.null s 
  then putStrLn "Goodbye."
  else do 
    case apply atom s of
      (x:xs) -> print . reduce $ fst x
      _ -> putStrLn $ "Cannot Parse Term:" ++ s
    repl


























