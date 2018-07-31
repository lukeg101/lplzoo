module SystemT where

import Data.Map as M
import Data.Set as S

import Control.Monad (guard)

-- Simple Types for system T, of the form 'Nat' or 'o -> o' or a mix of both
data T 
  = TNat
  | TArr T T
  deriving (Eq, Ord) --Type equality is simply equality of the type tree like STLC

paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x

isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False

-- Naive show instance for types, follows bracketing convention
instance Show T where
  show TNat        = "Nat"
  show (TArr a b)  = paren (isArr a) (show a) ++ "->" ++ show b

-- System T Terms
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style
-- Zero is a value in the language
-- Succ n is an function that takes a term and makes a term
data STTerm
  = Var String
  | Abs String T STTerm
  | App STTerm STTerm
  | Zero
  | Succ -- Succ n is acheived with App Succ n 
  | RecNat -- Same as Succ
  deriving Ord

{-
It's possible to define Succ as a no-arg constructor:
data STTerm = ... | NatSucc | ...
and then give it type Nat -> Nat which Haskell can infer
then you can apply NatSucc like a function. However I stick to 
the inductive approach instead
-}

-- alpha equivalence of terms, same as STLC
instance Eq STTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- syntactic equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality 
-- if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS
termEquality :: (STTerm, STTerm) 
  -> (Map String Int, Map String Int) 
  -> Int 
  -> Bool
termEquality (Var x, Var y) (m1, m2) s = case M.lookup x m1 of
  Just a -> case M.lookup y m2 of
    Just b -> a == b
    _ -> False
  _ -> x == y
termEquality (Abs x t1 l1, Abs y t2 l2) (m1, m2) s = 
  t1 == t2 && termEquality (l1, l2) (m1', m2') (s+1) 
  where 
    m1' = M.insert x s m1
    m2' = M.insert y s m2
termEquality (App a1 b1, App a2 b2) c s = 
  termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality (Succ, Succ) c s = True
termEquality (Zero, Zero) c s = True
termEquality (RecNat, RecNat) c s = True
termEquality _ _ _ = False

-- show instance for STTerms, following bracketing convention
instance Show STTerm where
  show Zero    = "z"
  show Succ    = "s"
  show RecNat  = "rec"
  show (Var x) = x
  show (App t1 t2)  = 
    paren (isAbs t1) (show t1) ++ ' ' 
      : paren (isAbs t2 || isApp t2 || isSucc t2) (show t2)
  show (Abs x t l1) = 
    "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1

isSucc :: STTerm -> Bool
isSucc Succ = True
isSucc _    = False

isAbs :: STTerm -> Bool
isAbs (Abs _ _ _) = True
isAbs _         = False

isApp :: STTerm -> Bool
isApp (App _ _) = True
isApp _         = False

-- type context is a mapping from variable name to type T
type Context = M.Map String T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term (not in \->+Nat)
typeof :: STTerm -> Context -> Maybe T
typeof Zero ctx = Just TNat
typeof (Var v) ctx = M.lookup v ctx
typeof l@(Abs x t l1) ctx = do 
  t' <- typeof l1 (M.insert x t ctx)
  return $ TArr t t'
typeof l@(App (App (App RecNat h) a) n) ctx = do 
  t1 <- typeof h ctx
  t2 <- typeof a ctx
  t3 <- typeof n ctx
  case t1 of
    TArr TNat (TArr t1' t1'') -> do
        guard (t1' == t1'' && t2 == t1' && t3 == TNat) 
        Just $ t2
    _ -> Nothing
typeof l@(App Succ l2) ctx = do
  guard (typeof l2 ctx == Just TNat)
  Just TNat
typeof l@(App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> do
      guard (t1 == t2)
      return t3
    _ -> Nothing
typeof _ _ = Nothing

-- top level typing derivation, passing empty context to typeof
typeof' l = typeof l M.empty

--bound variables of a term
bound :: STTerm -> Set String
bound Zero         = S.empty
bound Succ         = S.empty
bound RecNat       = S.empty
bound (Var n)      = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)

--free variables of a term
free :: STTerm -> Set String
free Zero         = S.empty
free Succ         = S.empty
free RecNat       = S.empty
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)

--test to see if a term is closed (has no free vars)
closed :: STTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: STTerm -> Set STTerm
sub l@Zero         = S.singleton l
sub l@Succ         = S.singleton l
sub l@RecNat       = S.singleton l
sub l@(Var x)      = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)

--element is bound in a term
notfree :: String -> STTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: STTerm -> Set String
vars Zero         = S.empty
vars Succ         = S.empty
vars RecNat       = S.empty
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1

--generates a fresh variable name for a term
newlabel :: STTerm -> String
newlabel x = head . dropWhile (`elem` vars x) 
  $ iterate genVar $  S.foldr biggest "" $ vars x

--generates fresh variable names from a given variable
genVar :: String -> String 
genVar []       = "a"
genVar ('z':xs) = 'a':genVar xs
genVar ( x :xs) = succ x:xs

--length-observing maximum function that falls back on lexicographic ordering
biggest :: String -> String -> String 
biggest xs ys = if length xs > length ys then xs else max xs ys


--rename t (x,y): renames free occurences of x in t to y
rename :: STTerm -> (String, String) -> STTerm
rename Zero c = Zero
rename Succ c = Succ
rename RecNat c = RecNat
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: STTerm -> (STTerm, STTerm) -> STTerm
substitute Zero c = Zero
substitute Succ c = Succ
substitute RecNat c = RecNat
substitute l1@(Var c1) (Var c2, l2) 
  = if c1 == c2 then l2 else l1 
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute (Abs y t l1) c@(Var x, l2)
  | y == x = Abs y t l1
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise = Abs z t $ substitute (rename l1 (y,z)) c
  where z = max (newlabel l1) (newlabel l2)

--one-step reduction relation 
reduce1 :: STTerm -> Maybe STTerm 
reduce1 Zero = Nothing
reduce1 Succ = Nothing
reduce1 RecNat = Nothing
reduce1 (App Succ n) = case reduce1 n of
  Just n' -> Just $ App Succ n'
  _ -> Nothing
reduce1 l@(App (App (App RecNat h) a) Zero) = Just a
reduce1 l@(App (App (App RecNat h) a) (App Succ n)) =
  Just $ App (App h n) (App (App (App RecNat h) a) n)
reduce1 l@(App (App (App RecNat h) a) x) = Nothing
reduce1 l@(Var x) = Nothing
reduce1 l@(Abs x t s) = do
  s' <- reduce1 s
  Just $ Abs x t s'
reduce1 l@(App (Abs x t l') l2) = 
  Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 l@(App l1 l2) = do
  l' <- reduce1 l1
  Just $ App l' l2

--multi-step reduction relation - NOT GUARANTEED TO TERMINATE
reduce :: STTerm -> STTerm
reduce t = case reduce1 t of 
  Just t' -> reduce t'
  Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: STTerm -> [STTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []

--common combinators
i_Nat t = Abs "x" t (Var "x")
true t f = Abs "x" t (Abs "y" f (Var "x"))
false t f= Abs "x" t (Abs "y" f (Var "y"))
xx = Abs "x" (TArr TNat TNat) (App (Var "x") (Var "x")) --won't type check as expected
omega = App xx xx --won't type check, see above
_if = \c t f -> App (App c t) f
isZero Zero = true

-- function converting Haskell Int to Peano nat in System T
toPeano :: Int -> STTerm
toPeano 0 = Zero
toPeano n = App Succ (toPeano (n-1))

-- function converting Peano nat to Haskell Int
toInt :: STTerm -> Int
toInt Zero = 0
toInt (App Succ n) = 1 + toInt n
toInt _ = error "Not Nat"

--test cases
test1 = Abs "f" (TArr TNat TNat) $ Abs "x" TNat $ App (Var "f") (Var "x") -- \f x. f x
test2 = Abs "f" (TArr TNat TNat) $ Abs "x" TNat $ App (App (Var "f") (Var "x")) (Var "f") -- \f x. (f x) f
test3 = App (App (Abs "x" TNat (Abs "y" TNat (Var "y"))) (Var "y")) (Var "z")


