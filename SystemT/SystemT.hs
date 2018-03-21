 module SystemT where

import Data.Map as M
import Data.Set as S

-- Simple Types for system T, of the form 'Nat' or 'o -> o' or a mix of both
data T 
  = TNat
  | TArr T T
  deriving (Eq, Ord) --Type equality is simply equality of the type tree like STLC

-- Naive show instance for types, TODO Bracketing convention for System T Types
instance Show T where
  show TNat        = "Nat"
  show (TArr a b)  = '(':show a ++ "->" ++ show b ++")"

-- System T Terms
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style
-- Zero is a value in the language
-- Succ n is an function that takes a term and makes a term
data STTerm
  = Var Int
  | Abs Int T STTerm
  | App STTerm STTerm
  | Zero
  | Succ STTerm
  | RecNat STTerm STTerm STTerm
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
  t1 == t2 = checker (t1, t2) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality 
-- if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS
checker :: (STTerm, STTerm) 
  -> (Map Int Int, Map Int Int) 
  -> Int 
  -> Bool
checker (Var x, Var y) (m1, m2) s = case M.lookup x m1 of
  Just a -> case M.lookup y m2 of
    Just b -> a == b
    _ -> False
  _ -> x == y
checker (Abs x t1 l1, Abs y t2 l2) (m1, m2) s = 
  t1 == t2 && checker (l1, l2) (m1', m2') (s+1) 
  where 
    m1' = M.insert x s m1
    m2' = M.insert y s m2
checker (App a1 b1, App a2 b2) c s = 
  checker (a1, a2) c s && checker (b1, b2) c s
checker _ _ _ = False

-- Naive show instance for STTerms, TODO implement Bracketing convention for System T
instance Show STTerm where
  show Zero         = "Zero"
  show (Succ n)     = "S " ++ show n
  show (RecNat h a n)  = "Rec "++ show h ++ " (" ++ show a ++ ") (" ++ show n ++ ")"
  show (Var x)      = show x
  show (App t1 t2)  = '(':show t1 ++ ' ':show t2 ++ ")" 
  show (Abs x t l1) = '(':"\x03bb" ++ show x ++ ":" ++ show t ++ "." ++ show l1 ++ ")"

-- type context is a mapping from variable name to type T
type Context = M.Map Int T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term (not in \->+Nat)
typeof :: STTerm -> Context -> Maybe T
typeof Zero ctx = Just TNat
typeof (Succ n) ctx  = do
  t <- typeof n ctx
  if t == TNat then Just t else Nothing
typeof l@(RecNat h a n) ctx = do 
  t1 <- typeof h ctx
  t2 <- typeof a ctx
  t3 <- typeof n ctx
  case t1 of
    TArr TNat (TArr t1' t1'') -> if t1' == t1''
        && t2 == t1' 
        && t3 == TNat 
      then Just $ t2
      else Nothing
    _ -> Nothing
typeof (Var v) ctx = M.lookup v ctx
typeof l@(Abs x t l1) ctx = do 
  t' <- typeof l1 (M.insert x t ctx)
  return $ TArr t t'
typeof l@(App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> if t1 == t2 then return t3 else Nothing
    _ -> Nothing

-- top level typing derivation, passing empty context to typeof
typeof' l = typeof l M.empty

--bound variables of a term
bound :: STTerm -> Set Int
bound Zero         = S.empty
bound (Succ n)     = bound n
bound (RecNat h a n) = S.union (bound h) $ S.union (bound a) (bound n)
bound (Var n)      = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)

--free variables of a term
free :: STTerm -> Set Int
free Zero         = S.empty
free (Succ n)     = free n
free (RecNat h a n) = S.union (free h) $ S.union (free a) (free n)
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)

--test to see if a term is closed (has no free vars)
closed :: STTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: STTerm -> Set STTerm
sub l@Zero         = S.singleton l
sub l@(Succ n)     = S.insert l $ sub n
sub l@(RecNat h a n) = S.insert l $ S.union (sub h) $ S.union (sub a) (sub n)
sub l@(Var x)      = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)

--element is bound in a term
notfree :: Int -> STTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: STTerm -> Set Int
vars Zero         = S.empty
vars (Succ n)     = vars n
vars (RecNat h a n) = S.union (vars h) $ S.union (vars a) (vars n)
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1

--generates a fresh variable name for a term
newlabel :: STTerm -> Int
newlabel = (+1) . maximum . vars

--rename t (x,y): renames free occurences of x in t to y
rename :: STTerm -> (Int, Int) -> STTerm
rename Zero c = Zero
rename (Succ n) c = Succ $ rename n c
rename (RecNat h a n) c = RecNat (rename h c) (rename a c) (rename n c) 
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: STTerm -> (STTerm, STTerm) -> STTerm
substitute Zero c = Zero
substitute (Succ n) c = Succ $ substitute n c
substitute (RecNat h a n) c = RecNat (substitute h c) (substitute a c) (substitute n c)
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
reduce1 l@(Succ n) = do
  x <- reduce1 n 
  Just $ Succ x
reduce1 l@(RecNat h a Zero) = Just a
reduce1 l@(RecNat h a (Succ n)) =
  Just $ App (App h n) (RecNat h a n)
reduce1 l@(RecNat h a x) = Nothing 
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
i_Nat t = Abs 1 t (Var 1)
true t f = Abs 1 t (Abs 2 f (Var 1))
false t f= Abs 1 t (Abs 2 f (Var 2))
xx = Abs 1 (TArr TNat TNat) (App (Var 1) (Var 1)) --won't type check as expected
omega = App xx xx --won't type check, see above
_if = \c t f -> App (App c t) f
isZero Zero = true
plus = Abs 1 TNat $ Abs 2 TNat $ RecNat (Abs 3 TNat $ Abs 4 TNat (Succ (Var 4))) (Var 1) (Var 2)
plusApp n m = App (App plus n) m

-- function converting Haskell Int to Peano nat in System T
toPeano :: Int -> STTerm
toPeano 0 = Zero
toPeano n = Succ (toPeano (n-1))

-- function converting Peano nat to Haskell Int
toInt :: STTerm -> Int
toInt Zero = 0
toInt (Succ n) = 1 + toInt n
toInt _ = error "Not Nat"

--test cases
test1 = Abs 1 (TArr TNat TNat) $ Abs 2 TNat $ App (Var 1) (Var 2) -- \f x. f x
test2 = Abs 1 (TArr TNat TNat) $ Abs 2 TNat $ App (App (Var 1) (Var 2)) (Var 1) -- \f x. (f x) f
test3 = App (App (Abs 1 TNat (Abs 2 TNat (Var 2))) (Var 2)) (Var 4)
test4 = plusApp (toPeano 3) (toPeano 2)
--toInt $ reduce (plusApp (toChurch 3) (toChurch 2)) --> 5



