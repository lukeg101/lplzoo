 module STLC where

import Data.Map as M
import Data.Set as S

-- Simple Types for Lambda Calc, of the form 'o' or 'o -> o' or a mix of both
data T 
  = TVar
  | TArr T T
  deriving (Eq, Ord) --equivalence of types compares the binary trees of each type

-- Simple show instance, TODO brackets convention for types
instance Show T where
  show TVar        = "O"
  show (TArr a b)  = '(':show a ++ "->" ++ show b ++")"

-- Simply-Typed Lambda Calculus Terms
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style
data STTerm
  = Var Int
  | Abs Int T STTerm
  | App STTerm STTerm
  deriving Ord

-- Simple show instance for STLC, TODO Brackets Convention for terms
instance Show STTerm where
  show (Var x)    = show x
  show (App t1 t2)  = '(':show t1 ++ ' ':show t2 ++ ")" 
  show (Abs x t l1) = '(':"\x03bb" ++ show x ++ ":" ++ show t ++ "." ++ show l1 ++ ")"

-- alpha equivalence of terms using syntactic term equality
instance Eq STTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality 
-- if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS
termEquality :: (STTerm, STTerm) 
  -> (Map Int Int, Map Int Int) 
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
termEquality _ _ _ = False

-- Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map Int T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term (not in \->)
typeof :: STTerm -> Context -> Maybe T
typeof (Var v) ctx = M.lookup v ctx
typeof l@(Abs x t l1) ctx = do 
  t' <- typeof l1 (M.insert x t ctx)
  return $ TArr t t'
typeof l@(App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> if t1 == t2 then return t3 else Nothing
    _ -> Nothing

-- top level typing function providing empty context
typeof' l = typeof l M.empty

--bound variables of a term
bound :: STTerm -> Set Int
bound (Var n)      = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)

--free variables of a term
free :: STTerm -> Set Int
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)

--test to see if a term is closed (has no free vars)
closed :: STTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: STTerm -> Set STTerm
sub l@(Var x)      = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)

--element is bound in a term
notfree :: Int -> STTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: STTerm -> Set Int
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1

--generates a fresh variable name for a term
newlabel :: STTerm -> Int
newlabel = (+1) . maximum . vars

--rename t (x,y): renames free occurences of x in t to y
rename :: STTerm -> (Int, Int) -> STTerm
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: STTerm -> (STTerm, STTerm) -> STTerm
substitute l1@(Var c1) (Var c2, l2) 
  = if c1 == c2 then l2 else l1 
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute (Abs y t l1) c@(Var x, l2)
  | y == x = Abs y t l1
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise = Abs z t $ substitute (rename l1 (y,z)) c
  where z = max (newlabel l1) (newlabel l2)

-- one-step reduction relation 
reduce1 :: STTerm -> Maybe STTerm 
reduce1 l@(Var x) = Nothing
reduce1 l@(Abs x t s) = do
  s' <- reduce1 s
  return $ Abs x t s'
reduce1 l@(App (Abs x t l1) l2) 
  = Just $ substitute l1 (Var x, l2)  --beta conversion

-- multi-step reduction relation 
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reduce :: STTerm -> STTerm
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t

--- multi-step reduction relation that accumulates all reduction steps
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reductions :: STTerm -> [STTerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []

--common combinators
i = Abs 1 (TArr TVar TVar) (Var 1)
true = Abs 1 TVar (Abs 2 TVar (Var 1))
false = Abs 1 TVar (Abs 2 TVar (Var 2))
zero = false
xx = Abs 1 (TArr TVar TVar) (App (Var 1) (Var 1)) --won't type check as expected
omega = App xx xx --won't type check, see above
_if = \c t f -> App (App c t) f
_isZero = \n -> _if n false true

-- Haskell Int to Simply Typed Church Numeral
toChurch :: Int -> STTerm
toChurch n = Abs 0 (TArr TVar TVar)(Abs 1 TVar (toChurch' n))
  where
    toChurch' 0 = Var 1
    toChurch' n = App (Var 0) (toChurch' (n-1))

-- test cases
test1 = Abs 1 (TArr TVar TVar) $ Abs 2 TVar $ App (Var 1) (Var 2) -- \f x. f x
test2 = Abs 1 (TArr TVar TVar) $ Abs 2 TVar $ App (App (Var 1) (Var 2)) (Var 1) -- \f x. (f x) f
test3 = App i (Abs 1 (TArr TVar TVar) (App (Var 1) (Var 1))) -- i xx
test4 = App (App (Abs 1 TVar (Abs 2 TVar (Var 2))) (Var 2)) (Var 4)



