 module Cata where

import Data.Map as M
import Data.Set as S
import Control.Monad (guard)

-- Cata, of the form C or 'T -> T' or a mix of both
data T 
  = TVar Char
  | TArr T T
  | TUnit
  | TProd T T
  | TSum T T
  | TMu T
  | X
  deriving (Eq, Ord) 
--equivalence of types compares the binary trees of each type
--we use strict propositional equality
--we could equally encode this using system F (which is how Haskell does it)
--added product and sum types as well as type variables for convenience

-- Simple show instance
instance Show T where
  show X          = "X"
  show (TVar x)   = x:""
  show (TArr a b) = paren (isArr a) (show a) ++ "->" ++ show b
  show (TUnit)    = "\x22A4"
  show (TMu t)  = "\x03bc" ++ paren True (show t)
  show (TProd a b)= paren (isArr a) (show a) 
    ++ " \x00D7 " ++ paren (isArr b) (show b)
  show (TSum a b) = paren (isTProd a || isArr a) (show a) 
    ++ " + " ++ paren (isTProd b || isArr b) (show b)

paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x

isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False

isTProd :: T -> Bool
isTProd (TProd a b) = True
isTProd _           = False

isMu :: T -> Bool
isMu (TMu _) = True
isMu _       = False

-- Cata Terms
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style
data CataTerm
  = Var Int
  | Abs Int T CataTerm
  | App CataTerm CataTerm
  | In T  --inductive type wrapper
  | Cata  --Catamorphism (fold) on inductive type
  | Inl T --left injection
  | Inr T --right injection
  | Case --case analysis of sums, like case in haskell
  | Unit --unit type
  | Prod --2 term pair
  | Prj1 --left projection from pair
  | Prj2 --right projection
  deriving Ord

-- Simple show instance for Cata, adheres to bracketing convention
instance Show CataTerm where
  show (Var x)      = show x
  show (App (App (Prod) a) b) = paren True (show a ++ ", " ++ show b)
  show (App (Inl t) a) = "inl " ++ show a ++ ":" ++ show t
  show (App (Inr t) a) = "inr " ++ show a ++ ":" ++ show t --left of sum
  show (App Prj1 a) = "\x03C0" ++ "1 " ++ show a 
  show (App Prj2 a) = "\x03C0" ++ "2 " ++ show a 
  show (App (In t) a) = 
    "in " ++ paren (isAbs a || isApp a || isSum a || isProd a) (show a) 
    ++ ":" ++ show t  --inside inductive type
  show (App t1 t2)  = 
    paren (isAbs t1) (show t1) ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) = 
    "\x03bb" ++ show x ++ ":" ++ show t ++ "." ++ show l1
  show (In t)     = "in " --above case should handle this
  show Cata         = "cata"
  show (Inl t)      = "inl" --above should handle this case
  show (Inr t)      = "inr" --above should handle this case
  show Unit         = "()"
  show Prj1         = "\x03C0" ++ "1" --above should handle this case
  show Prj2         = "\x03C0" ++ "2" --above should handle this case
  show Case         = "case"

isAbs :: CataTerm -> Bool
isAbs (Abs _ _ _) = True
isAbs _         = False

isApp :: CataTerm -> Bool
isApp (App _ _) = True
isApp _         = False

isSum :: CataTerm -> Bool
isSum (Inl _) = True
isSum (Inr _) = True
isSum _ = False

isProd :: CataTerm -> Bool
isProd (Prod) = True
isProd _ = False

-- alpha equivalence of terms using syntactic term equality
instance Eq CataTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality 
-- if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS
termEquality :: (CataTerm, CataTerm) 
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
termEquality (In t1, In t2) c s = t1 == t2
termEquality (Inl t1, Inl t2) c s = t1 == t2
termEquality (Inr t1, Inr t2) c s = t1 == t2
termEquality (x, y) c s = x == y

-- Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map Int T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term
typeof :: CataTerm -> Context -> Maybe T
typeof Unit ctx = Just TUnit
typeof (Var v) ctx = M.lookup v ctx
typeof l@(Abs x t l1) ctx = do 
  t' <- typeof l1 (M.insert x t ctx)
  return $ TArr t t'
typeof l@(App (App Prod l1) l2) ctx = do
  t1 <- typeof l1 ctx
  t2 <- typeof l2 ctx
  return $ TProd t1 t2
typeof l@(App Prj1 l1) ctx = do
  t1 <- typeof l1 ctx
  case t1 of
    TProd t2 t3 -> Just t2
    _ -> Nothing
typeof l@(App Prj2 l1) ctx = do
  t1 <- typeof l1 ctx
  case t1 of
    TProd t2 t3 -> Just t2
    _ -> Nothing
typeof (App (Inl (TSum t1 t2)) l1) ctx = do
  t3 <- typeof l1 ctx
  guard (t1 == t3)
  return $ TSum t1 t2
typeof (App (Inr (TSum t1 t2)) l1) ctx = do
  t3 <- typeof l1 ctx
  case t3 of 
    TSum _ t4 -> do
      guard (t2 == t4)
      return $ TSum t1 t2
    _ -> Nothing
typeof (App (App (App Case l1) l2) l3) ctx = do
  t1 <- typeof l1 ctx
  t2 <- typeof l2 ctx
  t3 <- typeof l3 ctx
  case (t1,t2,t3) of
    (TSum t4 t5,TArr t6 t7, TArr t8 t9) -> do
      guard (t4 == t6 && t5 == t8 && t7 == t9)
      return $ t7
    _ -> Nothing
typeof (App (In t1) l1) ctx = do -- t : F (mu F) then In t : Mu F
  t2 <- typeof l1 ctx
  guard (t1 == t2)
  return $ typeSub t1 (X, TMu t1)
typeof (App Cata l1) ctx = do -- f : F X -> X then cata f : Mu F -> X
  t1 <- typeof l1 ctx
  case t1 of 
    TArr t2 X -> do
      guard (t2 `contains` X)
      return $ TArr (TMu t2) X
    _ -> Nothing 
typeof l@(App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> do
      guard (t1 == t2)
      return t3
    _ -> Nothing
typeof _ _ = Nothing

--simple function to see if any leaf in the type tree is of a type
contains :: T -> T -> Bool
contains t@(TVar a) y    = t == y
contains t@(TUnit) y     = t == y
contains t@(X) y         = t == y
contains t@(TProd a b) y = t == y || contains a y || contains b y
contains t@(TSum a b) y  = t == y || contains a y || contains b y
contains t@(TMu a) y     = t == y || contains a y
contains t@(TArr a b) y  = t == y || contains a y || contains b y

-- similar to type substitution but at the type level
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) c = l
typeSub X (_,z) = z
typeSub l@(TArr t1 t2) c = TArr (typeSub t1 c) (typeSub t2 c)
typeSub l@(TMu t1) c = l
typeSub TUnit c = TUnit
typeSub (TProd t1 t2) c = TProd (typeSub t1 c) (typeSub t2 c)
typeSub (TSum t1 t2) c = TSum (typeSub t1 c) (typeSub t2 c)

-- top level typing function providing empty context
typeof' l = typeof l M.empty

--bound variables of a term
bound :: CataTerm -> Set Int
bound (Var n) = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)
bound _      = S.empty

--free variables of a term
free :: CataTerm -> Set Int
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)
free _      = S.empty

--test to see if a term is closed (has no free vars)
closed :: CataTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: CataTerm -> Set CataTerm
sub l@(Var x) = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l    = S.singleton l

--element is bound in a term
notfree :: Int -> CataTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: CataTerm -> Set Int
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1
vars l = S.empty

--generates a fresh variable name for a term
newlabel :: CataTerm -> Int
newlabel = (+1) . maximum . vars

--rename t (x,y): renames free occurences of x in t to y
rename :: CataTerm -> (Int, Int) -> CataTerm
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))
rename l c = l

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: CataTerm -> (CataTerm, CataTerm) -> CataTerm
substitute l1@(Var c1) (Var c2, l2) 
  = if c1 == c2 then l2 else l1 
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute (Abs y t l1) c@(Var x, l2)
  | y == x = Abs y t l1
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise = Abs z t $ substitute (rename l1 (y,z)) c
  where z = max (newlabel l1) (newlabel l2)
substitute l _ = l

-- one-step reduction relation 
reduce1 :: CataTerm -> Maybe CataTerm 
reduce1 l@(Var x) = Nothing
reduce1 l@(Abs x t s) = do
  s' <- reduce1 s
  return $ Abs x t s'
reduce1 l@(App (Abs x t l1) l2) 
  = Just $ substitute l1 (Var x, l2)  --beta conversion
reduce1 l@(App (App Prod l1) l2) = do
  case reduce1 l1 of
    Just l1' -> Just $ App (App Prod l1') l2
    _ -> case reduce1 l2 of 
      Just l2' -> Just $ App (App Prod l1) l2'
      _ -> Nothing 
reduce1 l@(App Prj1 (App (App prod l1) _)) = Just l1
reduce1 l@(App Prj2 (App (App prod _) l2)) = Just l2
reduce1 l@(App (Inl t) l1) = do
  l1' <- reduce1 l1
  return $ App (Inl t) l1'
reduce1 l@(App (Inr t) l1) = do
  l1' <- reduce1 l1
  return $ App (Inr t) l1'
reduce1 (App (App (App Case (App (Inl t1) l1)) f) _) =
  return $ App f l1
reduce1 (App (App (App Case (App (Inr t1) l1)) _) g) =
  return $ App g l1
reduce1 l@(App (App Cata f) (App (In t) l1)) =
  return $ App f $ App (findFmap t (App Cata f)) l1 
-- cata f (in t) ~> f (F (cata f) t)
reduce1 t = Nothing

findFmap :: T -> CataTerm -> CataTerm
findFmap X f              = f 
findFmap TUnit f          = Unit
findFmap (TArr t1 t2) f   = Abs 1 t1 $ findFmap t2 f
findFmap (TProd t1 t2) f  = App (App Prod (findFmap t1 f)) (findFmap t2 f)
findFmap t@(TSum t1 t2) f = Abs 1 t $ App (App (App Case (Var 1)) inl) inr
  where 
    fm t = findFmap t f
    inl = App (Inl t) $ fm t1
    inr = App (Inr t) $ fm t2

testFind1 = Cata.findFmap (TSum TUnit X) (App Cata (Var 1))


-- multi-step reduction relation 
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reduce :: CataTerm -> CataTerm
reduce t = case reduce1 t of 
  Just t' -> reduce t'
  Nothing -> t

--- multi-step reduction relation that accumulates all reduction steps
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reductions :: CataTerm -> [CataTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []

-- function takes a functor and applies Mu to it
applMu :: T -> T 
applMu t = typeSub t (X, TMu t)

--common combinators
i = Abs 1 (TVar 'A') (Var 1)
true = Abs 1 (TVar 'A') (Abs 2 (TVar 'B') (Var 1))
false = Abs 1 (TVar 'A') (Abs 2 (TVar 'B') (Var 2))
typeNat = TSum TUnit X
zero = App (In typeNat) $ App (Inl typeNat) Unit
one = App (In $ applMu typeNat) (App (Inr $ applMu typeNat) zero)
succ = Abs 1 (applMu typeNat) $ App (In $ applMu typeNat) (App (Inr $ applMu typeNat) (Var 1))
succApp n = App Cata.succ n
typeTree = TSum TUnit (TProd (TVar 'A') X) --list type
nil = App (In typeTree) $ App (Inl typeTree) Unit
{-cons = Abs 1 (TVar 'A') 
  $ Abs 2 (TVar 'A') $ App (In (applMu typeTree)) 
  $ App (Inr (applMu typeTree)) $ App (App Prod (Var 1)) (Var 2)-}
xx = Abs 1 (TArr (TVar 'A') (TVar 'A')) (App (Var 1) (Var 1)) --won't type check as expected
omega = App xx xx --won't type check, see above

-- Haskell Int to Cataterm in Church Numeral style
toChurch :: Int -> CataTerm
toChurch 0 = zero
toChurch n = succApp (toChurch (n-1))

-- test cases - TODO interesting test cases with inductive types
test1 = Abs 1 (TArr (TVar 'A') (TVar 'B')) $ Abs 2 (TVar 'A') $ App (Var 1) (Var 2) -- \f x. f x
test2 = Abs 1 (TArr (TVar 'A') (TVar 'B')) $ Abs 2 (TVar 'A') $ App (App (Var 1) (Var 2)) (Var 1) -- \f x. (f x) f
test3 = App i xx -- i xx



