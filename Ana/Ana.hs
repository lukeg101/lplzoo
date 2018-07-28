module Ana where

import Data.Map as M
import Data.Set as S
import Control.Monad (guard)

-- Ana, of the form C or 'T -> T' or a mix of both
data T
  = TVar Char
  | TArr T T
  | TUnit
  | TProd T T
  | TSum T T
  | TNu T
  | X
  deriving (Eq, Ord)
--equivalence of types compares the binary trees of each type
--we use strict propositional equality

-- Simple show instance
instance Show T where
  show X          = "X"
  show (TVar x)   = x:""
  show (TArr a b) = paren (isArr a) (show a) ++ "->" ++ show b
  show (TUnit)    = "\x22A4"
  show (TNu t)  = "\x03BD" ++ paren True (show t)
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

-- Ana Terms
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style
data AnaTerm
  = Var Int
  | Abs Int T AnaTerm
  | App AnaTerm AnaTerm
  | Out    --coinductive type wrapper
  | Ana T --anamorphism on coinductive type - unfold
  | Inl T --left injection
  | Inr T --right injection
  | Case --case analysis of sums, like case in haskell
  | Unit --unit type
  | Prod --2 term pair
  | Prj1 --left projection from pair
  | Prj2 --right projection
  deriving Ord

-- Simple show instance for ana, adheres to bracketing convention
instance Show AnaTerm where
  show (Var x)      = show x
  show (App (App (Prod) a) b) = paren True (show a ++ ", " ++ show b)
  show (App (Inl t) a) = "inl " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a) ++ ":" ++ show t
  show (App (Inr t) a) = "inr " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a) ++ ":" ++ show t --left of sum
  show (App Prj1 a) = "\x03C0" ++ "1 " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a)
  show (App Prj2 a) = "\x03C0" ++ "2 " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a)
  show (App (Ana t) a) =
    "ana " ++ paren (isAbs a || isApp a || isSum a || isProd a) (show a)
    ++ ":" ++ show t  --cata term
  show (App t1 t2)  =
    paren (isAbs t1) (show t1) ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) =
    "\x03bb" ++ show x ++ ":" ++ show t ++ "." ++ show l1
  show (Ana t)      = "ana"--above case should handle this
  show (Inl t)      = "inl" --above should handle this case
  show (Inr t)      = "inr" --above should handle this case
  show Unit         = "()"
  show Prj1         = "\x03C0" ++ "1" --above should handle this case
  show Prj2         = "\x03C0" ++ "2" --above should handle this case
  show Case         = "case"

isAbs :: AnaTerm -> Bool
isAbs (Abs _ _ _) = True
isAbs _         = False

isApp :: AnaTerm -> Bool
isApp (App _ _) = True
isApp _         = False

isSum :: AnaTerm -> Bool
isSum (App (Inl t) a) = True
isSum (App (Inr t) a) = True
isSum _ = False

isProd :: AnaTerm -> Bool
isProd (Prod) = True
isProd _ = False

-- alpha equivalence of terms using syntactic term equality
instance Eq AnaTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality
-- if bound t1 XOR bound t2 == true then False
-- application recursively checks both the LHS and RHS
termEquality :: (AnaTerm, AnaTerm)
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
termEquality (Inl t1, Inl t2) c s = t1 == t2
termEquality (Inr t1, Inr t2) c s = t1 == t2
termEquality (Ana t1, Ana t2) c s = t1 == t2
termEquality (x, y) c s = x == y

-- Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map Int T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation
-- Nothing denotes failure to type the term
typeof :: AnaTerm -> Context -> Maybe T
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
  guard (t2 == t3)
  return $ TSum t1 t2
typeof (App (App (App Case l1) l2) l3) ctx = do
  t1 <- typeof l1 ctx
  t2 <- typeof l2 ctx
  t3 <- typeof l3 ctx
  case (t1,t2,t3) of
    (TSum t4 t5,TArr t6 t7, TArr t8 t9) -> do
      guard (t7 == t9)
      guard (t4 == t6)
      guard (t5 == t8)
      return $ t7
    _ -> Nothing
typeof (App Out l1) ctx = do -- u : vF then out u : F (vF)
  t1 <- typeof l1 ctx
  case t1 of 
    (TNu t1') -> do
      return $ typeSub t1' (X, TNu t1')
    _ -> Nothing
typeof (App (Ana l@(TArr t1 (TNu t2))) l1) ctx = do -- f : X -> F X then ana f : X  -> v(F)
  t3 <- typeof l1 ctx
  case t3 of
    (TArr t4 t5) -> do
      guard (t1 == t4 && (typeSub t2 (X, t4)) == t5)
      return l
    _ -> Nothing
typeof l@(App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> do
      guard (t1 == t2)
      return t3
    _ -> Nothing
typeof _ _ = Nothing

-- similar to type substitution but at the type level
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) c = l
typeSub X (_,z) = z
typeSub l@(TArr t1 t2) c = TArr (typeSub t1 c) (typeSub t2 c)
typeSub l@(TNu t1) c = l
typeSub TUnit c = TUnit
typeSub (TProd t1 t2) c = TProd (typeSub t1 c) (typeSub t2 c)
typeSub (TSum t1 t2) c = TSum (typeSub t1 c) (typeSub t2 c)

-- top level typing function providing empty context
typeof' l = typeof l M.empty

--bound variables of a term
bound :: AnaTerm -> Set Int
bound (Var n) = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)
bound _      = S.empty

--free variables of a term
free :: AnaTerm -> Set Int
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)
free _      = S.empty

--test to see if a term is closed (has no free vars)
closed :: AnaTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: AnaTerm -> Set AnaTerm
sub l@(Var x) = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l    = S.singleton l

--element is bound in a term
notfree :: Int -> AnaTerm -> Bool
notfree x = not . S.member x . free

--set of variables in a term
vars :: AnaTerm -> Set Int
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1
vars l = S.empty

--generates a fresh variable name for a term
newlabel :: AnaTerm -> Int
newlabel = (+1) . maximum . vars

--rename t (x,y): renames free occurences of x in t to y
rename :: AnaTerm -> (Int, Int) -> AnaTerm
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))
rename l c = l

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: AnaTerm -> (AnaTerm, AnaTerm) -> AnaTerm
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
reduce1 :: AnaTerm -> Maybe AnaTerm
reduce1 l@(Var x) = Nothing
reduce1 l@(Case)  = Nothing
reduce1 l@(Inl t1)= Nothing
reduce1 l@(Inr t1)= Nothing
reduce1 l@(Out)   = Nothing
reduce1 l@(Ana t1)= Nothing
reduce1 l@(Unit)  = Nothing
reduce1 l@(Prod)  = Nothing
reduce1 l@(Prj1)  = Nothing
reduce1 l@(Prj2)  = Nothing
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
reduce1 l@(App (Inl t) l1) = do -- reduce under inl
  l1' <- reduce1 l1
  return $ App (Inl t) l1'
reduce1 l@(App (Inr t) l1) = do -- reduce under inr
  l1' <- reduce1 l1
  return $ App (Inr t) l1'
reduce1 (App (App (App Case (App (Inl t1) l1)) f) _) =
  return $ App f l1 -- case (inl x) f g ~> f x
reduce1 (App (App (App Case (App (Inr t1) l1)) _) g) =
  return $ App g l1 -- case (inr x) f g ~> g x
reduce1 l@(App Out (App (App (Ana t1) f) x)) =
  return $ App (findFmap t1 (App (Ana t1) f)) (App f x)
-- out (ana f x) ~> F (ana f) (f x)
reduce1 (App Out l1) = do --reduce under out
  l1' <- reduce1 l1
  return $ App Out l1'
reduce1 (App (Ana t1) l1) = do -- reduce under ana
  l1' <- reduce1 l1
  return $ App (Ana t1) l1' 
reduce1 (App l1 l2) = do -- reduce under app
  case reduce1 l1 of
    Just l1' -> return $ App l1' l2
    _ -> case reduce1 l2 of
      Just l2' -> return $ App l1 l2'
      _ -> Nothing

findFmap :: T -> AnaTerm -> AnaTerm
findFmap X f               = f
findFmap t@(TArr  t1 t2) f = Abs 0 t $ Abs 1 t1 $ App (findFmap t2 f) $ App (Var 0) (Var 1)
findFmap t@(TProd t1 t2) f = Abs 0 t $ App (App Prod left) right
  where
    left  = App (findFmap t1 f) $ App Prj1 (Var 0)
    right = App (findFmap t2 f) $ App Prj2 (Var 0)
findFmap t@(TSum  t1 t2) f = Abs 0 t $ App (App (App Case (Var 0)) inl) inr
  where
    inl = Abs 1 t1 (App (Inl t) (App (findFmap t1 f) (Var 1)))
    inr = Abs 1 t1 (App (Inr t) (App (findFmap t2 f) (Var 1)))
findFmap t f               = Abs 0 t (Var 0)

-- multi-step reduction relation
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reduce :: AnaTerm -> AnaTerm
reduce t = case reduce1 t of
  Just t' -> reduce t'
  Nothing -> t

--- multi-step reduction relation that accumulates all reduction steps
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reductions :: AnaTerm -> [AnaTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []

--common combinators
--todo
