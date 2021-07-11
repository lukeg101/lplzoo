module Cata where

import Data.Map as M
import Data.Set as S
import Control.Monad (guard)

-- Cata, of the form C or 'T -> T' or a mix of both
data T
  = TVar String
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
  show (TVar x)   = x
  show (TArr a b) = paren (isArr a) (show a) ++ "->" ++ show b
  show TUnit    = "\x22A4"
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
isTProd (TProd _ _) = True
isTProd _           = False

isMu :: T -> Bool
isMu (TMu _) = True
isMu _       = False

-- Cata Terms
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style
data CataTerm
  = Var String
  | Abs String T CataTerm
  | App CataTerm CataTerm
  | In T  --inductive type wrapper
  | Cata T --Catamorphism on inductive type
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
  show (Var x)      = x
  show (App (App Prod a) b) = paren True (show a ++ ", " ++ show b)
  show (App (Inl t) a) = "inl " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a) ++ ":" ++ show t
  show (App (Inr t) a) = "inr " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a) ++ ":" ++ show t --left of sum
  show (App Prj1 a) = "\x03C0" ++ "1 " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a)
  show (App Prj2 a) = "\x03C0" ++ "2 " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a)
  show (App (In t) a) =
    "in " ++ paren (isAbs a || isApp a || isSum a || isProd a) (show a)
    ++ ":" ++ show t  --inside inductive type
  show (App (Cata t) a) =
    "cata " ++ paren (isAbs a || isApp a || isSum a || isProd a) (show a)
    ++ ":" ++ show t  --cata term
  show (App t1 t2)  =
    paren (isAbs t1) (show t1) ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) =
    "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1
  show (In _)       = "in " --above case should handle this
  show (Cata _)     = "cata"--above case should handle this
  show (Inl _)      = "inl" --above should handle this case
  show (Inr _)      = "inr" --above should handle this case
  show Unit         = "()"
  show Prj1         = "\x03C0" ++ "1" --above should handle this case
  show Prj2         = "\x03C0" ++ "2" --above should handle this case
  show Case         = "case"

isAbs :: CataTerm -> Bool
isAbs (Abs {}) = True
isAbs _         = False

isApp :: CataTerm -> Bool
isApp (App _ _) = True
isApp _         = False

isSum :: CataTerm -> Bool
isSum (App (Inl _) _) = True
isSum (App (Inr _) _) = True
isSum _ = False

isProd :: CataTerm -> Bool
isProd Prod = True
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
  -> (Map String Int, Map String Int)
  -> Int
  -> Bool
termEquality (Var x, Var y) (m1, m2) _ = case M.lookup x m1 of
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
termEquality (In t1, In t2) _ _ = t1 == t2
termEquality (Inl t1, Inl t2) _ _ = t1 == t2
termEquality (Inr t1, Inr t2) _ _ = t1 == t2
termEquality (Cata t1, Cata t2) _ _ = t1 == t2
termEquality (Unit, Unit) _ _ = True
termEquality (Case, Case) _ _ = True
termEquality (Prod, Prod) _ _ = True
termEquality (Prj1, Prj1) _ _ = True
termEquality (Prj2, Prj2) _ _ = True
termEquality _ _ _ = False

-- Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map String T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation
-- Nothing denotes failure to type the term
typeof :: CataTerm -> Context -> Maybe T
typeof Unit _ = Just TUnit
typeof (Var v) ctx = M.lookup v ctx
typeof (Abs x t l1) ctx = do
  t' <- typeof l1 (M.insert x t ctx)
  return $ TArr t t'
typeof (App (App Prod l1) l2) ctx = do
  t1 <- typeof l1 ctx
  t2 <- typeof l2 ctx
  return $ TProd t1 t2
typeof (App Prj1 l1) ctx = do
  t1 <- typeof l1 ctx
  case t1 of
    TProd t2 _ -> Just t2
    _ -> Nothing
typeof (App Prj2 l1) ctx = do
  t1 <- typeof l1 ctx
  case t1 of
    TProd t2 _ -> Just t2
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
      return t7
    _ -> Nothing
typeof (App (In l@(TMu t1)) l1) ctx = do -- t : F (mu F) then In t : Mu F
  t2 <- typeof l1 ctx
  guard (typeSub t1 (X, l) == t2)
  return $ TMu t1
typeof (App (Cata (TArr (TMu t1) t2)) l1) ctx = do -- f : F X -> X then cata f : Mu F -> X
  t3 <- typeof l1 ctx
  case t3 of
    (TArr t4 t5) -> do
      guard (t2 == t5 && typeSub t1 (X, t2) == t4)
      return $ TArr (TMu t1) t2
    _ -> Nothing
typeof (App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> do
      guard (t1 == t2)
      return t3
    _ -> Nothing
typeof _ _ = Nothing

-- similar to type substitution but at the type level
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) (TVar y, t)
  | x == y    = t
  | otherwise = l
typeSub X (_,z) = z
typeSub (TArr t1 t2) c = TArr (typeSub t1 c) (typeSub t2 c)
typeSub l@(TMu _) _ = l
typeSub TUnit _ = TUnit
typeSub (TProd t1 t2) c = TProd (typeSub t1 c) (typeSub t2 c)
typeSub (TSum t1 t2) c = TSum (typeSub t1 c) (typeSub t2 c)

-- function takes a functor and applies Mu to it
applMu :: T -> T
applMu t = typeSub t (X, TMu t)

-- top level typing function providing empty context
typeof' l = typeof l M.empty

--bound variables of a term
bound :: CataTerm -> Set String
bound (Var _) = S.empty
bound (Abs n _ l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)
bound _      = S.empty

--free variables of a term
free :: CataTerm -> Set String
free (Var n)      = S.singleton n
free (Abs n _ l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)
free _      = S.empty

--test to see if a term is closed (has no free vars)
closed :: CataTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: CataTerm -> Set CataTerm
sub l@(Var _) = S.singleton l
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l    = S.singleton l

--element is bound in a term
notfree :: String -> CataTerm -> Bool
notfree x = not . S.member x . free

--set of variables in a term
vars :: CataTerm -> Set String
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1
vars _ = S.empty

--set of variables in a type
typeVars :: T -> Set String
typeVars (TVar x)     = S.singleton x
typeVars (TArr t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars TUnit      = S.empty
typeVars (TProd t1 t2)= S.union (typeVars t1) (typeVars t2)
typeVars (TSum t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (TMu t1)     = typeVars t1
typeVars X          = S.empty

--type vars in a term
typeVarsInTerm :: CataTerm -> Set String
typeVarsInTerm (Var x)      = S.singleton x
typeVarsInTerm (Abs _ t l1) = S.union (typeVars t) $ typeVarsInTerm l1
typeVarsInTerm (App l1 l2)  = S.union (typeVarsInTerm l1) (typeVarsInTerm l2)
typeVarsInTerm (In t)       = typeVars t
typeVarsInTerm (Cata t)     = typeVars t
typeVarsInTerm (Inl t)      = typeVars t
typeVarsInTerm (Inr t)      = typeVars t
typeVarsInTerm _              = S.empty

-- function used to do type substitution on types in a term
tSubUnder :: CataTerm -> (T,T) -> CataTerm
tSubUnder l@(Var _) _      = l
tSubUnder (Abs x t l2) c = Abs x (typeSub t c) $ tSubUnder l2 c
tSubUnder (App l1 l2) c  = App (tSubUnder l1 c) (tSubUnder l2 c)
tSubUnder (In t) c       = In $ typeSub t c
tSubUnder (Cata t) c     = Cata $ typeSub t c
tSubUnder (Inl t) c      = Inl $ typeSub t c
tSubUnder (Inr t) c      = Inr $ typeSub t c
tSubUnder l _              = l

--generates a fresh variable name for a term
newlabel :: CataTerm -> String
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
rename :: CataTerm -> (String, String) -> CataTerm
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))
rename l _ = l

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
  where z     = foldr1 biggest [newlabel l1, newlabel l2, newlabel (Var x)]
substitute l _ = l

-- one-step reduction relation
reduce1 :: CataTerm -> Maybe CataTerm
reduce1 (Var _) = Nothing
reduce1 Case  = Nothing
reduce1 (Inl _) = Nothing
reduce1 (Inr _) = Nothing
reduce1 (In _) = Nothing
reduce1 (Cata _) = Nothing
reduce1 Unit = Nothing
reduce1 Prod = Nothing
reduce1 Prj1 = Nothing
reduce1 Prj2 = Nothing
reduce1 (Abs x t s) = do
  s' <- reduce1 s
  return $ Abs x t s'
reduce1 (App (Abs x _ l1) l2)
  = Just $ substitute l1 (Var x, l2)  --beta conversion
reduce1 (App (App Prod l1) l2) = do
  case reduce1 l1 of
    Just l1' -> Just $ App (App Prod l1') l2
    _ -> case reduce1 l2 of
      Just l2' -> Just $ App (App Prod l1) l2'
      _ -> Nothing
reduce1 (App Prj1 (App (App Prod l1) _)) = Just l1
reduce1 (App Prj2 (App (App Prod _) l2)) = Just l2
reduce1 (App (Inl t) l1) = do -- reduce under inl
  l1' <- reduce1 l1
  return $ App (Inl t) l1'
reduce1 (App (Inr t) l1) = do -- reduce under inr
  l1' <- reduce1 l1
  return $ App (Inr t) l1'
reduce1 (App (App (App Case (App (Inl _) l1)) f) _) =
  return $ App f l1 -- case (inl x) f g ~> f x
reduce1 (App (App (App Case (App (Inr _) l1)) _) g) =
  return $ App g l1 -- case (inr x) f g ~> g x
reduce1 (App (In t1) l1) = do --reduce under in
  l1' <- reduce1 l1
  return $ App (In t1) l1'
reduce1 (App (Cata t1) l1) = do -- reduce under cata
  l1' <- reduce1 l1
  return $ App (Cata t1) l1'
reduce1 (App (App (Cata t1) f) (App (In (TMu t2)) l1)) =
  return $ App f $ App (findFmap t2 (App (Cata t1) f)) l1
-- cata f (in t) ~> f (F (cata f) t)
reduce1 (App l1 l2) = do -- reduce under app
  case reduce1 l1 of
    Just l1' -> return $ App l1' l2
    _ -> case reduce1 l2 of
      Just l2' -> return $ App l1 l2'
      _ -> Nothing

findFmap :: T -> CataTerm -> CataTerm
findFmap X f               = f
findFmap t@(TArr  t1 t2) f = Abs "x" t $ Abs "y" t1 $ App (findFmap t2 f) $ App (Var "x") (Var "y")
findFmap t@(TProd t1 t2) f = Abs "x" t $ App (App Prod left) right
  where
    left  = App (findFmap t1 f) $ App Prj1 (Var "x")
    right = App (findFmap t2 f) $ App Prj2 (Var "x")
findFmap t@(TSum  t1 t2) f = Abs "x" t $ App (App (App Case (Var "x")) inl) inr
  where
    inl = Abs "y" t1 (App (Inl t) (App (findFmap t1 f) (Var "y")))
    inr = Abs "y" t1 (App (Inr t) (App (findFmap t2 f) (Var "y")))
findFmap t _               = Abs "x" t (Var "x")

-- multi-step reduction relation
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reduce :: CataTerm -> CataTerm
reduce t = maybe t reduce (reduce1 t)

--- multi-step reduction relation that accumulates all reduction steps
-- NOT GUARANTEED TO TERMINATE IF typeof' FAILS
reductions :: CataTerm -> [CataTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []


--common combinators
i = Abs "x" (TVar "A") (Var "x")
true = Abs "x" (TVar "A") (Abs "y" (TVar "B") (Var "x"))
false = Abs "x" (TVar "A") (Abs "y" (TVar "B") (Var "y"))
typeNat = TSum TUnit X
zero = App (In typeNat) $ App (Inl typeNat) Unit
one = App (In $ applMu typeNat) (App (Inr $ applMu typeNat) zero)
succ_ = Abs "x" (applMu typeNat) $ App (In $ applMu typeNat) (App (Inr $ applMu typeNat) (Var "x"))
succApp = App succ_
typeTree = TSum TUnit (TProd (TVar "A") X) --list type
nil = App (In typeTree) $ App (Inl typeTree) Unit
xx = Abs "x" (TArr (TVar "A") (TVar "A")) (App (Var "x") (Var "x")) --won't type check as expected
omega = App xx xx --won't type check, see above

-- Haskell Int to Cataterm in Church Numeral style
toChurch :: Int -> CataTerm
toChurch 0 = zero
toChurch n = succApp (toChurch (n-1))

-- test cases - TODO interesting test cases with inductive types
test1 = Abs "x" (TArr (TVar "A") (TVar "B")) $ Abs "y" (TVar "A") $ App (Var "x") (Var "y") -- \f x. f x
test2 = Abs "x" (TArr (TVar "A") (TVar "B")) $ Abs "y" (TVar "A") $ App (App (Var "x") (Var "y")) (Var "x") -- \f x. (f x) f
test3 = App i xx -- i xx



