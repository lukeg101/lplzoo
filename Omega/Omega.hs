module Omega where

import Data.Map.Lazy as M hiding (map)
import Data.Set as S hiding      (map)
import Data.List as L            (intersperse, group, sort, lookup)
import Control.Monad             (guard)

-- Kinds, consisting of proper types (*), or combinations of kinds
data K
  = KVar
  | KArr K K
  deriving (Eq, Ord)

-- Simple show instance
instance Show K where
  show KVar        = "*"
  show (KArr a b)  = paren (iskArr a) (show a) ++ "=>" ++ show b

paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x

iskArr :: K -> Bool
iskArr (KArr _ _) = True
iskArr _          = False

-- Omega Types, type variables are strings as it's easy to rename
-- Unit/Top is used as the final type, meant to represent Object
-- records are lists of variables and types (todo make this Set)
data T 
  = TVar String
  | TArr T T
  | TAbs String K T
  | TApp T T
  | TNat
  deriving (Eq, Ord)

-- show implementation, uses Unicode for Unit
-- uses bracketing convention for types
instance Show T where
  show (TVar x)    = x
  show (TNat)      = "Nat"
  show (TArr a b)  = paren (isArr a) (show a) ++ "->" ++ show b
  show (TAbs x k t)= 
    "\x03bb" ++ x ++ "::" ++ show k ++ "." ++ show t
  show (TApp t1 t2)=
    paren (isTAbs t1) (show t1) ++ ' ' : paren (isTAbs t2 || isTApp t2) (show t2)

isTAbs :: T -> Bool
isTAbs (TAbs _ _ _) = True
isTAbs _            = False

isTApp :: T -> Bool
isTApp (TApp _ _) = True
isTApp _         = False

isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False

-- Omega Term
-- variables are strings
-- Abstractions carry the type Church style\
data OTerm
  = Var String
  | Abs String T OTerm
  | App OTerm OTerm
  | Zero
  | Succ
  deriving Ord

-- alpha termEqualityalence of terms uses
instance Eq OTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality
-- if bound t1 XOR bound t2 == true then False
-- application recursively checks both the LHS and RHS
termEquality :: (OTerm, OTerm)
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
termEquality (Succ, Succ) _ _ = True
termEquality (Zero, Zero) _ _ = True
termEquality _ _ _ = False

-- show implementation for Omega terms
-- uses bracketing convention for terms
instance Show OTerm where
  show (Zero)       = "z"
  show (Succ)       = "s"
  show (Var x)      = x
  show (App t1 t2)  = 
    paren (isAbs t1) (show t1) ++ ' ' : paren (isAbs t2 || isApp t2 || isSucc t2) (show t2)
  show (Abs x t l1) = "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1

isSucc :: OTerm -> Bool
isSucc Succ = True
isSucc _    = False

isAbs :: OTerm -> Bool
isAbs (Abs _ _ _) = True
isAbs _         = False

isApp :: OTerm -> Bool
isApp (App _ _) = True
isApp _         = False

--type context of term variables and kinds
--input term or type variable as a String
--if the input is a term, return Left its type
--if the input is a type, return Right its kind
type Context = M.Map String (Either T K)

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term (not in \->+Sub)
typeof :: OTerm -> Context -> Maybe T
typeof Zero ctx = Just TNat
typeof (Var v) ctx = case M.lookup v ctx of
  (Just (Left t)) -> Just t
  _ -> Nothing
typeof l@(Abs x t l1) ctx = do
  guard (kindof t ctx == Just KVar) 
  t' <- typeof l1 (M.insert x (Left t) ctx)
  return $ TArr t t'
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

-- Kinding derivation
-- identical to typing but at the type level
kindof :: T -> Context -> Maybe K
kindof (TNat) ctx = Just KVar
kindof (TVar x) ctx = do
  k <- M.lookup x ctx
  case k of 
    (Right k) -> Just k
    _ -> Nothing
kindof (TAbs x k1 t1) ctx = do 
  k2 <- kindof t1 (M.insert x (Right k1) ctx)
  return $ KArr k1 k2
kindof (TApp t1 t2) ctx = do
  k1 <- kindof t1 ctx
  k2 <- kindof t2 ctx 
  case k1 of 
    (KArr k3 k4) -> do
      guard (k2 == k3)
      return k4   
    _ -> Nothing
kindof (TArr t1 t2) ctx = do
  guard (kindof t1 ctx == Just KVar)
  guard (kindof t2 ctx == Just KVar) 
  return KVar

-- top level typing function providing empty context
typeof' l = typeof l M.empty

-- top level kinding function for types
kindof' t = kindof t M.empty

--bound variables of a term
bound :: OTerm -> Set String
bound Zero         = S.empty
bound Succ         = S.empty
bound (Var n)      = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)

--free variables of a term
free :: OTerm -> Set String
free Zero         = S.empty
free Succ         = S.empty
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)

--test to see if a term is closed (has no free vars)
closed :: OTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: OTerm -> Set OTerm
sub l@Zero         = S.singleton l
sub l@Succ         = S.singleton l
sub l@(Var x)      = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)

--element is bound in a term
notfree :: String -> OTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: OTerm -> Set String
vars Zero         = S.empty
vars Succ         = S.empty
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1

--set of variables in a type
typeVars :: T -> Set String
typeVars (TNat)       = S.empty
typeVars (TVar x)     = S.singleton x
typeVars (TArr t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (TAbs x k t) = S.insert x $ typeVars t
typeVars (TApp t1 t2) = S.union (typeVars t1) (typeVars t2)

--type vars in a term
typeVarsInTerm :: OTerm -> Set String
typeVarsInTerm l@(Zero)       = S.empty
typeVarsInTerm l@(Succ)       = S.empty
typeVarsInTerm l@(Var x)      = S.empty
typeVarsInTerm l@(Abs c t l1) = S.union (typeVars t) $ typeVarsInTerm l1
typeVarsInTerm l@(App l1 l2)  = S.union (typeVarsInTerm l1) (typeVarsInTerm l2)

--generates a fresh variable name for a type
newTLabel :: T -> String
newTLabel x = head . dropWhile (`elem` typeVars x) 
  $ iterate genVar $  S.foldr biggest "" $ typeVars x

--rename t (x,y): renames free occurences of type variables x in t to y
renameT :: T -> (String, String) -> T
renameT (TNat) c       = TNat
renameT (TVar a) (x,y) = TVar $ if a == x then y else a
renameT (TArr t1 t2) c = TArr (renameT t1 c) (renameT t2 c)
renameT l@(TAbs x k t1) (y,z) 
  = if x == y then l else TAbs x k $ renameT t1 (y,z)
renameT (TApp t1 t2) c = TApp (renameT t1 c) (renameT t2 c)

-- type-level 'bound' function
notfreeT :: String -> T -> Bool
notfreeT x = not . S.member x . freeTVars

-- type level free variables
freeTVars :: T -> Set String
freeTVars (TNat)       = S.empty
freeTVars (TVar c)     = S.singleton c
freeTVars (TArr t1 t2) = S.union (freeTVars t1) (freeTVars t2) 
freeTVars (TAbs x k t) = S.delete x $ freeTVars t
freeTVars (TApp t1 t2) = S.union (freeTVars t1) (freeTVars t2)

-- similar to type substitution but at the type level
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) (TVar y, t) 
  | x == y            = t
  | otherwise         = l
typeSub l@(TArr t1 t2) c = TArr (typeSub t1 c) (typeSub t2 c)
typeSub l@(TNat) c    = l
typeSub l@(TApp t1 t2) c = TApp (typeSub t1 c) (typeSub t2 c)
typeSub l@(TAbs y k t1) c@(TVar x, t2) 
  | y == x = l
  | y `notfreeT` t2 = TAbs y k $ typeSub t1 c
  | otherwise = TAbs x k $ typeSub (renameT t1 (y,z)) c
  where z = max (newTLabel t1) (newTLabel t2)

-- function used to do type substitution on types in a term
tSubUnder :: OTerm -> (T,T) -> OTerm
tSubUnder l@(Var x) c      = l
tSubUnder l@(Abs x t l2) c = Abs x (typeSub t c) $ tSubUnder l2 c
tSubUnder l@(App l1 l2) c  = App (tSubUnder l1 c) (tSubUnder l2 c)
tSubUnder l c              = l

--generates a fresh variable name for a term
newlabel :: OTerm -> String
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

--rename t (x,y): renames free occurences of term variable x in t to y
rename :: OTerm -> (String, String) -> OTerm
rename Zero c = Zero
rename Succ c = Succ
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) c = App (rename l1 c) (rename l2 c)

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: OTerm -> (OTerm, OTerm) -> OTerm
substitute Zero c = Zero
substitute Succ c = Succ
substitute l1@(Var c1) (Var c2, l2) 
  | c1 == c2 = l2 
  | otherwise = l1 
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute l@(Abs y t l1) c@(Var x, l2)
  | y == x = l
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise = Abs z t $ substitute (rename l1 (y,z)) c
  where z = max (newlabel l1) (newlabel l2)


--one-step reduction relation, TODO implement other reduction strats
reduce1 :: OTerm -> Maybe OTerm
reduce1 Zero = Nothing
reduce1 Succ = Nothing 
reduce1 l@(Var x) = Nothing
reduce1 (App Succ n) = case reduce1 n of
  Just n' -> Just $ App Succ n'
  _ -> Nothing
reduce1 l@(Abs x t s) = case reduce1 s of
  Just s' -> Just $ Abs x t s'
  _ -> case reduce1T t of
    Just t' -> Just $ Abs x t' s
    _ -> Nothing
reduce1 l@(App (Abs x t l') l2) = 
  Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 l@(App l1 l2) = do
  case reduce1 l1 of 
    Just l' -> Just $ App l' l2
    _ -> case reduce1 l2 of
      Just l' -> Just $ App l1 l'
      _ -> Nothing 

--one-step reduction relation for types
--This is reduce1 for STLC but one level up
reduce1T :: T -> Maybe T
reduce1T TNat = Nothing
reduce1T (TVar x) = Nothing
reduce1T (TAbs x k t) = do
  t' <- reduce1T t
  Just $ TAbs x k t'
reduce1T t@(TApp (TAbs x k t1) t2) = 
  Just $ typeSub t1 (TVar x, t2)
reduce1T (TApp t1 t2) = case reduce1T t1 of
  Just t1' -> Just $ TApp t1' t2
  _ -> case reduce1T t2 of 
    Just t2' -> Just $ TApp t1 t2'
    _ -> Nothing
reduce1T (TArr t1 t2) = case reduce1T t1 of
  Just t1' -> Just $ TArr t1' t2
  _ -> case reduce1T t2 of
    Just t2' -> Just $ TArr t1 t2'
    _ -> Nothing

-- multi-step reduction relation 
-- NOT GUARANTEED TO TERMINATE if it doesn't type check
reduce :: OTerm -> OTerm
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: OTerm -> [OTerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []
