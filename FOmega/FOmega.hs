module FOmega where

import Data.Map.Lazy as M
import Data.Set as S
import Control.Monad (guard)

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

-- FOmega Types, term and type variables are strings
-- The Pi type take types a type variable and type T. 
data T 
  = TVar String
  | TArr T T
  | TAbs String K T
  | TApp T T
  | Pi String K T
  | TNat
  deriving Ord

-- does syntactic type equality on types
instance Eq T where
  t1 == t2 = typeEquality (t1, t2) (M.empty, M.empty) 0

-- test for syntactic type equality on types
-- very similar to equality on first-order terms ie STLC equality
typeEquality :: (T, T) 
  -> (Map (Either String String) Int, Map (Either String String) Int) 
  -> Int 
  -> Bool
typeEquality (TVar x, TVar y) (m1, m2) s = case M.lookup (Right x) m1 of
  Just a -> case M.lookup (Right y) m2 of
    Just b -> a == b
    _ -> False
  _ -> x == y
typeEquality (TArr a1 b1, TArr a2 b2) c s = 
  typeEquality (a1, a2) c s && typeEquality (b1, b2) c s
typeEquality (TApp a1 b1, TApp a2 b2) c s =
  typeEquality (a1, a2) c s && typeEquality (b1, b2) c s
typeEquality (Pi x1 k1 t1, Pi x2 k2 t2) (m1, m2) s = 
  k1 == k2 && typeEquality (t1, t2) (m1', m2') (s+1) 
  where 
    m1' = M.insert (Right x1) s m1
    m2' = M.insert (Right x2) s m2
typeEquality (TAbs x k1 t1, TAbs y k2 t2) (m1, m2) s =
  k1 == k2 && typeEquality (t1, t2) (m1', m2') (s+1)
  where
    m1' = M.insert (Right x) s m1
    m2' = M.insert (Right y) s m2
typeEquality (TNat, TNat) _ _ = True
typeEquality _ _ _ = False

-- show implementation, uses Unicode for Pi type
-- uses bracketing convention for types
instance Show T where
  show (TVar c)    = c
  show (TNat)      = "Nat"
  show (TArr a b)  = paren (isArr a || isPi a) (show a) ++ "->" ++ show b
  show (Pi t1 k t2)  = "\x3a0" ++ t1 ++ "::" ++ show k 
    ++ "." ++ show t2
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

isPi :: T -> Bool
isPi (Pi _ _ _) = True
isPi _        = False

-- FOmega Term
-- variables are strings
-- Abstractions carry the type Church style
-- Second order abstractions carry term variables as ints
data FOTerm
  = Var String
  | Typ T
  | Abs String T FOTerm
  | App FOTerm FOTerm
  | PiAbs String K FOTerm 
  | Zero
  | Succ
  deriving Ord

-- alpha termEqualityalence of terms uses
instance Eq FOTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- determines syntactic alpha-termEqualityalence of terms
-- has maps (either term, id) for each term/type variable
-- each TERM abstraction adds (left var) to the map and increments the id
-- each TYPE abstraction adds (right var) to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality 
-- if bound t1 XOR bound t2 then False 
-- application recursively checks both the LHS and RHS
-- Type equality is called for types
termEquality :: (FOTerm, FOTerm) 
  -> (Map (Either String String) Int, Map (Either String String) Int) 
  -> Int 
  -> Bool
termEquality (Var x, Var y) (m1, m2) s = case M.lookup (Left x) m1 of
  Just a -> case M.lookup (Left y) m2 of
    Just b -> a == b
    _ -> False
  _ -> x == y
termEquality (Abs x t1 l1, Abs y t2 l2) (m1, m2) s = 
  t1 == t2 && termEquality (l1, l2) (m1', m2') (s+1) 
  where 
    m1' = M.insert (Left x) s m1
    m2' = M.insert (Left y) s m2
termEquality (App a1 b1, App a2 b2) c s = 
  termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality (Typ t1, Typ t2) c s =  
  typeEquality (t1, t2) c s --terms can be types now, pass ctx with type mappings
termEquality (PiAbs x1 k1 t1, PiAbs x2 k2 t2) (m1,m2) s = 
  k1 == k2 && termEquality (t1, t2) (m1', m2') s
  where 
    m1' = M.insert (Right x1) s m1
    m2' = M.insert (Right x2) s m2
termEquality (Succ, Succ) _ _ = True
termEquality (Zero, Zero) _ _ = True
termEquality _ _ _ = False

-- show implementation for System F terms
-- uses bracketing convention for terms
instance Show FOTerm where
  show (Var x)      = x
  show (Zero)       = "z"
  show (Succ)       = "s"
  show (Typ t)      = "[" ++ show t ++"]"
  show (App t1 t2)  = 
    paren (isAbs t1 || isPiAbs t1 ) (show t1) ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) = "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1
  show (PiAbs t k l1) = "\x39b" ++ t ++ "::" ++ show k ++ "." ++ show l1

isSucc :: FOTerm -> Bool
isSucc Succ = True
isSucc _    = False

isAbs :: FOTerm -> Bool
isAbs (Abs _ _ _) = True
isAbs _         = False

isApp :: FOTerm -> Bool
isApp (App _ _) = True
isApp _         = False

isPiAbs :: FOTerm -> Bool
isPiAbs (PiAbs _ _ _) = True
isPiAbs _           = False

--type context of term variables and kinds
--input term or type variable as a String
--if the input is a term, return Left its type
--if the input is a type, return Right its kind
type Context = M.Map String (Either T K)

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term (not valid in System F)
-- see how the context works to make sense of case statements
typeof :: FOTerm -> Context -> Maybe T
typeof Zero ctx = Just TNat
typeof (Typ t) ctx = Nothing
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
typeof l@(App l1 l2) ctx = case typeof l1 ctx of
  Just (TArr t2 t3) -> do
    t1 <- typeof l2 ctx
    guard (t1 == t2)
    Just t3
  Just (Pi x k t) -> case l2 of
    Typ a -> Just $ typeSub t (TVar x, a) -- type substitution under 2nd-order abstraction
    _ -> Nothing
  _ -> Nothing
typeof l@(PiAbs t k l1) ctx = do 
  case typeof l1 (M.insert t (Right k) ctx) of 
    Just t' -> Just (Pi t k t')
    _ -> Nothing

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
kindof (Pi x k1 t1) ctx = do
  k2 <- kindof t1 (M.insert x (Right k1) ctx)
  guard (k2 == KVar)
  return KVar

-- top-level typing derivation, passing an empty context
typeof' l = typeof l M.empty
 
-- top level kinding function for types
kindof' t = kindof t M.empty

--bound variables of a term
bound :: FOTerm -> Set String
bound Zero         = S.empty
bound Succ         = S.empty
bound (Var n)      = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)
bound (Typ t)      = S.empty
bound (PiAbs x k t)= bound t

--free variables of a term
free :: FOTerm -> Set String
free Zero         = S.empty
free Succ         = S.empty
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)
free (Typ t)      = S.empty
free (PiAbs x k t)= free t

--test to see if a term is closed (has no free vars)
closed :: FOTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: FOTerm -> Set FOTerm
sub l@Zero         = S.singleton l
sub l@Succ         = S.singleton l
sub l@(Var x)      = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l@(Typ t)      = S.singleton l
sub l@(PiAbs x k t)= S.insert l $ sub t

--element is bound in a term
notfree :: String -> FOTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: FOTerm -> Set String
vars Zero         = S.empty
vars Succ         = S.empty
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1
vars (Typ t)      = S.empty
vars (PiAbs x k t)= vars t

--generates a fresh variable name for a term
newlabel :: FOTerm -> String
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
rename :: FOTerm -> (String, String) -> FOTerm
rename Zero c        = Zero
rename Succ c        = Succ
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) c     = App (rename l1 c) (rename l2 c)
rename l@(Typ t) c       = l
rename l@(PiAbs x k t) c = PiAbs x k $ rename t c

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: FOTerm -> (FOTerm, FOTerm) -> FOTerm
substitute Zero c = Zero
substitute Succ c = Succ
substitute l1@(Var c1) (Var c2, l2) 
  | c1 == c2 = l2 
  | otherwise = l1 
substitute l1@(Typ (TVar x)) c@(Var y, l2) 
  | x == y = l2
  | otherwise = l1
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute l@(Abs y t l1) c@(Var x, l2)
  | y == x = l
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise = Abs z t $ substitute (rename l1 (y,z)) c
  where z     = foldr1 biggest [newlabel l1, newlabel l2, newlabel (Var x)]
substitute l@(PiAbs x k t) c@(Var y, l2)
  | x /= y = PiAbs x k $ substitute t c

--set of variables in a type
typeVars :: T -> Set String
typeVars (TNat)       = S.empty
typeVars (TVar x)     = S.singleton x
typeVars (TArr t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (TAbs x k t) = S.insert x $ typeVars t
typeVars (TApp t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (Pi x k t)   = S.insert x $ typeVars t

-- type level free variables
freeTVars :: T -> Set String
freeTVars (TNat)       = S.empty
freeTVars (TVar c)     = S.singleton c
freeTVars (TArr t1 t2) = S.union (freeTVars t1) (freeTVars t2) 
freeTVars (TAbs x k t) = S.delete x $ freeTVars t
freeTVars (TApp t1 t2) = S.union (freeTVars t1) (freeTVars t2)
freeTVars (Pi x k t1)  = S.delete x (freeTVars t1)

-- type-level 'bound' function
notfreeT :: String -> T -> Bool
notfreeT x = not . S.member x . freeTVars

--type vars in a term
typeVarsInTerm :: FOTerm -> Set String
typeVarsInTerm l@(Zero)       = S.empty
typeVarsInTerm l@(Succ)       = S.empty
typeVarsInTerm l@(Var x)      = S.empty
typeVarsInTerm l@(Abs c t l1) = S.union (typeVars t) $ typeVarsInTerm l1
typeVarsInTerm l@(App l1 l2)  = S.union (typeVarsInTerm l1) (typeVarsInTerm l2)
typeVarsInTerm l@(Typ t)      = typeVars t
typeVarsInTerm l@(PiAbs x k t)= S.insert x $ typeVarsInTerm t

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
renameT l@(Pi a k t) c@(x,y) 
  = if a == x then l else Pi a k $ renameT t c


-- similar to term substitution but at the type level
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
  where z = foldr1 biggest [newTLabel t1, newTLabel t2, newTLabel (TVar x)]
typeSub l@(Pi x k t) c@(TVar y, z)
  | x == y = l
  | x `notfreeT` z = Pi x k $ typeSub t c
  | otherwise = Pi n k $ typeSub (renameT t (x, n)) c
  where n = max (newTLabel t) (newTLabel z)


-- function used to do type substitution under a second order abstraction
tSubUnder :: FOTerm -> (T,T) -> FOTerm
tSubUnder l@(Var x) c      = l
tSubUnder l@(Abs x t l2) c = Abs x (typeSub t c) $ tSubUnder l2 c
tSubUnder l@(App l1 l2) c  = App (tSubUnder l1 c) (tSubUnder l2 c)
tSubUnder l@(Typ t) c      = Typ $ typeSub t c
tSubUnder l@(PiAbs x k t) c@(TVar y, z) 
  | x /= y = PiAbs x k (tSubUnder t c)  
  | otherwise =  l


--one-step reduction relation, TODO implement other reduction strats
reduce1 :: FOTerm -> Maybe FOTerm 
reduce1 l@(Var x) = Nothing
reduce1 Zero = Nothing
reduce1 Succ = Nothing 
reduce1 l@(Abs x t s) = case reduce1 s of
  Just s' -> Just $ Abs x t s'
  _ -> case reduce1T t of
    Just t' -> Just $ Abs x t' s
    _ -> Nothing
reduce1 (App Succ n) = case reduce1 n of
  Just n' -> Just $ App Succ n'
  _ -> Nothing
reduce1 l@(App (Abs x t l') l2) = 
  Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 l@(App (PiAbs x1 k t) (Typ x2)) = 
  Just $ tSubUnder t (TVar x1, x2)  --type-level beta: (Pi X. t) A ~> t[X := A]
reduce1 l@(App l1 l2) = do
  case reduce1 l1 of 
    Just l' -> Just $ App l' l2
    _ -> case reduce1 l2 of
      Just l' -> Just $ App l1 l'
      _ -> Nothing
reduce1 l@(Typ t) = Nothing
reduce1 l@(PiAbs x k t) = do
  t' <- reduce1 t
  Just $ PiAbs x k t'

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
reduce1T (Pi x k t1) = do 
  t1' <- reduce1T t1 
  Just $ Pi x k t1'

-- multi-step reduction relation 
-- NOT GUARANTEED TO TERMINATE if it doesn't type check
reduce :: FOTerm -> FOTerm
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: FOTerm -> [FOTerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []

