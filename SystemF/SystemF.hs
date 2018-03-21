 module SystemF where

import Data.Map.Lazy as M
import Data.Set as S
import Control.Monad (guard)
import Debug.Trace

data T 
  = TVar Int
  | TArr T T
  | Pi Int T
  deriving (Eq, Ord)

instance Show T where
  show (TVar c)    = show c
  show (TArr a b)  = '(':show a ++ "->" ++ show b ++")"
  show (Pi t1 t2)  = "\x3a0" ++ show t1 ++ "." ++ show t2

data SFTerm
  = Var Int
  | Typ T
  | Abs Int T SFTerm
  | App SFTerm SFTerm
  | PiAbs Int SFTerm 
  deriving (Eq, Ord)

instance Show SFTerm where
  show (Var x)      = show x
  show (Typ t)      = show t
  show (App t1 t2)  = '(':show t1 ++ ' ':show t2 ++ ")" 
  show (Abs x t l1) = '(':"\x03bb" ++ show x ++ ":" ++ show t ++ "." ++ show l1 ++ ")"
  show (PiAbs t l1) = "\x39b" ++ show t ++ "." ++ show l1

type Context = M.Map Int (Maybe T)

typeof :: SFTerm -> Context -> Maybe T
typeof (Var v) ctx = case M.lookup v ctx of 
  Just ml -> ml
  _ -> Nothing
typeof (Typ t) ctx = Nothing
typeof l@(Abs x t l1) ctx = do 
  t' <- typeof l1 (M.insert x (Just t) ctx)
  guard (all (\v -> M.lookup v ctx == Just Nothing) (freeTVars t))
  Just $ TArr t t'
typeof l@(App l1 l2) ctx = case typeof l1 ctx of
  Just (TArr t2 t3) -> do
    t1 <- typeof l2 ctx
    guard (t1 == t2)
    Just t3
  Just (Pi x t) -> case l2 of
    Typ a -> Just $ typeSub t (TVar x, a)
    _ -> Nothing
  _ -> Nothing
typeof l@(PiAbs t l1) ctx = do 
  case typeof l1 (M.insert t Nothing ctx) of 
    Just t' -> Just (Pi t t')
    _ -> Nothing

typeof' l = typeof l M.empty
 
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) (TVar y,z) 
  | x == y = z
  | otherwise = l
typeSub l@(TArr t1 t2) c = TArr (typeSub t1 c) (typeSub t2 c)
typeSub l@(Pi x t) c@(TVar y, z)
  | x == y = l
  | x `notfreeT` z = Pi x $ typeSub t c
  | otherwise = Pi n $ typeSub (renameT t (x, n)) c
  where n = max (newTLabel t) (newTLabel z)

notfreeT :: Int -> T -> Bool
notfreeT x = not . S.member x . freeTVars

freeTVars :: T -> Set Int
freeTVars (TVar c) = S.singleton c
freeTVars (TArr t1 t2) = S.union (freeTVars t1) (freeTVars t2) 
freeTVars (Pi x t1) = S.delete x (freeTVars t1)

--set of variables in a type
typeVars :: T -> Set Int
typeVars (TVar x)     = S.singleton x
typeVars (TArr t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (Pi x t)     = S.insert x $ typeVars t

--generates a fresh variable name for a type
newTLabel :: T -> Int
newTLabel = (*10) . maximum . typeVars

--rename t (x,y): renames free occurences of type-variables x in t to y
renameT :: T -> (Int, Int) -> T
renameT (TVar a) (x,y) = TVar $ if a == x then y else a
renameT l@(Pi a t) c@(x,y) = if a == x then l else Pi a $ renameT t c
renameT (TArr t1 t2) c = TArr (renameT t1 c) (renameT t2 c)

--bound variables of a term
bound :: SFTerm -> Set Int
bound (Var n)      = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)
bound (Typ t)      = S.empty
bound (PiAbs x t)  = bound t

--free variables of a term
free :: SFTerm -> Set Int
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)
free (Typ t)      = S.empty
free (PiAbs x t)  = free t

--test to see if a term is closed (has no free vars)
closed :: SFTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: SFTerm -> Set SFTerm
sub l@(Var x)      = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l@(Typ t)      = S.singleton l
sub l@(PiAbs x t)  = S.insert l $ sub t

--element is bound in a term
notfree :: Int -> SFTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: SFTerm -> Set Int
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1
vars (Typ t)      = S.empty
vars (PiAbs x t)  = vars t

--generates a fresh variable name for a term
newlabel :: SFTerm -> Int
newlabel = (+1) . maximum . vars

--rename t (x,y): renames free occurences of term variable x in t to y
rename :: SFTerm -> (Int, Int) -> SFTerm
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))
rename l@(Typ t) c = l
rename l@(PiAbs x t) c = PiAbs x $ rename t c

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: SFTerm -> (SFTerm, SFTerm) -> SFTerm
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
reduce1 :: SFTerm -> Maybe SFTerm 
reduce1 l@(Var x) = Nothing
reduce1 l@(Abs x t s) = do
  s' <- reduce1 s
  Just $ Abs x t s'
reduce1 l@(App (Abs x t l') l2) = 
  Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 l@(App (PiAbs x1 t) (Typ x2)) = 
  Just $ tSubUnder t (TVar x1, x2)
reduce1 l@(App l1 l2) = do
  l' <- reduce1 l1
  Just $ App l' l2
reduce1 l@(Typ t) = Nothing
reduce1 l@(PiAbs x t) = do
  t' <- reduce1 t
  Just $ PiAbs x t'


tSubUnder :: SFTerm -> (T,T) -> SFTerm
tSubUnder l@(Var x) c      = l
tSubUnder l@(Typ t) c      = Typ $ typeSub t c
tSubUnder l@(Abs x t l2) c = Abs x (typeSub t c) $ tSubUnder l2 c
tSubUnder l@(App l1 l2) c  = App (tSubUnder l1 c) (tSubUnder l2 c)
tSubUnder l@(PiAbs x t) c@(TVar y, z) 
  | x /= y = PiAbs x (tSubUnder t c)  
  | otherwise =  l

--multi-step reduction relation - NOT GUARANTEED TO TERMINATE
reduce :: SFTerm -> SFTerm
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: SFTerm -> [SFTerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []

--common combinators
ident = PiAbs 0 (Abs 1 (TVar 0) (Var 1))
natType = Pi 2 (TArr (TArr (TVar 2) (TVar 2)) (TArr (TVar 2) (TVar 2)))
zero = PiAbs 2 (Abs 4 (TArr (TVar 2) (TVar 2)) (Abs 5 (TVar 2) (Var 5)))


{-
true t f = Abs 1 t (Abs 2 f (Var 1))
false t f= Abs 1 t (Abs 2 f (Var 2))
xx = Abs 1 (TArr TNat TNat) (App (Var 1) (Var 1)) --won't type check as expected
omega = App xx xx --won't type check, see above
_if = \c t f -> App (App c t) f
isZero Zero = true
plus = Abs 1 TNat $ Abs 2 TNat $ RecNat (Abs 3 TNat $ Abs 4 TNat (Succ (Var 4))) (Var 1) (Var 2)
plusApp n m = App (App plus n) m
-}

--i = PiAbs 1 $ Abs 2 (TVar 1) (Var 2)
--zero = PiAbs 1 $ Abs 2 (TArr (TVar 1) (TVar 1)) $ Abs 3 (TVar 1) (Var 3)
{-
toChurch :: Int -> SFTerm
toChurch 0 = zero
toChurch n = Abs 1  (toChurch (n-1))

toInt :: SFTerm -> Int
toInt Zero = zero
toInt (Succ n) = 1 + toInt n
toInt _ = error "Not Nat"

test1 = Abs 1 (TArr TNat TNat) $ Abs 2 TNat $ App (Var 1) (Var 2) -- \f x. f x
test2 = Abs 1 (TArr TNat TNat) $ Abs 2 TNat $ App (App (Var 1) (Var 2)) (Var 1) -- \f x. (f x) f
test3 = App (App (Abs 1 TNat (Abs 2 TNat (Var 2))) (Var 2)) (Var 4)
test4 = plusApp (toChurch 3) (toChurch 2)
--toInt $ reduce (plusApp (toChurch 3) (toChurch 2)) --> 5

-}



