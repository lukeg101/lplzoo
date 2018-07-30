module PCF where

import Data.Map as M
import Data.Set as S

import Control.Monad (guard)

-- Simple Types for PCF, of the form 'Nat' or 'o -> o' or a mix of both
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

-- PCF Terms
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style
-- Zero is a value in the language
-- Succ is applied to a term
data PCFTerm
  = Var String
  | Abs String T PCFTerm
  | App PCFTerm PCFTerm
  | Zero
  | Succ --Succ n is implemented as App Succ n 
  | Pred --same as succ, app reduction rules handle this
  | Y
  | If
  deriving Ord

-- alpha equivalence of terms, same as STLC
instance Eq PCFTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- syntactic equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality 
-- if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS
termEquality :: (PCFTerm, PCFTerm) 
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
termEquality (Pred, Pred) c s = True
termEquality (Zero, Zero) c s = True
termEquality (If, If) c s     = True
termEquality (Y, Y) c s       = True
termEquality _ _ _            = False

-- use application to combine E.g App Y f and let the lambda machinery do the work

-- show instance for PCFTerms, following bracketing convention
instance Show PCFTerm where
  show Zero    = "z"
  show Succ    = "s"
  show Pred    = "p"
  show Y       = "Y"
  show If      = "if"
  show (Var x) = x
  show (App t1 t2)  = 
    paren (isAbs t1) (show t1) ++ ' ' 
      : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) = 
    "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1

isAbs :: PCFTerm -> Bool
isAbs (Abs _ _ _) = True
isAbs _         = False

isApp :: PCFTerm -> Bool
isApp (App _ _) = True
isApp _         = False

-- type context is a mapping from variable name to type T
type Context = M.Map String T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term (not in PCF)
typeof :: PCFTerm -> Context -> Maybe T
typeof Zero ctx = Just TNat
typeof (Var v) ctx = M.lookup v ctx
typeof l@(Abs x t l1) ctx = do 
  t' <- typeof l1 (M.insert x t ctx)
  return $ TArr t t'
typeof l@(App Succ l2) ctx = do
  guard (typeof l2 ctx == Just TNat)
  Just TNat
typeof l@(App Pred l2) ctx = do
  guard (typeof l2 ctx == Just TNat)
  Just TNat
typeof l@(App Y l2) ctx = case typeof l2 ctx of
  Just (TArr t1 t2) -> do
    guard (t1 == t2)
    Just t1
  _ -> Nothing
typeof l@(App (App (App If l2) l3) l4) ctx =
  case typeof l2 ctx of
    Just TNat -> do
      t3 <- typeof l3 ctx
      t4 <- typeof l4 ctx
      guard (t3 == t4)
      Just t3
    _ -> Nothing
typeof l@(App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> do
      guard (t1 == t2)
      Just t3
    _ -> Nothing
typeof _ _ = Nothing

-- top level typing derivation, passing empty context to typeof
typeof' l = typeof l M.empty

--bound variables of a term
bound :: PCFTerm -> Set String
bound Zero         = S.empty
bound Succ         = S.empty
bound Pred         = S.empty
bound Y            = S.empty
bound If           = S.empty
bound (Var n)      = S.empty
bound (Abs n t l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)

--free variables of a term
free :: PCFTerm -> Set String
free Zero         = S.empty
free Succ         = S.empty
free Pred         = S.empty
free Y            = S.empty
free If           = S.empty
free (Var n)      = S.singleton n
free (Abs n t l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)

--test to see if a term is closed (has no free vars)
closed :: PCFTerm -> Bool
closed = S.null . free

--subterms of a term
sub :: PCFTerm -> Set PCFTerm
sub l@Zero         = S.singleton l
sub l@Succ         = S.singleton l
sub l@Pred         = S.singleton l
sub l@Y            = S.singleton l
sub l@If           = S.singleton l
sub l@(Var x)      = S.singleton l
sub l@(Abs c t l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)

--element is bound in a term
notfree :: String -> PCFTerm -> Bool
notfree x = not . S.member x . free 

--set of variables in a term
vars :: PCFTerm -> Set String
vars Zero         = S.empty
vars Succ         = S.empty
vars Pred         = S.empty
vars Y            = S.empty
vars If           = S.empty
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t l1) = S.insert x $ vars l1

--generates a fresh variable name for a term
newlabel :: PCFTerm -> String
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
rename :: PCFTerm -> (String, String) -> PCFTerm
rename Zero c = Zero
rename Succ c = Succ
rename Pred c = Pred
rename Y c    = Y
rename If c   = If
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) = App (rename l1 (x,y)) (rename l2 (x,y))

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: PCFTerm -> (PCFTerm, PCFTerm) -> PCFTerm
substitute Zero c = Zero
substitute Succ c = Succ
substitute Pred c = Pred
substitute Y c    = Y
substitute If c   = If
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
reduce1 :: PCFTerm -> Maybe PCFTerm 
reduce1 Zero = Nothing
reduce1 Succ = Nothing
reduce1 Pred = Nothing
reduce1 Y    = Nothing 
reduce1 If   = Nothing
reduce1 l@(Var x) = Nothing
reduce1 l@(Abs x t s) = do
  s' <- reduce1 s
  Just $ Abs x t s'
reduce1 l@(App (Abs x t l') l2) = 
  Just $ substitute l' (Var x, l2) --beta conversion
reduce1 (App Pred (App Succ l1)) = Just l1
reduce1 (App Pred Zero) = Just Zero
reduce1 (App Pred n) = case reduce1 n of
  Just n' -> Just $ App Pred n'
  _ -> Nothing
reduce1 (App Succ n) = case reduce1 n of
  Just n' -> Just $ App Succ n'
  _ -> Nothing
reduce1 l@(App (App (App If Zero) l3) l4) = do
  Just l3
reduce1 l@(App (App (App If (App Succ l2)) l3) l4) = do
  Just l4
reduce1 l@(App (App (App If l2) l3) l4) = do
  l2' <- reduce1 l2
  Just $ App (App (App If l2') l3) l4
reduce1 l@(App Y (Abs x t l')) = 
  Just $ substitute l' (Var x, l)  -- Y f ~> f (Y f)
reduce1 l@(App Y l2) = do
  l2' <- reduce1 l2
  Just $ App Y l2'
reduce1 l@(App l1 l2) = do
  l' <- reduce1 l1
  Just $ App l' l2

--multi-step reduction relation - NOT GUARANTEED TO TERMINATE
reduce :: PCFTerm -> PCFTerm
reduce t = case reduce1 t of 
  Just t' -> reduce t'
  Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: PCFTerm -> [PCFTerm]
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
incr = App Succ

-- function converting Haskell Int to Peano nat in PCF
toPeano :: Int -> PCFTerm
toPeano 0 = Zero
toPeano n = incr (toPeano (n-1))

-- function converting Peano nat to Haskell Int
toInt :: PCFTerm -> Int
toInt Zero = 0
toInt (App Succ n) = 1 + toInt n
toInt _ = error "Not Nat"

--test cases
test1 = Abs "f" (TArr TNat TNat) $ Abs "x" TNat $ App (Var "f") (Var "x") -- \f x. f x
test2 = Abs "f" (TArr TNat TNat) $ Abs "x" TNat $ App (App (Var "f") (Var "x")) (Var "f") -- \f x. (f x) f
test3 = App (App (Abs "x" TNat (Abs "y" TNat (Var "y"))) (Var "y")) (Var "z")

