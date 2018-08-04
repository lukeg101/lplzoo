module SKI where

import Data.Set as S
import Data.Map.Lazy as M

--untyped lambda calculus - variables are numbers now as it's easier for renaming
data SKTerm
  = Var String
  | S
  | K
  | I
  | App SKTerm SKTerm
  deriving (Eq,Ord)

{-
-- alpha equivalence of lambda SKTerms as Eq instance for Lambda SKTerms
instance Eq SKTerm where
  a == b = SKTermEquality (a, b) (M.empty, M.empty) 0

-- checks for equality of SKTerms, has a map (SKTerm, id) for each variable
-- each abstraction adds to the map and increments the id
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality
-- if bound t1 XOR bound t2 == true then False
-- application recursively checks both the LHS and RHS
SKTermEquality :: (SKTerm, SKTerm) -> (Map Int Int, Map Int Int) -> Int -> Bool
SKTermEquality (Var x, Var y) (m1, m2) s = case M.lookup x m1 of
  Just a -> case M.lookup y m2 of
    Just b -> a == b
    _ -> False
  _ -> x == y
SKTermEquality (Abs x t1, Abs y t2) (m1, m2) s =
  SKTermEquality (t1, t2) (m1', m2') (s+1)
  where
    m1' = M.insert x s m1
    m2' = M.insert y s m2
SKTermEquality (App a1 b1, App a2 b2) c s =
  SKTermEquality (a1, a2) c s && SKTermEquality (b1, b2) c s
SKTermEquality _ _ _ = False
-}

  --Show instance
instance Show SKTerm where
  show (Var x)      = x
  show (App t1 t2)  = show t1 ++ " " ++ paren (isApp t2) (show t2)
  show S = "S"
  show K = "K"
  show I = "I"

paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x

isApp :: SKTerm -> Bool
isApp (App _ _) = True
isApp _         = False

--bound variables of an SKTerm
bound :: SKTerm -> Set String
bound (Var n)      = S.empty
bound S            = S.empty
bound K            = S.empty
bound I            = S.empty
bound (App t1 t2)  = S.union (bound t1) (bound t2)

--free variables of an SKTerm
free :: SKTerm -> Set String
free (Var n)      = S.singleton n
free S            = S.empty
free K            = S.empty
free I            = S.empty
free (App t1 t2)  = S.union (free t1) (free t2)

--test to see if an SKTerm is closed (has no free vars)
closed :: SKTerm -> Bool
closed = S.null . free

--subSKTerms of an SKTerm
sub :: SKTerm -> Set SKTerm
sub t@(Var x)      = S.singleton t
sub S              = S.singleton S
sub K              = S.singleton K
sub I              = S.singleton I
sub t@(App t1 t2)  = S.insert t $ S.union (sub t1) (sub t2)

--element is bound in an SKTerm
notfree :: String -> SKTerm -> Bool
notfree x = not . S.member x . free

--set of variables in an SKTerm
vars :: SKTerm -> Set String
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars S            = S.empty
vars K            = S.empty
vars I            = S.empty

--generates a fresh variable name for a term
newlabel :: SKTerm -> String
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

--substitute one term for another in a term
--does capture avoiding substitution (Berenregt)
substitute :: SKTerm -> (SKTerm, SKTerm) -> SKTerm
substitute t1@(Var c1) (Var c2, t2)
  = if c1 == c2 then t2 else t1
substitute (App t1 t2) c
  = App (substitute t1 c) (substitute t2 c)
substitute t1 c = t1

--one-step reduction relation
reduce1 :: SKTerm -> Maybe SKTerm
reduce1 t@(Var x) = Nothing
reduce1 t@(App I x) = Just x --K x ~> x
reduce1 t@(App (App K x) y) = Just x --K x ~> x
reduce1 t@(App K x) = case reduce1 x of
  Just x' -> Just $ App K x'
  _ -> Nothing
reduce1 t@(App (App (App S x) y) z) = --S x y z ~> x z (y z)
  Just $ App (App x z) (App y z)
reduce1 t@(App S x) = case reduce1 x of
  Just x' -> Just $ App S x'
  _ -> Nothing
reduce1 t@(App t1 t2) = case reduce1 t1 of
  Just t' -> Just $ App t' t2
  _ -> case reduce1 t2 of
    Just t' -> Just $ App t1 t'
    _ -> Nothing
reduce1 _ = Nothing

--multi-step reduction relation - NOT GUARANTEED TO SKTermINATE (haha)
reduce :: SKTerm -> SKTerm
reduce t = case reduce1 t of
  Just t' -> reduce t'
  Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: SKTerm -> [SKTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []

--common combinators
i x = App (App (App S K) K) x
b = App (App S $ App K S) $ K
c = App (App S (App (App S $ App K b) S)) (App K K)
s4  = App (App (App S S) S) S
sii a = App (App (App S I) I) a
t a = App K a
f a = App (App S K) a
ci x y = App (App (App c I) x) y







