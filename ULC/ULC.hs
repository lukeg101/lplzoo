module ULC where

import Data.Set as S
import Data.Map.Lazy as M

--untyped lambda calculus - variables are numbers now as it's easier for renaming
data Term
  = Var String
  | Abs String Term
  | App Term Term
  deriving Ord

-- alpha equivalence of lambda terms as Eq instance for Lambda Terms
instance Eq Term where
  a == b = termEquality (a, b) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds to the map and increments the id
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality
-- if bound t1 XOR bound t2 == true then False
-- application recursively checks both the LHS and RHS
termEquality :: (Term, Term) -> (Map String Int, Map String Int) -> Int -> Bool
termEquality (Var x, Var y) (m1, m2) s = case M.lookup x m1 of
  Just a -> case M.lookup y m2 of
    Just b -> a == b
    _ -> False
  _ -> x == y
termEquality (Abs x t1, Abs y t2) (m1, m2) s =
  termEquality (t1, t2) (m1', m2') (s+1)
  where
    m1' = M.insert x s m1
    m2' = M.insert y s m2
termEquality (App a1 b1, App a2 b2) c s =
  termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality _ _ _ = False

  --Show instance
instance Show Term where
  show (Var x)      = x
  show (App t1 t2)  = paren (isAbs t1) (show t1) ++ " " ++ paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t1)   = "\x03bb" ++ x ++ "." ++ show t1

paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x

isAbs :: Term -> Bool
isAbs (Abs _ _) = True
isAbs _         = False

isApp :: Term -> Bool
isApp (App _ _) = True
isApp _         = False

--bound variables of a term
bound :: Term -> Set String
bound (Var n)      = S.empty
bound (Abs n t)    = S.insert n $ bound t
bound (App t1 t2)  = S.union (bound t1) (bound t2)

--free variables of a term
free :: Term -> Set String
free (Var n)      = S.singleton n
free (Abs n t)    = S.delete n (free t)
free (App t1 t2)  = S.union (free t1) (free t2)

--test to see if a term is closed (has no free vars)
closed :: Term -> Bool
closed = S.null . free

--subterms of a term
sub :: Term -> Set Term
sub t@(Var x)      = S.singleton t
sub t@(Abs c t')   = S.insert t $ sub t'
sub t@(App t1 t2)  = S.insert t $ S.union (sub t1) (sub t2)

--element is bound in a term
notfree :: String -> Term -> Bool
notfree x = not . S.member x . free

--set of variables in a term
vars :: Term -> Set String
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t1)   = S.insert x $ vars t1

--generates a fresh variable name for a term
newlabel :: Term -> String
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
rename :: Term -> (String, String) -> Term
rename (Var a) (x,y) = if a == x then Var y else Var a
rename t@(Abs a t') (x,y) = if a == x then t else Abs a $ rename t' (x, y)
rename (App t1 t2) (x,y) = App (rename t1 (x,y)) (rename t2 (x,y))

--substitute one term for another in a term
--does capture avoiding substitution (Berenregt)
substitute :: Term -> (Term, Term) -> Term
substitute t1@(Var c1) (Var c2, t2)
  = if c1 == c2 then t2 else t1
substitute (App t1 t2) c
  = App (substitute t1 c) (substitute t2 c)
substitute (Abs y s) c@(Var x, t)
  | y == x = Abs y s
  | y `notfree` t = Abs y $ substitute s c
  | otherwise = Abs z $ substitute (rename s (y,z)) c
  where z = max (newlabel s) (newlabel t)

--eta reduction
eta :: Term -> Maybe Term
eta (Abs x (App t (Var y)))
  | x == y && x `notfree` t = Just t
  | otherwise = Nothing

--term is normal form if has no subterms of the form (\x.s) t
isNormalForm :: Term -> Bool
isNormalForm = not . any testnf . sub
  where
    testnf t = case t of
      (App (Abs _ _) _) -> True
      _ -> False

--one-step reduction relation
reduce1 :: Term -> Maybe Term
reduce1 t@(Var x) = Nothing
reduce1 t@(Abs x s) = case reduce1 s of
  Just s' -> Just $ Abs x s'
  Nothing -> Nothing
reduce1 t@(App (Abs x t1) t2)
  = Just $ substitute t1 (Var x, t2)  --beta conversion
reduce1 t@(App t1 t2) = case reduce1 t1 of
  Just t' -> Just $ App t' t2
  _ -> case reduce1 t2 of
    Just t' -> Just $ App t1 t'
    _ -> Nothing

--multi-step reduction relation - NOT GUARANTEED TO TERMINATE
reduce :: Term -> Term
reduce t = case reduce1 t of
    Just t' -> reduce t'
    Nothing -> t

---multi-step reduction relation that accumulates all reduction steps
reductions :: Term -> [Term]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []

--common combinators
i = Abs "a" (Var "a")
true = Abs "a" (Abs "b" (Var "a"))
false = Abs "a" (Abs "b" (Var "b"))
zero = false
xx = Abs "x" (App (Var "x") (Var "x"))
omega = App xx xx
_if = \c t f -> App (App c t) f
_isZero = \n -> _if n false true
_succ = Abs "x" $ Abs "y" 
  $ Abs "z" $ App (Var "y") $ App (App (Var "x") (Var "y")) (Var "z")
appsucc = App _succ

-- function from Haskell Int to Church Numeral
toChurch :: Int -> Term
toChurch n = Abs "x" (Abs "f" (toChurch' n))
  where
    toChurch' 0 = Var "x"
    toChurch' n = App (Var "x") (toChurch' (n-1))

test1 = App i (Abs "x" (App (Var "x") (Var "x")))
test2 = App (App (Abs "x" (Abs "y" (Var "y"))) (Var "y")) (Var "z")
test3 = App (App (toChurch 3) (Abs "x" (App (Var "x") (toChurch 2)))) (toChurch 1)
