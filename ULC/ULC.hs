{-|
Module      : ULC
Description : Deep-embedding of the untyped lambda calculus in Haskell.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com
Stability   : stable
Portability : POSIX

The "ULC" module denotes the tree types of the untyped lambda calculus. This languages is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module ULC where


-- Tool imports.
import qualified Data.Set      as S
import qualified Data.Map.Lazy as M
import qualified Data.Maybe    as Maybe


-- | Untyped lambda calculus ADT.
-- Variables are strings of lower case characters.
-- Abstractions take a variable name "x" and a subterm.
data Term
  = Var VarName
  | Abs VarName Term
  | App Term Term
  deriving Ord

-- | Type Synonym for variable names in a term.
type VarName = String


-- | Alpha equivalence of lambda terms is the Eq instance Terms.
-- Concretely we use the "termEquality" function to represent this
instance Eq Term where
  a == b = termEquality (a, b) (M.empty, M.empty) 0


-- | Checks for equality of terms using a map (term, id) for each variable.
-- Each abstraction inserts into the map and increments the id.
-- Variable occurrence checks for occurrences in t1 and t2 using the logic:
--  * if both variables bound, check that s is same in both maps
--  * if neither is bound, check literal equality
--  * if bound t1 XOR bound t2 == True then False
-- application recursively checks both the LHS and RHS of applications.
termEquality :: (Term, Term)
             -> (M.Map String Int, M.Map String Int)
             -> Int
             -> Bool
termEquality (Var x, Var y) (m1, m2) _
  = let testEq = do a <- M.lookup x m1
                    b <- M.lookup y m2
                    return $ a == b
    in Maybe.fromMaybe (x == y) testEq
termEquality (Abs x t1, Abs y t2) (m1, m2) s
  = let newm1 = M.insert x s m1
        newm2 = M.insert y s m2
    in termEquality (t1, t2) (newm1, newm2) (s+1)
termEquality (App a1 b1, App a2 b2) c s
  = termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality _ _ _ = False


-- | Show instance for ULC terms.
instance Show Term where
  show (Var x)
    = x
  show (App t1 t2)
    = paren (isAbs t1) (show t1)
        ++ " "
        ++ paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t1)
    = "\x03bb"
        ++ x
        ++ "."
        ++ show t1


-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | Helper function returns true if the term is an abstraction.
isAbs :: Term -> Bool
isAbs (Abs _ _) = True
isAbs _         = False


-- | Helper function returns true if the term is an application.
isApp :: Term -> Bool
isApp (App _ _) = True
isApp _         = False


-- | Function returns a set of bound variables of a term.
bound :: Term -> S.Set VarName
bound (Var _)      = S.empty
bound (Abs n t)    = S.insert n $ bound t
bound (App t1 t2)  = S.union (bound t1) (bound t2)


-- | Function returns a set of free variables of a term.
free :: Term -> S.Set VarName
free (Var n)      = S.singleton n
free (Abs n t)    = S.delete n (free t)
free (App t1 t2)  = S.union (free t1) (free t2)


-- | Function tests to see if a term is closed (has no free vars).
closed :: Term -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: Term -> S.Set Term
sub t@(Var _)      = S.singleton t
sub t@(Abs _ t')   = S.insert t $ sub t'
sub t@(App t1 t2)  = S.insert t $ S.union (sub t1) (sub t2)


-- | Function determines if a variable is bound in a term.
notfree :: VarName -> Term -> Bool
notfree x = not . S.member x . free


-- | Function returns the set of variables in a term.
vars :: Term -> S.Set VarName
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x t1)   = S.insert x $ vars t1


-- | Function generates a fresh variable name for a term.
newlabel :: Term -> VarName
newlabel x = head . dropWhile (`elem` vars x)
  $ iterate genVar $ S.foldr biggest "" $ vars x


-- | Function generates fresh variable names from a given variable.
genVar :: VarName -> VarName
genVar []       = "a"
genVar ('z':xs) = 'a':genVar xs
genVar ( x :xs) = succ x:xs


-- | Function is the length-observing maximum function
-- that falls back on lexicographic ordering
biggest :: VarName -> VarName -> VarName
biggest xs ys = if length xs > length ys
                  then xs
                  else max xs ys


-- | Function renames a term.
--rename t (x,y) renames free occurrences of x in t to y
rename :: Term -> (VarName, VarName) -> Term
rename (Var a)  (x,y)     = if a == x
                              then Var y
                              else Var a
rename t@(Abs a t') (x,y) = if a == x
                              then t
                              else Abs a $ rename t' (x, y)
rename (App t1 t2) (x,y)  = App (rename t1 (x,y)) (rename t2 (x,y))


-- | Function substitutes one term for another in a term
-- does capture avoiding substitution (Berendregt)
substitute :: Term -> (Term, Term) -> Term
substitute t1@(Var c1) (Var c2, t2)
  = if c1 == c2
      then t2
      else t1
substitute (App t1 t2) c
  = App (substitute t1 c) (substitute t2 c)
substitute (Abs y s) c@(Var x, t)
  | y == x        = Abs y s
  | y `notfree` t = Abs y $ substitute s c
  | otherwise     = Abs z $ substitute (rename s (y,z)) c
  where z         = foldr1 biggest [newlabel s, newlabel t, newlabel (Var x)]
substitute s _    = s


-- | Eta reduction on a term. May fail if the term is not of the form:
-- (\x.M) y
eta :: Term -> Maybe Term
eta (Abs x (App t (Var y)))
  | x == y && x `notfree` t = Just t
  | otherwise               = Nothing
eta _                       = Nothing


-- | Function determines whether a term is a normal form.
-- A term is a normal form if has no subterms of the form (\x.s) t
isNormalForm :: Term -> Bool
isNormalForm
  = let testnf t = case t of
          (App (Abs _ _) _) -> True
          _                 -> False
    in not . any testnf . sub


-- | One-step reduction relation on terms.
reduce1 :: Term -> Maybe Term
reduce1 (Var _)
  = Nothing
reduce1 (Abs x s)
  = Abs x <$> reduce1 s
reduce1 (App (Abs x t1) t2)
  = return $ substitute t1 (Var x, t2)  --beta conversion
reduce1 (App t1 t2)
  = let reduceLeft  = do t' <- reduce1 t1
                         return $ App t' t2
        reduceRight = App t1 <$> reduce1 t2
    in if Maybe.isJust reduceLeft
         then reduceLeft
         else reduceRight

-- | Multi-step reduction relation.
-- NOT GUARANTEED TO TERMINATE
reduce :: Term -> Term
reduce t = case reduce1 t of
  Just t' -> reduce t'
  Nothing -> t


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reductions :: Term -> [Term]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []


--common combinators

-- | Identity Combinator
i :: Term
i = Abs "a" (Var "a")

-- | True
true :: Term
true = Abs "a" (Abs "b" (Var "a"))

-- | False
false :: Term
false = Abs "a" (Abs "b" (Var "b"))

-- | Zero, Church Style
zero = false

-- | Canonical self-applying \x.x x
--used as the basis for recursion
xx = Abs "x" (App (Var "x") (Var "x"))

-- | Canonical infinitely reducing term.
omega = App xx xx

-- | If statement
_if c t = App (App c t)

-- | Test of is_zero encoded
_isZero n = _if n false true

-- | The sucessor of numbers church style
_succ = Abs "x" $ Abs "y"
  $ Abs "z" $ App (Var "y")
  $ App (App (Var "x") (Var "y")) (Var "z")

-- | Applies succ to some term
appsucc = App _succ


-- | Function from Haskell Int to Church Numeral
toChurch :: Int -> Term
toChurch n = Abs "x" (Abs "f" (toChurch' n))
  where
    toChurch' 0 = Var "x"
    toChurch' n = App (Var "x") (toChurch' (n-1))
