{-|
Module      : STLC
Description : Deep-embedding of the Simply typed lambda calculus in Haskell.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "STLC" module denotes the tree types of the simply typed lambda calculus. This language is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module STLC where


-- Tool Imports.
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Maybe    as Maybe 
import qualified Control.Monad as C


-- |  Simple Types for Lambda Calc, of the form 'o' or 'o -> o' or a mix of both
data T 
  = TVar
  | TArr T T
  deriving (Eq, Ord) --equivalence of types compares the trees of each type


-- | Simple show instance
instance Show T where
  show TVar        = "O"
  show (TArr a b)  = paren (isArr a) (show a) ++ "->" ++ show b


-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | Helper function returns true if the term is an abstraction.
isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False


-- | Simply-Typed Lambda Calculus Terms
-- variables are String
-- Abstractions carry the type Church style
data STTerm
  = Var VarName
  | Abs VarName T STTerm
  | App STTerm STTerm
  deriving Ord


-- | Type Synonym for variable names in a term.
type VarName = String


-- | Simple show instance for STLC
instance Show STTerm where
  show (Var x)      
    = x
  show (App t1 t2)  
    = paren (isAbs t1) (show t1) 
        ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) 
    = "\x03bb" ++ x ++ ":" 
        ++ show t ++ "." ++ show l1


-- | Helper function returns true if the term is an abstraction.
isAbs :: STTerm -> Bool
isAbs Abs{} = True
isAbs _      = False


-- | Helper function returns true if the term is an application.
isApp :: STTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | alpha equivalence of terms using syntactic term equality
instance Eq STTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0


-- | Checks for equality of terms, has a map (term, id) for each variable.
-- Each abstraction adds vars to the map and increments the id.
-- Variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS.
termEquality :: (STTerm, STTerm) 
             -> (M.Map String Int, M.Map String Int) 
             -> Int 
             -> Bool
termEquality (Var x, Var y) (m1, m2) _ 
  = let testEq = do a <- M.lookup x m1
                    b <- M.lookup y m2
                    return $ a == b
    in Maybe.fromMaybe (x == y) testEq
termEquality (Abs x t1 l1, Abs y t2 l2) (m1, m2) s 
  = let newm1 = M.insert x s m1
        newm2 = M.insert y s m2
    in t1 == t2 && termEquality (l1, l2) (newm1, newm2) (s+1) 
termEquality (App a1 b1, App a2 b2) c s 
  = termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality _ _ _ = False


-- | Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map String T


-- | Typing derivation for a term in a given context
-- * Just T denotes successful type derivation 
-- * Nothing denotes failure to type the term (not in \->)
typeof :: STTerm -> Context -> Maybe T
typeof (Var v) ctx 
  = M.lookup v ctx
typeof (Abs x t l1) ctx 
  = TArr t <$> typeof l1 (M.insert x t ctx)
typeof (App l1 l2) ctx 
  = do t1 <- typeof l2 ctx
       case typeof l1 ctx of
         Just (TArr t2 t3) -> do C.guard $ t1 == t2 
                                 return t3   
         _                 -> Nothing


-- | Top level typing function providing empty context
typeof' l = typeof l M.empty


-- | Function returns a set of bound variables of a term.
bound :: STTerm -> S.Set String
bound (Var _)      = S.empty
bound (Abs n _ l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)


-- | Function returns a set of free variables of a term.
free :: STTerm -> S.Set String
free (Var n)      = S.singleton n
free (Abs n _ l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)


-- | Function tests to see if a term is closed (has no free vars).
closed :: STTerm -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: STTerm -> S.Set STTerm
sub l@(Var _)      = S.singleton l
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)


-- | Function determines if a variable is bound in a term.
notfree :: String -> STTerm -> Bool
notfree x = not . S.member x . free 


-- | Function returns the set of variables in a term.
vars :: STTerm -> S.Set String
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1


-- | Function generates a fresh variable name for a term.
newlabel :: STTerm -> String
newlabel x = head . dropWhile (`elem` vars x) 
  $ iterate genVar $  S.foldr biggest "" $ vars x


-- | Function generates fresh variable names from a given variable.
genVar :: String -> String 
genVar []       = "a"
genVar ('z':xs) = 'a':genVar xs
genVar ( x :xs) = succ x:xs


-- | Function is the length-observing maximum function 
-- that falls back on lexicographic ordering
biggest :: String -> String -> String 
biggest xs ys = if length xs > length ys 
                  then xs 
                  else max xs ys


-- | Function renames a term.
--rename t (x,y) renames free occurrences of x in t to y
rename :: STTerm -> (String, String) -> STTerm
rename (Var a) (x,y)        = if a == x 
                                then Var y 
                                else Var a
rename l@(Abs a t l1) (x,y) = if a == x 
                                then l 
                                else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y)    = App (rename l1 (x,y)) (rename l2 (x,y))


-- | Function substitutes one term for another in a term
-- does capture avoiding substitution (Berendregt)
substitute :: STTerm -> (STTerm, STTerm) -> STTerm
substitute l1@(Var c1) (Var c2, l2) 
  = if c1 == c2 
      then l2 
      else l1 
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute (Abs y t l1) c@(Var x, l2)
  | y == x         = Abs y t l1
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise      = Abs z t $ substitute (rename l1 (y,z)) c
  where z          = foldr1 biggest [newlabel l1, newlabel l2, newlabel (Var x)]
substitute s _     = s

-- | One-step reduction relation on terms.
reduce1 :: STTerm -> Maybe STTerm 
reduce1 (Var _) 
  = Nothing
reduce1 (Abs x t s) 
  = Abs x t <$> reduce1 s
reduce1 (App (Abs x _ l1) l2) 
  = Just $ substitute l1 (Var x, l2)  --beta conversion
reduce1 _ = Nothing


-- | Multi-step reduction relation.
-- NOT GUARANTEED TO TERMINATE
reduce :: STTerm -> STTerm
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reductions :: STTerm -> [STTerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []

--common combinators

-- | Identity combinator
i :: STTerm
i = Abs "a" (TArr TVar TVar) (Var "a")

-- | True
true :: STTerm
true = Abs "a" TVar (Abs "b" TVar (Var "a"))

-- | False
false :: STTerm
false = Abs "a" TVar (Abs "b" TVar (Var "b"))

-- | Zero, Church Style
zero :: STTerm
zero = false

-- | Canonical self-applying \x.x x 
--used as the basis for recursion
xx :: STTerm
xx = Abs "x" (TArr TVar TVar) (App (Var "x") (Var "x")) --won't type check as expected

-- | Canonical infinitely reducing term.
omega :: STTerm
omega = App xx xx --won't type check, see above

-- | If statement
_if :: STTerm -> STTerm -> STTerm -> STTerm
_if c t = App (App c t)

-- | Test of is_zero encoded
_isZero :: STTerm -> STTerm
_isZero n = _if n false true

-- | Function from Haskell Int to Church Numeral
toChurch :: Int -> STTerm
toChurch n = Abs "f" (TArr TVar TVar)(Abs "x" TVar (toChurch' n))
  where
    toChurch' 0 = Var "x"
    toChurch' n = App (Var "f") (toChurch' (n-1))


