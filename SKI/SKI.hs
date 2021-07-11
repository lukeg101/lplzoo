{-|
Module      : SKI
Description : Deep-embedding of the SKI combinator calculus in Haskell.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "SKI" module denotes the abstract syntax and evaluation of the SKI combinator calculus. This language is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module SKI where


-- Tool Imports
import qualified Data.Set      as S


-- | SKI combinator calculus
-- the pure calculus is symbols S,K,I and application. Variables are added to simplify use
data SKTerm
  = Var String
  | S
  | K
  | I
  | App SKTerm SKTerm
  deriving (Eq,Ord)

-- | Type Synonym for variable names in a term.
type VarName = String


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

-- | Show instance for SKI terms
instance Show SKTerm where
  show (Var x)      
    = x
  show (App t1 t2)  
    = show t1 ++ " " ++ paren (isApp t2) (show t2)
  show S 
    = "S"
  show K 
    = "K"
  show I 
    = "I"


-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | Helper function returns true if the term is an application.
isApp :: SKTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | Function returns a set of bound variables of a term.
bound :: SKTerm -> S.Set VarName
bound (Var _)      = S.empty
bound S            = S.empty
bound K            = S.empty
bound I            = S.empty
bound (App t1 t2)  = S.union (bound t1) (bound t2)


-- | Function returns a set of free variables of a term.
free :: SKTerm -> S.Set VarName
free (Var n)      = S.singleton n
free S            = S.empty
free K            = S.empty
free I            = S.empty
free (App t1 t2)  = S.union (free t1) (free t2)


-- | Function tests to see if a term is closed (has no free vars).
closed :: SKTerm -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: SKTerm -> S.Set SKTerm
sub t@(Var _)      = S.singleton t
sub S              = S.singleton S
sub K              = S.singleton K
sub I              = S.singleton I
sub t@(App t1 t2)  = S.insert t $ S.union (sub t1) (sub t2)


-- | Function determines if a variable is bound in a term.
notfree :: String -> SKTerm -> Bool
notfree x = not . S.member x . free


-- | Function returns the set of variables in a term.
vars :: SKTerm -> S.Set VarName
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars S            = S.empty
vars K            = S.empty
vars I            = S.empty


-- | Function generates a fresh variable name for a term.
newlabel :: SKTerm -> String
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


-- | Function substitutes one term for another in a term
-- does capture avoiding substitution (Berendregt)
substitute :: SKTerm -> (SKTerm, SKTerm) -> SKTerm
substitute t1@(Var c1) (Var c2, t2)
  = if c1 == c2 
      then t2 
      else t1
substitute (App t1 t2) c
  = App (substitute t1 c) (substitute t2 c)
substitute t1 _ = t1


-- | One-step reduction relation on terms.
reduce1 :: SKTerm -> Maybe SKTerm
reduce1 (Var _) 
  = Nothing
reduce1 (App I x) 
  = Just x --K x ~> x
reduce1 (App (App K x) _) 
  = Just x --K x ~> x
reduce1 (App K x) 
  = case reduce1 x of
    Just x' -> Just $ App K x'
    _       -> Nothing
reduce1 (App (App (App S x) y) z) 
  = --S x y z ~> x z (y z)
    Just $ App (App x z) (App y z)
reduce1 (App S x) 
  = case reduce1 x of
      Just x' -> Just $ App S x'
      _       -> Nothing
reduce1 (App t1 t2) 
  = case reduce1 t1 of
    Just t' -> Just $ App t' t2
    _       -> case reduce1 t2 of
                 Just t' -> Just $ App t1 t'
                 _       -> Nothing
reduce1 _ = Nothing


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reduce :: SKTerm -> SKTerm
reduce t = maybe t reduce (reduce1 t)


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reductions :: SKTerm -> [SKTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []

-- | Identity Combinator
i = App (App (App S K) K)

-- | B combinator
b = App (App S $ App K S) K

-- | C Combinator
c = App (App S (App (App S $ App K b) S)) (App K K)

-- | s4 Combinator
s4  = App (App (App S S) S) S

-- | Sii Combinator
sii = App (App (App S I) I)

-- | true
t = App K

-- | false
f = App (App S K)

-- | ci Combinator
ci x = App (App (App c I) x)







