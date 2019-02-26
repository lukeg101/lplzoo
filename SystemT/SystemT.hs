{-|
Module      : SystemT
Description : Deep-embedding of SystemT in Haskell.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "SystemT" module denotes the AST of Kurt Godel's T. This languages is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module SystemT where


-- Tool Imports.
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Control.Monad as Monad


-- |  Simple Types for T, of the form Nat or 'T -> T' or a mix of both
data T 
  = TNat
  | TArr T T
  deriving (Eq, Ord) --Type equality is simply equality of the type tree like STLC


-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | Helper function returns true if the term is an abstraction.
isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False


-- | Naive show instance for types, follows bracketing convention
instance Show T where
  show TNat        = "Nat"
  show (TArr a b)  = paren (isArr a) (show a) ++ "->" ++ show b


-- | System T Terms
-- variables are Strings
-- Abstractions carry the type Church style
-- Zero is a value in the language
-- Succ n is an function that takes a term and makes a term
data STTerm
  = Var String
  | Abs String T STTerm
  | App STTerm STTerm
  | Zero
  | Succ   -- Succ n is acheived with App Succ n 
  | RecNat -- Same as Succ
  deriving Ord

{-
It's possible to define Succ as a no-arg constructor:
data STTerm = ... | NatSucc | ...
and then give it type Nat -> Nat which Haskell can infer
then you can apply NatSucc like a function. However I stick to 
the inductive approach instead
-}

-- | Type Synonym for variable names in a term.
type VarName = String


-- | Alpha equivalence of terms, same as STLC
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
termEquality (Succ, Succ) _ _
  = True
termEquality (Zero, Zero) _ _ 
  = True
termEquality (RecNat, RecNat) _ _ 
  = True
termEquality _ _ _ = False


-- | Show instance for STTerms, following bracketing convention
instance Show STTerm where
  show Zero    = "z"
  show Succ    = "s"
  show RecNat  = "rec"
  show (Var x) = x
  show (App t1 t2)  = 
    paren (isAbs t1) (show t1) ++ ' ' 
      : paren (isAbs t2 || isApp t2 || isSucc t2) (show t2)
  show (Abs x t l1) = 
    "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1


-- | Helper function returns true if the term is the successor
isSucc :: STTerm -> Bool
isSucc Succ = True
isSucc _    = False


-- | Helper function returns true if the term is an abstraction.
isAbs :: STTerm -> Bool
isAbs Abs{} = True
isAbs _      = False


-- | Helper function returns true if the term is an application.
isApp :: STTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map String T


-- | Typing derivation for a term in a given context
-- * Just T denotes successful type derivation 
-- * Nothing denotes failure to type the term (not in \->)
typeof :: STTerm -> Context -> Maybe T
typeof Zero _ 
  = Just TNat
typeof (Var v) ctx 
  = M.lookup v ctx
typeof (Abs x t l1) ctx 
  = TArr t <$> typeof l1 (M.insert x t ctx)
typeof (App (App (App RecNat h) a) n) ctx 
  = do t1 <- typeof h ctx
       t2 <- typeof a ctx
       t3 <- typeof n ctx
       case t1 of
         TArr TNat (TArr t1' t1'') -> do
           Monad.guard (t1' == t1'' && t2 == t1' && t3 == TNat) 
           Just t2
         _                         -> Nothing
typeof (App Succ l2) ctx 
  = do Monad.guard (typeof l2 ctx == Just TNat)
       Just TNat
typeof (App l1 l2) ctx 
  = do t1 <- typeof l2 ctx
       case typeof l1 ctx of
         Just (TArr t2 t3) -> do Monad.guard (t1 == t2)
                                 return t3
         _                 -> Nothing
typeof _ _ = Nothing


-- | Top level typing function providing empty context
typeof' l = typeof l M.empty


-- | Function returns a set of bound variables of a term.
bound :: STTerm -> S.Set VarName
bound Zero         = S.empty
bound Succ         = S.empty
bound RecNat       = S.empty
bound (Var _)      = S.empty
bound (Abs n _ l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)


-- | Function returns a set of free variables of a term.
free :: STTerm -> S.Set VarName
free Zero         = S.empty
free Succ         = S.empty
free RecNat       = S.empty
free (Var n)      = S.singleton n
free (Abs n _ l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)


-- | Function tests to see if a term is closed (has no free vars).
closed :: STTerm -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: STTerm -> S.Set STTerm
sub l@Zero         = S.singleton l
sub l@Succ         = S.singleton l
sub l@RecNat       = S.singleton l
sub l@(Var _)      = S.singleton l
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)


-- | Function determines if a variable is bound in a term.
notfree :: VarName -> STTerm -> Bool
notfree x = not . S.member x . free 


-- | Function returns the set of variables in a term.
vars :: STTerm -> S.Set VarName
vars Zero         = S.empty
vars Succ         = S.empty
vars RecNat       = S.empty
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1


-- | Function generates a fresh variable name for a term.
newlabel :: STTerm -> VarName
newlabel x = head . dropWhile (`elem` vars x) 
  $ iterate genVar $  S.foldr biggest "" $ vars x


-- | Function generates fresh variable names from a given variable.
genVar :: VarName -> VarName 
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
rename :: STTerm -> (VarName, VarName) -> STTerm
rename Zero _ 
  = Zero
rename Succ _ 
  = Succ
rename RecNat _ 
  = RecNat
rename (Var a) (x,y) 
  = if a == x 
      then Var y 
      else Var a
rename l@(Abs a t l1) (x,y) 
  = if a == x 
      then l 
      else Abs a t $ rename l1 (x, y)
rename (App l1 l2) (x,y) 
  = App (rename l1 (x,y)) (rename l2 (x,y))


-- | Function substitutes one term for another in a term
-- does capture avoiding substitution (Berendregt)
substitute :: STTerm -> (STTerm, STTerm) -> STTerm
substitute Zero _ 
  = Zero
substitute Succ _ 
  = Succ
substitute RecNat _ 
  = RecNat
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
  where z = max (newlabel l1) (newlabel l2)
substitute x _ = x -- should never happen

-- | One-step reduction relation on terms.
reduce1 :: STTerm -> Maybe STTerm 
reduce1 Zero 
  = Nothing
reduce1 Succ 
  = Nothing
reduce1 RecNat 
  = Nothing
reduce1 (App Succ n) 
  = case reduce1 n of
    Just n' -> Just $ App Succ n'
    _       -> Nothing
reduce1 (App (App (App RecNat _) a) Zero) 
  = Just a
reduce1 (App (App (App RecNat h) a) (App Succ n)) 
  = Just $ App (App h n) (App (App (App RecNat h) a) n)
reduce1 (App (App (App RecNat _) _) _) 
  = Nothing
reduce1 (Var _) 
  = Nothing
reduce1 (Abs x t s) 
  = do s' <- reduce1 s
       Just $ Abs x t s'
reduce1 (App (Abs x _ l') l2) 
  = Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 (App l1 l2) 
  = do l' <- reduce1 l1
       Just $ App l' l2

-- | Multi-step reduction relation.
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


-- | Identity function for nats
iNat :: T -> STTerm
iNat t = Abs "x" t (Var "x")


-- | true
true :: T -> T -> STTerm
true t f = Abs "x" t (Abs "y" f (Var "x"))


-- | false
false :: T -> T -> STTerm
false t f = Abs "x" t (Abs "y" f (Var "y"))


-- | xx combinator (won't type check)
xx :: STTerm
xx = Abs "x" (TArr TNat TNat) (App (Var "x") (Var "x")) --won't type check as expected


-- | omega combinator, won't type check
omega :: STTerm
omega = App xx xx --won't type check, see above


-- | if combinator
_if :: STTerm -> STTerm -> STTerm -> STTerm
_if c t = App (App c t)


-- | isZero
isZero :: STTerm -> T -> T -> STTerm
isZero Zero = true
isZero _    = undefined 


-- | Function converting Haskell Int to Peano nat in System T
toPeano :: Int -> STTerm
toPeano 0 = Zero
toPeano n = App Succ (toPeano (n-1))


-- | Function converting Peano nat to Haskell Int
toInt :: STTerm -> Int
toInt Zero         = 0
toInt (App Succ n) = 1 + toInt n
toInt _            = error "Not Nat"

