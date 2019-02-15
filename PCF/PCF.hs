{-|
Module      : PCF
Description : Deep-embedding of PCF in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "PCF" module denotes the AST of Gordon Plotkin's Programming Computable Functions. This languages is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module PCF where

-- Tool Imports.
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.Set as S
import qualified Control.Monad as Monad


-- | Simple Types for PCF, of the form 'Nat' or 'o -> o' or a mix of both
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


-- | PCF Terms
-- variables are Strings
-- Abstractions carry the type Church style
-- Zero is a value in the language
-- Succ is applied to a term
-- Pred is the predecessor of a number
-- Y is the classic general recursion combinator
-- If is used for conditional statements
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


-- | Type Synonym for variable names in a term.
type VarName = String


-- | Alpha equivalence of terms, same as STLC
instance Eq PCFTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- | Checks for equality of terms, has a map (term, id) for each variable.
-- Each abstraction adds vars to the map and increments the id.
-- Variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS.
termEquality :: (PCFTerm, PCFTerm) 
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
termEquality (Pred, Pred) _ _ 
  = True
termEquality (Zero, Zero) _ _ 
  = True
termEquality (If, If) _ _     
  = True
termEquality (Y, Y) _ _       
  = True
termEquality _ _ _            
  = False

-- use application to combine E.g App Y f and let the lambda machinery do the work

-- | Show instance for PCFTerms, following bracketing convention
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


-- | Helper function returns true if the term is an abstraction.
isAbs :: PCFTerm -> Bool
isAbs Abs{} = True
isAbs _      = False


-- | Helper function returns true if the term is an application.
isApp :: PCFTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map String T


-- | Typing derivation for a term in a given context
-- * Just T denotes successful type derivation 
-- * Nothing denotes failure to type the term (not in PCF)
typeof :: PCFTerm -> Context -> Maybe T
typeof Zero _ 
  = Just TNat
typeof (Var v) ctx 
  = M.lookup v ctx
typeof (Abs x t l1) ctx 
  = TArr t <$> typeof l1 (M.insert x t ctx)
typeof (App Succ l2) ctx 
  = do Monad.guard (typeof l2 ctx == Just TNat)
       Just TNat
typeof (App Pred l2) ctx 
  = do Monad.guard (typeof l2 ctx == Just TNat)
       Just TNat
typeof (App Y l2) ctx 
  = case typeof l2 ctx of
      Just (TArr t1 t2) -> do Monad.guard (t1 == t2)
                              return t1
      _                 -> Nothing
typeof (App (App (App If l2) l3) l4) ctx 
  = case typeof l2 ctx of
      Just TNat -> do t3 <- typeof l3 ctx
                      t4 <- typeof l4 ctx
                      Monad.guard (t3 == t4)
                      Just t3
      _         -> Nothing
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
bound :: PCFTerm -> S.Set VarName
bound Zero         = S.empty
bound Succ         = S.empty
bound Pred         = S.empty
bound Y            = S.empty
bound If           = S.empty
bound (Var _)      = S.empty
bound (Abs n _ l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)


-- | Function returns a set of free variables of a term.
free :: PCFTerm -> S.Set VarName
free Zero         = S.empty
free Succ         = S.empty
free Pred         = S.empty
free Y            = S.empty
free If           = S.empty
free (Var n)      = S.singleton n
free (Abs n _ l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)


-- | Function tests to see if a term is closed (has no free vars).
closed :: PCFTerm -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: PCFTerm -> S.Set PCFTerm
sub l@Zero         = S.singleton l
sub l@Succ         = S.singleton l
sub l@Pred         = S.singleton l
sub l@Y            = S.singleton l
sub l@If           = S.singleton l
sub l@(Var _)      = S.singleton l
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)


-- | Function determines if a variable is bound in a term.
notfree :: VarName -> PCFTerm -> Bool
notfree x = not . S.member x . free 


-- | Function returns the set of variables in a term.
vars :: PCFTerm -> S.Set VarName
vars Zero         = S.empty
vars Succ         = S.empty
vars Pred         = S.empty
vars Y            = S.empty
vars If           = S.empty
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1


-- | Function generates a fresh variable name for a term.
newlabel :: PCFTerm -> VarName
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
rename :: PCFTerm -> (VarName, VarName) -> PCFTerm
rename Zero _ 
  = Zero
rename Succ _ 
  = Succ
rename Pred _ 
  = Pred
rename Y _   
  = Y
rename If _   
  = If
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
substitute :: PCFTerm -> (PCFTerm, PCFTerm) -> PCFTerm
substitute Zero _ 
  = Zero
substitute Succ _ 
  = Succ
substitute Pred _ 
  = Pred
substitute Y _   
  = Y
substitute If _   
  = If
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
reduce1 :: PCFTerm -> Maybe PCFTerm 
reduce1 Zero 
  = Nothing
reduce1 Succ 
  = Nothing
reduce1 Pred 
  = Nothing
reduce1 Y    
  = Nothing 
reduce1 If   
  = Nothing
reduce1 (Var _) 
  = Nothing
reduce1 (Abs x t s) 
  = do s' <- reduce1 s
       Just $ Abs x t s'
reduce1 (App (Abs x _ l') l2) 
  = Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 (App Pred (App Succ l1)) 
  = Just l1
reduce1 (App Pred Zero) 
  = Just Zero
reduce1 (App Pred n) 
  = case reduce1 n of
      Just n' -> Just $ App Pred n'
      _       -> Nothing
reduce1 (App Succ n) 
  = case reduce1 n of
      Just n' -> Just $ App Succ n'
      _       -> Nothing
reduce1 (App (App (App If Zero) l3) _) 
  = Just l3
reduce1 (App (App (App If (App Succ _)) _) l4) 
  = Just l4
reduce1 (App (App (App If l2) l3) l4) 
  = do l2' <- reduce1 l2
       Just $ App (App (App If l2') l3) l4
reduce1 l@(App Y (Abs x _ l')) 
  = Just $ substitute l' (Var x, l)  -- Y f ~> f (Y f)
reduce1 (App Y l2) 
  = do l2' <- reduce1 l2
       Just $ App Y l2'
reduce1 (App l1 l2) 
  = do l' <- reduce1 l1
       Just $ App l' l2


-- | Multi-step reduction relation.
reduce :: PCFTerm -> PCFTerm
reduce t = case reduce1 t of 
  Just t' -> reduce t'
  Nothing -> t


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reductions :: PCFTerm -> [PCFTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []

--common combinators

-- | Identity function for nats
iNat :: T -> PCFTerm
iNat t = Abs "x" t (Var "x")


-- | true
true :: T -> T -> PCFTerm
true t f = Abs "x" t (Abs "y" f (Var "x"))


-- | false
false :: T -> T -> PCFTerm
false t f = Abs "x" t (Abs "y" f (Var "y"))


-- | xx combinator (won't type check)
xx :: PCFTerm
xx = Abs "x" (TArr TNat TNat) (App (Var "x") (Var "x")) --won't type check as expected


-- | omega combinator, won't type check
omega :: PCFTerm
omega = App xx xx --won't type check, see above


-- | if combinator
_if :: PCFTerm -> PCFTerm -> PCFTerm -> PCFTerm
_if c t = App (App c t)


-- | isZero
isZero :: PCFTerm -> T -> T -> PCFTerm
isZero Zero = true
isZero _    = undefined 


-- | increment function
incr :: PCFTerm -> PCFTerm
incr = App Succ


-- | Function converting Haskell Int to Peano nat in PCF
toPeano :: Int -> PCFTerm
toPeano 0 = Zero
toPeano n = incr (toPeano (n-1))


-- | Function converting Peano nat to Haskell Int
toInt :: PCFTerm -> Int
toInt Zero         = 0
toInt (App Succ n) = 1 + toInt n
toInt _            = error "Not Nat"


