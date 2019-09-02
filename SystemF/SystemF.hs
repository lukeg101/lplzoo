{-|
Module      : SystemF
Description : Deep-embedding of SystemF in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com
Stability   : stable
Portability : POSIX

The "SystemF" module denotes the AST of Jean Yves-Girard's System F. This languages is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module SystemF where


-- Tool Imports.
import Data.Map.Lazy as M
import qualified Data.Maybe as Maybe
import Data.Set as S
import Control.Monad (guard)


-- | SF Types, of the form, both term and type variables are ints as it's easy to rename
-- The Pi type take types a type variable and type T. 
data T 
  = TVar String
  | TArr T T
  | TPi String T
  deriving Ord


-- | Does syntactic type equality on types
instance Eq T where
  t1 == t2 = typeEquality (t1, t2) (M.empty, M.empty) 0


-- | Test for syntactic type equality on types
-- very similar to equality on first-order terms ie STLC equality
typeEquality :: (T, T) 
  -> (Map (Either String String) Int, Map (Either String String) Int) 
  -> Int 
  -> Bool
typeEquality (TVar x, TVar y) (m1, m2) _ 
  = let testEq = do a <- M.lookup (Right x) m1
                    b <- M.lookup (Right y) m2
                    return $ a == b
    in Maybe.fromMaybe (x == y) testEq
typeEquality (TArr a1 b1, TArr a2 b2) c s 
  = typeEquality (a1, a2) c s && typeEquality (b1, b2) c s
typeEquality (TPi x1 t1, TPi x2 t2) (m1, m2) s 
  = let newm1 = M.insert (Right x1) s m1
        newm2 = M.insert (Right x2) s m2
    in typeEquality (t1, t2) (newm1, newm2) (s+1) 
typeEquality _ _ _ 
  = False


-- | Show instance, uses Unicode for Pi type
-- uses bracketing convention for types
instance Show T where
  show (TVar c)    = c
  show (TArr a b)  = 
    paren (isArr a || isPi a) (show a)
      ++ "->" ++ show b
  show (TPi t1 t2)  = 
      "\x3a0" ++ t1 ++ "." ++ show t2


-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | Helper function returns true if the term is an abstraction.
isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False


-- | Helper function returns true if the term is a Pi type.
isPi :: T -> Bool
isPi (TPi _ _) = True
isPi _         = False


-- | System F Term
-- variables are Strings
-- Abstractions carry the type Church style
-- Second order abstractions carry term variables as strings
data SFTerm
  = Var String
  | Typ T
  | Abs String T SFTerm
  | App SFTerm SFTerm
  | PiAbs String SFTerm 
  deriving Ord


-- | Alpha equivalence of terms, same as STLC
instance Eq SFTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0


-- | Determines syntactic alpha-termEqualityalence of terms
-- has maps (either term, id) for each term/type variable
-- each TERM abstraction adds (left var) to the map and increments the id
-- each TYPE abstraction adds (right var) to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS
-- Type equality is called for types
termEquality :: (SFTerm, SFTerm) 
  -> (Map (Either String String) Int, Map (Either String String) Int) 
  -> Int 
  -> Bool
termEquality (Var x, Var y) (m1, m2) _
  = let testEq = do a <- M.lookup (Left x) m1
                    b <- M.lookup (Left y) m2
                    return $ a == b
    in Maybe.fromMaybe (x == y) testEq
termEquality (Abs x t1 l1, Abs y t2 l2) (m1, m2) s
  = let newm1 = M.insert (Left x) s m1
        newm2 = M.insert (Left y) s m2
    in t1 == t2 && termEquality (l1, l2) (newm1, newm2) (s+1) 
termEquality (App a1 b1, App a2 b2) c s = 
  termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality (Typ t1, Typ t2) c s =  
  typeEquality (t1, t2) c s --terms can be types now, pass ctx with type mappings
termEquality (PiAbs x1 t1, PiAbs x2 t2) (m1,m2) s
  = let newm1 = M.insert (Right x1) s m1
        newm2 = M.insert (Right x2) s m2
    in t1 == t2 && termEquality (t1, t2) (newm1, newm2) (s+1) 
termEquality _ _ _ = False

-- | Show instance for System F terms
-- uses bracketing convention for terms
instance Show SFTerm where
  show (Var x)      = x
  show (Typ t)      = "[" ++ show t ++"]"
  show (App t1 t2)  = 
    paren (isAbs t1 || isPiAbs t1 ) (show t1) 
      ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) = 
    "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1
  show (PiAbs t l1) = 
    "\x39b" ++ t ++ "." ++ show l1


-- | Type Synonym for variable names in a term.
type VarName = String


-- | Helper function returns true if the term is an abstraction.
isAbs :: SFTerm -> Bool
isAbs Abs{} = True
isAbs _      = False


-- | Helper function returns true if the term is an application.
isApp :: SFTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | Helper function returns true if the term is a Pi type.
isPiAbs :: SFTerm -> Bool
isPiAbs (PiAbs _ _) = True
isPiAbs _           = False


-- | Type context of term variables and type variables
--input term or type variable as an Int
--if the input is a term variable, return Just its type
--if the input is a type variable, return Nothing (isomorphic to 'Type')
type Context = M.Map String (Maybe T)


-- | Typing derivation for a term in a given context
-- * Just T denotes successful type derivation 
-- * Nothing denotes failure to type the term (not valid in System F)
-- see how the context works to make sense of case statements
typeof :: SFTerm -> Context -> Maybe T
typeof (Var v) ctx 
  = Maybe.fromMaybe Nothing $ M.lookup v ctx
typeof (Typ _) _ 
  = Nothing
typeof (Abs x t l1) ctx 
  = do t' <- typeof l1 (M.insert x (Just t) ctx)
       guard (all (\v -> M.lookup v ctx == Just Nothing) (freeTVars t))
       Just $ TArr t t'
typeof (App l1 l2) ctx
  = do t <- typeof l1 ctx 
       case t of 
         (TArr t2 t3)-> do t1 <- typeof l2 ctx
                           guard (t1 == t2)
                           return t3
         (TPi x t)    -> do let (Typ a) = l2
                            return $ typeSub t (TVar x, a) -- type substitution under 2nd-order abstraction
         _ -> Nothing
typeof (PiAbs t l1) ctx
  = TPi t <$> typeof l1 (M.insert t Nothing ctx)


-- | Top level typing function providing empty context
typeof' l = typeof l M.empty

 
-- | Similar to type substitution but at the type level
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) (TVar y,z) 
  | x == y    = z
  | otherwise = l
typeSub (TArr t1 t2) c 
  = TArr (typeSub t1 c) (typeSub t2 c)
typeSub l@(TPi x t) c@(TVar y, z)
  | x == y         = l
  | x `notfreeT` z = TPi x $ typeSub t c
  | otherwise      = TPi n $ typeSub (renameT t (x, n)) c
  where n = foldr1 
              biggest 
              [newTLabel t, newTLabel z, newTLabel (TVar x)]
typeSub l _ = l


-- | Function determines if a variable is bound in a term.
notfreeT :: VarName -> T -> Bool
notfreeT x = not . S.member x . freeTVars


-- | Type-level free variables
freeTVars :: T -> Set VarName
freeTVars (TVar c)     = S.singleton c
freeTVars (TArr t1 t2) = S.union (freeTVars t1) (freeTVars t2) 
freeTVars (TPi x t1)   = S.delete x (freeTVars t1)


-- | Set of variables in a type
typeVars :: T -> Set VarName
typeVars (TVar x)     = S.singleton x
typeVars (TArr t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (TPi x t)    = S.insert x $ typeVars t


-- | Type vars in a term
typeVarsInTerm :: SFTerm -> Set VarName
typeVarsInTerm (Var _)      = S.empty
typeVarsInTerm (Abs _ t l1) = S.union (typeVars t) $ typeVarsInTerm l1
typeVarsInTerm (App l1 l2)  = S.union (typeVarsInTerm l1) (typeVarsInTerm l2)
typeVarsInTerm (Typ t)      = typeVars t
typeVarsInTerm (PiAbs x t)  = S.insert x $ typeVarsInTerm t


-- | Function generates a fresh variable name for a term.
newTLabel :: T -> VarName
newTLabel x = head . dropWhile (`elem` typeVars x) 
  $ iterate genVar $  S.foldr biggest "" $ typeVars x


-- | Rename t (x,y): renames free occurences of type variables x in t to y
renameT :: T -> (VarName, VarName) -> T
renameT (TVar a) (x,y) 
  = if a == x 
      then TVar y 
      else TVar a
renameT l@(TPi a t) c@(x,_) 
  = if a == x 
      then l 
      else TPi a $ renameT t c
renameT (TArr t1 t2) c 
  = TArr (renameT t1 c) (renameT t2 c)


-- | Bound variables of a term
bound :: SFTerm -> Set VarName
bound (Var _)      = S.empty
bound (Abs n _ l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)
bound (Typ _)      = S.empty
bound (PiAbs _ t)  = bound t


-- | Free variables of a term
free :: SFTerm -> Set VarName
free (Var n)      = S.singleton n
free (Abs n _ l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)
free (Typ _)      = S.empty
free (PiAbs _ t)  = free t


-- | Test to see if a term is closed (has no free vars)
closed :: SFTerm -> Bool
closed = S.null . free


-- | Subterms of a term
sub :: SFTerm -> Set SFTerm
sub l@(Var _)      = S.singleton l
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l@(Typ _)      = S.singleton l
sub l@(PiAbs _ t)  = S.insert l $ sub t


-- | Element is bound in a term
notfree :: VarName -> SFTerm -> Bool
notfree x = not . S.member x . free 


-- | Set of variables in a term
vars :: SFTerm -> Set VarName
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1
vars (Typ _)      = S.empty
vars (PiAbs _ t)  = vars t


-- | Generates a fresh variable name for a term
newlabel :: SFTerm -> VarName
newlabel x = head . dropWhile (`elem` vars x) 
  $ iterate genVar $  S.foldr biggest "" $ vars x


-- | Generates fresh variable names from a given variable
genVar :: VarName -> VarName 
genVar []       = "a"
genVar ('z':xs) = 'a':genVar xs
genVar ( x :xs) = succ x:xs


-- | Length-observing maximum function that falls back on lexicographic ordering
biggest :: VarName -> VarName -> VarName 
biggest xs ys = if length xs > length ys then xs else max xs ys


-- | Rename t (x,y): renames free occurences of term variable x in t to y
rename :: SFTerm -> (VarName, VarName) -> SFTerm
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
rename l@(Typ _) _ = l
rename (PiAbs x t) c 
  = PiAbs x $ rename t c


-- | Substitute one term for another in a term
--does capture avoiding substitution
substitute :: SFTerm -> (SFTerm, SFTerm) -> SFTerm
substitute l1@(Var c1) (Var c2, l2) 
  | c1 == c2  = l2 
  | otherwise = l1 
substitute l1@(Typ (TVar x)) (Var y, l2) 
  | x == y    = l2
  | otherwise = l1
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute l@(Abs y t l1) c@(Var x, l2)
  | y == x         = l
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise      = Abs z t $ substitute (rename l1 (y,z)) c
  where z          = foldr1 
                       biggest 
                       [newlabel l1, newlabel l2, newlabel (Var x)]
substitute (PiAbs x t) c@(Var y, _)
  | x /= y = PiAbs x $ substitute t c
substitute l _ = l

-- | One-step reduction relation on terms.
reduce1 :: SFTerm -> Maybe SFTerm 
reduce1 (Var _) 
  = Nothing
reduce1 (Abs x t s) 
  = Abs x t <$> reduce1 s
reduce1 (App (Abs x _ l') l2)
  = Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 (App (PiAbs x1 t) (Typ x2)) 
  = Just $ tSubUnder t (TVar x1, x2)  --type-level beta: (Pi X. t) A ~> t[X := A]
reduce1 (App l1 l2) 
  = case reduce1 l1 of 
      Just l' -> Just $ App l' l2
      _       -> App l1 <$> reduce1 l2
reduce1 (Typ _) 
  = Nothing
reduce1 (PiAbs x t)
  = PiAbs x <$> reduce1 t


-- | Function used to do type substitution under a second order abstraction
tSubUnder :: SFTerm -> (T,T) -> SFTerm
tSubUnder l@(Var _) _    = l
tSubUnder (Typ t) c      = Typ $ typeSub t c
tSubUnder (Abs x t l2) c = Abs x (typeSub t c) $ tSubUnder l2 c
tSubUnder (App l1 l2) c  = App (tSubUnder l1 c) (tSubUnder l2 c)
tSubUnder l@(PiAbs x t) c@(TVar y, _) 
  | x /= y = PiAbs x (tSubUnder t c)  
  | otherwise = l
tSubUnder l _ = l


-- | Multi-step reduction relation 
-- NOT GUARANTEED TO TERMINATE if it doesn't type check
reduce :: SFTerm -> SFTerm
reduce t = case reduce1 t of 
  Just t' -> reduce t'
  Nothing -> t


-- | Multi-step reduction relation that accumulates all reduction steps
reductions :: SFTerm -> [SFTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []

-- Common combinators

-- | Identity functor
ident = PiAbs "X" (Abs "x" (TVar "X") (Var "x"))


-- | Nat type 
natType = TPi "X" (TArr (TArr (TVar "X") (TVar "X")) (TArr (TVar "X") (TVar "X")))

-- | Church Numeral for zero
zero = PiAbs "X" (Abs "x" (TArr (TVar "X") (TVar "X")) (Abs "y" (TVar "X") (Var "y")))

-- | Untypeable omega
xx = Abs "x" (TArr natType natType) (App (Var "x") (Var "x")) --won't type check as expected




