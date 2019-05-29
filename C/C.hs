{-|
Module      : C
Description : Deep-embedding of the Calculus of Constructions in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "C" module denotes the AST and operations for the Calculus of Constructions in Pure Type System Style. This language is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module C where


-- Tool Imports.
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Maybe    as Maybe 
import qualified Control.Monad as C


-- | Sorts in C, needed to distinguish proper types from well-formed kinds
data S
  = SStar
  | SBox
  deriving (Eq, Ord)


-- | Simple show instance for kinds
instance Show S where
  show SStar = "*"
  show SBox  = "\x25A1" 


-- Since we adopt the Pure Type system approach, typing is squashed into terms


-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | C Terms
-- We squash terms into types and so the language is very simple
-- variables are Strings
-- Abstractions carry the type Church style, same with Pi types
-- sorts have their own special term
data CTerm
  = Var VarName
  | Abs VarName CTerm CTerm
  | Pi  VarName CTerm CTerm
  | App CTerm CTerm
  | Sort S
  deriving Ord


-- | Type Synonym for variable names in a term.
type VarName = String


-- | Simple show instance for C
instance Show CTerm where
  show (Var x)      
    = x
  show (Abs x t1 l1) 
    = "\x03bb" ++ x ++ ":" 
        ++ show t1
        ++ "." ++ show l1
  show ty@(Pi t t1 t2)  
    | isArr ty
      = paren (isApp t1 || isPi t1 || isAbs t1 || isArr t1) (show t1) 
          ++ "->" ++ show t2
    | otherwise 
      = "\x3a0 " ++ t ++ ":" ++ show t1 ++ "." ++ show t2
  show (App t1 t2)  
    = paren (isAbs t1 || isPi t1) (show t1) 
        ++ ' ' : paren (isAbs t2 || isPi t2 || isApp t2) (show t2)
  show (Sort s)
    = show s


-- | Helper function returns true if the term is an abstraction.
isAbs :: CTerm -> Bool
isAbs Abs{} = True
isAbs _     = False


-- | Helper function returns true if the term is an abstraction.
isPi :: CTerm -> Bool
isPi Pi{} = True
isPi _    = False


-- | Helper function returns true if the term is an application.
isApp :: CTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | Helper function returns true if the term is an non-dependent pi type.
isArr :: CTerm -> Bool
isArr (Pi x _ t2) = x `notElem` vars t2
isArr _           = False


-- | alpha equivalence of terms using syntactic term equality
instance Eq CTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0


-- | Checks for equality of terms, has a map (term, id) for each variable.
-- Each abstraction adds vars to the map and increments the id.
-- Variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS.
termEquality :: (CTerm, CTerm) 
             -> (M.Map VarName Int, M.Map VarName Int) 
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
    in termEquality (t1, t2) (newm1, newm2) (s+1) 
       && termEquality (l1, l2) (newm1, newm2) (s+1)
termEquality (Pi "_" t1 l1, Pi "_" t2 l2) c s
  = termEquality (t1, t2) c s && termEquality (l1, l2) c s
termEquality (Pi x t1 l1, Pi y t2 l2) (m1, m2) s 
  = let newm1 = M.insert x s m1
        newm2 = M.insert y s m2
    in termEquality (t1, t2) (newm1, newm2) (s+1) 
       && termEquality (l1, l2) (newm1, newm2) (s+1) 
termEquality (App a1 b1, App a2 b2) c s 
  = termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality (Sort x, Sort y) _ _
  = x == y
termEquality _ _ _ = False


-- | Type context of term variables and kinds
--input term or type variable as a String
--if the input is a term, return Left its type
--if the input is a type, return Right its kind
type Context = M.Map String CTerm


-- | Typing derivation for a term in a given context
-- * Just T denotes successful type derivation 
-- * Nothing denotes failure to type the term (not in C)
typeof :: CTerm -> Context -> Maybe CTerm
typeof (Sort SStar) _ 
  = return $ Sort SBox
typeof (Var v) ctx 
  = M.lookup v ctx
typeof (Abs x ty1 l1) ctx
  = do typeof ty1 ctx
       ty2 <- typeof l1 (M.insert x ty1 ctx)
       return $ Pi x ty1 ty2
typeof (Pi x t1 t2) ctx
  = do typeof t1 ctx
       typeof t2 (M.insert x t1 ctx)
typeof (App t1 t2) ctx
  = do (Pi x t3 t4) <- typeof t1 ctx
       t5 <- typeof t2 ctx
       C.guard (t3 == t5)
       return $ substitute t4 (Var x, t2)
typeof _ _ = Nothing


-- | Top level typing function providing empty context
typeof' l = typeof l M.empty


-- | Substitute one term into another
substitute :: CTerm -> (CTerm, CTerm) -> CTerm
substitute l1@(Var c1) (Var c2, l2) 
  = if c1 == c2 
      then l2 
      else l1 
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute (Abs y t l1) c@(Var x, l2)
  | y == x         = Abs y (substitute t c) l1 -- sub under the 'type'
  | y `notfree` l2 = Abs y (substitute t c) (substitute l1 c)
  | otherwise      = Abs z (substitute t c) l1'
  where l1' = substitute (rename l1 (Var y,Var z)) c
        z          = max (newlabel l1) (newlabel l2)
substitute (Pi y t l1) c@(Var x, l2)
  | y == x         = Pi y (substitute t c) l1 -- sub under the 'type'
  | y `notfree` l2 = Pi y (substitute t c) (substitute l1 c)
  | otherwise      = Pi z (substitute t c) l1'
  where l1' = substitute (rename l1 (Var y,Var z)) c
        z          = max (newlabel l1) (newlabel l2)
substitute l _ = l


-- | Function returns a set of free variables of a term.
free :: CTerm -> S.Set VarName
free (Var n)       = S.singleton n
free (Abs n l1 l2) = S.delete n $ S.union (free l1) (free l2)
free (Pi n l1 l2)  = S.delete n $ S.union (free l1) (free l2)
free (App l1 l2)   = S.union (free l1) (free l2)
free (Sort _)      = S.empty


-- | Function determines if a variable is bound in a term.
notfree :: VarName -> CTerm -> Bool
notfree x = not . S.member x . free 


-- | Function returns a set of bound variables of a term.
bound :: CTerm -> S.Set VarName
bound (Var _)       = S.empty
bound (Abs n l1 l2) = S.insert n $ S.union (bound l1) (bound l2)
bound (Pi n l1 l2)  = S.insert n $ S.union (bound l1) (bound l2)
bound (App l1 l2)   = S.union (bound l1) (bound l2)
bound (Sort _)      = S.empty


-- | Function tests to see if a term is closed (has no free vars).
closed :: CTerm -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: CTerm -> S.Set CTerm
sub l@(Var _)       = S.singleton l
sub l@(Abs _ l1 l2) = S.insert l $ S.union (sub l1) (sub l2)
sub l@(Pi _ l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l@(App l1 l2)   = S.insert l $ S.union (sub l1) (sub l2)
sub l@(Sort _)      = S.singleton l


-- | Function returns the set of variables in a term.
vars :: CTerm -> S.Set VarName
vars (Var x)         = S.singleton x
vars (App t1 t2)     = S.union (vars t1) (vars t2)
vars (Abs "_" l1 l2) = S.union (vars l1) (vars l2)
vars (Abs x l1 l2)   = S.insert x $ S.union (vars l1) (vars l2)
vars (Pi "_" l1 l2)  = S.union (vars l1) (vars l2) -- special case for parsing
vars (Pi x l1 l2)    = S.insert x $ S.union (vars l1) (vars l2)
vars (Sort _)        = S.empty


-- | Function generates a fresh variable name for a term.
newlabel :: CTerm -> VarName
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
rename :: CTerm -> (CTerm, CTerm) -> CTerm
rename = substitute


-- | One-step reduction relation on terms.
-- Warning: may not terminate on untypable terms
reduce1 :: CTerm -> Maybe CTerm 
reduce1 (Var _)  = Nothing
reduce1 (Sort _) = Nothing
reduce1 (Abs x t s)
  = case reduce1 s of 
      Just s' -> Just $ Abs x t s'
      _       -> do t' <- reduce1 t
                    return $ Abs x t' s
reduce1 (Pi x t s)
  = case reduce1 s of 
      Just s' -> Just $ Pi x t s'
      _       -> do t' <- reduce1 t
                    return $ Pi x t' s
reduce1 (App (Abs x _ l1) l2) --beta conversion
  = Just $ substitute l1 (Var x, l2)  
reduce1 (App (Pi x _ l1) l2) --beta conversion
  = Just $ substitute l1 (Var x, l2)  
reduce1 (App l1 l2) 
  = case reduce1 l1 of 
      Just l' -> Just $ App l' l2
      _       -> App l1 <$> reduce1 l2


-- | Multi-step reduction relation.
-- NOT GUARANTEED TO TERMINATE if typing fails
reduce :: CTerm -> CTerm
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reductions :: CTerm -> [CTerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []


