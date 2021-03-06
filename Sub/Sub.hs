module Sub where

import Data.Map.Lazy as M hiding (map)
import Data.Set as S hiding      (map)
import Data.List as L            (intercalate, group, sort, lookup)
import Control.Monad             (guard)

-- Sub Terms, type variables are ints as it's easy to rename
-- Unit/Top is used as the final type, meant to represent Object
-- records are lists of variables and types (todo make this Set)
data T
  = TVar String
  | TArr T T
  | TUnit
  | TRec [(String,T)]
  deriving (Eq, Ord)

-- show implementation, uses Unicode for Unit
-- uses bracketing convention for types
instance Show T where
  show (TVar x)   = x
  show (TArr a b)  = paren (isArr a) (show a) ++ "->" ++ show b
  show TUnit   = "\x22A4"
  show (TRec xs) = wparen (L.intercalate ", " (map (\(v,t)->v ++ ":" ++ show t) xs))

paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x

wparen :: String -> String
wparen x = "{" ++ x ++ "}"

isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False

-- Sub Term
-- variables are numbers as it's easier for renaming
-- Abstractions carry the type Church style\
-- Records are sets of variable names and terms
data STerm
  = Var String
  | Abs String T STerm
  | App STerm STerm
  | Rec [(String ,STerm)]
  | Proj String
  | Unit
  deriving Ord

-- alpha termEqualityalence of terms uses
instance Eq STerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0

-- checks for equality of terms, has a map (term, id) for each variable
-- each abstraction adds vars to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- if both bound, check that s is same in both maps
-- if neither is bound, check literal equality
-- if bound t1 XOR bound t2 == true then False
-- application recursively checks both the LHS and RHS
termEquality :: (STerm, STerm)
  -> (Map String Int, Map String Int)
  -> Int
  -> Bool
termEquality (Var x, Var y) (m1, m2) _ = case M.lookup x m1 of
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
termEquality (Rec l1, Rec l2) c s = length l1 == length l2
  && all (>0) counts && length counts == length l1
  where
    counts = map length . group . sort $
      [v1 | (v1,t1) <- l1, (_,t2) <- l2, termEquality (t1, t2) c s]
termEquality (Unit, Unit) _ _ = True
termEquality (Proj x, Proj y) _ _ = x == y
termEquality _ _ _ = False

-- show implementation for sub terms
-- uses bracketing convention for terms
instance Show STerm where
  show (Var x)      = x
  show (App l1 (Proj x)) = show l1 ++ "." ++ x
  show (Rec xs) = wparen (L.intercalate ", " (map (\(v,t)->v ++ "=" ++ show t) xs))
  show (App t1 t2)  =
    paren (isAbs t1) (show t1) ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) = "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1
  show Unit = "()"
  show (Proj _) = error "should be handled by above case"

isAbs :: STerm -> Bool
isAbs (Abs {}) = True
isAbs _         = False

isApp :: STerm -> Bool
isApp (App _ _) = True
isApp _         = False

-- Type context for t:T is Map v T where v is a variable name and T is it's supplied Type
type Context = M.Map String T

-- typing derivation for a term in a given context
-- Just T denotes successful type derivation 
-- Nothing denotes failure to type the term (not in \->+Sub)
typeof :: STerm -> Context -> Maybe T
typeof Unit _ = Just TUnit
typeof (Var v) ctx = M.lookup v ctx
typeof (Abs x t l1) ctx = do
  t' <- typeof l1 (M.insert x t ctx)
  return $ TArr t t'
typeof (App (Rec l1) (Proj x)) ctx = -- projecting from a record directly
  case L.lookup x l1 of
    Just t -> typeof t ctx
    _ -> Nothing
typeof (App (Var v) (Proj x)) ctx = do -- projecting from a variable
  case M.lookup v ctx of -- we view a record as a function to an element inside
    Just (TRec l1) -> L.lookup x l1
    _ -> Nothing
typeof (App l1 l2) ctx = do
  t1 <- typeof l2 ctx
  case typeof l1 ctx of
    Just (TArr t2 t3) -> do
      guard (subtype t1 t2)
      return t3
    _ -> Nothing
typeof (Rec l1) ctx = do
  tl1 <- mapM (\(v,t) -> case typeof t ctx of
    Just t' -> Just (v,t')
    _ -> Nothing) l1
  return $ TRec tl1

-- top level typing function providing empty context
typeof' l = typeof l M.empty

-- function performs width subtyping on types
-- such that T < Top, T < T
-- if T1 < S1 and S2 < T2 then S1 -> S2 < T1 -> T2
-- records have width subtyping on record names
subtype :: T -> T -> Bool
subtype t1 t2 = t1 == t2 ||
  case (t1, t2) of
    (_, TUnit) -> True
    (TRec l1, TRec l2) -> all
      (\(v,t1) -> case L.lookup v l1 of
        Just t2 -> subtype t2 t1
        _ -> False)
      l2
    (TArr t1a t1b, TArr t2a t2b)
      -> subtype t1a t2a && subtype t1b t2b
    (TVar _, TVar _) -> False --base types are at the same level
    (_, _) -> False

--bound variables of a term
bound :: STerm -> Set String
bound (Var _)      = S.empty
bound (Abs n _ l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)
bound Unit       = S.empty
bound (Rec l1)     = S.unions $ map (bound . snd) l1
bound (Proj _)     = S.empty

--free variables of a term
free :: STerm -> Set String
free (Var n)      = S.singleton n
free (Abs n _ l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)
free Unit       = S.empty
free (Rec l1)     = S.unions $ map (free . snd) l1
free (Proj _)     = S.empty

--test to see if a term is closed (has no free vars)
closed :: STerm -> Bool
closed = S.null . free

--subterms of a term
sub :: STerm -> Set STerm
sub l@(Var _)      = S.singleton l
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l@Unit       = S.singleton l
sub l@(Rec l1)     = S.insert l $ S.unions $ map (sub . snd) l1
sub (Proj _)     = S.empty

--element is bound in a term
notfree :: String -> STerm -> Bool
notfree x = not . S.member x . free

--set of variables in a term
vars :: STerm -> Set String
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1
vars Unit       = S.empty
vars (Rec l1)     = S.unions $ map (vars . snd) l1
vars (Proj _)     = S.empty

--set of variables in a type
typeVars :: T -> Set String
typeVars (TVar x)     = S.singleton x
typeVars (TArr t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars TUnit      = S.empty
typeVars (TRec t1)    = S.unions $ map (typeVars.snd) t1

--type vars in a term
typeVarsInTerm :: STerm -> Set String
typeVarsInTerm (Var x)      = S.singleton x
typeVarsInTerm (Abs _ t l1) = S.union (typeVars t) $ typeVarsInTerm l1
typeVarsInTerm (App l1 l2)  = S.union (typeVarsInTerm l1) (typeVarsInTerm l2)
typeVarsInTerm (Rec ls)     = S.unions $ map (typeVarsInTerm.snd) ls
typeVarsInTerm _              = S.empty

-- similar to type substitution but at the type level
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) (TVar y, t)
  | x == y            = t
  | otherwise         = l
typeSub (TArr t1 t2) c = TArr (typeSub t1 c) (typeSub t2 c)
typeSub (TRec t1) c = TRec $ map (\(v,t) -> (v,typeSub t c)) t1
typeSub TUnit _       = TUnit

-- function used to do type substitution on types in a term
tSubUnder :: STerm -> (T,T) -> STerm
tSubUnder l@(Var _) _    = l
tSubUnder (Abs x t l2) c = Abs x (typeSub t c) $ tSubUnder l2 c
tSubUnder (App l1 l2) c  = App (tSubUnder l1 c) (tSubUnder l2 c)
tSubUnder (Rec ls) c     = Rec $ map (\(v,l) -> (v, tSubUnder l c)) ls
tSubUnder l _            = l

--generates a fresh variable name for a term
newlabel :: STerm -> String
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

--rename t (x,y): renames free occurences of term variable x in t to y
rename :: STerm -> (String, String) -> STerm
rename (Var a) (x,y) = if a == x then Var y else Var a
rename l@(Abs a t l1) (x,y) = if a == x then l else Abs a t $ rename l1 (x, y)
rename (App l1 l2) c = App (rename l1 c) (rename l2 c)
rename (Rec l1) c = Rec $ map (\(v,t) -> (v, rename t c)) l1
rename (Proj x) _ = Proj x

--substitute one term for another in a term
--does capture avoiding substitution
substitute :: STerm -> (STerm, STerm) -> STerm
substitute l1@Unit _ = l1
substitute l1@(Var c1) (Var c2, l2)
  | c1 == c2 = l2
  | otherwise = l1
substitute (App l1 l2) c
  = App (substitute l1 c) (substitute l2 c)
substitute l@(Abs y t l1) c@(Var x, l2)
  | y == x = l
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise = Abs z t $ substitute (rename l1 (y,z)) c
  where z     = foldr1 biggest [newlabel l1, newlabel l2, newlabel (Var x)]
substitute (Rec l1) c = Rec $ map (\(v,t) -> (v, substitute t c)) l1
substitute (Proj x) _ = Proj x

--one-step reduction relation, TODO implement other reduction strats
reduce1 :: STerm -> Maybe STerm
reduce1 (Var _) = Nothing
reduce1 (Abs x t s) = do
  s' <- reduce1 s
  Just $ Abs x t s'
reduce1 (App (Abs x _ l') l2) =
  Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 (App l1 (Proj x)) = case l1 of
  Rec l2 -> L.lookup x l2
  _ -> Nothing
reduce1 (App l1 l2) = do
  case reduce1 l1 of
    Just l' -> Just $ App l' l2
    _ -> case reduce1 l2 of
      Just l' -> Just $ App l1 l'
      _ -> Nothing
reduce1 (Rec l1)
  | l1' == l1 = Nothing
  | otherwise = Just $ Rec l1'
  where
    l1' = f l1
    f [] = []
    f ((x, t):xs) = case reduce1 t of
      Just t' -> (x,t'):xs
      Nothing -> (x,t) :f xs
reduce1 Unit = Nothing

-- multi-step reduction relation 
-- NOT GUARANTEED TO TERMINATE if it doesn't type check
reduce :: STerm -> STerm
reduce t = maybe t reduce (reduce1 t)

---multi-step reduction relation that accumulates all reduction steps
reductions :: STerm -> [STerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []


