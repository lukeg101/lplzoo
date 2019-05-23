{-|
Module      : LF
Description : Deep-embedding of the Pure first-order dependent types in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "LF" module denotes the AST and operations for the Edinburgh Logical Framework. This language is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module LF where


-- Tool Imports.
import qualified Data.Map      as M
import qualified Data.Set      as S
import qualified Data.Maybe    as Maybe 
import qualified Control.Monad as C


-- | Kinds in LF, needed to distinguish proper types from type families
data K
  = KVar
  | KPi VarName T K
  deriving (Eq, Ord)


-- | Simple show instance for kinds
instance Show K where
  show KVar        = "*"
  show (KPi v t k)  
    = "\x3a0 " ++ v ++ ":" 
        ++ show t ++ "." ++ show k


-- | Helper function returns true if the term is a type family.
iskpi :: K -> Bool
iskpi KPi{} = True
iskpi _     = False


-- |  Types for LF, of the form X, Pi x:T.T or T t where t is a term.
data T 
  = TVar VarName
  | TNat
  | TVec
  | TAbs VarName T T
  | TPi VarName T T
  | TTerm LFTerm -- purely to make parsing easier, typing allows only T t
  | TApp T T
  deriving Ord --equivalence of types compares the trees of each type


-- | We do typed definitional equality with beta reduction here
-- as the language is stronly normalising and simple
instance Eq T where
  t1 == t2 = typeEquality (t1, t2) (M.empty, M.empty) 0


-- | Definitional equality on types with beta reduction
-- Each abstraction adds vars to the map and increments the id.
-- Variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS.
typeEquality :: (T, T) 
  -> (M.Map VarName Int, M.Map VarName Int) 
  -> Int 
  -> Bool
typeEquality (TVar x, TVar y) (m1, m2) _ 
  = let testEq = do a <- M.lookup x m1
                    b <- M.lookup y m2
                    return $ a == b
    in Maybe.fromMaybe (x == y) testEq
typeEquality (TNat, TNat) _ _
  = True
typeEquality (TVec, TVec) _ _ 
  = True
typeEquality (TApp a1 a2, TApp b1 b2) c n
  = typeEquality (a1,b1) c n && typeEquality (a2, b2) c n
typeEquality (TTerm t1, TTerm t2) _ _ 
  = let typeTest t = Maybe.isJust (typeof' t)
    in typeTest t1 && typeTest t2
       && termEquality (reduce t1, reduce t2) (M.empty, M.empty) 0
typeEquality (TPi x1 a1 a2, TPi x2 b1 b2) c@(m1, m2) s 
  = let newm1 = M.insert x1 s m1
        newm2 = M.insert x2 s m2
    in typeEquality (a1, b1) c s
       && typeEquality (a2, b2) (newm1, newm2) (s+1)
typeEquality (TAbs x1 a1 a2, TAbs x2 b1 b2) c@(m1, m2) s 
  = let newm1 = M.insert x1 s m1
        newm2 = M.insert x2 s m2
    in typeEquality (a1, b1) c s
       && typeEquality (a2, b2) (newm1, newm2) (s+1)
typeEquality _ _ _ = False


-- Note TTerm isn't formally in LF, but is rather the RHS of TApp

-- | Simple show instance
instance Show T where
  show (TVar x)        = x
  show TNat            = "Nat" -- added to make the calc useful
  show TVec            = "Vec"
  show ty@(TPi t t1 t2)  
    | isArr ty
      = paren (isArr t1 || isTAbs t1) (show t1) ++ "->" ++ show t2
    | otherwise 
      = "\x3a0 " ++ t ++ ":" ++ show t1 ++ "." ++ show t2
  show (TAbs t t1 t2)  
    = "\x03bb" ++ t ++ ":" ++ show t1 ++ "." ++ show t2
  show (TApp ty1 t1)
    = paren (isPi ty1 || isTAbs ty1) (show ty1)
        ++ ' ' :show t1
  show (TTerm t1)       
    = paren (isAbs t1 || isApp t1) (show t1)
  -- always on the RHS so we can put the test here 


-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | Helper function returns true if the term is a pi abstraction.
isPi :: T -> Bool
isPi TPi {} = True
isPi _      = False


-- | Helper function returns true if the term is an abstraction.
isTAbs :: T -> Bool
isTAbs TAbs {} = True
isTAbs _       = False


-- | Helper function returns true if the term is an non-dependent pi type.
isArr :: T -> Bool
isArr (TPi x _ t2) = x `notElem` typeVars t2
isArr _            = False


-- | LF Terms
-- variables are String
-- Abstractions carry the type Church style
-- Identical to STLC here
data LFTerm
  = Var VarName
  | Nat Int  -- added to make the calc useful
  | Succ
  | Nil      -- needed for vect types
  | Cons
  | Abs VarName T LFTerm
  | App LFTerm LFTerm
  deriving Ord


-- | Type Synonym for variable names in a term.
type VarName = String


-- | Simple show instance for LF/STLC
instance Show LFTerm where
  show (Var x)      
    = x
  show (Nat n)
    = show n
  show Succ
    = "succ"
  show Nil
    = "nil"
  show Cons
    = "cons"
  show (App t1 t2)  
    = paren (isAbs t1) (show t1) 
        ++ ' ' : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t1 l1) 
    = "\x03bb" ++ x ++ ":" 
        ++ show t1
        ++ "." ++ show l1


-- | Helper function returns true if the term is an abstraction.
isAbs :: LFTerm -> Bool
isAbs Abs{} = True
isAbs _     = False


-- | Helper function returns true if the term is an application.
isApp :: LFTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | alpha equivalence of terms using syntactic term equality
instance Eq LFTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0


-- | Checks for equality of terms, has a map (term, id) for each variable.
-- Each abstraction adds vars to the map and increments the id.
-- Variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS.
termEquality :: (LFTerm, LFTerm) 
             -> (M.Map VarName Int, M.Map VarName Int) 
             -> Int 
             -> Bool
termEquality (Nat x, Nat y) _ _ = x == y
termEquality (Succ, Succ) _ _   = True
termEquality (Nil, Nil) _ _     = True
termEquality (Cons, Cons) _ _   = True
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


-- | Type context of term variables and kinds
--input term or type variable as a String
--if the input is a term, return Left its type
--if the input is a type, return Right its kind
type Context = M.Map String (Either T K)


-- | Typing derivation for a term in a given context
-- * Just T denotes successful type derivation 
-- * Nothing denotes failure to type the term (not in LF)
typeof :: LFTerm -> Context -> Maybe T
typeof (Nat _) _
  = return TNat
typeof Succ _
  = return $ TPi "_" TNat TNat
typeof (Var v) ctx 
  = do (Left t) <- M.lookup v ctx
       C.guard (kindof t ctx == Just KVar)
       return t
typeof Nil _ -- this is first order LF: no polymorphism ([Nat] only)
  = return $ TApp (TApp TVec TNat) (TTerm (Nat 0))
typeof Cons _
  = let vecn  = TApp (TApp TVec TNat) (TVar "n")
        vecn' = TApp (TApp TVec TNat) (TTerm (App Succ (Var "n")))
    in return $ TPi "n" TNat $ TPi "_" TNat $ TPi "_ " vecn vecn'
typeof (Abs x ty1 l1) ctx
  = do KVar <- kindof ty1 ctx
       ty2 <- typeof l1 (M.insert x (Left ty1) ctx)
       return $ TPi x ty1 ty2
typeof (App Succ l2) ctx
  = do TNat <- typeof l2 ctx
       return TNat
typeof (App l1 l2) ctx 
  = do t1 <- typeof l2 ctx
       case typeof l1 ctx of
         Just (TPi x t2 t3) -> do C.guard $ t1 == t2 
                                  return $ subTermInType t3 (Var x, l2)   
         _                  -> Nothing


-- | Top level typing function providing empty context
typeof' l = typeof l M.empty


-- | Kinding derivation
-- identical to typing but at the type level
kindof :: T -> Context -> Maybe K
kindof TNat _     
  = Just KVar
kindof (TVar v) ctx 
  = do (Left k) <- M.lookup v ctx
       kindof k ctx
kindof TVec ctx
  = kindof (TAbs "_" TNat $ TAbs "_" TNat TNat) ctx
kindof (TPi x t1 t2) ctx
  = do KVar <- kindof t1 ctx
       KVar <- kindof t2 (M.insert x (Left t1) ctx)
       return KVar
kindof (TAbs x ty1 ty2) ctx
  = KPi x ty1 <$> kindof ty2 (M.insert x (Left ty1) ctx)
kindof (TApp ty1 ty2) ctx
  = do (KPi x ty3 k1) <- kindof ty1 ctx
       ty2' <- case ty2 of
                 TTerm x -> typeof x ctx
                 _       -> return ty2
       C.guard (ty2' == ty3)
       kindof ty2' ctx
       return $ subTypeInKind k1 (TVar x, ty2')   
kindof (TTerm t1) ctx
  = do t <- typeof t1 ctx
       kindof t ctx


-- | Top-level kinding function for types
kindof' t = kindof t M.empty


-- | Function takes a type, and substitutes term x wherever y
-- is expected
subTermInType :: T -> (LFTerm, LFTerm) -> T
subTermInType l@(TVar x) (Var y, z)
  | x == y    = TTerm z
  | otherwise = l
subTermInType l@(TPi x t1 t2) c@(Var y, _)
  | x /= y    = TPi x (subTermInType t1 c) (subTermInType t2 c)
  | otherwise = l
subTermInType l@(TAbs x t1 t2) c@(Var y, _)
  | x /= y    = TAbs x (subTermInType t1 c) (subTermInType t2 c)
  | otherwise = l
subTermInType (TApp ty1 t1) c 
  = TApp (subTermInType ty1 c) (subTermInType t1 c)
subTermInType (TTerm t1) c 
  = TTerm $ subTermInTerm t1 c
subTermInType l _ = l


-- | Function used to do type substitution on types in a term
subTypeInTerm :: LFTerm -> (T,T) -> LFTerm
subTypeInTerm l@(Var x) (TVar y, TTerm l2)
  | x == y    = l2
  | otherwise = l
subTypeInTerm l@(Nat _) _ = l
subTypeInTerm l@Succ    _ = l
subTypeInTerm l@Nil     _ = l
subTypeInTerm l@Cons    _ = l
subTypeInTerm (Abs x t l2) c 
  = Abs x (subTypeInType t c) $ subTypeInTerm l2 c
subTypeInTerm (App l1 l2) c  
  = App (subTypeInTerm l1 c) (subTypeInTerm l2 c)


-- | Term substitution in a term
-- does capture avoiding substitution (Berendregt)
subTermInTerm :: LFTerm -> (LFTerm, LFTerm) -> LFTerm
subTermInTerm l1@(Var c1) (Var c2, l2) 
  = if c1 == c2 
      then l2 
      else l1 
subTermInTerm (App l1 l2) c 
  = App (subTermInTerm l1 c) (subTermInTerm l2 c)
subTermInTerm (Abs y t l1) c@(Var x, l2)
  | y == x         = Abs y t l1
  | y `notfree` l2 = Abs y t $ subTermInTerm l1 c
  | otherwise      = Abs z t $ subTermInTerm (rename l1 (y,z)) c
  where z          = max (newlabel l1) (newlabel l2)
subTermInTerm s _  = s


-- | Type substitution in a type
subTypeInType :: T -> (T, T) -> T
subTypeInType l@(TVar x) (TVar y, t) 
  | x == y             = t
  | otherwise          = l
subTypeInType l@TNat _ = l
subTypeInType l@TVec _ = l
subTypeInType (TPi x t1 t2) c 
  = TPi x (subTypeInType t1 c) (subTypeInType t2 c)
subTypeInType (TAbs x t1 t2) c 
  = TAbs x (subTypeInType t1 c) (subTypeInType t2 c)
subTypeInType (TApp t1 t2) c 
  = TApp (subTypeInType t1 c) (subTypeInType t2 c)
subTypeInType (TTerm t1) c
  = TTerm $ subTypeInTerm t1 c
subTypeInType l _      = l


-- | Function substitutes types in kinds
-- in LF we only have concrete types and type families
-- so this is equivalent to type substition one level up
subTypeInKind :: K -> (T, T) -> K
subTypeInKind KVar _        = KVar
subTypeInKind l@(KPi x ty1 k) c@(TVar y, _)
  | x /= y        = KPi x ty1 $ subTypeInKind k c
  | otherwise     = l
subTypeInKind l _ = l


-- In the future, all variations of subXinY would be solved
-- by Pure Type Systems and a clever parser.


-- | Function returns a set of bound variables of a term.
bound :: LFTerm -> S.Set VarName
bound (Var _)      = S.empty
bound Nil          = S.empty
bound Cons         = S.empty
bound (Nat _)      = S.empty
bound Succ         = S.empty
bound (Abs n _ l1) = S.insert n $ bound l1
bound (App l1 l2)  = S.union (bound l1) (bound l2)


-- | Function returns a set of free variables of a term.
free :: LFTerm -> S.Set VarName
free (Var n)      = S.singleton n
free Nil          = S.empty
free Cons         = S.empty
free (Nat _)      = S.empty
free Succ         = S.empty
free (Abs n _ l1) = S.delete n (free l1)
free (App l1 l2)  = S.union (free l1) (free l2)


-- | Function tests to see if a term is closed (has no free vars).
closed :: LFTerm -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: LFTerm -> S.Set LFTerm
sub l@(Var _)      = S.singleton l
sub l@Nil          = S.singleton l
sub l@Cons         = S.singleton l
sub l@(Nat _)      = S.singleton l
sub l@Succ         = S.singleton l
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)


-- | Function determines if a variable is bound in a term.
notfree :: VarName -> LFTerm -> Bool
notfree x = not . S.member x . free 


-- | Function returns the set of variables in a term.
vars :: LFTerm -> S.Set VarName
vars (Var x)      = S.singleton x
vars Nil          = S.empty
vars Cons         = S.empty
vars (Nat _)      = S.empty
vars Succ         = S.empty
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1


-- | Set of type variables in a type
typeVars :: T -> S.Set VarName
typeVars TNat           = S.empty
typeVars (TVar x)       = S.singleton x
typeVars TVec           = S.empty
typeVars (TPi _ t1 t2)  = S.union (typeVars t1) (typeVars t2)
typeVars (TAbs _ t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (TApp t1 t2)   = S.union (typeVars t1) (typeVars t2)
typeVars (TTerm t1)     = typeVarsInTerm t1


-- | Type vars in a term
typeVarsInTerm :: LFTerm -> S.Set VarName
typeVarsInTerm (Var x)      = S.singleton x
typeVarsInTerm Nil          = S.empty
typeVarsInTerm Cons         = S.empty
typeVarsInTerm (Nat _)      = S.empty
typeVarsInTerm Succ         = S.empty
typeVarsInTerm (Abs _ t l1) = S.union (typeVars t) $ typeVarsInTerm l1
typeVarsInTerm (App l1 l2)  = S.union (typeVarsInTerm l1) (typeVarsInTerm l2)


-- | Function generates a fresh variable name for a term.
newlabel :: LFTerm -> VarName
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
rename :: LFTerm -> (VarName, VarName) -> LFTerm
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
rename l _ = l


-- | One-step reduction relation on terms.
reduce1 :: LFTerm -> Maybe LFTerm 
reduce1 (Nat _) = Nothing
reduce1 Succ    = Nothing
reduce1 (Var _) = Nothing
reduce1 Nil     = Nothing
reduce1 Cons    = Nothing
reduce1 (Abs x t s)
  = case reduce1 s of 
      Just s' -> Just $ Abs x t s'
      _       -> case reduce1T t of
                   Just t' -> Just $ Abs x t' s
                   _       -> Nothing
reduce1 (App (Abs x _ l1) l2) --beta conversion
  = Just $ subTermInTerm l1 (Var x, l2)  
reduce1 (App Succ n) 
  = case reduce1 n of
    Just n' -> Just $ App Succ n'
    _       -> case n of 
      (Nat x) -> Just (Nat (x+1))
      _       -> Nothing
reduce1 (App l1 l2) 
  = case reduce1 l1 of 
      Just l' -> Just $ App l' l2
      _       -> App l1 <$> reduce1 l2


-- | One-step reduction relation for types
-- This is reduce1 for STLC but one level up
reduce1T :: T -> Maybe T
reduce1T TNat      = Nothing
reduce1T (TVar _)  = Nothing
reduce1T TVec      = Nothing
reduce1T (TTerm t) = TTerm <$> reduce1 t
reduce1T (TPi x t1 t2) 
  = case reduce1T t1 of
      Just t1' -> Just $ TPi x t1' t2
      _        -> TPi x t1 <$> reduce1T t2
reduce1T (TApp (TAbs x _ l1) l2) --beta conversion
  = Just $ subTypeInType l1 (TVar x, l2)  
reduce1T (TAbs x t1 t2) 
  = case reduce1T t1 of
      Just t1' -> Just $ TAbs x t1' t2
      _        -> TAbs x t1 <$> reduce1T t2
reduce1T (TApp t1 t2) 
  = case reduce1T t1 of
      Just t1' -> Just $ TApp t1' t2
      _        -> TApp t1 <$> reduce1T t2


-- | Multi-step type reduction relation.
-- NOT GUARANTEED TO TERMINATE if typing/kinding fails
reduceT :: T -> T
reduceT t = case reduce1T t of 
    Just t' -> reduceT t'
    Nothing -> t


-- | Multi-step reduction relation.
-- NOT GUARANTEED TO TERMINATE if typing fails
reduce :: LFTerm -> LFTerm
reduce t = case reduce1 t of 
    Just t' -> reduce t'
    Nothing -> t


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reductions :: LFTerm -> [LFTerm]
reductions t = case reduce1 t of
    Just t' -> t' : reductions t'
    _       -> []

