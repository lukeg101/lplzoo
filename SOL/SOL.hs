{-|
Module      : SOL
Description : Deep-embedding of SOL in Haskell.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "SOL" module denotes the AST of SOL. This languages is implemented as a deep embedding into Haskell, with all the canonical helper functions it needs.
-}
module SOL where

-- Tool imports.
import qualified Data.Map.Lazy as M
import qualified Data.Maybe    as Maybe
import qualified Data.Set      as S
import qualified Control.Monad as Monad
import qualified Data.List     as L


-- | SOL Types, both term and type variables are Strings
-- The forall type take types a type variable and type T. 
-- existentials take a type and type var
-- products and (disjoint) unions are what you'd expect
-- Record types are added too to make the module/ADT types useful
data T 
  = TVar VarName
  | TArr T T
  | TNat
  | TBool
  | TForall VarName T
  | TExists VarName T
  | TRec [(String,T)]
  | TProd T T
  | TSum T T
  deriving Ord


-- | Type Synonym for variable names in a term.
type VarName = String


-- | Does syntactic type equality on types
instance Eq T where
  t1 == t2 = typeEquality (t1, t2) (M.empty, M.empty) 0


-- | Checks for equality of Types, has a map (term, id) for each type variable.
-- Each Pi type adds vars to the map and increments the id.
-- Variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 == true then False 
-- application recursively checks both the LHS and RHS.
typeEquality :: (T, T) 
  -> (M.Map (Either VarName VarName) Int, M.Map (Either VarName VarName) Int) 
  -> Int 
  -> Bool
typeEquality (TVar x, TVar y) (m1, m2) _ 
  = let testEq = do a <- M.lookup (Right x) m1
                    b <- M.lookup (Right y) m2
                    return $ a == b
    in Maybe.fromMaybe (x == y) testEq
typeEquality (TArr a1 b1, TArr a2 b2) c s 
  = typeEquality (a1, a2) c s && typeEquality (b1, b2) c s
typeEquality (TForall x1 t1, TForall x2 t2) (m1, m2) s 
  = let newm1 = M.insert (Right x1) s m1
        newm2 = M.insert (Right x2) s m2
    in typeEquality (t1, t2) (newm1, newm2) (s+1)
typeEquality (TExists x1 t1, TExists x2 t2) (m1, m2) s 
  = let newm1 = M.insert (Right x1) s m1
        newm2 = M.insert (Right x2) s m2
    in typeEquality (t1, t2) (newm1, newm2) (s+1)
typeEquality (TNat, TNat)   _ _ = True
typeEquality (TBool, TBool) _ _ = True
typeEquality (TProd a1 a2, TProd b1 b2) c s 
  = typeEquality (a1, b1) c s && typeEquality (a2, b2) c s
typeEquality (TSum a1 a2, TSum b1 b2) c s 
  = typeEquality (a1, a2) c s && typeEquality (b1, b2) c s
typeEquality (TRec r1, TRec r2) _ _ 
  = r1 == r2 -- in an ideal world we don't care about record order.
typeEquality _ _ _ = False


-- | Show instance for Types, following bracketing convention
-- uses Unicode for universal/existential types
-- uses bracketing convention for types
instance Show T where
  show TNat        = "Nat"
  show TBool       = "Bool"
  show (TVar c)    = c
  show (TArr a b)  
    = paren (isArr a || isPi a) (show a) ++ "->" ++ show b
  show (TForall t1 t2)  
    = "\x2200" ++ t1 ++ "." ++ show t2
  show (TExists t1 t2)  
    = "\x2203" ++ t1 ++ "." ++ show t2
  show (TProd a b)= paren (isArr a) (show a)
    ++ " \x00D7 " ++ paren (isArr b) (show b)
  show (TSum a b) = paren (isTProd a || isArr a) (show a)
    ++ " + " ++ paren (isTProd b || isArr b) (show b)
  show (TRec xs) = wparen . concat $ 
    L.intersperse ", " $ map (\(v,t)->v ++ ":" ++ show t) xs



-- | Helper function encloses the input string in (parens) if the
-- first parameter is true.
paren :: Bool -> String -> String
paren True  x = "(" ++ x ++ ")"
paren False x = x


-- | Function wraps string in curly parens
wparen :: String -> String
wparen x = "{" ++ x ++ "}"


-- | Helper function returns true if the term is an arrow type.
isArr :: T -> Bool
isArr (TArr _ _) = True
isArr _          = False


-- | Helper function returns true if the term is an pi type.
isPi :: T -> Bool
isPi (TForall _ _) = True
isPi _             = False


-- | Helper function returns true if the term is a product
isTProd :: T -> Bool
isTProd (TProd _ _) = True
isTProd _           = False


-- | SOL Terms
-- variables are Strings
-- Abstractions carry the type Church style
-- foralls/exists carry term variables as strings
data SOLTerm
  = Var VarName
  | Nat Int  -- Add prims to demo the records idea
  | BTrue
  | BFalse
  | If
  | Typ T
  | Abs VarName T SOLTerm
  | App SOLTerm SOLTerm
  | Forall VarName SOLTerm
  | Pack T SOLTerm T        -- pack {T,t} as T
  | Unpack VarName VarName SOLTerm SOLTerm    -- unpack {T,t} = t in t
  | Rec [(String ,SOLTerm)] -- records
  | Proj VarName
  | Case   --case analysis of sums, like case in haskell
  | Inl T  --left injection
  | Inr T  --right injection
  | Prod   --2 term pair
  | Prj1   --left projection from pair
  | Prj2   --right projection
  deriving Ord


-- | Alpha equivalence of terms, same as STLC
instance Eq SOLTerm where
  t1 == t2 = termEquality (t1, t2) (M.empty, M.empty) 0


-- | Determines syntactic alpha-equivalence of terms
-- has maps (either term, id) for each term/type variable
-- each TERM abstraction adds (left var) to the map and increments the id
-- each TYPE abstraction adds (right var) to the map and increments the id
-- also checks that each term is identical
-- variable occurrence checks for ocurrences in t1 and t2 using the logic:
-- * if both bound, check that s is same in both maps
-- * if neither is bound, check literal equality 
-- * if bound t1 XOR bound t2 then False 
-- application recursively checks both the LHS and RHS
-- Type equality is called for types
termEquality :: (SOLTerm, SOLTerm) 
  -> (M.Map (Either VarName VarName) Int, M.Map (Either VarName VarName) Int) 
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
termEquality (App a1 b1, App a2 b2) c s 
  = termEquality (a1, a2) c s && termEquality (b1, b2) c s
termEquality (Typ t1, Typ t2) c s 
  = typeEquality (t1, t2) c s
termEquality (Forall x1 t1, Forall x2 t2) (m1,m2) s
  = let newm1 = M.insert (Right x1) s m1
        newm2 = M.insert (Right x2) s m2
    in termEquality (t1, t2) (newm1, newm2) s
termEquality (Pack a1 t1 a2, Pack b1 t2 b2) c s
  = typeEquality (a1, b1) c s
      && typeEquality (a2, b2) c s
      && termEquality (t1, t2) c s
termEquality (Unpack x1 x2 a1 a2, Unpack y1 y2 b1 b2) (m1,m2) s
  = let newm1 = let m1' = M.insert (Left x2) s m1
                in M.insert (Right x1) (s+1) m1'
        newm2 = let m2' = M.insert (Left y2) s m2
                in M.insert (Right y1) (s+1) m2'
    in termEquality (a2, b2) (newm1, newm2) (s+2)
         && termEquality (a1, b1) (m1, m2) (s+2)
termEquality (Rec l1, Rec l2) c s 
  = let counts = map length . L.group . L.sort $
          [v1 | (v1,t1) <- l1, (_,t2) <- l2, termEquality (t1, t2) c s]
    in length l1 == length l2
         && all (>0) counts 
         && length counts == length l1
termEquality (Proj x, Proj y) _ _ = x == y
termEquality (Prod, Prod) _ _     = True
termEquality (Prj1, Prj1) _ _     = True
termEquality (Prj2, Prj2) _ _     = True
termEquality (Inl t1, Inl t2) _ _ = t1 == t2
termEquality (Inr t1, Inr t2) _ _ = t1 == t2
termEquality (Case, Case) _ _     = True
termEquality (BTrue, BTrue) _ _   = True
termEquality (BFalse, BFalse) _ _ = True
termEquality (Nat x, Nat y) _ _   = x == y
termEquality (If, If) _ _         = True
termEquality _ _ _                = False


-- | Show implementation for SOL terms
-- uses bracketing convention for terms
instance Show SOLTerm where
  show (Var x)      = x
  show BTrue        = "true"
  show BFalse       = "false"
  show If           = "if"
  show (Nat n)      = show n
  show (Typ t)      = "[" ++ show t ++"]"
  show (App l1 (Proj x)) = show l1 ++ "." ++ x
  show (App (App Prod a) b) = paren True (show a ++ ", " ++ show b)
  show (App (Inl t) a) = "inl " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a) ++ ":" ++ show t
  show (App (Inr t) a) = "inr " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a) ++ ":" ++ show t --left of sum
  show (App Prj1 a) = "\x03C0" ++ "1 " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a)
  show (App Prj2 a) = "\x03C0" ++ "2 " ++
    paren (isAbs a || isApp a || isSum a || isProd a) (show a)
  show (App t1 t2)  = 
    paren (isAbs t1 || isForall t1 ) (show t1) ++ ' ' 
      : paren (isAbs t2 || isApp t2) (show t2)
  show (Abs x t l1) 
    = "\x03bb" ++ x ++ ":" ++ show t ++ "." ++ show l1
  show (Forall t l1) 
    = "\x39b" ++ t ++ "." ++ show l1
  show (Pack t1 t t2)
    = let pair = wparen . concat $ L.intersperse ", " [show t1, show t]
      in "pack " ++ pair ++ " as " ++ show t2
  show (Unpack t1 t t2 t3) 
    = let pair = wparen . concat $ L.intersperse ", " [show t1, show t]
      in  "unpack " ++ pair ++ " = " ++ show t2 ++ " in " ++ show t3 
  show (Rec xs) = wparen . concat $ 
    L.intersperse ", " $ map (\(v,t)->v ++ "=" ++ show t) xs
  show (Inl _)      = "inl" --above should handle this case
  show (Inr _)      = "inr" --above should handle this case
  show Prj1         = "\x03C0" ++ "1" --above should handle this case
  show Prj2         = "\x03C0" ++ "2" --above should handle this case
  show Case         = "case" --above shuld handle this case
  show Prod         = "prod" --above should handle this case
  show (Proj _)     = "proj" --above should handle this case


-- | Helper function returns true if the term is an abstraction.
isAbs :: SOLTerm -> Bool
isAbs Abs{} = True
isAbs _     = False


-- | Helper function returns true if the term is an application.
isApp :: SOLTerm -> Bool
isApp (App _ _) = True
isApp _         = False


-- | Helper function returns true if the term is a Forall.
isForall :: SOLTerm -> Bool
isForall (Forall _ _) = True
isForall _            = False


-- | Helper function returns true if term is a sum type
isSum :: SOLTerm -> Bool
isSum (App (Inl _) _) = True
isSum (App (Inr _) _) = True
isSum _               = False


-- | Helper function returns true if term is a prod type
isProd :: SOLTerm -> Bool
isProd Prod = True
isProd _    = False


-- | Type context of term variables and type variables
--input term or type variable as a String
-- * if the input is a term variable, return Just its type
-- * if the input is a type variable, return Nothing (isomorphic to 'Type')
type Context = M.Map VarName (Maybe T)


-- | Typing derivation for a term in a given context
-- * Just T denotes successful type derivation 
-- * Nothing denotes failure to type the term (not valid in SOL)
-- see how the context works to make sense of case statements
typeof :: SOLTerm -> Context -> Maybe T
typeof (Nat _) _ = return TNat
typeof BTrue   _ = return TBool
typeof BFalse  _ = return TBool 
typeof (App (App (App If l2) l3) l4) ctx 
  = case typeof l2 ctx of
      Just TBool -> do t3 <- typeof l3 ctx
                       t4 <- typeof l4 ctx
                       Monad.guard (t3 == t4)
                       Just t3
      _          -> Nothing
typeof (Var v) ctx 
  = Monad.join $ M.lookup v ctx
typeof (Typ _) _ 
  = Nothing
typeof (Abs x t l1) ctx 
  = do let ctx' = M.insert x (Just t) ctx
       Monad.guard (all (\v -> M.lookup v ctx' == Just Nothing) (freeTVars t))
       TArr t <$> typeof l1 (M.insert x (Just t) ctx)
typeof (App (Rec l1) (Proj x)) ctx -- projecting from a record directly
  = case L.lookup x l1 of
      Just t -> typeof t ctx
      _      -> Nothing
typeof (App (Var v) (Proj x)) ctx  -- projecting from a variable
  = case M.lookup v ctx of 
      Just (Just (TRec l1)) -> L.lookup x l1
      _                     -> Nothing
typeof (App (App Prod l1) l2) ctx 
  = do t1 <- typeof l1 ctx
       t2 <- typeof l2 ctx
       return $ TProd t1 t2
typeof (App Prj1 l1) ctx 
  = do (TProd _ t2) <- typeof l1 ctx
       return t2
typeof (App Prj2 l1) ctx 
  = do (TProd t1 _) <- typeof l1 ctx
       return t1
typeof (App (Inl (TSum t1 t2)) l1) ctx 
  = do t3 <- typeof l1 ctx
       Monad.guard (t1 == t3)
       return $ TSum t1 t2
typeof (App (Inr (TSum t1 t2)) l1) ctx 
  = do t3 <- typeof l1 ctx
       Monad.guard (t2 == t3)
       return $ TSum t1 t2
typeof (App (App (App Case l1) l2) l3) ctx 
  = do t1 <- typeof l1 ctx
       t2 <- typeof l2 ctx
       t3 <- typeof l3 ctx
       case (t1,t2,t3) of
         (TSum t4 t5,TArr t6 t7, TArr t8 t9) ->
           do Monad.guard (t7 == t9)
              Monad.guard (t4 == t6)
              Monad.guard (t5 == t8)
              return t7
         _ -> Nothing
typeof (App l1 l2) ctx 
  = case typeof l1 ctx of
      Just (TArr t2 t3)  -> do t1 <- typeof l2 ctx
                               Monad.guard (t1 == t2)
                               Just t3
      -- type substitution under 2nd-order abstraction
      Just (TForall x t) -> case l2 of
                              Typ a -> Just $ typeSub t (TVar x, a) 
                              _     -> Nothing
      _                 -> Nothing
typeof (Forall t l1) ctx 
  = TForall t <$> typeof l1 (M.insert t Nothing ctx)
typeof (Pack t1 t (TExists v t2)) ctx
  = do t2' <- typeof t ctx
       Monad.guard (t2' == typeSub t2 (TVar v, t1))
       return $ TExists v t2
typeof (Unpack x v t1 t2) ctx 
  = do (TExists _ ty1) <- typeof t1 ctx
       let ctx1 = M.insert v Nothing ctx
       let ctx2 = M.insert x (Just ty1) ctx1
       typeof t2 $ M.insert v (Just ty1) ctx2
typeof (Rec l1) ctx 
  = let fieldType (l, t) = do t' <- typeof t ctx
                              return (l, t')
    in TRec <$> mapM fieldType l1
typeof _ _ = Nothing

 
-- | Top level typing function providing empty context
typeof' l = typeof l M.empty
 

-- | Function returns the set of free type variables in a type.
freeTVars :: T -> S.Set VarName
freeTVars (TProd t1 t2)  = S.union (freeTVars t1) (freeTVars t2)
freeTVars (TSum t1 t2)   = S.union (freeTVars t1) (freeTVars t2)
freeTVars (TVar c)       = S.singleton c
freeTVars (TArr t1 t2)   = S.union (freeTVars t1) (freeTVars t2)
freeTVars (TRec ts)      = S.unions $ map (freeTVars . snd) ts 
freeTVars (TForall x t1) = S.delete x (freeTVars t1)
freeTVars (TExists x t1) = S.delete x (freeTVars t1)
freeTVars _              = S.empty


-- | Similar to term substitution but at the type level
typeSub :: T -> (T, T) -> T
typeSub l@(TVar x) (TVar y,z) 
  = if x == y
      then z
      else l
typeSub (TArr t1 t2) c 
  = TArr (typeSub t1 c) (typeSub t2 c)
typeSub (TProd t1 t2) c 
  = TProd (typeSub t1 c) (typeSub t2 c) 
typeSub (TSum t1 t2) c 
  = TSum (typeSub t1 c) (typeSub t2 c)
typeSub (TRec t1) c 
  = TRec $ map (\(v,t) -> (v,typeSub t c)) t1
typeSub l@(TForall x t) c@(TVar y, z)
  | x == y         = l
  | x `notfreeT` z = TForall x $ typeSub t c
  | otherwise      = TForall n $ typeSub (renameT t (x, n)) c
  where n = max (newTLabel t) (newTLabel z)
typeSub l@(TExists x t) c@(TVar y, z)
  | x == y         = l
  | x `notfreeT` z = TExists x $ typeSub t c
  | otherwise      = TExists n $ typeSub (renameT t (x, n)) c
  where n = max (newTLabel t) (newTLabel z)
typeSub t _ = t


-- | Function determines if a type variable is bound in a term.
notfreeT :: VarName -> T -> Bool
notfreeT x = not . S.member x . freeTVars


-- | Function generates a fresh type variable name for a term.
newTLabel :: T -> VarName
newTLabel x = head . dropWhile (`elem` typeVars x) 
  $ iterate genVar $  S.foldr biggest "" $ typeVars x


-- | Function returns the set of variables in a term.
typeVars :: T -> S.Set VarName
typeVars (TProd t1 t2) = S.union (typeVars t1) (typeVars t2)
typeVars (TSum t1 t2)  = S.union (typeVars t1) (typeVars t2)
typeVars (TVar x)      = S.singleton x
typeVars (TArr t1 t2)  = S.union (typeVars t1) (typeVars t2)
typeVars (TForall x t) = S.insert x $ typeVars t
typeVars (TExists x t) = S.insert x $ typeVars t
typeVars (TRec t1)     = S.unions $ map (typeVars.snd) t1
typeVars _             = S.empty


-- | Function renameds type variables 
-- rename t (x,y): renames free occurences of 
-- type variables x in t to y
renameT :: T -> (VarName, VarName) -> T
renameT (TVar a) (x,y) 
  = if a == x 
      then TVar y 
      else TVar a
renameT l@(TForall a t) c@(x,_) 
  = if a == x 
      then l 
      else TForall a $ renameT t c
renameT l@(TExists a t) c@(x,_) 
  = if a == x 
      then l 
      else TExists a $ renameT t c
renameT (TArr t1 t2) c 
  = TArr (renameT t1 c) (renameT t2 c)
renameT (TProd t1 t2) c
  = TProd (renameT t1 c) (renameT t2 c)
renameT (TSum t1 t2) c
  = TSum (renameT t1 c) (renameT t2 c)
renameT (TRec ts) c
  = TRec $ map (\(n,t)->(n, renameT t c)) ts
renameT t _ = t


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


-- | Function returns the set of type variables in a term.
typeVarsInTerm :: SOLTerm -> S.Set VarName
typeVarsInTerm (Rec rs)      = S.unions $ map (typeVarsInTerm . snd) rs
typeVarsInTerm (Inl t1)      = typeVars t1
typeVarsInTerm (Inr t1)      = typeVars t1
typeVarsInTerm (Abs _ t l1)  = S.union (typeVars t) $ typeVarsInTerm l1
typeVarsInTerm (App l1 l2)   = S.union (typeVarsInTerm l1) (typeVarsInTerm l2)
typeVarsInTerm (Typ t)       = typeVars t
typeVarsInTerm (Forall x t)  = S.insert x $ typeVarsInTerm t
typeVarsInTerm (Pack x t ty)  
  = S.unions [typeVars x, typeVarsInTerm t, typeVars ty]
typeVarsInTerm (Unpack _ _ t1 t2) 
  = S.union (typeVarsInTerm t1) (typeVarsInTerm t2)
typeVarsInTerm _             = S.empty


-- | Function returns a set of bound variables of a term.
bound :: SOLTerm -> S.Set VarName
bound (Rec rs)         = S.unions $ map (bound . snd) rs
bound (Abs n _ l1)     = S.insert n $ bound l1
bound (App l1 l2)      = S.union (bound l1) (bound l2)
bound (Forall _ t)     = bound t
bound (Pack _ t _)     = bound t
bound (Unpack _ x _ t) = S.insert x (bound t)
bound _                = S.empty


-- | Function returns a set of free variables of a term.
free :: SOLTerm -> S.Set VarName
free (Var n)          = S.singleton n
free (Rec l1)         = S.unions $ map (free . snd) l1
free (Abs n _ l1)     = S.delete n (free l1)
free (App l1 l2)      = S.union (free l1) (free l2)
free (Forall _ t)     = free t
free (Pack _ t _)     = free t
free (Unpack _ x _ t) = S.delete x (free t)
free _                = S.empty


-- | Function tests to see if a term is closed (has no free vars).
closed :: SOLTerm -> Bool
closed = S.null . free


-- | Function returns the set of subterms of a term.
sub :: SOLTerm -> S.Set SOLTerm
sub l@(Rec l1)     = S.insert l $ S.unions $ map (sub . snd) l1
sub l@(Abs _ _ l1) = S.insert l $ sub l1
sub l@(App l1 l2)  = S.insert l $ S.union (sub l1) (sub l2)
sub l@(Forall _ t) = S.insert l $ sub t
sub l@(Pack _ t _) = S.insert l $ sub t
sub l@(Unpack _ _ t1 t2) 
  = S.insert l $ S.unions [sub t1, sub t2] 
sub l              = S.singleton l


-- | Function determines if a variable is bound in a term.
notfree :: VarName -> SOLTerm -> Bool
notfree x = not . S.member x . free 


-- | Function returns the set of variables in a term.
vars :: SOLTerm -> S.Set VarName
vars (Var x)      = S.singleton x
vars (App t1 t2)  = S.union (vars t1) (vars t2)
vars (Abs x _ l1) = S.insert x $ vars l1
vars (Forall _ t) = vars t
vars (Pack _ t _) = vars t
vars (Unpack _ x t1 t2)
  = S.insert x $ S.unions [vars t1, vars t2]
vars (Rec l1)     = S.unions $ map (vars . snd) l1
vars _            = S.empty


-- | Function generates a fresh variable name for a term.
newlabel :: SOLTerm -> VarName
newlabel x = head . dropWhile (`elem` vars x) 
  $ iterate genVar $  S.foldr biggest "" $ vars x


-- | Function renames a term.
--rename t (x,y) renames free occurrences of x in t to y
rename :: SOLTerm -> (VarName, VarName) -> SOLTerm
rename (Rec l1) c 
  = Rec $ map (\(v,t) -> (v, rename t c)) l1
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
rename (Forall x t) c 
  = Forall x $ rename t c
rename (Pack ty1 t ty2) c
  = Pack ty1 (rename t c) ty2
rename (Unpack x y t1 t2) c
  = Unpack x y (rename t1 c) (rename t2 c)
rename l _ = l


-- | Function substitutes one term for another in a term
-- does capture avoiding substitution (Berendregt)
substitute :: SOLTerm -> (SOLTerm, SOLTerm) -> SOLTerm
substitute l1@(Var c1) (Var c2, l2) 
  = if c1 == c2 
      then l2 
      else l1 
substitute l1@(Typ (TVar x)) (Var y, l2) 
  = if x == y
      then l2
      else l1
substitute (App l1 l2) c 
  = App (substitute l1 c) (substitute l2 c)
substitute l@(Abs y t l1) c@(Var x, l2)
  | y == x         = l
  | y `notfree` l2 = Abs y t $ substitute l1 c
  | otherwise      = Abs z t $ substitute (rename l1 (y,z)) c
  where z          = foldr1 biggest [newlabel l1, newlabel l2, newlabel (Var x)]
substitute (Forall x t) c@(Var y, _)
  | x /= y         = Forall x $ substitute t c
substitute (Rec l1) c 
  = Rec $ map (\(v,t) -> (v, substitute t c)) l1
substitute (Pack x t y) c 
  = Pack x (substitute t c) y
substitute (Unpack x y t1 t2) c
  = Unpack x y (substitute t1 c) t2
substitute l _ = l


-- | One-step reduction relation on terms.
reduce1 :: SOLTerm -> Maybe SOLTerm 
reduce1 (Abs x t s)
  = Abs x t <$> reduce1 s
reduce1 (App (Abs x _ l') l2) 
  = Just $ substitute l' (Var x, l2)  --beta conversion
reduce1 (App (Rec l1) (Proj x)) 
  = L.lookup x l1
reduce1 (App (App Prod l1) l2)
  = case reduce1 l1 of
      Just l1' -> Just $ App (App Prod l1') l2
      _        -> case reduce1 l2 of
                    Just l2' -> Just $ App (App Prod l1) l2'
                    _        -> Nothing
reduce1 (App Prj1 (App (App _ l1) _)) 
  = Just l1
reduce1 (App Prj2 (App (App _ _) l2)) 
  = Just l2
reduce1 (App (Inl t) l1) -- reduce under inl
  = App (Inl t) <$> reduce1 l1
reduce1 (App (Inr t) l1) -- reduce under inr
  = App (Inr t) <$> reduce1 l1
reduce1 (App (App (App Case (App (Inl _) l1)) f) _) 
  = return $ App f l1 -- case (inl x) f g ~> f x
reduce1 (App (App (App Case (App (Inr _) l1)) _) g) 
  = return $ App g l1 -- case (inr x) f g ~> g x
reduce1 (App (Forall x1 t) (Typ x2)) 
  = Just $ tSubUnder t (TVar x1, x2)  --type-level beta-reduction
reduce1 (App (App (App If BTrue) l3) _) 
  = Just l3
reduce1 (App (App (App If BFalse) _) l4) 
  = Just l4
reduce1 (App (App (App If l2) l3) l4) 
  = do l2' <- reduce1 l2
       Just $ App (App (App If l2') l3) l4
reduce1 (App l1 l2) 
  = case reduce1 l1 of 
      Just l' -> Just $ App l' l2
      _       -> App l1 <$> reduce1 l2
reduce1 (Forall x t) 
  = Forall x <$> reduce1 t
reduce1 (Rec l1)
  = let reduceRec []          = []
        reduceRec ((x, t):xs) = case reduce1 t of
                                  Just t' -> (x,t'):xs
                                  _       -> (x,t) :reduceRec xs
        newRec                = reduceRec l1
    in do Monad.guard (newRec /= l1)
          return $ Rec newRec
reduce1 (Pack ty1 t ty2)
  = do t' <- reduce1 t
       return $ Pack ty1 t' ty2
reduce1 (Unpack x1 y1 (Pack ty1 t1 _) t2)
  = let t2' = substitute t2 (Var x1, t1)
    in Just $ tSubUnder t2' (TVar y1, ty1)
reduce1 (Unpack x y t1 t2)
  = case (reduce1 t1, reduce1 t2) of
      (Just t1', Nothing) -> return $ Unpack x y t1' t2
      (Nothing, Just t2') -> return $ Unpack x y t1 t2'
      (_,_)               -> Nothing
reduce1 _ = Nothing


-- | Function used to do type substitution under a second order abstraction
tSubUnder :: SOLTerm -> (T,T) -> SOLTerm
tSubUnder (Inl t1) c
  = Inl $ typeSub t1 c
tSubUnder (Inr t1) c
  = Inr $ typeSub t1 c
tSubUnder (Rec ls) c
  = Rec $ map (\(l,t)->(l, tSubUnder t c)) ls
tSubUnder (Typ t) c      
  = Typ $ typeSub t c
tSubUnder (Abs x t l2) c 
  = Abs x (typeSub t c) $ tSubUnder l2 c
tSubUnder (App l1 l2) c  
  = App (tSubUnder l1 c) (tSubUnder l2 c)
tSubUnder l@(Forall x t) c@(TVar y, _) 
  = if x /= y 
      then Forall x (tSubUnder t c)  
      else l
tSubUnder (Pack ty1 t ty2) c@(TVar _, _)
  = Pack (typeSub ty1 c) (tSubUnder t c) (typeSub ty2 c)
tSubUnder (Unpack x y t1 t2) c
  = Unpack x y (tSubUnder t1 c) (tSubUnder t2 c)
tSubUnder l _ = l 



-- | Multi-step reduction relation.
-- NOT GUARANTEED TO TERMINATE if it doesn't type check
reduce :: SOLTerm -> SOLTerm
reduce t = case reduce1 t of 
  Just t' -> reduce t'
  Nothing -> t


-- | Multi-step reduction relation that accumulates all reduction steps.
-- useful for logging how a term reduces.
reductions :: SOLTerm -> [SOLTerm]
reductions t = case reduce1 t of
  Just t' -> t' : reductions t'
  _       -> []


