{-|
Module      : Tests
Description : The Testing for PCF.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Tests" module provides the Unit tests for various aspects of PCF
including expected reductions, pretty printing, and useful features.
-}
module Tests where

-- Tool Imports.
import qualified Test.QuickCheck as QC
import qualified Control.Monad   as M


-- PCF Imports.
import PCF
  (PCFTerm(App, Abs, Var, Succ, Pred, Zero, Y, If), T(TNat, TArr))
import qualified Parser as P


-- | Type encapsulating test pass, expected value, actual value
type UnitTest = (Bool, String, String)


-- | Type Denoting set of tests
type UnitTests = [UnitTest]


-- Test Showable produces the correct instance, first with some simple tests
-- and the examples in the README, and then automatically with QuickCheck.

-- | Function that constructs a unit test from an input term, expected output,
-- and actual value
mkUnitTest :: Bool -> String -> String -> UnitTest
mkUnitTest result exp got = (result, exp, got)


-- | Test pretty printing for \x:Nat.x
unittest1 :: UnitTest
unittest1 = let term = Abs "x" TNat (Var "x")
                exp  = "λx:Nat.x"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for s (s z)
unittest2 :: UnitTest
unittest2 = let term = App Succ (App Succ Zero)
                exp  = "s (s z)"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for p (s z)
unittest3 :: UnitTest
unittest3 = let term = App Pred (App Succ Zero)
                exp  = "p (s z)"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for iseven 2
unittest4 :: UnitTest
unittest4 = let term = App (App Y (Abs "f" (TArr TNat TNat) (Abs "x" TNat (App (App (App If (Var "x")) (Var "z")) (App (App (App If (App (Var "p") (Var "x"))) (App Succ Zero)) (App (Var "f") (App (Var "p") (App (Var "p") (Var "x"))))))))) (App (Var "p") (App (Var "p") Zero))
                exp  = "Y (λf:Nat->Nat.λx:Nat.if x z (if (p x) (s z) (f (p (p x))))) (p (p z))"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for (\x:Nat.\y:Nat. x) z (s z)
unittest5 :: UnitTest
unittest5 = let term = App (App (Abs "x" TNat $ Abs "y" TNat (Var "x")) Zero) (App Succ Zero)
                exp  = "(λx:Nat.λy:Nat.x) z (s z)"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for \f x. f x
unittest6 :: UnitTest
unittest6 = let term = Abs "f" (TArr TNat TNat) $ Abs "x" TNat $ App (Var "f") (Var "x")
                exp  = "λf:Nat->Nat.λx:Nat.f x"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for \f x. f x f
unittest7 :: UnitTest
unittest7 = let term = Abs "f" (TArr TNat TNat) $ Abs "x" TNat $ App (App (Var "f") (Var "x")) (Var "f") 
                exp  = "λf:Nat->Nat.λx:Nat.f x f"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for (\x:Nat.\y:Nat.y y) z
unittest8 :: UnitTest
unittest8 = let term = App (App (Abs "x" TNat (Abs "y" TNat (Var "y"))) (Var "y")) (Var "z")
                exp  = "(λx:Nat.λy:Nat.y) y z"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | List of pretty printing unit tests
ppunittests :: UnitTests
ppunittests = [unittest1, unittest2, unittest2, unittest3, 
               unittest4, unittest5, unittest6, unittest7,
               unittest8]


-- | Function that runs the tests and returns true if they all pass
testPP :: Maybe UnitTests
testPP = let passed = filter (\(a,_,_)->a) ppunittests
         in if null passed then return passed else Nothing


-- | Use QuickCheck to generate terms and show that both the Parsing and
--Printing align.
instance QC.Arbitrary PCFTerm where
  arbitrary = QC.sized term
    where varname  = do s <- QC.listOf1 (QC.choose ('a', 'z'))
                        if s `elem` P.keywords --make more general
                          then varname
                          else return s
          genAbs n = do s  <- varname
                        ty <- genType n
                        t  <- term n
                        return $ Abs s ty t
          genApp n = do t1 <- term n
                        t2 <- term n
                        return $ App t1 t2 
          genZero  = return Zero
          genRec   = return Y
          genSucc  = return Succ
          genPred  = return Pred
          genIf    = return If
          genType n | n == 0    = return TNat
                    | n > 0     = TArr TNat <$> genType (n-1)
                    | otherwise = genType (abs n)
          term n | n == 0    = QC.oneof [Var <$> varname,
                                         genZero, 
                                         genSucc,
                                         genPred,
                                         genIf,
                                         genRec]
                 | n > 0     = QC.oneof [genAbs (n-1), 
                                         genApp (n-1)]
                 | otherwise = term (abs n)
  shrink (App t1 t2)   = [t1, t2]
  shrink (Abs _ _ t1)  = [t1]
  shrink _             = []


-- | QuickCheck predicate to show that parsing a printed term
--produces the same term.
propShow :: PCFTerm -> Bool
propShow t = let parsed = snd . fst . head $ P.apply P.pTerm (show t)
             in parsed == t


-- | QuickCheck predicate to test parsing succeeds for a term
propParse :: PCFTerm -> Bool
propParse t = let parse = P.apply P.pTerm (show t)
              in case parse of
                  [(("", _), "")] -> True
                  _               -> False


-- | Helper function to run the above unit tests, and the quickCheck tests
runTests :: IO ()
runTests = let tests = all (\(a,_,_)->a) ppunittests
           in do M.unless tests (putStrLn "unit tests failed!")
                 QC.quickCheck (QC.withMaxSuccess 20 propShow)
                 QC.quickCheck (QC.withMaxSuccess 20 propParse) 

-- deep diving syntax trees is slow! run this a couple of times if you can!

-- TODO implement testing reduction for terms
