{-|
Module      : Tests
Description : The Testing for ULC.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com
Stability   : stable
Portability : POSIX

The "Tests" module provides the Unit tests for various aspects of ULC
including expected reductions, pretty printing, and useful features.
-}
module Tests where

-- Tool Imports.
import qualified Test.QuickCheck as QC
import qualified Control.Monad   as M


-- ULC Imports.
import ULC
  (Term(App, Abs, Var), toChurch, i, true, false, xx, omega)
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

-- | Test pretty printing for id (\x.x x)
unittest1 :: UnitTest
unittest1 = let term = App i (Abs "x" (App (Var "x") (Var "x")))
                exp  = "(λa.a) (λx.x x)"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for (\x y.y) y z
unittest2 :: UnitTest
unittest2 = let term = App (App (Abs "x" (Abs "y" (Var "y"))) (Var "y")) (Var "z")
                exp  = "(λx.λy.y) y z"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for (\x y.y) y z
unittest3 :: UnitTest
unittest3 = let term = App (App (toChurch 3) (Abs "x" (App (Var "x") (toChurch 2)))) (toChurch 1)
                exp  = "(λx.λf.x (x (x x))) (λx.x (λx.λf.x (x x))) (λx.λf.x x)"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for id
unittest4 :: UnitTest
unittest4 = let term = i
                exp  = "λa.a"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for true
unittest5 :: UnitTest
unittest5 = let term = true
                exp  = "λa.λb.a"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for true
unittest6 :: UnitTest
unittest6 = let term = false
                exp  = "λa.λb.b"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for true
unittest7 :: UnitTest
unittest7 = let term = xx
                exp  = "λx.x x"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Test pretty printing for true
unittest8 :: UnitTest
unittest8 = let term = omega
                exp  = "(λx.x x) (λx.x x)"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | Testing pretty printing for (\x.x) (\y.y) z
unittest9 :: UnitTest
unittest9 = let term = App (App (Abs "x" (Var "x")) (Abs "y" (Var "y"))) (Var "z")
                exp  = "(λx.x) (λy.y) z"
                got  = show term
            in mkUnitTest (exp==got) exp got


-- | List of pretty printing unit tests
ppunittests :: UnitTests
ppunittests = [unittest1, unittest2, unittest2, unittest3, unittest4,
  unittest5, unittest6, unittest7, unittest8, unittest9]


-- | Function that runs the tests and returns true if they all pass
testPP :: Maybe UnitTests
testPP = let passed = filter (\(a,_,_)->a) ppunittests
         in if null passed then return passed else Nothing


-- | Use QuickCheck to generate terms and show that both the Parsing and
--Printing align.
instance QC.Arbitrary Term where
  arbitrary = QC.sized term
    where varname  = do s <- QC.listOf1 (QC.choose ('a', 'z'))
                        if s `elem` ["let", "="] --make more general
                          then varname
                          else return s
          genAbs n = do s <- varname
                        t <- term n
                        return $ Abs s t
          genApp n = do t1 <- term n
                        t2 <- term n
                        return $ App t1 t2
          term n | n == 0    = Var <$> varname
                 | n > 0     = QC.oneof [genAbs (n-1), genApp (n-1)]
                 | otherwise = term (abs n)
  shrink (Var _)     = []
  shrink (App t1 t2) = [t1, t2]
  shrink (Abs _ t1)  = [t1]


-- | QuickCheck predicate to show that parsing a printed term
--produces the same term.
propShow :: Term -> Bool
propShow t = let parsed = fst . head $ P.apply P.pTerm (show t)
             in parsed == t


-- | QuickCheck predicate to test parsing succeeds for a term
propParse :: Term -> Bool
propParse t = let parse = P.apply P.pTerm (show t)
              in case parse of
                  [(_,"")] -> True
                  _        -> False


-- | Helper function to run the above unit tests, and the quickCheck tests
runTests :: IO ()
runTests = let tests = all (\(a,_,_)->a) ppunittests
           in do M.unless tests (putStrLn "unit tests failed!")
                 QC.quickCheck (QC.withMaxSuccess 20 propShow)
                 QC.quickCheck (QC.withMaxSuccess 20 propParse)


-- | Main function to run the tests
hs :: IO ()
hs = runTests

-- deep diving syntax trees is slow! run this a couple of times if you can!

-- TODO implement testing reduction for terms
