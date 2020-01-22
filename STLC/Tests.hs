{-|
Module      : Tests
Description : The Testing for STLC.
Copyright   : (c) Luke Geeson, 2020
License     : GPL-3
Maintainer  : mail@lukegeeson.com
Stability   : stable
Portability : POSIX

The "Tests" module provides the Unit tests for various aspects of STLC
including expected reductions, pretty printing, and useful features.
-}
module Tests where

-- Tool Imports.
import qualified Test.QuickCheck as QC
import qualified Control.Monad   as M
import System.Exit (exitFailure)


-- STLC Imports.
import STLC
  (STTerm(App, Abs, Var), T(TVar, TArr))
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


-- TODO add unit tests in readme

-- | Use QuickCheck to generate terms and show that both the Parsing and
--Printing align.
instance QC.Arbitrary STTerm where
  arbitrary = QC.sized term
    where typeterm n | n == 0 = return TVar
                     | n > 0  = do x <- typeterm (n-1)
                                   y <- typeterm (n-1)
                                   return $ TArr x y 
                     | otherwise = typeterm (abs n)
          varname  = do s <- QC.listOf1 (QC.choose ('a', 'z'))
                        if s `elem` P.keywords
                          then varname
                          else return s
          genAbs n = do s  <- varname
                        t  <- term n
                        ty <- typeterm n
                        return $ Abs s ty t
          genApp n = do t1 <- term n
                        t2 <- term n
                        return $ App t1 t2
          term n | n == 0    = Var <$> varname
                 | n > 0     = QC.oneof [genAbs (n-1), genApp (n-1)]
                 | otherwise = term (abs n)
  shrink (Var _)       = []
  shrink (App t1 t2)   = [t1, t2]
  shrink (Abs _ _ t1)  = [t1]


-- | QuickCheck predicate to show that parsing a printed term
--produces the same term.
propShow :: STTerm -> Bool
propShow t = let parsed = snd . fst . head $ P.apply P.pTerm (show t)
             in parsed == t


-- | QuickCheck predicate to test parsing succeeds for a term
propParse :: STTerm -> Bool
propParse t = let parse = P.apply P.pTerm (show t)
              in case parse of
                  [(_,"")] -> True
                  _        -> False


-- | Helper function to run the above unit tests, and the quickCheck tests
runTests :: IO ()
runTests = let {-tests    = all (\(a,_,_)->a) ppunittests-}
                -- todo add unit tests
               failed t = case t of
                           QC.Failure {} -> True
                           _             -> False
           in do {-M.unless tests (do {putStrLn "unit tests failed!"; exitFailure})-}
                 -- todo add unit tests
                 r1 <- QC.quickCheckResult (QC.withMaxSuccess 20 propShow)
                 r2 <- QC.quickCheckResult (QC.withMaxSuccess 20 propParse)
                 M.when (any failed [r1, r2]) exitFailure

-- | Main function to run the tests
hs :: IO ()
hs = runTests

-- deep diving syntax trees is slow! run this a couple of times if you can!

-- TODO implement testing reduction for terms
