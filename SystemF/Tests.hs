{-|
Module      : Tests
Description : The Testing for SystemF.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Tests" module provides the Unit tests for various aspects of SystemF
including expected reductions, pretty printing, and useful features.
-}
module Tests where

-- Tool Imports.
import qualified Test.QuickCheck as QC
import qualified Control.Monad   as M
import System.Exit (exitFailure)


-- Sysrem F Imports.
import SystemF
  (SFTerm(App, Abs, Var, PiAbs, Typ), T(TVar, TArr, TPi))
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

-- todo unit tests

-- | List of pretty printing unit tests
ppunittests :: UnitTests
ppunittests = []


-- | Function that runs the tests and returns true if they all pass
testPP :: Maybe UnitTests
testPP = let passed = filter (\(a,_,_)->a) ppunittests
         in if null passed then return passed else Nothing


-- | Function to generate type variables for arbitrary instances
genTypVar :: QC.Gen String
genTypVar = do s <- QC.listOf1 (QC.choose ('A', 'Z'))
               if s `elem` P.keywords
                 then genTypVar
                 else return s


-- | Use QuickCheck to generate terms and show that both the Parsing and
--Printing align.
instance QC.Arbitrary T where
  arbitrary = QC.sized term
    where genTypPi n = do x <- genTypVar
                          ty <- term (n-1) 
                          return $ TPi x ty
          genTypArr n = do t1 <- term (n-1) 
                           t2 <- term (n-1) 
                           return $ TArr t1 t2
          term n | n == 0    = TVar <$> genTypVar
                 | n > 0     = genTypPi n
                 | otherwise = genTypArr (abs n)
  shrink (TArr t1 t2) = [t1, t2]
  shrink (TPi _ t1)   = [t1]
  shrink _            = []


-- | Use QuickCheck to generate terms and show that both the Parsing and
--Printing align.
instance QC.Arbitrary SFTerm where
  arbitrary = QC.sized term
    where varname    = do s <- QC.listOf1 (QC.choose ('a', 'z'))
                          if s `elem` P.keywords
                            then varname
                            else return s
          genTermTyp = Typ <$> QC.arbitrary
          genAbs n   = do s  <- varname
                          ty <- QC.arbitrary
                          t  <- term n
                          return $ Abs s ty t
          genApp n   = do t1 <- term n
                          t2 <- term n
                          return $ App t1 t2
          genPiAbs n = do s  <- genTypVar
                          t  <- term n
                          return $ PiAbs s t
          
          term n | n == 0 = Var <$> varname
                 | n > 0  = QC.oneof [genAbs (n-1), 
                                      genApp (n-1),
                                      genPiAbs (n-1),
                                      genTermTyp]
                 | otherwise = term (abs n)
  shrink (App t1 t2)   = [t1, t2]
  shrink (Abs _ _ t1)  = [t1]
  shrink (PiAbs _ t1)  = [t1]
  shrink (Typ t1)      = map Typ (QC.shrink t1)
  shrink _             = []


-- | QuickCheck predicate to show that parsing a printed term
--produces the same term.
propShow :: SFTerm -> Bool
propShow t = let (Left parsed) = snd . fst . head $ P.apply P.pTerm (show t)
             in parsed == t


-- | QuickCheck predicate to test parsing succeeds for a term
propParse :: SFTerm -> Bool
propParse t = let parse = P.apply P.pTerm (show t)
              in case parse of
                  [(("", Left _), "")] -> True
                  _               -> False


-- | QuickCheck predicate to show that parsing a printed term
--produces the same term.
propTypShow :: T -> Bool
propTypShow t = let parsed = snd . fst . head $ P.apply P.pType (show t)
                in parsed == t


-- | QuickCheck predicate to test parsing succeeds for a term
propTypParse :: T -> Bool
propTypParse t = let parse = P.apply P.pType (show t)
                 in case parse of
                      [(("", _), "")] -> True
                      _               -> False


-- | Helper function to run the above unit tests, and the quickCheck tests
runTests :: IO ()
runTests = let tests    = all (\(a,_,_)->a) ppunittests
               failed t = case t of
                           QC.Failure {} -> True
                           _             -> False
           in do M.unless tests (do {putStrLn "unit tests failed!"; exitFailure})
                 t1 <- QC.quickCheckResult (QC.withMaxSuccess 20 propTypParse)
                 t2 <- QC.quickCheckResult (QC.withMaxSuccess 20 propTypParse)
                 r1 <- QC.quickCheckResult (QC.withMaxSuccess 20 propShow)
                 r2 <- QC.quickCheckResult (QC.withMaxSuccess 20 propParse)
                 M.when (any failed [r1, r2, t1, t2]) exitFailure

-- deep diving syntax trees is slow! run this a couple of times if you can!

-- | Main function to run the tests
hs :: IO ()
hs = runTests

-- TODO implement testing reduction for terms

