{-|
Module      : Main
Description : The Main access point for STLC.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running STLC. This is
targeted by main when compiled and run.
-}
module Main where

-- STLC Imports.
import Repl

-- | Simple STLC Repl
main :: IO ()
main = replMain