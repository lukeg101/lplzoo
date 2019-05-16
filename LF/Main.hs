{-|
Module      : Main
Description : The Main access point for LF.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running LF. This is
targeted by main when compiled and run.
-}
module Main where

-- LF Imports.
import Repl

-- | Simple LF Repl
main :: IO ()
main = replMain