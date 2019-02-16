{-|
Module      : Main
Description : The Main access point for PCF.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running PCF. This is
targeted by main when compiled and run.
-}
module Main where


-- PCF Imports.
import Repl

-- | Simple PCF Repl
main :: IO ()
main = replMain