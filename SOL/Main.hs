{-|
Module      : Main
Description : The Main access point for SOL.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running SOL. This is
targeted by main when compiled and run.
-}
module Main where

-- SOL Imports.
import Repl

-- | Simple SOL Repl
main :: IO ()
main = replMain