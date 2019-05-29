{-|
Module      : Main
Description : The Main access point for C.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running C. This is
targeted by main when compiled and run.
-}
module Main where

-- C Imports.
import Repl

-- | Simple C Repl
main :: IO ()
main = replMain