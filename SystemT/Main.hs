{-|
Module      : Main
Description : The Main access point for STLC.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running SystemT. This is
targeted by main when compiled and run.
-}
module Main where

-- System T imports.
import Repl

-- | Simple System T Repl
main :: IO ()
main = replMain