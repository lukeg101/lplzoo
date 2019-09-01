{-|
Module      : Main
Description : The Main access point for SystemF.
Copyright   : (c) Luke Geeson, 2019
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running SystemF. This is
targeted by main when compiled and run.
-}
module Main where

-- System F imports.
import Repl

-- | Simple System F Repl
main :: IO ()
main = replMain