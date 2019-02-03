{-|
Module      : Main
Description : The Main access point for SKI.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running SKI. This is
targeted by main when compiled and run.
-}
module Main where

-- SKI imports.
import Repl

-- | Simple SKI Repl
main :: IO ()
main = replMain