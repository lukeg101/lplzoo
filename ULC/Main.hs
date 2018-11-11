{-|
Module      : Main
Description : The Main access point for ULC.
Copyright   : (c) Luke Geeson, 2018
License     : GPL-3
Maintainer  : mail@lukegeeson.com 
Stability   : stable
Portability : POSIX

The "Main" module provides the access point for running ULC. This is
targeted by main when compiled and run.
-}
module Main where

-- ULC imports
import Repl

-- | Simple ULC Repl
main :: IO ()
main = replMain