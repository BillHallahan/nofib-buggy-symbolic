module KeyIORef where

import Data.IORef
import System.IO.Unsafe

import G2.Symbolic

data Status a = NotEver Int | YesIts Int a deriving (Eq)

{-# NOINLINE keysIORef #-}
keysIORef :: IORef [String]
keysIORef = unsafePerformIO (newIORef =<< mkSymbolic)

{-# NOINLINE keys #-}
keys :: [String]
keys = unsafePerformIO (readIORef keysIORef)