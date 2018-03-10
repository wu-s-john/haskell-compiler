{-# OPTIONS_GHC -Wall #-}

module Util where

(=:) :: a -> b -> (a, b)
a =: b = (a, b)
