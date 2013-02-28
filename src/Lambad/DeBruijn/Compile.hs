{-# LANGUAGE OverloadedStrings #-}

module Lambad.DeBruijn.Compile
  ( toPure
  , fromPure
  ) where

import Data.Monoid
import Data.List
import Data.Text                        as T hiding (length)
import qualified Lambad.Pure.Syntax     as P
import qualified Lambad.DeBruijn.Syntax as D

alphabet :: [Text]
alphabet = T.chunksOf 1 "abcdefghijklmnopqrstuvwxyz"

toPure :: D.Expression -> P.Expression
toPure = walk []
  where
    walk xs (D.FreeVariable x)   = P.Variable x
    walk xs (D.BoundVariable n)  = P.Variable (xs !! n)
    walk xs (D.Application f a)  = P.Application (walk xs f) (walk xs a)
    walk xs (D.Abstraction b)    =
      let n = length xs
          q = n `mod` length alphabet
          r = n `div` length alphabet
          x = alphabet !! q <> T.replicate r "'"
       in P.Abstraction x $ walk (x:xs) b

fromPure :: P.Expression -> D.Expression
fromPure = walk []
  where
    walk xs (P.Application f a)  = D.Application (walk xs f) (walk xs a)
    walk xs (P.Abstraction x b)  = D.Abstraction $ walk (x:xs) b
    walk xs (P.Variable x)       = case elemIndex x xs of
                                     Just n  -> D.BoundVariable n
                                     Nothing -> D.FreeVariable x
