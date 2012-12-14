{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Pure.Syntax
  ( Expression(..)
  , Declaration(..)
  ) where

import Data.Text

type Id
  = Text

data Expression
  = Variable Id
  | Application Expression Expression
  | Abstraction Id Expression
  deriving (Eq, Show)

data Declaration
  = Declaration Id Expression
  deriving (Eq, Show)
