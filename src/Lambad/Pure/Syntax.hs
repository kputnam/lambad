{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Syntax
  ( Term(..)
  , Definition(..)
  ) where

import Prelude    hiding (unwords)
import Data.Text  hiding (reverse)
import Text.PrettyPrint

import Lambad.Pretty

type Id
  = Text

data Term
  = Variable Id
  | Application Term Term
  | Abstraction Id Term
  deriving (Show)

data Definition
  = Definition Id Term
  deriving (Show)

instance Pretty Definition where
  pretty (Definition x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Term where
  pretty = prettyTerm 0

-- Parenthesize subexpressions of non-associative operators
specialTerm :: Int -> Term -> Doc
specialTerm p e
  | p == precTerm e = parens (prettyTerm p e)
  | otherwise       = prettyTerm p e

-- Assign a precedence to each operator (highest precedence binds tightest)
precTerm :: Term -> Int
precTerm (Variable _)      = 3
precTerm (Application _ _) = 2
precTerm (Abstraction _ _) = 1

-- Parenthesize subexpressions with looser-binding operators than parent
prettyTerm :: Int -> Term -> Doc
prettyTerm _ (Variable x)
  = text (unpack x)
prettyTerm p e@(Application a b)
  | p > q     = parens (prettyTerm q e)
  | otherwise = prettyTerm q a <+> specialTerm q b
  where q = precTerm e
prettyTerm p e@(Abstraction a b)
  | p > q     = parens (prettyTerm q e)
  | otherwise = text "λ"  <> text (unpack (unwords vars)) <>
                text ". " <> prettyTerm q body
  where
    q     = precTerm e
    body  = snd inner
    vars  = reverse (fst inner)
    inner = collapse ([a], b)

    -- Sugar "λa. λb. λc. E" to "λa b c. E"
    collapse (as, Abstraction a' b') = collapse (a':as, b')
    collapse (as, b')                = (as, b')
