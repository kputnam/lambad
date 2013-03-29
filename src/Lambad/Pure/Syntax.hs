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
  pretty = prettyTerm (-1) False

-- First parameter is precedence level of parent node. Second parameter
-- is True when this node overrides default left/right associativity.
prettyTerm :: Int -> Bool -> Term -> Doc
prettyTerm p s e@(Variable x)
  | s || p > p' = parens (prettyTerm p' False e)
  | otherwise   = text (unpack x)
  where p' = 3
prettyTerm p s e@(Application f a)
  | s || p > p' = parens (prettyTerm p' False e)
  | otherwise   = prettyTerm p' r f <+> prettyTerm p' l a
  where p' = 2
        r  = False
        l  = case a of (Variable _) -> False; _ -> True
prettyTerm p s e@(Abstraction x b)
  | s || p > p' = parens (prettyTerm p' False e)
  | otherwise   = text "λ"  <> text (unpack (unwords vars))
               <> text "." <+> prettyTerm p' False (snd inner)
  where
    p'    = 1
    vars  = reverse (fst inner)
    inner = collapse ([x], b)
    collapse (xs, Abstraction x' e') = collapse (x':xs, e')
    collapse (xs, e')                = (xs, e')
