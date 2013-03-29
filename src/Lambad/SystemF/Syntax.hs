{-# LANGUAGE OverloadedStrings #-}

module Lambad.SystemF.Syntax
  ( Term(..)
  , Type(..)
  , Definition(..)
  ) where

import Prelude    hiding (unwords)
import Data.Text  hiding (reverse)
import Text.PrettyPrint hiding ((<>))
import Data.Monoid ((<>))

import Lambad.Pretty

type Id
  = Text

data Type                       -- τ ::=
  = TyVariable Id               --     | α
  | TyForall Id Type            --     | ∀α.τ
  | TyArrow Type Type           --     | τ → τ
  deriving (Show)

data Term                       -- e ::=
  = TmVariable Id               --     | x
  | TmApplication Term Term     --     | e e
  | TmAbstraction Id Type Term  --     | λx:τ.e
  | TyApplication Term Type     --     | e [τ]
  | TyAbstraction Id Term       --     | Λα.e
  deriving (Show)

data Definition
  = Definition Id Term
  deriving (Show)

instance Pretty Definition where
  pretty (Definition x e)
    = parens $ text "define" <+> text (unpack x) <+> pretty e

instance Pretty Term where
  pretty = prettyTerm (-1) False

instance Pretty Type where
  pretty = prettyType (-1) False

-- First parameter is precedence level of parent node. Second parameter
-- is True when this node overrides default left/right associativity.
prettyTerm :: Int -> Bool -> Term -> Doc
prettyTerm _ _ (TmVariable x) -- 5
  = text (unpack x)
prettyTerm p s e@(TmApplication f a) -- 4
  | s || p > p' = parens (prettyTerm p' False e)
  | otherwise   = prettyTerm p' r f <+> prettyTerm p' l a
  where p' = 4
        r  = False
        l  = case a of (TmVariable _) -> False; _ -> True
prettyTerm p s e@(TyApplication f a) -- 4
  | s || p > p' = parens (prettyTerm p' False e)
  | otherwise   = prettyTerm p' r f <+> brackets (pretty a)
  where p' = 4
        r  = False
prettyTerm p s e@(TmAbstraction _ _ _) -- 3
  | s || p > p' = parens (prettyTerm p' False e)
  | otherwise   = text "λ" <> collapse e ""
  where
    p' = 3
    collapse (TmAbstraction x t b) w
                 = text (unpack (w <> x <> ":"))
                <> prettyType p' False t <> collapse b " "
    collapse b _ = text "." <+> pretty b
prettyTerm p s e@(TyAbstraction x b) -- 3
  | s || p > p' = parens (prettyTerm p' False e)
  | otherwise   = text "Λ"  <> text (unpack (unwords vars))
                            <> text ". " <> prettyTerm p' False (snd inner)
  where
    p'    = 3
    vars  = reverse (fst inner)
    inner = collapse ([x], b)
    collapse (xs, TyAbstraction x' e') = collapse (x':xs, e')
    collapse (xs, e')                  = (xs, e')

-- First parameter is precedence level of parent node. Second parameter
-- is True when this node overrides default left/right associativity.
prettyType :: Int -> Bool -> Type -> Doc
prettyType _ _ (TyVariable a)   -- 3
  = text (unpack a)
prettyType p s e@(TyArrow i o)  -- 2
  | s || p > p' = parens (prettyType p' False e)
  | otherwise   = prettyType p' l i <+> text "→" <+> prettyType p' r o
  where p' = 2
        l  = case i of (TyArrow _ _) -> True; _ -> False
        r  = False
prettyType p s e@(TyForall a t) -- 1
  | s || p > p' = parens (prettyType p' False e)
  | otherwise   = text (unpack ("∀" <> a <> ".")) <+> prettyType p' False t
  where p' = 1

