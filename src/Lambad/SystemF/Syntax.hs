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
  pretty = prettyTerm 0

instance Pretty Type where
  pretty = prettyType 0

-- Parenthesize subexpressions of non-associative operators
specialPrint :: Int -> a -> (a -> Int) -> (Int -> a -> Doc) -> Doc
specialPrint p e prec walk
  | p == prec e = parens (walk p e)
  | otherwise   = walk p e

-- Assign a precedence to each operator (highest precedence binds tightest)
precTerm :: Term -> Int
precTerm (TmVariable _)        = 5
precTerm (TmApplication _ _)   = 4
precTerm (TyApplication _ _)   = 4
precTerm (TmAbstraction _ _ _) = 3
precTerm (TyAbstraction _ _)   = 3

-- Assign a precedence to each operator (highest precedence binds tightest)
precType :: Type -> Int
precType (TyVariable _) = 3
precType (TyArrow _ _)  = 2
precType (TyForall _ _) = 1

-- Parenthesize subexpressions with looser-binding operators than parent
prettyTerm :: Int -> Term -> Doc
prettyTerm _ (TmVariable x)
  = text (unpack x)
prettyTerm p e@(TmApplication a b)
  | p > q     = parens (prettyTerm q e)
  | otherwise = prettyTerm q a <+> specialPrint q b precTerm prettyTerm
  where q = precTerm e
prettyTerm p e@(TyApplication a b)
  | p > q     = parens (prettyTerm q e)
  | otherwise = prettyTerm q a <+> brackets (prettyType 0 b)
  where q = precTerm e
prettyTerm p e@(TmAbstraction _ _ _)
  | p > q     = parens (prettyTerm q e)
  | otherwise = text "λ" <> collapse e ""
  where
    q = precTerm e
    collapse (TmAbstraction x t b) w
                 = text (unpack (w <> x <> ":")) <> prettyType q t <> collapse b " "
    collapse b _ = text "." <+> pretty b
prettyTerm p e@(TyAbstraction a b)
  | p > q     = parens (prettyTerm q e)
  | otherwise   = text "Λ"  <> text (unpack (unwords vars))
                            <> text ". " <> prettyTerm q body
  where
    q     = precTerm e
    vars  = reverse (fst inner)
    body  = snd inner
    inner = collapse ([a], b)
    collapse (as, TyAbstraction a' b') = collapse (a':as, b')
    collapse (as, b')                  = (as, b')

-- Parenthesize subexpressions with looser-binding operators than parent
prettyType :: Int -> Type -> Doc
prettyType _ (TyVariable a)
  = text (unpack a)
prettyType p e@(TyArrow a b)
  | p > q     = parens (prettyType q e)
  | otherwise = specialPrint q a precType prettyType <+> text "→" <+> prettyType q b
  where q = precType e
prettyType p e@(TyForall a b)
  | p > q     = parens (prettyType q e)
  | otherwise = text "∀"  <> text (unpack (unwords vars)) <>
                text ". " <> prettyType q body
  where
    q     = precType e
    body  = snd inner
    vars  = reverse (fst inner)
    inner = collapse ([a], b)

    -- Sugar "∀a. ∀b. ∀c. E" to "∀a b c. E"
    collapse (as, TyForall a' b') = collapse (a':as, b')
    collapse (as, b')             = (as, b')
