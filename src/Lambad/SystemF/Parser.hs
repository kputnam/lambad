{-# LANGUAGE OverloadedStrings #-}

module Lambad.SystemF.Parser
  ( parseTerm
  , parseType
  , parseFile
  , parseDefn
  ) where

import Data.Char
import Data.Text
import Data.Attoparsec.Text
import Control.Applicative hiding (empty)

import Lambad.SystemF.Syntax

parseFile :: Parser [Definition]
parseFile
  = many1 (skipSpace *> parseDefn) <* skipSpace

-- (define name body)
parseDefn :: Parser Definition
parseDefn
  = parenthesized parseDefn
  <|> Definition <$> (declare *> name) <*> body
  where
    declare = "define"    .*> skipSpace1
    name    = parseTmVarId <* skipSpace1
    body    = parseTerm

-- Types (τ)
--------------------------------------------------------------------------------

parseType :: Parser Type
parseType
  = look =<< skipSpace *> term
  where
    look    = liftA2 (<|>) arrow pure
    arrow t = TyArrow t <$> (delim *> parseType)
    delim   = skipSpace *> (string "->" <|> string "→")
    term    = parseForall
          <|> parseTyVar
          <|> parenthesized parseType

-- ∀α.τ
parseForall :: Parser Type
parseForall
  = TyForall <$> (delim *> parseTyVarId) <*> inside
  where
    inside = skipSpace *> ("." .*> skipSpace *> parseType)
    delim  = ("forall" .*> skipSpace1)
         <|> ("∀"      .*> skipSpace)

-- α
parseTyVar :: Parser Type
parseTyVar
  = TyVariable <$> parseTyVarId

parseTyVarId :: Parser Text
parseTyVarId
  = takeWhile1 (`notElem` " .-→\r\n\t([])")

-- Terms (e)
--------------------------------------------------------------------------------

parseTerm :: Parser Term
parseTerm
  = right =<< skipSpace *> term
  where
    right  = liftA2 (<|>) app pure
    app f  = right =<< (skipSpace *> rest f)
    rest f = TyApplication f <$> bracketed parseType
         <|> TmApplication f <$> term
    term   = parseTmAbs
         <|> parseTyAbs
         <|> parseTmVar
         <|> parenthesized parseTerm

-- λx:τ.e
parseTmAbs :: Parser Term
parseTmAbs
  = delim *> rest
  where
    rest    = TmAbstraction <$> parseTmVarId <*> argType <*> inside
    argType = skipSpace *> (":" .*> skipSpace *> parseType)
    inside  = skipSpace *> ("." .*> parseTerm <|> rest)
    delim   = ("lambda" .*> skipSpace1)
          <|> ("λ"      .*> skipSpace)

-- Λα.e
parseTyAbs :: Parser Term
parseTyAbs
  = lambda *> rest
  where
    rest    = TyAbstraction <$> parseTyVarId <*> inside
    inside  = skipSpace *> ("." .*> parseTerm <|> rest)
    lambda  = ("LAMBDA" .*> skipSpace1)
          <|> ("Λ"      .*> skipSpace)

-- x
parseTmVar :: Parser Term
parseTmVar
  = TmVariable <$> parseTmVarId

parseTmVarId :: Parser Text
parseTmVarId
  = takeWhile1 (`notElem` " :\r\n\t([])")

---------------------------------------------------------------------------

skipSpace1 :: Parser ()
skipSpace1
  = takeWhile1 isSpace *> skipSpace

bracketed :: Parser a -> Parser a
bracketed p
  = open *> p <* close
  where
    open  = char '[' *> skipSpace
    close = skipSpace <* char ']'

parenthesized :: Parser a -> Parser a
parenthesized p
  = open *> p <* close
  where
    open  = char '(' *> skipSpace
    close = skipSpace <* char ')'
