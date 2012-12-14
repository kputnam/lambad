{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Pure.Parser
  ( parseExpr
  , parseFile
  , parseDecl
  ) where

import Data.Char
import Data.Text
import Data.Attoparsec.Text
import Control.Applicative hiding (empty)

import Language.Pure.Syntax

parseFile ∷ Parser [Declaration]
parseFile
  = many1 parseDecl <* skipSpace

-- declare name body
parseDecl ∷ Parser Declaration
parseDecl
  = skipSpace *> decl
  where
    declare = "declare"  .*> skipSpace1
    name    = parseVarId <*  skipSpace1
    body    = parseExpr
    decl    = parenthesized decl
          <|> Declaration <$> (declare *> name) <*> body

parseExpr ∷ Parser Expression
parseExpr
  = right =<< expr
  where
    right = liftA2 (<|>) app pure
    app f = right =<< Application f <$> expr
    expr  = skipSpace *> (parseAbs
                     <|>  parseVar
                     <|>  parenthesized parseExpr)

-- lambda x body
parseAbs ∷ Parser Expression
parseAbs
  = Abstraction <$> (lambda *> name) <*> body
  where
    name   = parseVarId <* skipSpace <* char '.' <* skipSpace
    body   = parseExpr
    lambda = ("lambda" .*> skipSpace1)
         <|> ("λ"      .*> skipSpace)

parseVar ∷ Parser Expression
parseVar
  = Variable <$> parseVarId

parseVarId ∷ Parser Text
parseVarId
  = takeWhile1 (`notElem` " .\r\n\t()")

---------------------------------------------------------------------------

skipSpace1 ∷ Parser ()
skipSpace1
  = takeWhile1 isSpace *> skipSpace

parenthesized ∷ Parser a → Parser a
parenthesized p
  = open *> p <* close
  where
    open  = char '(' *> skipSpace
    close = skipSpace <* char ')'
