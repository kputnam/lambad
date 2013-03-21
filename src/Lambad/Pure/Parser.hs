{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Parser
  ( parseTerm
  , parseFile
  , parseDecl
  ) where

import Data.Char
import Data.Text
import Data.Attoparsec.Text
import Control.Applicative hiding (empty)

import Lambad.Pure.Syntax

parseFile :: Parser [Definition]
parseFile
  = many1 (skipSpace *> parseDecl) <* skipSpace

-- define name body
parseDecl :: Parser Definition
parseDecl
  = defn
  where
    define  = "define"  .*> skipSpace1
    name    = parseVarId <* skipSpace1
    body    = parseTerm
    defn    = parenthesized defn
          <|> Definition <$> (define *> name) <*> body

parseTerm :: Parser Term
parseTerm
  = right =<< expr
  where
    right = liftA2 (<|>) app pure
    app f = right =<< Application f <$> expr
    expr  = skipSpace *> (parseAbs
                     <|>  parseVar
                     <|>  parenthesized parseTerm)

-- lambda x. body
parseAbs :: Parser Term
parseAbs
  = lambda *> rest
  where
    rest   = Abstraction <$> parseVarId <*> (skipSpace *> (body <|> rest))
    body   = "." .*> parseTerm
    lambda = ("lambda" .*> skipSpace1)
         <|> ("λ"      .*> skipSpace)

parseVar :: Parser Term
parseVar
  = Variable <$> parseVarId

parseVarId :: Parser Text
parseVarId
  = takeWhile1 (`notElem` " .\r\n\t()")

---------------------------------------------------------------------------

skipSpace1 :: Parser ()
skipSpace1
  = takeWhile1 isSpace *> skipSpace

parenthesized :: Parser a -> Parser a
parenthesized p
  = open *> p <* close
  where
    open  = char '(' *> skipSpace
    close = skipSpace <* char ')'
