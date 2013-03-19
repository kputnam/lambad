{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Parser
  ( parseExpr
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
    body    = parseExpr
    defn    = parenthesized defn
          <|> Definition <$> (define *> name) <*> body

parseExpr :: Parser Expression
parseExpr
  = right =<< expr
  where
    right = liftA2 (<|>) app pure
    app f = right =<< Application f <$> expr
    expr  = skipSpace *> (parseAbs
                     <|>  parseVar
                     <|>  parenthesized parseExpr)

-- lambda x. body
parseAbs :: Parser Expression
parseAbs
  = lambda *> rest
  where
    rest   = Abstraction <$> parseVarId <*> (skipSpace *> (body <|> rest))
    body   = "." .*> parseExpr
    lambda = ("lambda" .*> skipSpace1)
         <|> ("λ"      .*> skipSpace)

parseVar :: Parser Expression
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
