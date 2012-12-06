{-# LANGUAGE OverloadedStrings #-}

module Language.Lambad.Parser
  ( parseExpr
  , parseFile
  , parseDecl
  ) where

import Language.Lambad.Syntax
import Control.Applicative hiding (empty)
import Data.Char
import Data.Text
import Data.Attoparsec.Text

parseFile :: Parser [Declaration]
parseFile
  = many1 parseDecl <* skipSpace

-- declare name body
parseDecl :: Parser Declaration
parseDecl
  = skipSpace *> decl
  where
    declare = string "declare" <* skipSpace1
    name    = parseVarId       <* skipSpace1
    body    = parseExpr
    decl    = parenthesized decl
          <|> Declaration <$> (declare *> name) <*> body

parseExpr :: Parser Expression
parseExpr
  = right =<< expr
  where
    right = liftA2 (<|>) app pure
    app f = right =<< Application f <$> expr
    expr  = skipSpace *> (parseAbs
                     <|>  parseVar
                     <|>  parenthesized parseExpr)

-- lambda x body
parseAbs :: Parser Expression
parseAbs
  = Abstraction <$> (lambda *> name) <*> body
  where
    lambda = string "lambda" <* skipSpace1
    name   = parseVarId      <* skipSpace <* char '.' <* skipSpace
    body   = parseExpr

parseVar :: Parser Expression
parseVar
  = Variable <$> parseVarId

parseVarId :: Parser Text
parseVarId
  = takeWhile1 (notInClass " .\r\n\t()")

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
