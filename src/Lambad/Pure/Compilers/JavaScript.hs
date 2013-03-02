{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Compilers.JavaScript
  ( ToJS(..)
  ) where

import Prelude hiding (span)
import Data.Text hiding (foldr)
import Data.Monoid
import Lambad.Pure.Syntax

-- This is an unoptimized compiler for *closed terms*

class ToJS a where
  toJS :: a -> Text

instance ToJS Expression where
  toJS (Variable id)     = mangle id
  toJS (Application (Variable id) e)
                         = mangle id <> "(" <> toJS e <> ")"
  toJS (Application f e) = "(" <> toJS f <> ")(" <> toJS e <> ")"
  toJS (Abstraction x e) = "function(" <> x <> ") { return " <> toJS e <> "; }"

instance ToJS Declaration where
  toJS (Declaration x e) = "var " <> mangle x <> " = " <> toJS e <> ";"

mangle :: Text -> Text
mangle t = if isKeyword t
           then "__" <> t
           else let (n, x) = span isDigit t
                 in (if n == "" then "" else "__") <> n <> substitute x
  where
    isDigit   = flip elem "0123456789"
    isKeyword = flip elem ["break","const","continue","delete","do","while"
                          ,"export","for","in","function","if","else","import"
                          ,"instanceOf","label","let","new","return","switch"
                          ,"this","throw","try","catch","typeof","var","void"
                          ,"with","yield","true","false","null"]
    substitute = flip (foldr (\(src, dst) memo -> replace src dst memo))
                   [("*", "__mul")
                   ,("+", "__add")
                   ,("-", "__sub")
                   ,("/", "__div")
                   ,("~", "__tilde")
                   ,("!", "__bang")
                   ,("@", "__at")
                   ,("#", "__pound")
                   ,("$", "__dollar")
                   ,("%", "__pct")
                   ,("^", "__carat")
                   ,("&", "__amp")
                   ,("?", "__q")
                   ,("=", "__eq")
                   ,("<", "__lt")
                   ,(">", "__gt")
                   ,(".", "__dot")
                   ,("(", "__lparen")
                   ,(")", "__rparen")
                   ,("[", "__lbrack")
                   ,("]", "__rbrack")]
