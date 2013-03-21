{-# LANGUAGE OverloadedStrings #-}

module Lambad.Eval
  ( Eval
  , Step(..)
  , Environment
  , runEval
  , renderTrace
  , emptyEnv
  , extendEnv
  ) where

import qualified Data.Text as T
import qualified Data.Map  as M
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import Lambad.Pretty

instance Error T.Text where
  noMsg   = ""
  strMsg  = T.pack

type Id
  = T.Text

data Step a
  = Antecedent a
  | Consequent a
  deriving (Eq, Show)

type Environment a
  = M.Map Id a

type Eval a
  = ReaderT (Environment a, Bool) (ErrorT T.Text (WriterT [Step a] Identity)) a

runEval :: Environment a -> Eval a -> (Either T.Text a, [Step a])
runEval env action
  = runIdentity $ runWriterT $ runErrorT $ runReaderT action (env, False)

renderTrace :: Pretty a => [Step a] -> T.Text
renderTrace = T.unlines . map trace . indentTrace
  where
    trace (n, e)        = T.replicate n "  " <> step e
    step (Antecedent e) = ">> " <> renderText e
    step (Consequent e) = "=> " <> renderText e

indentTrace :: [Step a] -> [(Int, Step a)]
indentTrace = reverse . walk' 0 []
  where
    walk' _ r []                  = r
    walk' n r (Antecedent x:xs) = walk' (n + 1) ((n,     Antecedent x):r) xs
    walk' n r (Consequent x:xs) = walk' (n - 1) ((n - 1, Consequent x):r) xs

--------------------------------------------------------------------------------

emptyEnv :: Environment a
emptyEnv = M.empty

extendEnv :: Id -> a -> Environment a -> Environment a
extendEnv = M.insert
