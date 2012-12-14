{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Pure.Repl
  ( eval
  , pval
  ) where

import Data.Attoparsec.Text (parse, feed, IResult(..))
import Data.Monoid (mempty)
import qualified Data.Text as T

import Language.Pure.Eval
import Language.Pure.Misc
import Language.Pure.Parser
import Language.Pure.Pretty
import Language.Pure.Syntax

eval ∷ Pretty a ⇒ (Expression → Eval a) → T.Text → (Either T.Text a, [Step a])
eval interpreter code
  = case ast of
      Right e -> runEval emptyEnv (interpreter e)
      Left e  -> (Left e, mempty)
  where
    ast = case feed (parse parseExpr code) T.empty of
            Done "" e  -> Right e
            Done x  _  -> Left (T.append "error: unparsed " x)
            Fail x _ _ -> Left (T.append "error: fail " x)

pval ∷ Pretty a ⇒ (Expression → Eval a) → T.Text → IO ()
pval interpreter code
  = putStrLn $ either T.unpack renderString $ fst $ eval interpreter code
