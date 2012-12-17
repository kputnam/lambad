{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Pure.Repl
  ( eval
  , pval
  , aval
  ) where

import Data.Attoparsec.Text (parse, feed, skipSpace, IResult(..))
import Data.Monoid (mempty)
import Control.Applicative ((<*))
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
    ast = case feed (parse (parseExpr <* skipSpace) code) T.empty of
            Done "" e  -> Right e
            Done x  _  -> Left x
            Fail x _ _ -> Left x

pval ∷ Pretty a ⇒ (Expression → Eval a) → T.Text → IO (Either T.Text a)
pval interpreter code
  = do putStr   $ T.unpack (renderTrace trace)
       putStrLn $ show (length trace `div` 2) ++ " steps\n"
       return val
  where (val, trace) = eval interpreter code

aval ∷ T.Text → IO ()
aval code
  = sequence_ [ putStrLn $ name ++ " (" ++ show nsteps ++ "): " ++ pretty
              | (name, interpreter) <- es
              , let (res, trace) = eval interpreter code
              , let nsteps       = length trace `div` 2
              , let pretty       = either T.unpack renderString res ]
  where
    es ∷ [(String, Expression → Eval Expression)]
    es = [ ("bn", callByName)
         , ("no", normalOrder)
         , ("bv", callByValue)
         , ("ao", applicativeOrder)
         , ("ha", hybridApplicative)
         , ("he", headSpine)
         , ("hn", hybridNormal) ]
