{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Repl
  ( eval
  , evalPrint
  , evalEach
  , evalEnv
  ) where

import Data.Attoparsec.Text (parse, feed, skipSpace, IResult(..))
import Data.Monoid
import Control.Applicative ((<*))
import qualified Data.Text as T

import Lambad.Pretty
import Lambad.Pure.Eval
import Lambad.Pure.Parser
import Lambad.Pure.Syntax

eval :: Pretty a => (Term -> Eval a)
                -> Environment a
                -> T.Text
                -> (Either T.Text a, [Step a])
eval interpreter environment code
  = case ast of
      Right e -> runEval environment (interpreter e)
      Left e  -> (Left e, mempty)
  where
    ast = case feed (parse (parseTerm <* skipSpace) code) T.empty of
            Done "" e  -> Right e
            Done x _   -> Left x
            Fail x _ _ -> Left x

evalPrint :: Pretty a => (Term -> Eval a)
                     -> Environment a
                     -> T.Text
                     -> IO (Either T.Text a)
evalPrint interpreter environment code
  = do putStr   $ T.unpack (renderTrace trace)
       putStrLn $ show (length trace `div` 2) ++ " steps\n"
       return val
  where (val, trace) = eval interpreter environment code

evalEach :: T.Text -> Environment Term -> IO ()
evalEach code environment
  = sequence_ [ putStrLn $ name ++ " (" ++ show nsteps ++ "): " ++ pretty
              | (name, interpreter) <- es
              , let (res, trace) = eval interpreter environment code
              , let nsteps = length trace `div` 2
              , let pretty = either (T.unpack . ("error: " <>)) renderString res
              ]
  where
    es :: [(String, Term -> Eval Term)]
    es = [("ao", applicativeOrder)
         ,("no", normalOrder)
         ,("ha", hybridApplicative)
         ,("hn", hybridNormal)
         ,("bv", callByValue)
         ,("he", headSpine)
         ,("bn", callByName)]

evalEnv :: (Term -> Eval a) -> T.Text -> Either T.Text (Environment a)
evalEnv interpreter code
  = buildEnv interpreter =<< ast
  where
    ast :: Either T.Text [Definition]
    ast = case feed (parse (parseFile <* skipSpace) code) T.empty of
            Done "" e  -> Right e
            Done x _   -> Left x
            Fail x _ _ -> Left x
