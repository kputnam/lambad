{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Pure.Eval
  ( emptyEnv
  , runEval
  , callByName
  , normalOrder
  , callByValue
  , applicativeOrder
  , hybridApplicative
  , headSpine
  , hybridNormal
  , renderTrace
  , Eval(..)
  , Step(..)
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Data.List            ((\\), nub)
import Data.Functor         ((<$>))
import Data.Function        (on)
import Data.Attoparsec.Text (parseOnly)
import Control.Arrow        (second)
import Control.Applicative  ((<*), (<*>), (*>), liftA2)

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import Language.Pure.Misc
import Language.Pure.Syntax
import Language.Pure.Parser
import Language.Pure.Pretty

instance Error T.Text where
  noMsg   = ""
  strMsg  = T.pack

type Id
  = T.Text

data Step a
  = Antecedent Expression
  | Consequent a
  deriving (Eq, Show)

type Environment a
  = M.Map Id a

--------------------------------------------------------------------------------

type Eval a
  = ReaderT (Environment a) (ErrorT T.Text (WriterT [Step a] Identity)) a

runEval ∷ Environment a → Eval a → (Either T.Text a, [Step a])
runEval env action
  = runIdentity $ runWriterT $ runErrorT $ runReaderT action env

renderTrace ∷ Pretty a ⇒ [Step a] → T.Text
renderTrace = T.unlines . map trace . indentTrace
  where
    trace (n, e)        = T.append (T.replicate n "  ") (step e)
    step (Antecedent e) = T.append ">> " (renderText e)
    step (Consequent e) = T.append "=> " (renderText e)

indentTrace ∷ [Step a] → [(Int, Step a)]
indentTrace = reverse . walk' 0 []
  where
    walk' n r []                  = r
    walk' n r (Antecedent x:xs) = walk' (n + 1) ((n,     Antecedent x):r) xs
    walk' n r (Consequent x:xs) = walk' (n - 1) ((n - 1, Consequent x):r) xs

--------------------------------------------------------------------------------

emptyEnv ∷ Environment a
emptyEnv = M.empty

extendEnv ∷ Id → a → Environment a → Environment a
extendEnv = M.insert

substitute ∷ (Id, Expression) → Expression → Expression
substitute s (Application e f)
  = Application (substitute s e) (substitute s f)
substitute (x, v) e@(Variable x')
  | x == x'   = v
  | otherwise = e
substitute s@(_, v) (Abstraction x b)
  | x `notElem` freevars v = Abstraction x (substitute s b)
  | otherwise = let x' = freshvar x (freevars v)
                    b' = substitute (x, Variable x') b
                 in Abstraction x' (substitute (x', v) b')
  where
    freshvar x xs
      | x `elem` xs = freshvar (T.append x "'") xs
      | otherwise   = x
    freevars (Variable x)      = [x]
    freevars (Application e f) = nub (freevars e ++ freevars f)
    freevars (Abstraction x e) = freevars e \\ [x]

wrap ∷ (Expression → Eval a) → Expression → Eval a
wrap r e = antecedent e *> r e >>= liftA2 (*>) consequent return
  where antecedent = tell . (:[]) . Antecedent
        consequent = tell . (:[]) . Consequent

--------------------------------------------------------------------------------

callByName ∷ Expression → Eval Expression
callByName = bn
  where
    bn = wrap bn'
    bn' e@(Variable _)      = return e
    bn' e@(Abstraction _ _) = return e
    bn' (Application (Abstraction x b) a)
      = bn (substitute (x, a) b)
    bn' e@(Application _ _) = return e

normalOrder ∷ Expression → Eval Expression
normalOrder = no
  where
    no = wrap no'
    no' e@(Variable _)    = return e
    no' (Abstraction x b) = Abstraction x <$> no b
    no' (Application f a)
      = do f' <- callByName f
           case f' of
             Abstraction x b → no (substitute (x, a) b)
             _               → Application <$> no f' <*> no a

callByValue ∷ Expression → Eval Expression
callByValue = bv
  where
    bv = wrap bv'
    bv' e@(Variable _)      = return e
    bv' e@(Abstraction _ _) = return e
    bv' (Application f a)
      = do f' <- bv f
           a' <- bv a
           case f' of
             Abstraction x b → bv (substitute (x, a') b)
             _               → return (Application f' a')

applicativeOrder ∷ Expression → Eval Expression
applicativeOrder = ao
  where
    ao = wrap ao'
    ao' e@(Variable _)    = return e
    ao' (Abstraction x b) = Abstraction x <$> ao b
    ao' (Application f a)
      = do f' <- ao f
           a' <- ao a
           case f' of
             Abstraction x b → ao (substitute (x, a') b)
             _               → return (Application f' a')

hybridApplicative ∷ Expression → Eval Expression
hybridApplicative = ha
  where
    ha = wrap ha'
    ha' e@(Variable _)    = return e
    ha' (Abstraction x b) = Abstraction x <$> ha b
    ha' (Application f a)
      = do f' <- ha f
           a' <- ha a
           case f' of
             Abstraction x b → ha (substitute (x, a') b)
             _               → flip Application a' <$> ha f'

headSpine ∷ Expression → Eval Expression
headSpine = he
  where
    he = he'
    he' e@(Variable _)    = return e
    he' (Abstraction x b) = Abstraction x <$> he b
    he' (Application f a)
      = do f' <- he f
           case f' of
             Abstraction x b → he (substitute (x, a) b)
             _               → return (Application f' a)

hybridNormal ∷ Expression → Eval Expression
hybridNormal = hn
  where
    hn = wrap hn'
    hn' e@(Variable _)    = return e
    hn' (Abstraction x b) = Abstraction x <$> hn b
    hn' (Application f a)
      = do f' <- headSpine f
           case f' of
             Abstraction x b → hn (substitute (x, a) b)
             _               → Application <$> hn f' <*> hn a
