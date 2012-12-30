{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambad.DeBruijn.Eval
  ( emptyEnv
  , buildEnv
  , extendEnv
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
  , Environment
  ) where

import qualified Data.Map  as M
import qualified Data.Text as T

import Control.Arrow (second)
import Control.Applicative
import Data.Maybe

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Error

import Lambad.Misc
import Lambad.Pretty
import Lambad.DeBruijn.Syntax

-- import Language.DeBruijn.Misc
-- import Language.DeBruijn.Syntax
-- import Language.DeBruijn.Pretty

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
  = ReaderT (Environment a, Bool) (ErrorT T.Text (WriterT [Step a] Identity)) a

runEval ∷ Environment a → Eval a → (Either T.Text a, [Step a])
runEval env action
  = runIdentity $ runWriterT $ runErrorT $ runReaderT action (env, False)

renderTrace ∷ Pretty a ⇒ [Step a] → T.Text
renderTrace = T.unlines . map trace . indentTrace
  where
    trace (n, e)        = T.append (T.replicate n "  ") (step e)
    step (Antecedent e) = T.append ">> " (renderText e)
    step (Consequent e) = T.append "=> " (renderText e)

indentTrace ∷ [Step a] → [(Int, Step a)]
indentTrace = reverse . walk' 0 []
  where
    walk' n r []                = r
    walk' n r (Antecedent x:xs) = walk' (n + 1) ((n,     Antecedent x):r) xs
    walk' n r (Consequent x:xs) = walk' (n - 1) ((n - 1, Consequent x):r) xs

--------------------------------------------------------------------------------

emptyEnv ∷ Environment a
emptyEnv = M.empty

extendEnv ∷ Id → a → Environment a → Environment a
extendEnv = M.insert

buildEnv ∷ (Expression → Eval a) → [Declaration] → Either T.Text (Environment a)
buildEnv interpreter = be emptyEnv
  where
    be env [] = Right env
    be env (Declaration id expr:xs)
      = let (res, _) = runEval env (interpreter expr)
        in do val <- res
              be (extendEnv id val env) xs

--------------------------------------------------------------------------------

appHelper ∷ Expression → Expression → Expression
appHelper arg body = shift (-1) $ substitute (0, shift 1 arg) body
  where
    substitute ∷ (Int, Expression) → Expression → Expression
    substitute (n, v)
      = mapTerm (\c m → if m == c + n then shift c v else BoundVariable m)

    shift ∷ Int → Expression → Expression
    shift d
      = mapTerm (\c m → BoundVariable $ if m >= c then m + d else m)

    mapTerm ∷ (Int → Int → Expression) → Expression → Expression
    mapTerm f = walk 0
      where
        walk _ e@(FreeVariable _) = e
        walk c (Application f a)  = Application (walk c f) (walk c a)
        walk c (Abstraction b)    = Abstraction $ walk (c + 1) b
        walk c (BoundVariable n') = f c n'

eqHelper ∷ (Expression, Expression) → Eval Expression → Eval Expression
eqHelper (a, b) other
  = do inLambda <- asks snd
       if inLambda
          then other
          else alphaEq hybridApplicative a b
  where
    alphaEq ∷ Eq a ⇒ (Expression → Eval a) → Expression → Expression → Eval a
    alphaEq interpreter a b
      = do a' <- interpreter a
           b' <- interpreter b
           interpreter (if a' == b' then true else false)
      where
        true   = Abstraction (Abstraction (BoundVariable 1))
        false  = Abstraction (Abstraction (BoundVariable 0))

etaReduce ∷ Expression → Expression
etaReduce e@(Abstraction (Application f (BoundVariable 0)))
  = fromMaybe e (walk 0 f)
  where
    walk _ e@(FreeVariable _) = Just e
    walk n (Application f a)  = Application <$> walk n f <*> walk n a
    walk n (Abstraction b)    = Abstraction <$> walk (n+1) b
    walk n (BoundVariable m)
      | n == m    = Nothing
      | otherwise = Just (BoundVariable (m-1))
etaReduce e = e

inLambda ∷ Eval a → Eval a
inLambda = local $ second (const True)

trace ∷ (Expression → Eval a) → Expression → Eval a
trace r e = antecedent e *> r e >>= liftA2 (*>) consequent return
  where antecedent = tell . (:[]) . Antecedent
        consequent = tell . (:[]) . Consequent

--------------------------------------------------------------------------------

-- Reduce to weak head normal form
callByName ∷ Expression → Eval Expression
callByName = bn
  where
    bn = trace bn'
    bn' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    bn' e@(BoundVariable _) = pure e
    bn' e@(Abstraction _)   = pure (etaReduce e)
    bn' (Application f a)   = applyM2 app (bn f) (pure a)
    app (Abstraction b) a   = bn $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
normalOrder ∷ Expression → Eval Expression
normalOrder = no
  where
    bn = callByName
    no = trace no'
    no' e@(BoundVariable _) = pure e
    no' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    no' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> no b
    no' (Application f a)   = applyM2 app (no f) (pure a)
    app (Abstraction b) a   = no $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (Application <$> no f <*> no b)
    app f a                 = Application <$> no f <*> no a

-- Reduce to weak normal form
callByValue ∷ Expression → Eval Expression
callByValue = bv
  where
    bv = trace bv'
    bv' e@(BoundVariable _) = pure e
    bv' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    bv' e@(Abstraction _)   = pure (etaReduce e)
    bv' (Application f a)   = applyM2 app (bv f) (bv a)
    app (Abstraction b) a   = bv $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
applicativeOrder ∷ Expression → Eval Expression
applicativeOrder = ao
  where
    ao = trace ao'
    ao' e@(BoundVariable _) = pure e
    ao' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    ao' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> ao b
    ao' (Application f a)   = applyM2 app (ao f) (ao a)
    app (Abstraction b) a   = ao $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
hybridApplicative ∷ Expression → Eval Expression
hybridApplicative = ha
  where
    bv = callByValue
    ha = trace ha'
    ha' e@(BoundVariable _) = pure e
    ha' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    ha' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> ha b
    ha' (Application f a)   = applyM2 app (bv f) (ha a)
    app (Abstraction b) a   = ha $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (Application <$> ha f <*> pure b)
    app f a                 = Application <$> ha f <*> pure a

-- Reduce to head normal form
headSpine ∷ Expression → Eval Expression
headSpine = he
  where
    he = trace he'
    he' e@(BoundVariable _) = pure e
    he' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    he' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> he b
    he' (Application f a)   = applyM2 app (he f) (pure a)
    app (Abstraction b) a   = he $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (pure $ Application f b)
    app f a                 = pure $ Application f a

-- Reduce to normal form
hybridNormal ∷ Expression → Eval Expression
hybridNormal = hn
  where
    he = headSpine
    hn = trace hn'
    hn' e@(BoundVariable _) = pure e
    hn' e@(FreeVariable x)  = M.findWithDefault e x <$> asks fst
    hn' (Abstraction b)     = inLambda $ etaReduce . Abstraction <$> hn b
    hn' (Application f a)   = applyM2 app (he f) (pure a)
    app (Abstraction b) a   = hn $ appHelper a b
    app f@(Application (FreeVariable "=") a) b
                            = eqHelper (a, b) (Application <$> hn f <*> hn b)
    app f a                 = Application <$> hn f <*> hn a
