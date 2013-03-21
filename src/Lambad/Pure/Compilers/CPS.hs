{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Compilers.CPS
  ( cpsDecl
  , cpsExpr
  ) where

import Lambad.Pure.Syntax
import Control.Monad.State
import Data.Text (Text, pack)

type Id
  = Text

cpsDecl :: Definition -> State Int Definition
cpsDecl (Definition x e@(Variable _))
  = return $ Definition x e
cpsDecl (Definition x e@(Application _ _))
  = let idExpr = Abstraction "x" (Variable "x")
     in Definition x `fmap` cpsExpr idExpr e
cpsDecl (Definition x e@(Abstraction _ _))
  = do (Application _ e') <- cpsExpr (Variable "_") e
       return $ Definition x e'

withFresh :: (Id -> State Int b) -> State Int b
withFresh f = modify (+ 1) >> (get >>= f . pack . ("k" ++) . show)

cpsExpr :: Term -> Term -> State Int Term
cpsExpr k (Variable x)
  = return $ Application k (Variable x)
cpsExpr k (Abstraction x e)
  -- Pass the rewritten function to k
  = withFresh $ \k' ->
      do inner <- cpsExpr (Variable k') e
         return $ Application k $
                    Abstraction x $
                      Abstraction k' inner
cpsExpr k (Application f@(Variable _) e@(Variable _))
  -- Special case for application
  = return $ Application (Application f e) k
cpsExpr k (Application f@(Variable _) e)
  -- Special case for application
  = withFresh $ \kE ->
      cpsExpr (Abstraction kE $
             Application
               (Application f (Variable kE)) k) e
cpsExpr k (Application f e@(Variable _))
  -- Special case for application
  = withFresh $ \kF ->
      cpsExpr (Abstraction kF (Application (Application (Variable kF) e) k)) f
cpsExpr k (Application f e)
  -- General case for application
  = withFresh $ \kF ->
      withFresh $ \kE ->
        do e' <- cpsExpr (Abstraction kE $
                          Application
                            (Application (Variable kF)
                                         (Variable kE)) k) e
           cpsExpr (Abstraction kF e') f
