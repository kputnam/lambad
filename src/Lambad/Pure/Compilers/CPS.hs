{-# LANGUAGE OverloadedStrings #-}

module Lambad.Pure.Compilers.CPS
  ( cpsDecl
  , cpsExpr
  ) where

import Lambad.Pure.Syntax
import Control.Monad.State
import Data.Text (Text, pack)

type Id = Text

cpsDecl :: Declaration -> State Int Declaration
cpsDecl (Declaration x e@(Variable _))
  = return $ Declaration x e
cpsDecl (Declaration x e@(Application _ _))
  = let idExpr = (Abstraction "x" (Variable "x"))
     in Declaration x `fmap` (cpsExpr idExpr e)
cpsDecl (Declaration x e@(Abstraction _ _))
  = do (Application _ e) <- cpsExpr (Variable "_") e
       return $ Declaration x e

withFresh :: (Id -> State Int b) -> State Int b
withFresh f = modify (+ 1) >> (get >>= f . pack . ("k" ++) . show)

cpsExpr :: Expression -> Expression -> State Int Expression
cpsExpr k (Variable x)
  = return $ Application k (Variable x)
cpsExpr k (Abstraction x e)
  = withFresh $ \k' ->
      do inner <- cpsExpr (Variable k') e
         return $ Application k $
                    Abstraction x $
                      Abstraction k' inner
cpsExpr k (Application f@(Variable _) e@(Variable _))
  = return $ Application (Application f e) k
cpsExpr k (Application f@(Variable _) e)
  = withFresh $ \kE ->
      cpsExpr (Abstraction kE $
             flip Application k
               (Application f (Variable kE))) e
cpsExpr k (Application f e@(Variable _))
  = withFresh $ \kF ->
      cpsExpr (Abstraction kF (Application (Application (Variable kF) e) k)) f
cpsExpr k (Application f e)
  = withFresh $ \kF ->
      withFresh $ \kE ->
        do e' <- cpsExpr (Abstraction kE $
                          flip Application k
                            (Application (Variable kF)
                                         (Variable kE))) e
           cpsExpr (Abstraction kF e') f
