{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambad.Machines.CEKGo
  ( eval
  ) where

import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe

import Lambad.Pure.Syntax

type Id
  = Text

data Value
  = Closure Id Program Environment
  deriving (Show)

-- C: Control
type Program
  = Expression

-- E: Environment
type Environment
  = M.Map Id Value

-- K: Continuation
data Frame
  = Stop
  | Operand Value Frame                 -- need an operand
  | Operator Program Environment Frame  -- need an operator
  | Mark Frame

type State
  = (Program, Environment, Frame)

eval ∷ Program → Value
eval
  = uncurry lift . two . fold . inject
  where
    final (Abstraction _ _, _, Stop) = True
    final _                          = False

    two (a, b, c)      = (a, b)
    fold s | final s   = s
           | otherwise = fold (step s)

inject ∷ Program → State
inject e
  = (e, M.empty, Stop)

lift ∷ Program → Environment → Value
lift (Abstraction x b)
  = Closure x b

step ∷ State → State

-- Lookup references in the environment ρ
step (Variable x, ρ, κ)
  = unlift (fromJust $ M.lookup x ρ)
  where
    unlift (Closure x b ρ) = (Abstraction x b, ρ, κ)

-- Here pushes a marker onto the stack
step (Application (Variable "here") e, ρ, κ)
  = (e, ρ, Mark κ)

-- Jump erases the stack up to the nearest marker
step (Application (Variable "jump") e, ρ, κ)
  = let find (Mark κ)         = κ
        find (Operand _ κ)    = find κ
        find (Operator _ _ κ) = find κ
     in (e, ρ, find κ)

-- For application, first evaluate the operator
step (Application f a, ρ, κ)
  = (f, ρ, Operator a ρ κ)

-- We have an operator, next evaluate the operand
step (Abstraction x b, ρ, Operator a ρ' κ)
  = (a, ρ', Operand (Closure x b ρ) κ)

-- Remove the topmost marker, assuming e becomes a Value
step (e, ρ, Mark κ)
  = (e, ρ, κ)

-- We have an operand, next evaluate operator body
step (e, ρ, Operand (Closure x' b' ρ') κ)
  = (b', M.insert x' (lift e ρ) ρ', κ)
