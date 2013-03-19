module Lambad.Pure.Machines.CEK
  ( eval
  ) where

import qualified Data.Map as M
import Data.Text (Text)
import Data.Maybe

import Lambad.Pure.Syntax

type Id
  = Text

type Program
  = Expression

-- C: Control
data Value
  = Closure Id Program Environment
  deriving (Show)

-- E: Environment
type Environment
  = M.Map Id Value

-- K: Continuation
data Frame
  = Stop
  | Operand Value Frame                 -- need an operand
  | Operator Program Environment Frame  -- need an operator

type State
  = (Program, Environment, Frame)

eval :: Program -> Value
eval
  = uncurry lift . two . fold . inject
  where
    final (Abstraction _ _, _, Stop) = True
    final _                          = False

    two (a, b, _)      = (a, b)
    fold s | final s   = s
           | otherwise = fold (step s)


inject :: Program -> State
inject e
  = (e, M.empty, Stop)

lift :: Program -> Environment -> Value
lift (Abstraction x b)
  = Closure x b

step :: State -> State
step (Variable x, ρ, κ)
  -- Lookup references in the environment ρ
  = unlift (fromJust $ M.lookup x ρ)
  where
    unlift (Closure x b ρ) = (Abstraction x b, ρ, κ)
step (Application f a, ρ, κ)
  -- For application, first evaluate the operator
  = (f, ρ, Operator a ρ κ)
step (Abstraction x b, ρ, Operator a ρ' κ)
  -- We have an operator, next evaluate the operand
  = (a, ρ', Operand (Closure x b ρ) κ)
step (e, ρ, Operand (Closure x' b' ρ') κ)
  -- We have an operand, next evaluate operator body
  = (b', M.insert x' (lift e ρ) ρ', κ)
