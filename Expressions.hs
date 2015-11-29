{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

module Expressions where

import Prelude hiding (foldl)

import qualified Data.List as List

type Function = [StackValue] -> [StackValue]

data StackValue =
    IntVal Int
  | BoolVal Bool
  | FunVal Function
  | ListVal [StackValue]

instance Show StackValue where
  show (IntVal i) = show i
  show (BoolVal True) = "true"
  show (BoolVal False) = "false"
  show (FunVal _) = "<function>"
  show (ListVal sv) = show sv

push :: StackValue -> Function
push = (:)

stackPrimitives :: [(String, Function)]
stackPrimitives =
  [ ("pop", pop)
  , ("dup", dup)
  , ("swap", swap)
  , ("dip", dip)
  , ("eval", eval)
  ]

pop :: Function
pop = tail

dup :: Function
dup = (:) =<< head

swap :: Function
swap l = (head . tail) l : head l : (tail . tail) l

dip :: Function
dip l = (head . swap) l : (eval . tail . swap) l

eval :: Function
eval (FunVal f : s) = f s
eval _              = error "Can't eval a non-function"

listPrimitives :: [(String, Function)]
listPrimitives = [ ("nil", nil)
                 , ("cons", cons)
                 , ("foldl", foldl)]

nil :: Function
nil = push $ ListVal []

cons :: Function
cons (v : ListVal l : s) = ListVal (v : l) : s
cons _                   = error "Can't cons onto a non-list"

foldl :: Function
foldl (ListVal l : b : FunVal f : s) = List.foldl (\acc val -> head . f $ [acc, val]) b l : s
foldl _ = undefined
