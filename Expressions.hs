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
  , ("apply", apply)
  , ("fix", fix)
  , ("plus", plus)
  , ("minus", minus)
  , ("times", times)
  , ("equal", equal)
  , ("if", if_)
  ]

pop :: Function
pop = tail

dup :: Function
dup = (:) =<< head

swap :: Function
swap l = (head . tail) l : head l : (tail . tail) l

dip :: Function
dip l = (head . swap) l : (apply . tail . swap) l

apply :: Function
apply (FunVal f : s) = f s
apply s              = error $ "this is not a function, it cannot be applied; " ++ show s

newtype Mu a = Roll { unroll :: Mu a -> a }

fixImpl :: ((a -> b) -> a -> b) -> a -> b
fixImpl f = (\x a -> f (unroll x x) a) $ Roll (\x a -> f (unroll x x) a)

fix :: Function
fix (FunVal f : x : s) = fixImpl (\g a -> f $ FunVal g : a) [x] ++ s
fix _                  = error "cannot apply fix to a non-function"

plus :: Function
plus (IntVal y : IntVal x : s) = IntVal (x + y) : s
plus _                         = error "cannot add non-integers"

minus :: Function
minus (IntVal y : IntVal x : s) = IntVal (x - y) : s
minus _                         = error "cannot subtract non-integers"

times :: Function
times (IntVal y : IntVal x : s) = IntVal (x * y) : s
times _                         = error "cannot multiply non-integers"

equal :: Function
equal (IntVal y : IntVal x : s) = BoolVal (x == y) : s
equal _                         = error "cannot equate non-integers"

if_ :: Function
if_ (BoolVal b : tVal : fVal : s) = (if b then tVal else fVal) : s
if_ _                             = error "invalid call to if"

listPrimitives :: [(String, Function)]
listPrimitives = [ ("nil", nil)
                 , ("cons", cons)
                 , ("foldl", foldl)
                 , ("listMatch", listMatch) ]

nil :: Function
nil = push $ ListVal []

cons :: Function
cons (v : ListVal l : s) = ListVal (v : l) : s
cons _                   = error "Can't cons onto a non-list"

foldl :: Function
foldl (ListVal l : b : FunVal f : s) = List.foldl (\acc val -> head . f $ [acc, val]) b l : s
foldl _ = undefined

listMatch :: Function
listMatch (ListVal [] : FunVal fNil : FunVal _ : s) = fNil [] ++ s
listMatch (ListVal (x : xs) : FunVal _ : FunVal fCons : s) = fCons [x, ListVal xs] ++ s
listMatch _                                                = error "bad listMatch"
