{-# OPTIONS_GHC -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Builtin where

import Types
import Terms

import Data.Map (Map)
import qualified Data.Map as Map

arithTy :: FuncType
arithTy = F (S "A" [VIntTy, VIntTy]) (S "A" [VIntTy])

plus :: [Value] -> [Value]
plus (IntVal y : IntVal x : s) = IntVal (x + y) : s

minus :: [Value] -> [Value]
minus (IntVal y : IntVal x : s) = IntVal (x - y) : s

times :: [Value] -> [Value]
times (IntVal y : IntVal x : s) = IntVal (x * y) : s

equal :: [Value] -> [Value]
equal (IntVal y : IntVal x : s) = BoolVal (x == y) : s
equalTy :: FuncType
equalTy = F (S "A" [VIntTy, VIntTy]) (S "A" [VBoolTy])

apply :: [Value] -> [Value]
apply (FuncVal f : s) = f s
applyTy :: FuncType
applyTy = F (S "A" [VVarTy "a", VFuncTy (F (S "B" [VVarTy "a"]) (S "B" [VVarTy "b"]))]) (S "A" [VVarTy "b"])

apply2 :: [Value] -> [Value]
apply2 (FuncVal f : x : y : s) = f [x, y] ++ s
apply2Ty :: FuncType
apply2Ty = F (S "A" [VVarTy "b", VVarTy "a", VFuncTy (F (S "B" [VVarTy "b", VVarTy "a"]) (S "B" [VVarTy "c"]))]) (S "A" [VVarTy "c"])

pop :: [Value] -> [Value]
pop (x : s) = s
popTy :: FuncType
popTy = F (S "A" [VVarTy "a"]) (S "A" [])

dup :: [Value] -> [Value]
dup (x : s) = x : x : s
dupTy :: FuncType
dupTy = F (S "A" [VVarTy "a"]) (S "A" [VVarTy "a", VVarTy "a"])

swap :: [Value] -> [Value]
swap (y : x : s) = x : y : s
swapTy :: FuncType
swapTy = F (S "A" [VVarTy "a", VVarTy "b"]) (S "A" [VVarTy "b", VVarTy "a"])

dip :: [Value] -> [Value]
dip (FuncVal f : b : a : s) = b : f [a] ++ s
dipTy :: FuncType
dipTy = F (S "A" [VVarTy "a", VVarTy "b", VFuncTy (F (S "B" [VVarTy "a"]) (S "B" [VVarTy "c"]))]) (S "A" [VVarTy "c", VVarTy "b"])

dip2 :: [Value] -> [Value]
dip2 (FuncVal f : c : b : a : s) = c : f [b, a] ++ s
dip2Ty :: FuncType
dip2Ty = F (S "A" [VVarTy "a", VVarTy "b", VVarTy "c", VFuncTy (F (S "B" [VVarTy "a", VVarTy "b"]) (S "B" [VVarTy "d"]))]) (S "A" [VVarTy "d", VVarTy "c"])

-- adapted from https://programmers.stackexchange.com/questions/215712/type-checking-and-recursive-types-writing-the-y-combinator-in-haskell-ocaml
newtype Mu a = Roll { unroll :: Mu a -> a }
fixImpl :: ((a -> b) -> a -> b) -> a -> b
fixImpl f = (\x a -> f (unroll x x) a) $ Roll (\x a -> f (unroll x x) a)

fix :: [Value] -> [Value]
fix (FuncVal f : s) = fixImpl (\g a -> g (FuncVal f : a)) s
fixTy :: FuncType
fixTy = F (S "A" [VVarTy "a", VFuncTy (F (S "B" [VVarTy "a", VFuncTy (F (S "C" [VVarTy "a"]) (S "C" [VVarTy "b"]))]) (S "B" [VVarTy "b"]))]) (S "A" [VVarTy "b"]) 

ifFunc :: [Value] -> [Value]
ifFunc (BoolVal b : x : y : s) = (if b then x else y) : s
ifTy :: FuncType
ifTy = F (S "A" [VVarTy "a", VVarTy "a", VBoolTy]) (S "A" [VVarTy "a"])

nil :: [Value] -> [Value]
nil s = ListVal [] : s
nilTy :: FuncType
nilTy = F (S "A" []) (S "A" [VListTy $ VVarTy "a"])

cons :: [Value] -> [Value]
cons (x : ListVal l : s) = ListVal (x : l) : s
consTy :: FuncType
consTy = F (S "A" [VListTy $ VVarTy "a", VVarTy "a"]) (S "A" [VListTy $ VVarTy "a"])

listMatch :: [Value] -> [Value]
listMatch (ListVal [] : FuncVal nilCase : _ : s)        = nilCase [] ++ s
listMatch (ListVal (x : xs) : _ : FuncVal consCase : s) = consCase [x, ListVal xs] ++ s
listMatchTy :: FuncType
listMatchTy = F (S "A" [VFuncTy (F (S "B" [VListTy $ VVarTy "a", VVarTy "a"]) (S "B" [VVarTy "b"])), VFuncTy (F (S "C" []) (S "C" [VVarTy "b"])), VListTy $ VVarTy "a"]) (S "A" [VVarTy "b"])

builtins :: Map Builtin ([Value] -> [Value], FuncType)
builtins = Map.fromList
  [ ("plus", (plus, arithTy))
  , ("minus", (minus, arithTy))
  , ("times", (times, arithTy))
  , ("equal", (equal, equalTy))
  , ("apply", (apply, applyTy))
  , ("apply2", (apply2, apply2Ty))
  , ("pop", (pop, popTy))
  , ("dup", (dup, dupTy))
  , ("swap", (swap, swapTy))
  , ("dip", (dip, dipTy))
  , ("dip2", (dip2, dip2Ty))
  , ("fix", (fix, fixTy))
  , ("if", (ifFunc, ifTy))
  , ("nil", (nil, nilTy))
  , ("cons", (cons, consTy))
  , ("listMatch", (listMatch, listMatchTy))
  ]

builtinFuncs :: Map Builtin ([Value] -> [Value])
builtinFuncs = Map.map fst builtins

builtinTypes :: Map Builtin FuncType
builtinTypes = Map.map snd builtins

