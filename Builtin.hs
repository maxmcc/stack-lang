{-# OPTIONS_GHC -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Builtin where

import Language

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
apply (FuncVal _ f : s) = f s
apply1to1Ty :: FuncType
apply1to1Ty = F (S "A" [VVarTy "a", appliedFuncTy]) (S "A" [VVarTy "b"])
  where appliedFuncTy = VFuncTy (F (S "B" [VVarTy "a"]) (S "B" [VVarTy "b"]))
apply2to1Ty :: FuncType
apply2to1Ty = F (S "A" [VVarTy "b", VVarTy "a", appliedFuncTy])
                (S "A" [VVarTy "c"])
  where appliedFuncTy = VFuncTy (F (S "B" [VVarTy "b", VVarTy "a"])
                                   (S "B" [VVarTy "c"]))
apply2to2Ty :: FuncType
apply2to2Ty = F (S "A" [VVarTy "b", VVarTy "a", appliedFuncTy])
                (S "A" [VVarTy "d", VVarTy "c"])
  where appliedFuncTy = VFuncTy (F (S "B" [VVarTy "b", VVarTy "a"])
                                   (S "B" [VVarTy "d", VVarTy "c"]))

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
dip (FuncVal _ f : b : s) = b : f s
dip1to1Ty :: FuncType
dip1to1Ty = F (S "A" [VVarTy "a", VVarTy "b", appliedFuncTy])
              (S "A" [VVarTy "c", VVarTy "b"])
  where appliedFuncTy = VFuncTy (F (S "B" [VVarTy "a"]) (S "B" [VVarTy "c"]))
dip2to1Ty :: FuncType
dip2to1Ty = F (S "A" [VVarTy "a", VVarTy "b", VVarTy "c", appliedFuncTy])
              (S "A" [VVarTy "d", VVarTy "c"])
  where appliedFuncTy = VFuncTy (F (S "B" [VVarTy "a", VVarTy "b"])
                                   (S "B" [VVarTy "d"]))
dip2to2Ty :: FuncType
dip2to2Ty = F (S "A" [VVarTy "a", VVarTy "b", VVarTy "c", appliedFuncTy])
              (S "A" [VVarTy "d", VVarTy "e", VVarTy "c"])
  where appliedFuncTy = VFuncTy (F (S "B" [VVarTy "a", VVarTy "b"])
                                   (S "B" [VVarTy "d", VVarTy "e"]))

-- adapted from https://programmers.stackexchange.com/questions/215712/type-checking-and-recursive-types-writing-the-y-combinator-in-haskell-ocaml
newtype Mu a = Roll { unroll :: Mu a -> a }
fixImpl :: ((a -> b) -> a -> b) -> a -> b
fixImpl f = (\x a -> f (unroll x x) a) $ Roll (\x a -> f (unroll x x) a)

fix :: [Value] -> [Value]
fix (FuncVal ty f : s) = fixImpl (\g a -> f (FuncVal ty g : a)) s
fixTy :: FuncType
fixTy = F (S "A" [VVarTy "a", outerFuncTy]) (S "A" [VVarTy "b"]) 
  where outerFuncTy = VFuncTy (F (S "B" [VVarTy "a", innerFuncTy])
                                 (S "B" [VVarTy "b"]))
        innerFuncTy = VFuncTy (F (S "C" [VVarTy "a"]) (S "C" [VVarTy "b"]))

ifFunc :: [Value] -> [Value]
ifFunc (BoolVal b : x : y : s) = (if b then x else y) : s
ifTy :: FuncType
ifTy = F (S "A" [VVarTy "a", VVarTy "a", VBoolTy]) (S "A" [VVarTy "a"])

cons :: [Value] -> [Value]
cons (x : ListVal ty l : s) = ListVal ty (x : l) : s
consTy :: FuncType
consTy = F (S "A" [VListTy $ VVarTy "a", VVarTy "a"])
           (S "A" [VListTy $ VVarTy "a"])

listMatch :: [Value] -> [Value]
listMatch (ListVal _ [] : FuncVal _ nilCase : _ : s) =
  nilCase [] ++ s
listMatch (ListVal ty (x : xs) : _ : FuncVal _ consCase : s) =
  consCase [x, ListVal ty xs] ++ s
listMatchTy :: FuncType
listMatchTy = F (S "A" [consCaseTy, nilCaseTy, VListTy $ VVarTy "a"])
                (S "A" [VVarTy "b"])
  where consCaseTy = VFuncTy (F (S "B" [VListTy $ VVarTy "a", VVarTy "a"])
                                (S "B" [VVarTy "b"]))
        nilCaseTy  = VFuncTy (F (S "C" []) (S "C" [VVarTy "b"]))

builtins :: Map Builtin ([Value] -> [Value], FuncType)
builtins = Map.fromList
  [ ("plus", (plus, arithTy))
  , ("minus", (minus, arithTy))
  , ("times", (times, arithTy))
  , ("equal", (equal, equalTy))
  , ("apply1to1", (apply, apply1to1Ty))
  , ("apply2to1", (apply, apply2to1Ty))
  , ("apply2to2", (apply, apply2to2Ty))
  , ("pop", (pop, popTy))
  , ("dup", (dup, dupTy))
  , ("swap", (swap, swapTy))
  , ("dip1to1", (dip, dip1to1Ty))
  , ("dip2to1", (dip, dip2to1Ty))
  , ("dip2to2", (dip, dip2to2Ty))
  , ("fix", (fix, fixTy))
  , ("if", (ifFunc, ifTy))
  , ("cons", (cons, consTy))
  , ("listMatch", (listMatch, listMatchTy))
  ]

builtinFuncs :: Map Builtin ([Value] -> [Value])
builtinFuncs = Map.map fst builtins

builtinTypes :: Map Builtin FuncType
builtinTypes = Map.map snd builtins

