{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Builtin where

import Types
import Terms

import Data.Map (Map)
import qualified Data.Map as Map

arithTy :: FuncType
arithTy = F (S "A" [VIntTy, VIntTy]) (S "A" [VIntTy])

builtinTypes :: Map Builtin FuncType
builtinTypes = Map.fromList
  [ ("plus", arithTy)
  , ("minus", arithTy)
  , ("times", arithTy)
  , ("equal", F (S "A" [VIntTy, VIntTy]) (S "A" [VBoolTy]))
  , ("apply", F (S "A" [VFuncTy (F (S "A" []) (S "B" []))]) (S "B" []))
  , ("pop", F (S "A" [VVarTy "a"]) (S "A" []))
  , ("dup", F (S "A" [VVarTy "a"]) (S "A" [VVarTy "a", VVarTy "a"]))
  , ("swap", F (S "A" [VVarTy "a", VVarTy "b"]) (S "A" [VVarTy "b", VVarTy "a"]))
  , ("dip", F (S "A" [VVarTy "a", VFuncTy (F (S "A" []) (S "B" []))]) (S "B" [VVarTy "a"]))
  , ("fix", F (S "A" [VFuncTy (F (S "A" [VFuncTy (F (S "A" []) (S "B" []))]) (S "B" []))]) (S "B" []))
  , ("if", F (S "A" [VVarTy "a", VVarTy "a", VBoolTy]) (S "A" [VVarTy "a"]))
  ]

