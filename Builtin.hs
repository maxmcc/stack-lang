{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Builtin where

import Types
import Terms

import Data.Map (Map)
import qualified Data.Map as Map

builtinTypes :: Map Builtin FuncType
builtinTypes = Map.fromList
  [ ("plus", F 'A' (S 'B' [VIntTy, VIntTy]) (S 'B' [VIntTy]))
  , ("apply", F 'A' (S 'B' [VFuncTy (F 'D' (S 'B' []) (S 'C' []))]) (S 'C' []))
  ]

