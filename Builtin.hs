{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Builtin where

import Types
import Terms

import Data.Map (Map)
import qualified Data.Map as Map

infixl 5 -:
(-:) :: Type 'StackKind -> Type 'ValueKind -> Type 'StackKind
(-:) = SConsTy

infixl 4 ~>
(~>) :: Type 'StackKind -> Type 'StackKind -> StackFunc
(~>) = (,)

builtinTypes :: Map Builtin StackFunc
builtinTypes = Map.fromList
  [ ("plus", SAnyTy -: VIntTy -: VIntTy ~> SAnyTy -: VIntTy)
  ]

