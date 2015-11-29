{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

module Types where

import Data.Set (Set)
-- import qualified Data.Set as Set

{-

data Kind
  = Proper
  | Stack

data Value
  = IntVal Int
  | BoolVal Bool

data Type (a :: Kind) where
  Value :: Value -> Type 'Proper
  List  :: Type 'Proper -> Type 'Proper
  Func  :: Type 'Stack -> Type 'Stack -> Type 'Proper
  Stack :: Type 'Stack -> Type 'Proper -> Type 'Stack
  Empty :: Type 'Stack

-}

data Value =
    IntVal Int
  | BoolVal Bool

data ProperType =
    Value Value
  | List ProperType
  | Function StackType StackType

data StackType =
    Empty
  | Push StackType ProperType


data TypeId = String

data Quant = Quant { properVars :: Set TypeId, stackVars :: Set TypeId }

data TypeScheme =
    Mono ProperType
  | Forall Quant ProperType

