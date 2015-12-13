{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances #-}

module Types where

data Kind
  = StackKind
  | ValueKind
    deriving (Eq, Ord, Show)

data Value
  = IntVal Int
  | BoolVal Bool
  | ListVal [Value]
  | QuotVal [Value]
  | Builtin String
    deriving (Eq, Ord, Show)

data Type (k :: Kind) where
  VIntTy  :: Type 'ValueKind
  VBoolTy :: Type 'ValueKind
  VListTy :: Type 'ValueKind -> Type 'ValueKind
  VFuncTy :: Type 'StackKind -> Type 'StackKind -> Type 'ValueKind
  VVarTy  :: Char -> Type 'ValueKind
  SConsTy :: Type 'StackKind -> Type 'ValueKind -> Type 'StackKind
  SAnyTy  :: Type 'StackKind

instance Show (Type 'ValueKind) where
  show (VVarTy c)                  = ['\'', c]
  show VIntTy                      = "int"
  show VBoolTy                     = "bool"
  show (VListTy t)                 = show t ++ " list"
  show (VFuncTy t u)               = show t ++ " -> " ++ show u

instance Show (Type 'StackKind) where
  show (SConsTy s v@(VFuncTy _ _)) = show s ++ ", (" ++ show v ++ ")"
  show (SConsTy s v)               = show s ++ ", " ++ show v
  show SAnyTy                      = "..."

instance Eq (Type 'ValueKind) where
  VIntTy == VIntTy = True
  VBoolTy == VBoolTy = True
  VListTy t == VListTy u = t == u
  VFuncTy t u == VFuncTy v w = (t, u) == (v, w)
  VVarTy a == VVarTy b = a == b
  _ == _ = False

instance Eq (Type 'StackKind) where
  SConsTy s v == SConsTy t w = (s, v) == (t, w)
  SAnyTy == SAnyTy = True
  _ == _ = False

type StackFunc = (Type 'StackKind, Type 'StackKind)


