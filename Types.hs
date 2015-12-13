{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

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

data ValueType
  = VIntTy
  | VBoolTy
  | VListTy ValueType
  | VFuncTy FuncType
  | VVarTy Char
    deriving (Eq)

data Stack = S Char [ValueType]
  deriving (Eq)

data FuncType = F Stack Stack
  deriving (Eq)

instance Show ValueType where
  show VIntTy      = "int"
  show VBoolTy     = "bool"
  show (VListTy t) = show t ++ " list"
  show (VFuncTy f) = show f
  show (VVarTy c)  = ['\'', c]

instance Show Stack where
  show (S a s) = a : " ++ " ++ show s

instance Show FuncType where
  show (F s t) = show s ++ " -> " ++ show t

