{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

module Types where

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
  | VVarTy String
    deriving (Eq)

data Stack = S String [ValueType]
  deriving (Eq)

data FuncType = F Stack Stack
  deriving (Eq)

instance Show ValueType where
  show VIntTy      = "int"
  show VBoolTy     = "bool"
  show (VListTy t) = "list " ++ show t
  show (VFuncTy f) = show f
  show (VVarTy s)  = s

instance Show Stack where
  show (S a s) = a ++ " ++ " ++ show s

instance Show FuncType where
  show (F s t) = show s ++ " -> " ++ show t
