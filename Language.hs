{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

module Language where

type Builtin = String

data Value
  = IntVal Int
  | BoolVal Bool
  | ListVal [Value]
  | FuncVal ([Value] -> [Value])
  | Builtin Builtin

instance Show Value where
  show (IntVal i)      = show i
  show (BoolVal True)  = "true"
  show (BoolVal False) = "false"
  show (ListVal l)     = show l
  show (FuncVal _)     = "<function>"
  show (Builtin s)     = s

data TypedValue
  = TIntVal Int
  | TBoolVal Bool
  | TListVal ValueType [TypedValue]
  | TFuncVal FuncType ([TypedValue] -> [TypedValue])
  | TBuiltin Builtin

data Term
  = IdTerm
  | CatTerm Term Term
  | BuiltinTerm Builtin
  | PushIntTerm Int
  | PushBoolTerm Bool
  | PushFuncTerm Term
    deriving (Eq, Ord, Show)

data ValueType
  = VIntTy
  | VBoolTy
  | VListTy ValueType
  | VFuncTy FuncType
  | VVarTy String
    deriving (Eq)

instance Show ValueType where
  show VIntTy      = "int"
  show VBoolTy     = "bool"
  show (VListTy t) = "list " ++ show t
  show (VFuncTy f) = show f
  show (VVarTy s)  = s

data Stack = S String [ValueType]
  deriving (Eq)

instance Show Stack where
  show (S a s) = a ++ " ++ " ++ show s

data FuncType = F Stack Stack
  deriving (Eq)

instance Show FuncType where
  show (F s t) = show s ++ " -> " ++ show t
