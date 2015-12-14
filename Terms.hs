{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds #-}

module Terms where

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

data Term
  = IdTerm
  | CatTerm Term Term
  | BuiltinTerm Builtin
  | PushIntTerm Int
  | PushBoolTerm Bool
  | PushFuncTerm Term
    deriving (Eq, Ord, Show)

