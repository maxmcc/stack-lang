{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds #-}

module Terms where

type Builtin = String

data Term
  = IdTerm
  | CatTerm Term Term
  | BuiltinTerm Builtin
  | PushIntTerm Int
  | PushBoolTerm Bool
  | PushFuncTerm Term
    deriving (Eq, Ord, Show)

