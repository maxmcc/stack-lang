{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds #-}

module Terms where

data Term
  = IdTerm
  | CatTerm Term Term
  | BuiltinTerm String
  | PushIntTerm Int
  | PushBoolTerm Bool
  | PushFuncTerm Term
    deriving (Eq, Ord, Show)

