{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Inference where

import Types
import Terms

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

type TypeVariable = Char

type TC a =
  WriterT [Constraint]
  (StateT TypeVariable
  (Either String))
  a

fresh :: TC TypeVariable
fresh = state ((,) <*> succ)

runTC :: TC a -> Either String (a, [Constraint])
runTC m = evalStateT (runWriterT m) 'a'

--

data Constraint
  = VEqual (Type 'ValueKind) (Type 'ValueKind)
  | SEqual (Type 'StackKind) (Type 'StackKind)
    deriving (Show, Eq)

class Constrainable k where
  equate :: Type k -> Type k -> TC ()

instance Constrainable 'StackKind where
  equate s t | s == t    = return ()
             | otherwise = tell [SEqual s t]

instance Constrainable 'ValueKind where
  equate s t | s == t    = return ()
             | otherwise = tell [VEqual s t]


-- type checking and inference monad

type StackFunc = (Type 'StackKind, Type 'StackKind)

type Context = Map String StackFunc

inferType :: Context -> Term -> TC StackFunc

inferType _ IdTerm = return (SAnyTy, SAnyTy)

inferType c (CatTerm t u) =
  do (t1, u1) <- inferType c t
     (t2, u2) <- inferType c u
     equate t2 u1
     return (t1, u2)

inferType c (BuiltinTerm s) =
  case Map.lookup s c of
    Just t  -> return t
    Nothing -> throwError $ "Unbound identifier: " ++ s

inferType _ (PushIntTerm _) =
  return (SAnyTy, SConsTy SAnyTy VIntTy)

inferType _ (PushBoolTerm _) =
  return (SAnyTy, SConsTy SAnyTy VBoolTy)

inferType c (PushFuncTerm term) =
  do (t, u) <- inferType c term
     return (SAnyTy, SConsTy SAnyTy (VFuncTy t u))

