{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Inference where

import Types
import Terms

import Data.Char (toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

type TypeVariable = Char

type TC a =
  WriterT [Constraint]
  (StateT TypeVariable
  (Either String))
  a

freshVVar :: TC TypeVariable
freshVVar = state ((,) <*> succ)

freshSVar :: TC TypeVariable
freshSVar = toUpper <$> state ((,) <*> succ)

runTC :: TC a -> Either String (a, [Constraint])
runTC m = evalStateT (runWriterT m) 'm'

--

data Constraint
  = VEqual ValueType ValueType
  | SEqual Stack Stack
    deriving (Eq)

instance Show Constraint where
  show (VEqual s t) = show s ++ " :~: " ++ show t
  show (SEqual s t) = show s ++ " :~: " ++ show t

equateVTy :: ValueType -> ValueType -> TC ()
equateVTy t1 t2 | t1 == t2  = return ()
                | otherwise = tell [VEqual t1 t2]

equateSTy :: Stack -> Stack -> TC ()
equateSTy s t | s == t    = return ()
              | otherwise = tell [SEqual s t]

-- type checking and inference monad

type Context = Map String FuncType

inferType :: Context -> Term -> TC FuncType

inferType _ IdTerm =
  do a <- freshSVar
     return $ F (S a []) (S a [])

inferType c (CatTerm t u) =
  do F (S a1 t1) (S b1 u1) <- inferType c t
     F (S a2 t2) (S b2 u2) <- inferType c u
     equateSTy (S b1 u1) (S a2 t2)
     return $ F (S a1 t1) (S b2 u2)

inferType c (BuiltinTerm s) =
  case Map.lookup s c of
    Just t  -> return t
    Nothing -> throwError $ "Unbound identifier: " ++ s

inferType _ (PushIntTerm _) =
  do a <- freshSVar
     return $ F (S a []) (S a [VIntTy])

inferType _ (PushBoolTerm _) =
  do a <- freshSVar
     return $ F (S a []) (S a [VBoolTy])

inferType c (PushFuncTerm term) =
  do t <- inferType c term
     a <- freshSVar
     return $ F (S a []) (S a [VFuncTy t])

