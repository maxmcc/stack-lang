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
  | SEqual Char Stack Char Stack
    deriving (Eq)

instance Show Constraint where
  show (VEqual s t)     = show s ++ " :~: " ++ show t
  show (SEqual a s b t) =
    printf "forall %c, %s :~: forall %c, %s" a (show s) b (show t)

equateVTy :: ValueType -> ValueType -> TC ()
equateVTy t1 t2 | t1 == t2  = return ()
                | otherwise = tell [VEqual t1 t2]

equateSTy :: Char -> Stack -> Char -> Stack -> TC ()
equateSTy a s b t | (a, s) == (b, t) = return ()
                  | otherwise        = tell [SEqual a s b t]

-- type checking and inference monad

type Context = Map String FuncType

inferType :: Context -> Term -> TC FuncType

inferType _ IdTerm = do a <- freshSVar
                        b <- freshSVar
                        return $ F a (S b []) (S b [])

inferType c (CatTerm t u) =
  do F a1 (S b1 t1) (S c1 u1) <- inferType c t
     F a2 (S b2 t2) (S c2 u2) <- inferType c u
     d <- freshSVar
     equateSTy a1 (S c1 u1) a2 (S b2 t2)
     equateSTy a1 (S b1 t1) d (S b1 t1)
     equateSTy a2 (S c2 u2) d (S c2 u2)
     return $ F d (S b1 t1) (S c2 u2)

inferType c (BuiltinTerm s) =
  case Map.lookup s c of
    Just t  -> return t
    Nothing -> throwError $ "Unbound identifier: " ++ s

inferType _ (PushIntTerm _) = do a <- freshSVar
                                 b <- freshSVar
                                 return $ F a (S b []) (S b [VIntTy])

inferType _ (PushBoolTerm _) = do a <- freshSVar
                                  b <- freshSVar
                                  return $ F a (S b []) (S b [VBoolTy])

inferType c (PushFuncTerm term) =
  do t <- inferType c term
     a <- freshSVar
     b <- freshSVar
     return $ F a (S b []) (S b [VFuncTy t])

