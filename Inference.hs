{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Inference where

import Types
import Terms
import Builtin

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

type TypeVariable = String

type TC a =
  WriterT [Constraint]
  (StateT Int
  (Either String))
  a

freshVVar :: TC TypeVariable
freshVVar =
  do i <- get
     put $ succ i
     return $ "t" ++ show i

freshSVar :: TC TypeVariable
freshSVar =
  do i <- get
     put $ succ i
     return $ "s" ++ show i

type FreshenTC a =
  StateT (Map TypeVariable TypeVariable)
  (WriterT [Constraint]
  (StateT Int
  (Either String)))
  a

freshenVVars :: ValueType -> FreshenTC ValueType
freshenVVars VIntTy      = return VIntTy
freshenVVars VBoolTy     = return VBoolTy
freshenVVars (VListTy t) = VListTy <$> freshenVVars t
freshenVVars (VFuncTy t) = VFuncTy <$> freshenVVarsFunc t
freshenVVars (VVarTy a)  =
  do a' <- lift freshVVar
     ctxt <- get
     let newVar = Map.findWithDefault a' a ctxt
     let ctxt' = Map.insert a newVar ctxt
     put ctxt'
     return $ VVarTy newVar

freshenVVarsFunc :: FuncType -> FreshenTC FuncType
freshenVVarsFunc (F (S a s) (S b t)) =
  do s' <- mapM freshenVVars s
     t' <- mapM freshenVVars t
     return $ F (S a s') (S b t')

freshenSVars :: ValueType -> FreshenTC ValueType
freshenSVars VIntTy      = return VIntTy
freshenSVars VBoolTy     = return VBoolTy
freshenSVars (VListTy t) = VListTy <$> freshenSVars t
freshenSVars (VFuncTy t) = VFuncTy <$> freshenSVarsFunc t
freshenSVars (VVarTy a)  = return $ VVarTy a

freshenSVarsFunc :: FuncType -> FreshenTC FuncType
freshenSVarsFunc (F (S a s) (S b t)) =
  do a' <- lift freshSVar
     b' <- lift freshSVar
     ctxt <- get
     let newAVar = Map.findWithDefault a' a ctxt
     let newBVar = Map.findWithDefault b' b ctxt
     let ctxt' = Map.insert b newBVar (Map.insert a newAVar ctxt)
     put ctxt'
     s' <- mapM freshenSVars s
     t' <- mapM freshenSVars t
     return $ F (S newAVar s') (S newBVar t')

freshen :: FuncType -> TC FuncType
freshen t =
  do t' <- evalStateT (freshenVVarsFunc t) Map.empty
     evalStateT (freshenSVarsFunc t') Map.empty

runTC :: TC a -> Either String (a, [Constraint])
runTC m = evalStateT (runWriterT m) 0

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
    Just t  -> freshen t
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

genConstraints :: Term -> Either String (FuncType, [Constraint])
genConstraints = runTC . inferType builtinTypes

---

type SSubst = Map TypeVariable Stack
type VSubst = Map TypeVariable ValueType

substSVarsFunc :: SSubst -> FuncType -> FuncType
substSVarsFunc s (F st1 st2) = undefined

substSVarsValue :: SSubst -> ValueType -> ValueType
substSVarsValue s t = undefined

substVVarsFunc :: VSubst -> FuncType -> FuncType
substVVarsFunc s t = undefined

substVVarsValue :: VSubst -> ValueType -> ValueType
substVVarsValue s t = undefined


