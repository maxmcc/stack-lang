{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances #-}

module Inference where

import Types
import Terms
import Builtin

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

type TypeVariable = String

type TC a =
  WriterT [SConstraint]
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

-- Functions to collect sets of variables appearing in types.

collectSVars :: Stack -> Set TypeVariable
collectSVars (S a s) = Set.unions $ Set.singleton a : map collectSVarsValue s

collectSVarsValue :: ValueType -> Set TypeVariable
collectSVarsValue VIntTy            = Set.empty
collectSVarsValue VBoolTy           = Set.empty
collectSVarsValue (VListTy t)       = collectSVarsValue t
collectSVarsValue (VFuncTy (F l r)) = collectSVars l `Set.union` collectSVars r
collectSVarsValue (VVarTy _)        = Set.empty

collectVVars :: ValueType -> Set TypeVariable
collectVVars VIntTy            = Set.empty
collectVVars VBoolTy           = Set.empty
collectVVars (VListTy t)       = collectVVars t
collectVVars (VFuncTy (F l r)) = collectVVarsStack l `Set.union` collectVVarsStack r
collectVVars (VVarTy v)        = Set.singleton v

collectVVarsStack :: Stack -> Set TypeVariable
collectVVarsStack (S _ s) = Set.unions $ map collectVVars s

-- Functions to walk through a type and perform substitution.

type SSubst = Map TypeVariable Stack
type VSubst = Map TypeVariable ValueType

substSVars :: SSubst -> Stack -> Stack
substSVars subst (S a s) = stk
  where s' = map (substSVarsValue subst) s
        stk = Maybe.maybe (S a s') (\(S a' s'') -> S a' (s'' ++ s')) (Map.lookup a subst)

substSVarsValue :: SSubst -> ValueType -> ValueType
substSVarsValue _ VIntTy       = VIntTy
substSVarsValue _ VBoolTy      = VBoolTy
substSVarsValue s (VListTy t)  = VListTy $ substSVarsValue s t
substSVarsValue s (VFuncTy (F l r)) = VFuncTy $ F (substSVars s l) (substSVars s r)
substSVarsValue _ t@(VVarTy _) = t

substVVars :: VSubst -> ValueType -> ValueType
substVVars _ VIntTy            = VIntTy
substVVars _ VBoolTy           = VBoolTy
substVVars s (VListTy t)       = VListTy $ substVVars s t
substVVars s (VFuncTy (F l r)) = VFuncTy $ F (substVVarsStack s l) (substVVarsStack s r)
substVVars s t@(VVarTy v)      = Maybe.fromMaybe t (Map.lookup v s)

substVVarsStack :: VSubst -> Stack -> Stack
substVVarsStack subst (S a s) = S a (map (substVVars subst) s)

freshen :: FuncType -> TC FuncType
freshen (F s t) =
  do let vVars = Set.toList $ collectVVarsStack s `Set.union` collectVVarsStack t
         sVars = Set.toList $ collectSVars s `Set.union` collectSVars t
     newVVars <- mapM (\v -> freshVVar >>= \v' -> return (v, VVarTy v')) vVars
     let s' = substVVarsStack (Map.fromList newVVars) s
     let t' = substVVarsStack (Map.fromList newVVars) t
     newSVars <- mapM (\v -> freshSVar >>= \v' -> return (v, S v' [])) sVars
     let s'' = substSVars (Map.fromList newSVars) s'
     let t'' = substSVars (Map.fromList newSVars) t'
     return $ F s'' t''

runTC :: TC a -> Either String (a, [SConstraint])
runTC m = evalStateT (runWriterT m) 0

--

data VConstraint = VEqual ValueType ValueType
  deriving (Eq)
data SConstraint = SEqual Stack Stack
  deriving (Eq)

instance Show VConstraint where
  show (VEqual s t) = show s ++ " :~: " ++ show t
instance Show SConstraint where
  show (SEqual s t) = show s ++ " :~: " ++ show t

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

genConstraints :: Term -> Either String (FuncType, [SConstraint])
genConstraints = runTC . inferType builtinTypes

---

afterSSubst :: SSubst -> SSubst -> SSubst
s1 `afterSSubst` s2 = Map.map (substSVars s1) s2 `Map.union` s1

afterVSubst :: VSubst -> VSubst -> VSubst
s1 `afterVSubst` s2 = Map.map (substVVars s1) s2 `Map.union` s1

mguStack :: Stack -> Stack -> WriterT [VConstraint] (Either String) SSubst
mguStack (S a s) (S b t) | length s < length t = mguStack (S b t) (S a s)
                         | otherwise =
  do tell $ zipWith VEqual (reverse s) (reverse t)
     lift $ stackVarAsgn b (S a (take (length s - length t) s))

mguValue :: ValueType -> ValueType -> WriterT [SConstraint] (Either String) VSubst
mguValue VIntTy VIntTy = return Map.empty
mguValue VBoolTy VBoolTy = return Map.empty
mguValue (VListTy t1) (VListTy t2) = mguValue t1 t2
mguValue (VFuncTy (F l1 r1)) (VFuncTy (F l2 r2)) =
  -- TODO: unclear
  do tell [SEqual l1 l2, SEqual r1 r2]
     return Map.empty
mguValue (VVarTy v) t = lift $ valueVarAsgn v t
mguValue t (VVarTy v) = lift $ valueVarAsgn v t
mguValue _ _          = throwError "structural mismatch"

stackVarAsgn :: TypeVariable -> Stack -> Either String SSubst
stackVarAsgn a s
  | a `Set.member` collectSVars s =
      throwError $ "occurs check fails: " ++ show a ++ " in " ++ show s
  | otherwise = return $ Map.singleton a s

valueVarAsgn :: TypeVariable -> ValueType -> Either String VSubst
valueVarAsgn a t
  | t == VVarTy a = return Map.empty
  | a `Set.member` collectVVars t =
      throwError $ "occurs check fails: " ++ show a ++ " in " ++ show t
  | otherwise = return $ Map.singleton a t

solveStack :: SSubst -> [SConstraint] -> Either String (SSubst, [VConstraint])
solveStack ss =
  foldM (\(subst1, vcs1) (SEqual s t) -> do
          (subst2, vcs2) <- runWriterT $ mguStack (substSVars subst1 s) (substSVars subst1 t)
          return (subst2 `afterSSubst` subst1, vcs1 ++ vcs2)) (ss, [])

solveValue :: VSubst -> [VConstraint] -> Either String (VSubst, [SConstraint])
solveValue vs =
  foldM (\(subst1, scs1) (VEqual t1 t2) -> do
          (subst2, scs2) <- runWriterT $ mguValue (substVVars subst1 t1) (substVVars subst1 t2)
          return (subst2 `afterVSubst` subst1, scs1 ++ scs2)) (vs, [])

inferenceRound :: SSubst -> VSubst -> [SConstraint] -> Either String (SSubst, VSubst)
inferenceRound ss vs scs =
  do (ss', vcs) <- solveStack ss scs
     (vs', scs') <- solveValue vs vcs
     if null scs'
       then return (ss', vs')
       else inferenceRound ss' vs' scs'

typeInference :: Term -> Either String FuncType
typeInference term =
  do (F l r, scs) <- genConstraints term
     (ss, vs) <- inferenceRound Map.empty Map.empty scs
     return $ F (substSVars ss (substVVarsStack vs l)) (substSVars ss (substVVarsStack vs r))
