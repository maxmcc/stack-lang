{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE GADTs, DataKinds, FlexibleInstances, TupleSections #-}

module Inference where

import Language
import Builtin

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Printf
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

collectSVars :: Stack -> [TypeVariable]
collectSVars (S a s) = a : concatMap collectSVarsValue s

collectSVarsValue :: ValueType -> [TypeVariable]
collectSVarsValue VIntTy            = []
collectSVarsValue VBoolTy           = []
collectSVarsValue (VListTy t)       = collectSVarsValue t
collectSVarsValue (VFuncTy (F l r)) = collectSVars l ++ collectSVars r
collectSVarsValue (VVarTy _)        = []

collectVVars :: ValueType -> [TypeVariable]
collectVVars VIntTy            = []
collectVVars VBoolTy           = []
collectVVars (VListTy t)       = collectVVars t
collectVVars (VFuncTy (F l r)) = collectVVarsStack l ++ collectVVarsStack r
collectVVars (VVarTy v)        = [v]

collectVVarsStack :: Stack -> [TypeVariable]
collectVVarsStack (S _ s) = concatMap collectVVars s

-- Functions to walk through a type and perform substitution.

type SSubst = Map TypeVariable Stack
type VSubst = Map TypeVariable ValueType

substSVars :: SSubst -> Stack -> Stack
substSVars subst (S a s) = stk
  where s' = map (substSVarsValue subst) s
        stk = Maybe.maybe (S a s') (\(S a' s'') -> S a' (s'' ++ s'))
                          (Map.lookup a subst)

substSVarsValue :: SSubst -> ValueType -> ValueType
substSVarsValue _ VIntTy            = VIntTy
substSVarsValue _ VBoolTy           = VBoolTy
substSVarsValue s (VListTy t)       = VListTy $ substSVarsValue s t
substSVarsValue s (VFuncTy (F l r)) = VFuncTy $
                                        F (substSVars s l) (substSVars s r)
substSVarsValue _ t@(VVarTy _)      = t

substVVars :: VSubst -> ValueType -> ValueType
substVVars _ VIntTy            = VIntTy
substVVars _ VBoolTy           = VBoolTy
substVVars s (VListTy t)       = VListTy $ substVVars s t
substVVars s (VFuncTy (F l r)) = VFuncTy $
                                   F (substVVarsStack s l) (substVVarsStack s r)
substVVars s t@(VVarTy v)      = Maybe.fromMaybe t (Map.lookup v s)

substVVarsStack :: VSubst -> Stack -> Stack
substVVarsStack subst (S a s) = S a (map (substVVars subst) s)

substStack :: SSubst -> VSubst -> Stack -> Stack
substStack ss vs s = substVVarsStack vs (substSVars ss s)

substValue :: SSubst -> VSubst -> ValueType -> ValueType
substValue ss vs t = substVVars vs (substSVarsValue ss t)

substFuncType :: SSubst -> VSubst -> FuncType -> FuncType
substFuncType ss vs (F l r) = F (substStack ss vs l) (substStack ss vs r)

substTerm :: SSubst -> VSubst -> Term FuncType -> Term FuncType
substTerm ss vs (IdTerm ty)         = IdTerm (substFuncType ss vs ty)
substTerm ss vs (CatTerm ty t1 t2)  = CatTerm (substFuncType ss vs ty)
                                              (substTerm ss vs t1)
                                              (substTerm ss vs t2)
substTerm ss vs (BuiltinTerm ty s)  = BuiltinTerm (substFuncType ss vs ty) s
substTerm ss vs (PushIntTerm ty i)  = PushIntTerm (substFuncType ss vs ty) i
substTerm ss vs (PushBoolTerm ty b) = PushBoolTerm (substFuncType ss vs ty) b
substTerm ss vs (PushNilTerm ty)    = PushNilTerm (substFuncType ss vs ty)
substTerm ss vs (PushFuncTerm ty t) = PushFuncTerm (substFuncType ss vs ty)
                                                   (substTerm ss vs t)

afterSSubst :: SSubst -> SSubst -> SSubst
s1 `afterSSubst` s2 = Map.map (substSVars s1) s2 `Map.union` s1

afterVSubst :: VSubst -> VSubst -> VSubst
s1 `afterVSubst` s2 = Map.map (substVVars s1) s2 `Map.union` s1

freshen :: FuncType -> TC FuncType
freshen (F s t) =
  do let sVars = collectSVars s ++ collectSVars t
         vVars = collectVVarsStack s ++ collectVVarsStack t
     newSVars <- mapM (\v -> freshSVar >>= \v' -> return (v, S v' [])) sVars
     newVVars <- mapM (\v -> freshVVar >>= \v' -> return (v, VVarTy v')) vVars
     let s' = substStack (Map.fromList newSVars) (Map.fromList newVVars) s
     let t' = substStack (Map.fromList newSVars) (Map.fromList newVVars) t
     return $ F s' t'

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

equateSTy :: Monad m => Stack -> Stack -> WriterT [SConstraint] m ()
equateSTy s t | s == t    = return ()
              | otherwise = tell [SEqual s t]

-- type checking and inference monad

type Context = Map String FuncType

inferType :: Context -> Term () -> TC (Term FuncType)

inferType _ (IdTerm ()) =
  do a <- freshSVar
     return . IdTerm $ F (S a []) (S a [])

inferType c (CatTerm () t u) =
  do t' <- inferType c t
     u' <- inferType c u
     let F (S a1 t1) (S b1 u1) = extract t'
     let F (S a2 t2) (S b2 u2) = extract u'
     equateSTy (S b1 u1) (S a2 t2)
     return $ CatTerm (F (S a1 t1) (S b2 u2)) t' u'

inferType c (BuiltinTerm () s) =
  case Map.lookup s c of
    Just t  -> flip BuiltinTerm s <$> freshen t
    Nothing -> throwError $ "Unbound identifier: " ++ s

inferType _ (PushIntTerm () i) =
  do a <- freshSVar
     return $ PushIntTerm (F (S a []) (S a [VIntTy])) i

inferType _ (PushBoolTerm () b) =
  do a <- freshSVar
     return $ PushBoolTerm (F (S a []) (S a [VBoolTy])) b

inferType _ (PushNilTerm ()) =
  do a <- freshSVar
     b <- freshVVar
     return $ PushNilTerm (F (S a []) (S a [VListTy $ VVarTy b]))

inferType c (PushFuncTerm () t) =
  do t' <- inferType c t
     a <- freshSVar
     return $ PushFuncTerm (F (S a []) (S a [VFuncTy $ extract t'])) t'

genConstraints :: Term () -> Either String (Term FuncType, [SConstraint])
genConstraints = runTC . inferType builtinTypes

---

mguStack :: Stack -> Stack -> WriterT [VConstraint] (Either String) SSubst
mguStack (S a s) (S b t) | length s < length t = mguStack (S b t) (S a s)
                         | otherwise =
  do tell $ zipWith VEqual (reverse s) (reverse t)
     lift $ stackVarAsgn b (S a (take (length s - length t) s))

mguValue :: ValueType -> ValueType -> Either String VSubst
mguValue VIntTy VIntTy = return Map.empty
mguValue VBoolTy VBoolTy = return Map.empty
mguValue (VListTy t1) (VListTy t2) = mguValue t1 t2
mguValue (VFuncTy (F (S _ s) (S _ t))) (VFuncTy (F (S _ s') (S _ t'))) =
  if (length s, length t) == (length s', length t')
    then foldM (\vs (vt1, vt2) ->
                 afterVSubst vs <$>
                 mguValue (substVVars vs vt1) (substVVars vs vt2))
               Map.empty (zip (s ++ t) (s' ++ t'))
    else throwError $ printf "VFuncTy stacks mismatched: %s -> %s <> %s -> %s"
                             (show s) (show t) (show s') (show t')
mguValue (VVarTy v) t = valueVarAsgn v t
mguValue t (VVarTy v) = valueVarAsgn v t
mguValue _ _          = throwError "structural mismatch"

stackVarAsgn :: TypeVariable -> Stack -> Either String SSubst
stackVarAsgn a s
  | s == S a [] = return Map.empty
  | a `elem` collectSVars s =
      throwError $ "occurs check fails: " ++ show a ++ " in " ++ show s
  | otherwise = return $ Map.singleton a s

valueVarAsgn :: TypeVariable -> ValueType -> Either String VSubst
valueVarAsgn a t
  | t == VVarTy a = return Map.empty
  | a `elem` collectVVars t =
      throwError $ "occurs check fails: " ++ show a ++ " in " ++ show t
  | otherwise = return $ Map.singleton a t

solveStack :: [SConstraint] -> Either String (SSubst, [VConstraint])
solveStack =
  foldM (\(subst1, vcs1) (SEqual s t) -> do
          (subst2, vcs2) <- runWriterT $
                            mguStack (substSVars subst1 s) (substSVars subst1 t)
          return (subst2 `afterSSubst` subst1, vcs1 ++ vcs2)) (Map.empty, [])

solveValue :: SSubst -> [VConstraint] -> Either String VSubst
solveValue ss =
  foldM (\subst1 (VEqual t1 t2) -> do
          subst2 <- mguValue (substValue ss subst1 t1) (substValue ss subst1 t2)
          return $ subst2 `afterVSubst` subst1) Map.empty

typeInference :: Term () -> Either String (Term FuncType)
typeInference t =
  do (t', scs) <- genConstraints t
     (ss, vcs) <- solveStack scs
     vs <- solveValue ss vcs
     return $ substTerm ss vs t'

typeInferenceOnEmpty :: Term () -> Either String (Term FuncType)
typeInferenceOnEmpty t =
  do t' <- typeInference t
     let F (S _ s) _ = extract t'
     if null s
       then return t'
       else throwError $ "term expecting a non-empty stack: " ++ show s

