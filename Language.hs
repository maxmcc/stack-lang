{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}

module Language where

type Builtin = String

data Value
  = IntVal Int
  | BoolVal Bool
  | ListVal ValueType [Value]
  | FuncVal FuncType ([Value] -> [Value])

instance Show Value where
  show (IntVal i)      = show i
  show (BoolVal True)  = "true"
  show (BoolVal False) = "false"
  show (ListVal _ l)   = show l
  show (FuncVal _ _)   = "<function>"

data Term a
  = IdTerm a
  | CatTerm a (Term a) (Term a)
  | BuiltinTerm a Builtin
  | PushIntTerm a Int
  | PushBoolTerm a Bool
  | PushNilTerm a
  | PushFuncTerm a (Term a)
    deriving (Eq, Ord, Show)

instance Functor Term where
  fmap f (IdTerm a)         = IdTerm $ f a
  fmap f (CatTerm a t1 t2)  = CatTerm (f a) (fmap f t1) (fmap f t2)
  fmap f (BuiltinTerm a s)  = BuiltinTerm (f a) s
  fmap f (PushIntTerm a i)  = PushIntTerm (f a) i
  fmap f (PushBoolTerm a b) = PushBoolTerm (f a) b
  fmap f (PushNilTerm a)    = PushNilTerm $ f a
  fmap f (PushFuncTerm a t) = PushFuncTerm (f a) (fmap f t)

extract :: Term a -> a
extract (IdTerm a)         = a
extract (CatTerm a _ _)    = a
extract (BuiltinTerm a _)  = a
extract (PushIntTerm a _)  = a
extract (PushBoolTerm a _) = a
extract (PushNilTerm a)    = a
extract (PushFuncTerm a _) = a

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
