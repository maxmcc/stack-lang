{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Language
import Inference
import Builtin
import Parser

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import System.IO

interpret :: Term FuncType -> [Value] -> [Value]
interpret (IdTerm _)         = id
interpret (CatTerm _ t1 t2)  = interpret t2 . interpret t1
interpret (BuiltinTerm _ s)  = Maybe.fromJust $ Map.lookup s builtinFuncs
interpret (PushIntTerm _ i)  = (IntVal i :)
interpret (PushBoolTerm _ b) = (BoolVal b :)
interpret (PushNilTerm (F _ (S _ s))) = (ListVal (last s) [] :)
interpret (PushFuncTerm (F _ (S _ s)) f) = (FuncVal fty (interpret f) :)
  where VFuncTy fty = last s

main :: IO ()
main =
  do hSetBuffering stdin LineBuffering
     putStr "%> " >> hFlush stdout
     asl <- parse <$> getLine
     case asl of
       Right term ->
         case typeInferenceOnEmpty term of
           Right term' -> print (interpret term' []) >> putStr " : " >>
                          print (extract term') >> main
           Left s      -> putStrLn s >> main
       Left s ->
         print s >> main

