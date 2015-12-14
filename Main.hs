{-# OPTIONS_GHC -Wall -fwarn-incomplete-patterns -fwarn-tabs #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Language
import Inference
import Builtin
import Parser

import System.IO
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

slickify :: Term -> [Value] -> [Value]
slickify IdTerm           = id
slickify (CatTerm t1 t2)  = slickify t2 . slickify t1
slickify (BuiltinTerm s)  = Maybe.fromJust $ Map.lookup s builtinFuncs
slickify (PushIntTerm i)  = (IntVal i :)
slickify (PushBoolTerm b) = (BoolVal b :)
slickify (PushFuncTerm f) = (FuncVal (slickify f) :)

main :: IO ()
main =
  do hSetBuffering stdin LineBuffering
     putStr "%> " >> hFlush stdout
     asl <- parse <$> getLine
     case asl of
       Right term ->
         case typeInferenceOnEmpty term of
           Right ty -> print (slickify term []) >> putStr " : " >> print ty >> main
           Left s   -> putStrLn s >> main
       Left s ->
         print s >> main

