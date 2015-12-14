module Main where

import Terms
import Types
import Inference
import Builtin
import Parser

import System.IO
import Data.Map (Map)
import Data.Maybe (Maybe)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Control.Monad (when)

slickify :: Term -> [Value] -> [Value]
slickify IdTerm           = id
slickify (CatTerm t1 t2)  = slickify t1 . slickify t2
slickify (BuiltinTerm s)  = Maybe.fromJust $ Map.lookup s builtinFuncs
slickify (PushIntTerm i)  = (IntVal i :)
slickify (PushBoolTerm b) = (BoolVal b :)
slickify (PushFuncTerm f) = (FuncVal (slickify f) :)

main :: IO ()
main =
  do hSetBuffering stdin LineBuffering
     putStr "%> " >> hFlush stdout
     parse <- run <$> getLine
     case parse of
       Right term ->
         case typeInference term of
           Right t -> print t >> main
           Left s  -> putStrLn s >> main
       Left s ->
         print s >>
         main
