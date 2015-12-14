module Main where

import Types
import Inference
import Builtin
import Parser

import System.IO
import Data.Map (empty)
import Control.Monad (when)

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
