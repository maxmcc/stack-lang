module Main where

import Types
import Inference
import Builtin
import Parser

import System.IO
import Control.Monad (when)

main :: IO ()
main =
  do hSetBuffering stdin LineBuffering
     putStr "%> " >> hFlush stdout
     parse <- run <$> getLine
     case parse of
       Right terms ->
         case runTC $ inferType builtinTypes terms of
           Right (t, cs) ->
             print t >> putStrLn "----------------" >> mapM_ print cs >>
             main
           Left s ->
             putStrLn s >>
             main
       Left s ->
         print s >>
         main



