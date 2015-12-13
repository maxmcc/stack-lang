module Main where

import Types
import Inference
import Builtin
import Parser

main :: IO ()
main =
  do putStr "%> "
     l <- getLine
     let parse = run l
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



