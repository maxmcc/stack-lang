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
           Right (t@(F l r), cs) ->
             let Right (subst, vcs) = solveStack cs in
             print t >> putStrLn "----------------" >>
             mapM_ print cs >> putStrLn "----------------" >>
             print (F (substSVars subst l) (substSVars subst r)) >>
             main
           Left s ->
             putStrLn s >>
             main
       Left s ->
         print s >>
         main
