module Main where

import Types

import qualified Data.Map as Map
import Eval0
import Eval2

exampleExp = Plus (Lit 12) (App (Abs "x" (Var "x")) (Plus (Lit 4) (Lit 2)))
exampleExp2 = Plus (Lit 12) (App (Abs "x" (Plus (Lit 1) (Var "x"))) (Plus (Lit 4) (Lit 2)))

main :: IO ()
main = do
  print $ eval0 Map.empty exampleExp
--  print $ eval0 Map.empty (Var "x")

  print $ runEval2 $ eval2a Map.empty exampleExp
