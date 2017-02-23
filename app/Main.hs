module Main where

import Types

import qualified Data.Map as Map
import Eval0
import Eval2
import Eval2IO

import Control.Monad.Except

exampleExpError = Var "x"

exampleExp_18 = Plus (Lit 12) (App (Abs "x" (Var "x")) (Plus (Lit 4) (Lit 2)))
exampleExp_19 = Plus (Lit 12) (App (Abs "x" (Plus (Lit 1) (Var "x"))) (Plus (Lit 4) (Lit 2)))

main :: IO ()
main = do
  print $ eval0 Map.empty exampleExp_18
--  print $ eval0 Map.empty exampleExpError

  print $ runEval2 $ eval2a Map.empty exampleExp_19
  print $ runEval2 $ eval2a Map.empty exampleExpError `catchError` errorHandler

  val <- runEval2IO $ eval2aIO Map.empty exampleExp_19
  print $ "from Eval2IO " ++ show val

  val <- runEval2IO $ eval2aIO Map.empty exampleExpError `catchError` errorHandlerIO
  print "the END"

errorHandler :: String -> Eval2 Value
errorHandler s = do
  return $ IntVal 9999

errorHandlerIO :: String -> Eval2IO Value
errorHandlerIO s = do
  liftIO $ print "error!"
  return $ IntVal 9999
