module Eval2IO where

import Types

import Data.Functor.Identity
import qualified Data.Map as Map

import Control.Monad.Except

-- String is the type arg to ErrorT : the type of exceptions in example
type Eval2IO alpha = ExceptT String IO alpha

runEval2IO :: Eval2IO alpha -> IO (Either String alpha)
runEval2IO ev = runExceptT ev

eval2aIO :: Env -> Exp -> Eval2IO Value

eval2aIO env (Lit i) = return $ IntVal i

eval2aIO env (Var n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound var: " ++ n
    Just v -> return v

eval2aIO env (Plus e1 e2) = do
  IntVal i1 <- eval2aIO env e1
  IntVal i2 <- eval2aIO env e2
  return $ IntVal (i1 + i2)

eval2aIO env (Abs n e) = return $ FunVal env n e

eval2aIO env (App e1 e2) = do
  val1 <- eval2aIO env e1
  val2 <- eval2aIO env e2
  case val1 of
    FunVal env' n body -> eval2aIO (Map.insert n val2 env') body
