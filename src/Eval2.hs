module Eval2 where

import Types

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.Except

-- String is the type arg to ErrorT : the type of exceptions in example
type Eval2 alpha = ExceptT String Identity alpha

runEval2 :: Eval2 alpha -> Either String alpha
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Env -> Exp -> Eval2 Value

eval2a env (Lit i) = return $ IntVal i

-- eval1 / eval2a diff:
eval2a env (Var n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound var: " ++ n
    Just v -> return v

eval2a env (Plus e1 e2) = do
  IntVal i1 <- eval2a env e1
  IntVal i2 <- eval2a env e2
  return $ IntVal (i1 + i2)

eval2a env (Abs n e) = return $ FunVal env n e

eval2a env (App e1 e2) = do
  val1 <- eval2a env e1
  val2 <- eval2a env e2
  case val1 of
    FunVal env' n body -> eval2a (Map.insert n val2 env') body
