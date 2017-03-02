module Eval1 where

import Types

import Data.Functor.Identity
import qualified Data.Map as Map
import Data.Maybe

type Eval1 alpha = Identity alpha

runEval1 :: Eval1 alpha -> alpha
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value

eval1 _ (Lit i) = return $ IntVal i

eval1 env (Var n) = return $ fromJust (Map.lookup n env)

eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal (i1 + i2)

eval1 env (Abs n e) = return $ FunVal env n e

eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body
    IntVal _ -> undefined