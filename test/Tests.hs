import Eval0
import Eval1
import Eval2

import Types

import qualified Data.Map as Map

import qualified       Test.HUnit      as T
import qualified       Test.HUnit.Util as U
import                 System.IO.Unsafe -- for one unit test

exampleExp = Plus (Lit 12) (App (Abs "x" (Var "x")) (Plus (Lit 4) (Lit 2)))


t00 = U.t "t00"
     (eval0 Map.empty exampleExp)
     (IntVal 18)

t01 = U.e "t01"
     (eval0 Map.empty (Var "x"))
     "Maybe.fromJust: Nothing"

t01a = U.t "t01a"
     (eval0 (Map.fromList [("x", IntVal 42)]) (Var "x"))
     (IntVal 42)

t10 = U.t "t10"
     (runEval1 (eval1 Map.empty exampleExp))
     (IntVal 18)

t11 = U.e "t11"
     (runEval1 (eval1 Map.empty (Var "x")))
     "Maybe.fromJust: Nothing"

t2a0 = U.t "t2a0"
     (runEval2 (eval2a Map.empty exampleExp))
     (Right (IntVal 18))

t2a1 = U.t "t2a1"
     (runEval2 (eval2a Map.empty (Var "no-way")))
     (Left "unbound var: no-way")

main :: IO T.Counts
main = do
  T.runTestTT $ T.TestList $ t00 ++ t01 ++ t01a
  T.runTestTT $ T.TestList $ t10 ++ t11
  T.runTestTT $ T.TestList $ t2a0 ++ t2a1
