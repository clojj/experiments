module Main where

import Prelude hiding (id, (.))
import Control.Category
import ArrowFun


main :: IO ()
main = do
  let sfAdd = SimpleFunc (+ 1)
      sfMul = SimpleFunc (* 2)
      sfComp = sfMul . sfAdd
      sfComp' = sfMul >>> sfAdd
      sfComp'' = sfMul <<< sfAdd
  print $ runF sfComp 1
  print $ runF sfComp' 2
  print $ runF sfComp'' 2
