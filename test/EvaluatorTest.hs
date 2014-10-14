
-----------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module EvaluatorTest where

import Text.RawString.QQ
import qualified Data.ByteString.Char8 as C

import Evaluator
import Test.Framework

-----------------------------------------------------------------------------------------------------------------------

prop_simpleMain0:: Bool
prop_simpleMain0 = getResult (runProg "main = 1") == 1

prop_simpleMain1:: Bool
prop_simpleMain1 = getResult (runProg "main = S K K 3") == 3

prop_simpleMain2:: Bool
prop_simpleMain2 = getResult (runProg "id x = x\nmain = id 5") == 5

prop_simpleMain3:: Bool
prop_simpleMain3 = getResult (runProg "x = 2\nmain = x") == 2

prop_simpleMain4:: Bool
prop_simpleMain4 = getResult (runProg "main = neg 3") == -3

prop_simpleMain5:: Bool
prop_simpleMain5 = getResult (runProg "main = twice neg 3") == 3

prop_simpleMain6:: Bool
prop_simpleMain6 = getResult (runProg "main = neg (I 3)") == -3

prop_add:: Bool
prop_add = getResult (runProg "main = 1 + 2") == 3


prop_add2:: Bool
prop_add2 = getResult (runProg (C.pack add2)) == 5
   where
      add2 :: String
      add2 = [r|
      
main = 
   let
      x = 2
      y = 3
   in
      x + y
      
|]


prop_mul:: Bool
prop_mul = getResult (runProg "main = 2 * 3") == 6


prop_addMul:: Bool
prop_addMul = getResult (runProg "main = 1 + 2 * 3") == 7

prop_prog0 :: Bool
prop_prog0 = getResult (runProg (C.pack prog0)) == 4
   where
      prog0 :: String
      prog0 = [r|
      
pair x y f = f x y

fst p = p K

snd p = p K1

f x y =
   let
      a = pair x b
      b = pair y a
   in
      fst (snd (snd (snd a)))

main = f 3 4
      
|]
      

-----------------------------------------------------------------------------------------------------------------------



