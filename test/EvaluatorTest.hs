
-----------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

module EvaluatorTest where

import Text.RawString.QQ

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


prog0 :: String
prog0 = [r|

pair x y f = f x y ;

fst p = p K ;

snd p = p K1 ;

f x y = 
   let
      a = pair x b ;
      b = pair y a ;
   in
      fst (snd (snd (snd a))) ;
      
main = f 3 4 ;
   
|]


prop_prog0 :: Bool
prop_prog0 = getResult (runProg prog0) == 4

-----------------------------------------------------------------------------------------------------------------------



