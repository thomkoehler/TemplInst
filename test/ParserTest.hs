-----------------------------------------------------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserTest where

import Test.Framework


import Parser(parse)
import Language

-----------------------------------------------------------------------------------------------------------------------

simpleMain0 :: ScDefn Name 
simpleMain0 = ScDefn
   {
      scName = "main",
      scArgs = [],
      scExpr = ENum 1
   }
   
simpleMain1 :: ScDefn Name 
simpleMain1 = ScDefn
   {
      scName = "main",
      scArgs = ["x", "y"],
      scExpr = ENum 1
   }


prop_simpleMain0 :: Bool
prop_simpleMain0 = [simpleMain0] == parse "simpleMain0" "main = 1"

prop_simpleMain1 :: Bool
prop_simpleMain1 = [simpleMain1] == parse "simpleMain1" "main x y = 1"

prop_simpleMain2 :: Bool
prop_simpleMain2 = "main" == head (map scName (parse "simpleMain1" "main = twice neg 3"))

-----------------------------------------------------------------------------------------------------------------------

