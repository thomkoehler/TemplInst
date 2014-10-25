-----------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Text.Show.Pretty as Pr

import Evaluator
import ExprQuoter

-----------------------------------------------------------------------------------------------------------------------


main :: IO()
main = do
   putStrLn $ Pr.ppShow $ runProg "main = neg (I 1)"
   --putStrLn $ Pr.ppShow $ runProg "main = let x = 1 + 2 in x + 3"



-----------------------------------------------------------------------------------------------------------------------
