{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} EvaluatorTest
import {-@ HTF_TESTS @-} ParserTest

main :: IO()
main = htfMain htf_importedTests
