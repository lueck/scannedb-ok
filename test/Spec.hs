{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Test.Pdf.Extract.Syllable

main = htfMain htf_importedTests
-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
