{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} Test.Pdf.Extract.Syllable
import {-@ HTF_TESTS @-} Test.Pdf.Extract.PyPdfMiner
import {-@ HTF_TESTS @-} Test.Pdf.Extract.Lines
import {-@ HTF_TESTS @-} Test.Pdf.Extract.Spacing

main = htfMain htf_importedTests
