{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pdf.Extract.Utils where

import Test.Framework

import qualified Data.Text as T
import Data.Char

import Pdf.Extract.Utils

test_longestRomanNumber = do
  assertEqual 0 $ longestRomanNumber T.empty
  assertEqual 0 $ longestRomanNumber "a"
  assertEqual 1 $ longestRomanNumber "i"
  assertEqual 1 $ longestRomanNumber "ic"
  assertEqual 1 $ longestRomanNumber "The quick brown fox jumps of the lazy dog."
  assertEqual 5 $ longestRomanNumber "XLVII a.D."
  assertEqual 4 $ longestRomanNumber "ILVII a.D."
  assertEqual 3 $ longestRomanNumber "XLViI a.D."
  assertEqual 3 $ longestRomanNumber "de cive imperii romani"
  assertEqual 2 $ longestRomanNumber "ⅩXX" -- one character is a roman number

test_longestSubstring = do
  assertEqual 0 $ longestSubstringWith isNumber ""
  assertEqual 0 $ longestSubstringWith isNumber "That's all."
  assertEqual 3 $ longestSubstringWith isNumber "42 is bigger than -123"
  assertEqual 1 $ longestSubstringWith isNumber "ⅩXX" -- one character is a roman number
