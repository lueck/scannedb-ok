{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pdf.Extract.Lines where

import Test.Framework

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Range.Range as R

import Pdf.Extract.Glyph
import Pdf.Extract.PyPdfMiner
import Pdf.Extract.Lines

--p205 :: Glyph g => IO ([[g]])
p205 = B.readFile "test/Heg1835a_p205-p205.xml" >>= parseXml [R.InfiniteRange]

p205to207 = B.readFile "test/Heg1835a_p205-p207.xml" >>= parseXml [R.InfiniteRange]


test_findLines_drop = do
  input <- p205
  let lines = findLinesWindow 42 5 2 True $ head input
  assertEqual 34 $ length lines
  assertEqual
    [35,48,54,52,56,50,56,51,50,47,51,13,46,54,52,51,47,47,54,19,40,20,58,51,55,57,54,52,53,48,51,55,54,53]
    (map length lines) 

test_findLines_noDrop = do
  input <- p205
  let lines = findLinesWindow 42 5 2 False $ head input
  assertEqual 1635 $ length $ concat lines
  assertEqual 35 $ length lines
  assertEqual
    [35,48,54,52,56,50,56,51,50,47,51,13, 1, 46,54,52,51,47,47,54,19,40,20,58,51,55,57,54,52,53,48,51,55,54,53]
    (map length lines)
  -- There is a curly brace outside of the type area!
  assertEqual (Just "}") $ _pmg_text $ head $ concat $ filter (\l -> (length l) == 1) lines

test_findLines_threshold0 = do
  input <- p205
  let lines = findLinesWindow 42 5 0 True $ head input
  assertEqual 35 $ length lines
  assertEqual 1635 $ length $ concat lines
  assertEqual
    [35,48,54,52,56,50,56,51,50,47,51,13, 1, 46,54,52,51,47,47,54,19,40,20,58,51,55,57,54,52,53,48,51,55,54,53]
    (map length lines)
  -- There is a curly brace outside of the type area!
  assertEqual (Just "}") $ _pmg_text $ head $ concat $ filter (\l -> (length l) == 1) lines

