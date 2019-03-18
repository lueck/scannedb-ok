{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pdf.Extract.PyPdfMiner where

import Test.Framework

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Range.Range as R

import Pdf.Extract.PyPdfMiner

p205 = B.readFile "test/Heg1835a_p205-p205.xml"
p205to207 = B.readFile "test/Heg1835a_p205-p207.xml"

test_parseXml_singlePage = do
  input <- p205
  glyphs <- parseXml [R.InfiniteRange] input
  assertEqual 1 $ length glyphs -- one page
  assertEqual 1635 $ length $ concat glyphs

test_parseXml_severalPages = do
  input <- p205to207
  glyphs <- parseXml [R.InfiniteRange] input
  assertEqual 3 $ length glyphs -- one page
  assertEqual 1635 $ length $ head glyphs
  assertEqual 1577 $ length $ last glyphs
  assertEqual 4947 $ length $ concat glyphs

test_parseXml_singletonRange = do
  input <- p205to207
  glyphs <- parseXml [R.SingletonRange 1] input
  assertEqual 1 $ length glyphs -- one page
  assertEqual 1635 $ length $ head glyphs
  assertEqual 1635 $ length $ concat glyphs

test_parseXml_singletonRanges = do
  input <- p205to207
  glyphs <- parseXml [R.SingletonRange 1, R.SingletonRange 3] input
  assertEqual 2 $ length glyphs -- one page
  assertEqual 1635 $ length $ head glyphs
  assertEqual 1577 $ length $ last glyphs
  assertEqual (1635+1577) $ length $ concat glyphs

test_parseXml_spanRange = do
  input <- p205to207
  glyphs <- parseXml [R.SpanRange 1 2] input
  assertEqual 2 $ length glyphs -- one page
  assertEqual 1635 $ length $ head glyphs
  assertEqual 1735 $ length $ last glyphs
  assertEqual (1635+1735) $ length $ concat glyphs

test_parseXml_lowerBoundRange = do
  input <- p205to207
  glyphs <- parseXml [R.LowerBoundRange 2] input
  assertEqual 2 $ length glyphs -- one page
  assertEqual 1735 $ length $ head glyphs
  assertEqual 1577 $ length $ last glyphs
  assertEqual (1735+1577) $ length $ concat glyphs

test_parseXml_overlappingRanges = do
  input <- p205to207
  glyphs <- parseXml [R.InfiniteRange, R.LowerBoundRange 2] input
  assertEqual 3 $ length glyphs -- one page
  assertEqual 1635 $ length $ head glyphs
  assertEqual 1577 $ length $ last glyphs
  assertEqual 4947 $ length $ concat glyphs
