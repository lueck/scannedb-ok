{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pdf.Extract.Syllable where

import Test.Framework

import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M


import Pdf.Extract.Syllable

wordpool = "Geschmacks alle Donaudampfschifffahrtsgesellschaft"
wp = M.fromListWith (+) $ zip (T.words wordpool) (repeat 1)

repair' = repair (flip M.member wp) 


test_endsWithDash = do
  assertBool $ not $ endsWithDash "abdc"
  assertBool $ not $ endsWithDash "abdc."
  assertBool $ not $ endsWithDash "abdc,"
  assertBool $ not $ endsWithDash "abdc!"
  assertBool $ endsWithDash "abdc-"
  assertBool $ endsWithDash "abdc–"
  assertBool $ endsWithDash "abdc="
  assertBool $ not $ endsWithDash "abdc-ef"
  return ()

test_stripEndDash = do
  assertEqual "Ge" (stripEndDash "Ge-")
  assertEqual "Ge" (stripEndDash "Ge–")
  assertEqual "Ge" (stripEndDash "Ge=")
  assertEqual "Ge" (stripEndDash "Ge")
  assertEqual "Ge- " (stripEndDash "Ge- ") -- Right! Make sure that we don't loose whitespace!
  assertEqual "Ge-\n" (stripEndDash "Ge-\n")
  assertEqual "Ge-Birge" (stripEndDash "Ge-Birge")
  assertEqual "-Birge" (stripEndDash "-Birge")

test_firstWord = do
  assertEqual "Nach" (firstWord "Nach Ablauf der Zeit")
  assertEqual "Nach" (firstWord "  Nach Ablauf der Zeit")
  assertEqual "Nach" (firstWord "\nNach Ablauf der Zeit")
  assertEqual "Nach" (firstWord "Nach Ablauf")
  assertEqual "Nach." (firstWord "Nach. dem Punkt")
  assertEqual "Nach" (firstWord "Nach")
  assertEqual "Nach" (firstWord "Nach ")
  assertEqual "Nach" (firstWord "Nach\n")
  assertEqual "Nach" (firstWord "Nach\n\f")
  assertEqual "Nach." (firstWord "Nach.\n")


lines1 = [ "Die Organe des Geruchs und Ge\n"
         , "schmacks dagegen gehören..."]
lines1r = [ "Die Organe des Geruchs und Geschmacks\n"
          , "dagegen gehören..."]
lines1_ = [ "Die Organe des Geruchs und Ge-\n"
          , "schmacks dagegen gehören..."]

test_repair_withoutDash = do
  lt <- repair' True [] lines1
  assertEqual lines1 lt
  lf <- repair' False [] lines1
  assertEqual lines1r lf

test_repair_withDash = do
  lt <- repair' True [] lines1_
  assertEqual lines1r lt
  lf <- repair' False [] lines1_
  assertEqual lines1r lf


lines2 = [ "Die Organe des Geruchs und Ge.\n"
          , "schmacks dagegen gehören..."]

test_repair_withPunctuation = do
  lt <- repair' True [] lines2
  assertEqual lines2 lt
  lf <- repair' False [] lines2
  assertEqual lines2 lf

  
lines3 = [ "Die Organe des Geruchs und Ge.-\n"
          , "schmacks dagegen gehören..."]

test_repair_withPunctuationAndDash = do
  lt <- repair' True [] lines3
  assertEqual lines3 lt
  lf <- repair' False [] lines3
  assertEqual lines3 lf


lines4 = [ "Die Organe des Geruchs und Ge -\n"
          , "schmacks dagegen gehören..."]

test_repair_withWhiteSpaceAndDash = do
  lt <- repair' True [] lines4
  assertEqual lines4 lt
  lf <- repair' False [] lines4
  assertEqual lines4 lf


lines5 = [ "Die Organe des Ge\n"
         , "ruchs und Geschmacks dagegen gehören..."]

test_repair_notInWordpool = do
  lt <- repair' True [] lines5
  assertEqual lines5 lt
  lf <- repair' False [] lines5
  assertEqual lines5 lf


lines5_ = [ "Die Organe des Ge-\n"
          , "ruchs und Geschmacks dagegen gehören..."]

test_repair_notInWordpoolAndDash = do
  lt <- repair' True [] lines5_
  assertEqual lines5_ lt
  lf <- repair' False [] lines5_
  assertEqual lines5_ lf


lines6 = [ "Die Organe des Ge\n"
         , "schmacks, z.B. die Nase, sind ..." ]
lines6r = [ "Die Organe des Geschmacks,\n"
          , "z.B. die Nase, sind ..." ]
lines6_ = [ "Die Organe des Ge-\n"
          , "schmacks, z.B. die Nase, sind ..." ]

test_repair_punctuationAfterFirstWordInNextLineWithoutDash = do
  lt <- repair' True [] lines6
  assertEqual lines6 lt
  lf <- repair' False [] lines6
  assertEqual lines6r lf

test_repair_punctuationAfterFirstWordInNextLineWithDash = do
  lt <- repair' True [] lines6_
  assertEqual lines6r lt
  lf <- repair' False [] lines6_
  assertEqual lines6r lf


linesX = [ "Donau-\n"
         , "dampf-\n"
         , "schiff-\n"
         , "gesell-\n"
         , "schaftskapitän sucht eine neue Schaluppe."]
linesXr = [ "Donaudampfschifffahrtsgesellschaftskapitän\n"
          , "sucht eine neue Schaluppe." ]

-- -- FIXME
-- test_repair_severalLinesT = do
--   lt <- repair' True [] linesX
--   assertEqual linesXr lt

-- -- FIXME
-- test_repair_severalLinesF = do
--   lf <- repair' False [] linesX
--   assertEqual linesXr lf
