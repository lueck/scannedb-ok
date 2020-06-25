{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Pdf.Extract.Spacing where

import Test.Framework
import Data.List
import Data.Char

import Pdf.Extract.Spacing


test_representSpacingAfterDog = do
  assertEqual
    [NoSpace 'T',NoSpace 'h',SpaceAfter 'e',NoSpace 'q',NoSpace 'u',NoSpace 'i',NoSpace 'c',SpaceAfter 'k',NoSpace 'b',NoSpace 'r',NoSpace 'o',NoSpace 'w',SpaceAfter 'n',NoSpace 'f',NoSpace 'o',SpaceAfter 'x',NoSpace 'j',NoSpace 'u',NoSpace 'm',NoSpace 'p',SpaceAfter 's',NoSpace 'o',NoSpace 'v',NoSpace 'e',SpaceAfter 'r',NoSpace 't',NoSpace 'h',SpaceAfter 'e',NoSpace 'l',NoSpace 'a',NoSpace 'z',SpaceAfter 'y',NoSpace 'd',NoSpace 'o',NoSpace 'g',NoSpace '.']
    (representSpacesAfter "The quick brown fox jumps over the lazy dog.")

-- | Linearize to 'LeftSpacing' to 'String'.
leftSpacingToString :: [LeftSpacing Char] -> [Char]
leftSpacingToString [] = []
leftSpacingToString ((NoSpace a):xs) = a : (leftSpacingToString xs)
leftSpacingToString ((SpaceAfter a):xs) = a : ' ' : (leftSpacingToString xs)
leftSpacingToString ((SpaceBefore a):xs) = ' ' : a : (leftSpacingToString xs)


test_leftSpacingToString = do
  assertEqual
    "The quick brown fox jumps over the lazy dog."
    (leftSpacingToString $
     representSpacesAfter "The quick brown fox jumps over the lazy dog.")

prop_leftSpacingToString :: [Char] -> Bool
prop_leftSpacingToString s =
  (s == (leftSpacingToString $ representSpacesAfter s))
  ||
  (all isSpace s)
  ||
  (head s == ' ') -- True when " .*", which is too much.
