{-# LANGUAGE OverloadedStrings #-}
module Pdf.Extract.Utils
  ( longestSubstringWith
  , longestRomanNumber
  ) where

-- | This module contains helper functions.

import qualified Data.Text as T
import Text.Regex.TDFA


-- | Get the length of the longest substring that matches a predicate
-- function.
longestSubstringWith :: (Char -> Bool) -> T.Text -> Int
longestSubstringWith p =
  maximum . (0:) . map T.length . T.words . T.map (\c -> if p c then c else ' ')

-- | Get the length of the longest substring that may represent a
-- roman number.
longestRomanNumber :: T.Text -> Int
longestRomanNumber =
  maximum . (0:) . map length . romanNumbers . T.unpack
  where
    upperRoman = "M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})"
    lowerRoman = "m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})"
    romanRe = "("++upperRoman++"|"++lowerRoman++")"
    -- FIXME: execute on T.Text
    romanNumbers :: String -> [String]
    romanNumbers s = getAllTextMatches (s=~romanRe)
