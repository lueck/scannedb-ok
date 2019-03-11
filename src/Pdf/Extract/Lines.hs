module Pdf.Extract.Lines where

-- | Find the lines on a page.


import Data.List.Split
import Data.List

import Pdf.Extract.Glyph
import Pdf.Extract.Clustering

-- * Find the lines on a page.


-- | Collect the glyphs of a page into lines. This simple algorithm
-- works if the glyphs of a line are horizontally oriented. They
-- needed be lined up exactly, but the line must not be skew or bend
-- up or down.
findLinesWindow :: (Eq g, Glyph g) => Int -> Int -> Int -> Bool -> [g] -> [[g]]
findLinesWindow linesCount stepsPerLine threshold drop glyphs =
  reverse $ -- slidingWindow started at the bottom, so need to reverse
  slidingWindow1D (linesCount * stepsPerLine * 2) threshold drop yBottom bottom_ top_ glyphs
  where
    top = top_ + 1.0
    -- As long as we don't have bbox data from the page, we can asume
    -- bottom at 0. So when the page isn't filled vertically, the
    -- window size is still good, provided that the text still starts
    -- on the head or top of the page. Are there any systems of
    -- script, where the text starts at the bottom?
    bottom = 0 -- bottom_ - window - 1.0 -- bottom - window - 1
    bottom_ = 0 -- glyphsBottom glyphs
    top_ = glyphsTop glyphs


glyphsTop :: Glyph g => [g] -> Double
glyphsTop = foldl max 0 . map yBottom

glyphsBottom :: Glyph g => [g] -> Double
glyphsBottom [] = 0
glyphsBottom gs = foldl1 min $ map yBottom gs
