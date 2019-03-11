module Pdf.Extract.Lines where

-- | Find the lines on a page.


import Data.List.Split
import Data.List

import Pdf.Extract.Glyph

-- * Find the lines on a page.


-- | Collect the glyphs of a page into lines. This simple algorithm
-- works if the glyphs of a line are horizontally oriented. They
-- needed be lined up exactly, but the line must not be skew or bend
-- up or down.
--
-- 1) Construct a window, that spans the pagewidth but is not heigh
-- and that steps the height of the page from the bottom to the
-- top. It thereby counts the glyphs at each height. The steps
-- overlap.
--
-- 2) The resulting list of glyphs in each height is then split where
-- the count of glyphs falls below a certain threshold. The glyphs in
-- windows with count lower than the treshold are lost--this may be
-- wanted in case of dirt. The result is a list of lists, where each
-- inner list is a sequence of glyphs in a window. Since the windows
-- overlap, each glyph will show up in more than one window.
--
-- 3) We concat these inner lists and then remove the duplicate
-- glyphs. The result is a list of list of glyphs at similar heights,
-- i.e. a list of the lines.
--
-- The problem maybe 2, the splitting by zero glyphs at a window
-- height. The page may be dirty or the lines may be skew.
findLinesWindow :: (Eq g, Glyph g) => Int -> Int -> Int -> Bool -> [g] -> [[g]]
findLinesWindow linesCount stepsPerLine threshold drop glyphs =
  reverse $ -- we started at the bottom, so need to reverse
  (if drop
    then filter ((>=threshold) . length)    -- drop windows under threshold
    else filter ((/=threshold) . length)) $ -- remove empty windows
  map (nub . concat) $ -- concat windows and remove duplicates again
  splitWhen ((<threshold) . length) glyphsInWindow -- split by empty windows
  
  -- filter ((/=0) . length) $ -- remove empty windows
  -- map (nubBy glyphEq . concatMap snd) $ -- concat windows and remove duplicates again
  -- splitWhen ((==0) . fst) $  -- split by empty windows
  -- zip countGlyphsInWindow glyphsInWindow
  where
    --countGlyphsInWindow = map length glyphsInWindow
    glyphsInWindow = map (\stp -> (filter ((inWindow stp) . yBottom) glyphs)) steps
    inWindow yOffset yGlyph
      | yGlyph >= yOffset && yGlyph <= yOffset + window = True
      -- | yGlyph < bottom && yGlyph > top = True
      | otherwise = False
    stepsCount = stepsPerLine * linesCount * linesFactor
    steps = map (\n -> bottom + fromIntegral(n) * step) [0..stepsCount]
    step = abs((top - bottom) / fromIntegral(stepsCount - 1))
    window = abs((bottom_ - top_) / fromIntegral(linesCount * linesFactor))
    top = top_ + 1.0
    -- As long as we don't have bbox data from the page, we can asume
    -- bottom at 0. So when the page isn't filled vertically, the
    -- window size is still good, provided that the text still starts
    -- on the head or top of the page. Are there any systems of
    -- script, where the text starts at the bottom?
    bottom = 0 -- bottom_ - window - 1.0 -- bottom - window - 1
    bottom_ = 0 -- glyphsBottom glyphs
    top_ = glyphsTop glyphs
    linesFactor = 3


glyphsTop :: Glyph g => [g] -> Double
glyphsTop = foldl max 0 . map yBottom

glyphsBottom :: Glyph g => [g] -> Double
glyphsBottom [] = 0
glyphsBottom gs = foldl1 min $ map yBottom gs
