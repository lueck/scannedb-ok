module Pdf.Extract.Lines where

-- | Find the lines on a page.

import Data.List

import Pdf.Extract.Glyph
import Pdf.Extract.Clustering

-- * Find the lines on a page.

data LineOptions = LineOptions
  { linesPerPage :: Int         -- ^ estimate count of lines per page
  , threshold :: Int            -- ^ threshold of glyphs per line
                                -- under which clusters are separated
  , dropUnderThreshold :: Bool  -- ^ Drop clusters of glyphs when the
                                -- count is under 'threshold'.
  , stepsPerLine :: Int         -- ^ count of steps the clustering
                                -- algorithm should be taking per line
  }


-- | Collect the glyphs of a page into lines. This simple algorithm
-- works if the glyphs of a line are horizontally oriented. They
-- needed be lined up exactly, but the line must not be skew or bend
-- up or down.
findLinesWindow :: (Eq g, Glyph g) => LineOptions -> [g] -> [[g]]
findLinesWindow opts glyphs =
  reverse $ -- slidingWindow started at the bottom, so need to reverse
  slidingWindow1D
  (linesPerPage opts * stepsPerLine opts * 2)
  (threshold opts)
  (dropUnderThreshold opts)
  yBottom bottom_ top_ glyphs
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


-- * Pages

-- | A 'Page' has a number and lines of 'Glyph's or categorized items.
type Page g = (Int, [g])


-- | Run a calculation on the glyphs or already categorized items of a
-- page.
pageMap :: ([a] -> [b]) -> Page a -> Page b
pageMap f = (,) <$> fst <*> f . snd

-- | Find lines of a page using window clustering, preserve the page
-- numbers.
findLinesWindowOnPage :: (Eq g, Glyph g) => LineOptions -> Page g -> Page [g]
findLinesWindowOnPage opts =
  pageMap (map (sortOn xLeft) . findLinesWindow opts)
