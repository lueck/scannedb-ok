module Pdf.Extract.Clustering where

-- | Clustering algorithms suitable for finding lines, indents etc.

import Data.List
import Data.List.Split


-- | Simple clustering based on a sliding window.
-- 
-- The advantage of this algorithm is that it finds the number of
-- clusters on its own. This makes it suitable for finding lines,
-- because the number of lines may vary from page to page.
--
-- Explanation on the example of finding lines of glyphs:
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
slidingWindow1D ::
  (Eq a)
  => Int                      -- ^ count of steps
  -> Int                      -- ^ threshold for splitting clusters
  -> Bool                     -- ^ drop if count of points in a window
                              -- is below threshold
  -> (a -> Double)            -- ^ get the coordinate
  -> Double                   -- ^ starting point
  -> Double                   -- ^ end point
  -> [a]                      -- ^ list of data points to be clustered
  -> [[a]]                    -- ^ returns a list of clusters (lists)
slidingWindow1D steps threshold drp getter start end points =
  (if drp
    then filter ((>=threshold) . length)    -- drop windows under threshold
    else filter ((>0) . length)) $ -- remove empty windows
  -- concat windows and remove duplicates again
  map (nub . concat) $
  -- split by empty windows
  splitWhen ((<threshold) . length) $
  -- for each step filter the data points in window
  map (\stp -> (filter ((inWindow stp) . getter) points)) $
  map (\n -> start + fromIntegral(n) * stepWidth) [0..steps]
  where
    inWindow :: Double -> Double -> Bool
    inWindow offset coord = coord >= offset && coord <= offset + window
    stepWidth = abs((end - start) / fromIntegral(steps - 1))
    window = stepWidth * 2 -- overlapping
