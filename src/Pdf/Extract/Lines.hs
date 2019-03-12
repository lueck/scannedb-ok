{-# LANGUAGE ScopedTypeVariables #-}
module Pdf.Extract.Lines where

-- | Find the lines on a page.


import Data.List.Split
import Data.List
import Data.Maybe
import System.IO
import Control.Lens

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


-- * Information about lines for further parsing.

-- | Record for collecting information about the lines of a page.
data LineData = LineData
  { _line_left :: Double        -- ^ left pos of this line
  , _line_right :: Double       -- ^ right pos of this line
  , _line_glyphSize :: Double   -- ^ font size of this line (average)
  , _line_glyphsInLine :: Int   -- ^ glyphs in this line
  -- | data not special to this line
  , _line_linesOnPage :: Int    -- ^ count of lines on the page
  , _line_avgGlyphs :: Double   -- ^ average count of glyphs per line
  , _line_leftBorderLowerBound :: Double
  , _line_leftBorderUpperBound :: Double
  , _line_linesAtLeftBorder :: Int
  , _line_leftBorderClusters :: [[Double]]
  , _line_rightBorderLowerBound :: Double
  , _line_rightBorderUpperBound :: Double
  , _line_linesAtRightBorder :: Int
  , _line_rightBorderClusters :: [[Double]]
  , _line_glyphSizeLowerBound :: Double
  , _line_glyphSizeUpperBound :: Double
  , _line_glyphSizeClusters :: [[Double]]
  }

-- | Generate information about lines and page.
genLineInfo :: Glyph g => [[g]] -> [LineData]
genLineInfo lines =
  map (\(l, r, s, c) ->
         LineData l r s c --(s/fromIntegral c) c
         -- data not special to this line
         linesCount
         ((fromIntegral $ sumGlyphs lineTuple) / linesCountFrac)
         leftBorderLowerBound
         leftBorderUpperBound
         leftBorderSize
         leftBorderClusters
         rightBorderLowerBound
         rightBorderUpperBound
         rightBorderSize
         rightBorderClusters
         glyphSizeLowerBound
         glyphSizeUpperBound
         glyphSizeClusters)
  lineTuple
  where
    lineTuple :: [(Double, Double, Double, Int)]
    lineTuple = map (foldl (\(left, right, size, count) x ->
                               (min left $ fst x,
                                max right $ fst x,
                                max size $ snd x, --size + (snd x),
                                count + 1))
                      (1000, 0, 0, 0) .
                      map (\g -> (xLeft g, size g))) lines
    mostLeft = foldl1 min $ map getLeft lineTuple
    mostRight = foldl1 max $ map getRight lineTuple
    sumGlyphs = foldl (+) 0 . map getCount
    mostGlyphs = foldl max 0 $ map getCount lineTuple
    tallestGlyph = ceiling $ foldl max 0 $ map getSize lineTuple
    getLeft (l, _, _, _) = l
    getRight (_, r, _, _) = r
    getSize (_, _, s, _) = s
    getCount (_, _, _, c) = c
    linesCount = length lines
    linesCountFrac = fromIntegral $ linesCount
    -- Left Border: We assume that the non-indented lines make the
    -- biggest cluster. And the lower (right) bound of this cluster is
    -- assumed to be the left border and the upper bound is used to
    -- identify indented lines.
    leftBorderClusters = slidingWindow1D (4 * mostGlyphs) 0 False id 0 mostRight $
                         map getLeft lineTuple
    (leftBorderSize, leftBorderCluster) = longest' leftBorderClusters
    leftBorderUpperBound :: Double
    leftBorderUpperBound = foldl max 0 leftBorderCluster
    leftBorderLowerBound :: Double
    leftBorderLowerBound = foldl min leftBorderUpperBound leftBorderCluster
    -- Right Border:
    rightBorderClusters = slidingWindow1D mostGlyphs 0 False id 0 mostRight $
                          map getRight lineTuple
    (rightBorderSize, rightBorderCluster) = longest' rightBorderClusters
    rightBorderUpperBound :: Double
    rightBorderUpperBound = foldl max 0 rightBorderCluster
    rightBorderLowerBound :: Double
    rightBorderLowerBound = foldl min rightBorderUpperBound rightBorderCluster
    -- Clustering glyph size:
    glyphSizeClusters = slidingWindow1D (tallestGlyph * 10) 0 False id 0 (fromIntegral tallestGlyph) $
                        map getSize lineTuple
                        -- map (\l -> getSize l / (fromIntegral $ getCount l)) lineTuple
    glyphSizeCluster = longest glyphSizeClusters
    glyphSizeUpperBound :: Double
    glyphSizeUpperBound = foldl max 0 glyphSizeCluster
    glyphSizeLowerBound :: Double
    glyphSizeLowerBound = foldl min glyphSizeUpperBound glyphSizeCluster


-- | Print a list of 'LineData' in the IO monad.
printLineInfo :: [LineData] -> IO ()
printLineInfo infos = do
  putStr "#Lines: "
  print $ length $ infos
  putStr "Glyphs per Lines: "
  print $ map _line_glyphsInLine infos

  putStr "Left border, lines starting there: "
  print $ fromMaybe "<NA>" $ fmap (show . _line_linesAtLeftBorder) fstInfo
  putStr "Left border (upper bound): "
  print $ fromMaybe "<NA>" $ fmap (show . _line_leftBorderUpperBound) fstInfo
  putStr "Left border clusters: "
  print $ fromMaybe "<NA>" $ fmap (show . _line_leftBorderClusters) fstInfo
  putStr "Left most glyph of lines: "
  print $ map _line_left infos

  putStr "Right border, lines starting there: "
  print $ fromMaybe "<NA>" $ fmap (show . _line_linesAtRightBorder) fstInfo
  putStr "Right border (upper bound): "
  print $ fromMaybe "<NA>" $ fmap (show . _line_rightBorderUpperBound) fstInfo
  putStr "Right border clusters: "
  print $ fromMaybe "<NA>" $ fmap (show . _line_rightBorderClusters) fstInfo
  putStr "Right most glyph of lines: "
  print $ map _line_right infos

  putStr "Glyph Size (lower bound of biggest cluster): "
  print $ fromMaybe "<NA>" $ fmap (show . _line_glyphSizeLowerBound) fstInfo
  putStr "Glyph size clusters: "
  print $ fromMaybe "<NA>" $ fmap (show . _line_glyphSizeClusters) fstInfo
  putStr "Glyph sizes of lines: "
  print $ map _line_glyphSize infos


  return ()
  where
    fstInfo = infos ^? element 1
