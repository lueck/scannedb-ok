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
  { _line_left :: Double
  , _line_right :: Double
  , _line_glyphSize :: Double
  , _line_glyphsInLine :: Int
  , _line_avgLeft :: Double
  , _line_mostLeft :: Double
  , _line_pageWidth :: Double
  , _line_avgGlyphs :: Double
  , _line_avgGlyphWidth :: Double
  , _line_leftBorder :: Double
  , _line_linesAtLeftBorder :: Int
  , _line_leftBorderClusters :: [[Double]]
  , _line_linesOnPage :: Int
  }

-- | Generate information about lines and page.
genLineInfo :: Glyph g => [[g]] -> [LineData]
genLineInfo lines =
  map (\(l, r, s, c) ->
         LineData l r (s/fromIntegral c) c
         ((sumLeft lineTuple) / linesCount)
         mostLeft
         mostRight
         ((fromIntegral $ sumGlyphs lineTuple) / linesCount)
         ((avgGlyphWidth lineTuple) / linesCount)
         leftBorderUpperBound
         maxSize
         clusters
         (length lineTuple))
  lineTuple
  where
    lineTuple :: [(Double, Double, Double, Int)]
    lineTuple = map (foldl (\(left, right, size, count) x ->
                               (min left $ fst x,
                                max right $ fst x,
                                size + (snd x),
                                count + 1))
                      (1000, 0, 0, 0) .
                      map (\g -> (xLeft g, size g))) lines
    sumLeft = foldl (+) 0 . map getLeft
    mostLeft = foldl1 min $ map getLeft lineTuple
    mostRight = foldl1 max $ map getRight lineTuple
    sumGlyphs = foldl (+) 0 . map getCount
    mostGlyphs = foldl max 0 $ map getCount lineTuple
    avgGlyphWidth = foldl (\acc (l, r, s, c) -> acc + ((r - l) / fromIntegral(c))) 0
    getLeft (l, _, _, _) = l
    getRight (_, r, _, _) = r
    getCount (_, _, _, c) = c
    linesCount = fromIntegral $ length lineTuple
    clusters = slidingWindow1D mostGlyphs 0 False id 0 mostRight $
               map getLeft lineTuple
    -- We assume that the non-indented lines make the biggest
    -- cluster. And the centroid of this cluster is assumed to be the
    -- left border.
    (maxCluster, maxSize) :: ([Double], Int) =
      foldl (\(accL, accN) (l, n) ->
                (if n > accN then (l, n) else (accL, accN))) ([],0)
      $ zip clusters $ map length clusters
    leftBorderUpperBound :: Double
    leftBorderUpperBound = foldl max 0 maxCluster
    leftBorderLowerBound :: Double
    leftBorderLowerBound = foldl min leftBorderUpperBound maxCluster


printLineInfo :: [LineData] -> IO ()
printLineInfo infos = do
  putStr "#Lines: "
  print $ length $ infos
  putStr "Glyphs per Lines: "
  print $ map _line_glyphsInLine infos
  putStr "Left border, lines starting there: "
  print $ fromMaybe "<NA>" $ fmap (show . _line_linesAtLeftBorder) fstInfo
  putStr "Left border (upper bound): "
  print $ fromMaybe "<NA>" $ fmap (show . _line_leftBorder) fstInfo
  putStr "Left border clusters: "
  print $ fromMaybe "<NA>" $ fmap (show . _line_leftBorderClusters) fstInfo
  putStr "Left most glyph of lines: "
  print $ map _line_left infos
  return ()
  where
    fstInfo = infos ^? element 1
