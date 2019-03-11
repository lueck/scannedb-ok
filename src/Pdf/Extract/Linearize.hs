{-# LANGUAGE OverloadedStrings #-}
module Pdf.Extract.Linearize where

-- | Linearize the glyphs of a line

import qualified Data.Text as T
import Data.List
import Data.Maybe

import Pdf.Extract.Glyph


-- * Categorize Lines

-- | A wrapper for categorizing lines of glyphs.
data (Glyph g) => LineCategory g
  = DefaultLine [g]             -- ^ somewhere in the middle of a paragraph
  | FirstOfParagraph [g]        -- ^ first line of a paragraph
  | Custos [g]                  -- ^ bottom line with first syllable
                                -- of next page, cf.
                                -- https://www.typografie.info/3/wiki.html/k/kustode-r328/
  | SheetSignature [g]          -- ^ bottom line with sheet a short
                                -- title and number of the sheet (or
                                -- custos)
  | Headline [g]                -- ^ first line of a page that matches
                                -- some features, e.g. presence of a
                                -- page number
  | Footline [g]                -- ^ last line that matches some features

-- | Collect data about lines
data LineData = LineData
  { _line_left :: Double
  , _line_right :: Double
  , _line_glyphsInLine :: Int
  , _line_avgLeft :: Double
  , _line_mostLeft :: Double
  , _line_pageWidth :: Double
  , _line_avgGlyphs :: Double
  , _line_linesOnPage :: Int
  }


-- | Categorize lines using a categorization function.
categorizeLines :: Glyph g =>
                   ([([g], Int, LineData)] -> [LineCategory g])
                -> [[g]]
                -> [LineCategory g]
categorizeLines f lines = f zippedWithData
  where
    mostLeftRight :: [(Double, Double, Int)]
    mostLeftRight = map (foldl (\(left, right, count) x ->
                                  (min left x, max right x, count + 1))
                         (1000, 0, 0) . map xLeft) lines
    lineData = map (\(l, r, c) -> LineData l r c
                                  ((sumLeft mostLeftRight) / linesCount)
                                  (mostLeft mostLeftRight)
                                  (mostRight mostLeftRight)
                                  ((fromIntegral $ sumGlyphs mostLeftRight) / linesCount)
                                  (length mostLeftRight))
               mostLeftRight
    --zippedWithData :: Glyph g => [([g], Int, LineData)]
    zippedWithData = zip3 lines [1..] lineData
    sumLeft = foldl (+) 0 . map getLeft
    mostLeft = foldl1 min . map getLeft
    mostRight = foldl1 max . map getRight
    sumGlyphs = foldl (+) 0 . map getCount
    getLeft (l, _, _) = l
    getRight (_, r, _) = r
    getCount (_, _, c) = c
    linesCount = fromIntegral $ length mostLeftRight


-- | Categorize lines by indent and some other features.
byIndent :: Glyph g =>
            Double ->         -- ^ paragraph indent
            Double ->         -- ^ indent of the custos in partion of
                              -- the pagewidth
            Double ->         -- ^ indent of the sheet signature in
                              -- partion of the pagewidth
            Double ->         -- ^ line filling of the sheet signature
            [([g], Int, LineData)] -> -- ^ the lines and line data
            [LineCategory g]
byIndent _ _ _ _ [] = []
byIndent parIndent custIndent sigIndent sigFill ((line, count, ldata):ls)
  | (count == _line_linesOnPage ldata) &&
    ((_line_left ldata) > custIndent * (_line_pageWidth ldata)) =
    (Custos line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | (count == _line_linesOnPage ldata) &&
    ((_line_left ldata) > sigIndent * (_line_pageWidth ldata)) &&
    --((_line_left ldata) > (_line_avgLeft ldata)) &&
    ((fromIntegral $ _line_glyphsInLine ldata) < (sigFill * (_line_avgGlyphs ldata))) =
    (SheetSignature line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | (_line_left ldata) > parIndent * (_line_pageWidth ldata) =
    (FirstOfParagraph line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | otherwise = (DefaultLine line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls


-- | Treat every line as a line from the middle of a paragraph. Use
-- this if you don't want to categorize the lines at all.
asDefault :: Glyph g => [([g], Int, LineData)] -> [LineCategory g]
asDefault [] = []
asDefault ((line, count, ldata):ls) = (DefaultLine line):[] ++ asDefault ls


-- * Linearize line.

-- | Linearize a categorized line.
--
-- This requires a function for inserting spaces as the first
-- argument.
--
-- Use @(T.concat . mapMaybe (glyphText))@ for no spacing at all.
--
-- Use `spacingFactor` @(spacingFactor 1.8)@ for spacing on the basis
-- of the width of a glyph.
linearizeCategorizedLine :: Glyph g => ([g] -> T.Text) -> (LineCategory g) -> T.Text
linearizeCategorizedLine f (FirstOfParagraph glyphs) = "\n\t" <> linearizeLine f glyphs
linearizeCategorizedLine f (Custos glyphs) = "\t\t\t\t\t" <> linearizeLine f glyphs
linearizeCategorizedLine f (SheetSignature glyphs) = "\t\t\t" <> linearizeLine f glyphs
linearizeCategorizedLine f (DefaultLine glyphs) = linearizeLine f glyphs
linearizeCategorizedLine f (Headline glyphs) = linearizeLine f glyphs
linearizeCategorizedLine f (Footline glyphs) = linearizeLine f glyphs


-- | Linearize the glyphs of a line using a serialization function.
--
-- This requires a function for inserting spaces as the first
-- argument.
--
-- Use @(T.concat . mapMaybe (glyphText))@ for no spacing at all.
--
-- Use `spacingFactor` @(spacingFactor 1.8)@ for spacing on the basis
-- of the width of a glyph.
linearizeLine :: Glyph g => ([g] -> T.Text) -> [g] -> T.Text
linearizeLine f = f . sortOn xLeft

-- | Insert spaces on the basis of the width of a glyph. If the
-- distance to the next glyph exceeds the product of the width and a
-- spacing factor, insert a space.
--
-- Unfortunately this fails on google books--at least for OCRed scans
-- of german fractur fonts--because there is no width in these fonts:
-- The x value of the bounding box of a glyph has the same
-- value. (This is holds true for test cases parsed with pdf-toolbox
-- and with pdfminer.)
spacingFactor :: Glyph g => Double -> [g] -> T.Text
spacingFactor _ [] = T.empty
spacingFactor _ (g:[]) = fromMaybe T.empty $ text g
spacingFactor spacing (g:gn:gs)
  | dist > spacing * (width g) = T.append (fromMaybe T.empty $ text g) $
                                  T.append (T.pack " ") $
                                  spacingFactor spacing (gn:gs)
  | otherwise = T.append (fromMaybe T.empty $ text g) $
                spacingFactor spacing (gn:gs)
    where
      dist = abs((xLeft g) - (xLeft gn))
