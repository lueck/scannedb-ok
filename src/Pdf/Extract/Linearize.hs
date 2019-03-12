{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Pdf.Extract.Linearize where

-- | Linearize the glyphs of a line

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Lens

import Pdf.Extract.Glyph
import Pdf.Extract.Clustering

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
  , _line_glyphSize :: Double
  , _line_glyphsInLine :: Int
  , _line_avgLeft :: Double
  , _line_mostLeft :: Double
  , _line_pageWidth :: Double
  , _line_avgGlyphs :: Double
  , _line_avgGlyphWidth :: Double
  , _line_leftBorder :: Double
  , _line_linesOnPage :: Int
  }


-- | Categorize lines using a categorization function.
categorizeLines :: Glyph g =>
                   ([([g], Int, LineData)] -> [LineCategory g])
                -> [[g]]
                -> [LineCategory g]
categorizeLines f lines = f zippedWithData
  where
    mostLeftRight :: [(Double, Double, Double, Int)]
    mostLeftRight = map (foldl (\(left, right, size, count) x ->
                                  (min left $ fst x,
                                   max right $ fst x,
                                   size + (snd x),
                                   count + 1))
                          (1000, 0, 0, 0) .
                          map (\g -> (xLeft g, size g))) lines
    lineData = map (\(l, r, s, c) -> LineData l r (s/fromIntegral c) c
                                  ((sumLeft mostLeftRight) / linesCount)
                                  mostLeft
                                  (mostRight mostLeftRight)
                                  ((fromIntegral $ sumGlyphs mostLeftRight) / linesCount)
                                  ((avgGlyphWidth mostLeftRight) / linesCount)
                                  leftBorder
                                  (length mostLeftRight))
               mostLeftRight
    --zippedWithData :: Glyph g => [([g], Int, LineData)]
    zippedWithData = zip3 lines [1..] lineData
    sumLeft = foldl (+) 0 . map getLeft
    mostLeft = foldl1 min $ map getLeft mostLeftRight
    mostRight = foldl1 max . map getRight
    sumGlyphs = foldl (+) 0 . map getCount
    mostGlyphs = foldl1 max . map getCount
    avgGlyphWidth = foldl (\acc (l, r, s, c) -> acc + ((r - l) / fromIntegral(c))) 0
    getLeft (l, _, _, _) = l
    getRight (_, r, _, _) = r
    getCount (_, _, _, c) = c
    linesCount = fromIntegral $ length mostLeftRight
    clusters = slidingWindow1D (5 * mostGlyphs mostLeftRight) 0 False id 0 mostLeft $
               map getLeft mostLeftRight
    -- We assume that the non-indented lines make the biggest
    -- cluster. And the centroid of this cluster is assumed to be the
    -- left border.
    (maxCluster, maxSize) :: ([Double], Int) =
      foldl (\(accL, accN) (l, n) ->
                (if n > accN then (l, n) else (accL, accN))) ([],0)
      $ zip clusters $ map length clusters
    leftBorder :: Double
    leftBorder = foldl (+) 0 maxCluster / fromIntegral maxSize


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
  | (count == 1) &&
    (containsNumbers line) =
    -- TODO: head skip exceeds baseline skip
    (Headline line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | (count == _line_linesOnPage ldata) &&
    ((_line_left ldata) > custIndent * (_line_pageWidth ldata)) =
    (Custos line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | (count == _line_linesOnPage ldata) &&
    ((_line_left ldata) > sigIndent * (_line_pageWidth ldata)) &&
    ((fromIntegral $ _line_glyphsInLine ldata) < (sigFill * (_line_avgGlyphs ldata))) =
    (SheetSignature line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | (count == _line_linesOnPage ldata) &&
    (containsNumbers line) =
    -- TODO: foot skip exceeds baseline skip
    (Footline line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | (_line_left ldata) >
    (_line_leftBorder ldata + parIndent *
     -- avgWidth) = -- avgWidth ]3..4[
     _line_glyphSize ldata) = -- glyphSize ]3..4[
    --(fromMaybe avgWidth $ fmap size $ line ^? element 3)) =
    (FirstOfParagraph line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  | otherwise = (DefaultLine line):[] ++ byIndent parIndent custIndent sigIndent sigFill ls
  where
    avgWidth = (_line_right ldata - _line_left ldata) / fromIntegral(_line_glyphsInLine ldata)


-- | Returns True if the given line of glyphs contains at least one number.
containsNumbers :: Glyph g => [g] -> Bool
containsNumbers [] = False
containsNumbers (g:gs)
  -- | (T.head $ text g) `elem` ['0'..'9'] = True
  | fromMaybe False $ fmap ((flip elem ['0'..'9']) . T.head) $ text g = True
  | otherwise = containsNumbers gs


-- | Treat every line as a line from the middle of a paragraph. Use
-- this if you don't want to categorize the lines at all.
asDefault :: Glyph g => [([g], Int, LineData)] -> [LineCategory g]
asDefault [] = []
asDefault ((line, count, ldata):ls) = (DefaultLine line):[] ++ asDefault ls


-- * Linearize line.

data KeepDrop = Keep2 | Drop2

data KeepDropPart = Keep3 | Drop3 | Part3

data LinearizationOpts = LinOpts
  { _linopts_head :: KeepDropPart
  , _linopts_foot :: KeepDropPart
  , _linopts_custos :: KeepDrop
  , _linopts_sheetSig :: KeepDrop
  , _linopts_prePage :: T.Text
  , _linopts_postPage :: T.Text
  , _linopts_prePar :: T.Text
  , _linopts_preCustos :: T.Text
  , _linopts_preSheetSig :: T.Text
  }

-- | Linearize a categorized line.
--
-- This requires a function for inserting spaces as the first
-- argument.
--
-- Use @(T.concat . mapMaybe (glyphText))@ for no spacing at all.
--
-- Use `spacingFactor` @(spacingFactor 1.8)@ for spacing on the basis
-- of the width of a glyph.
linearizeCategorizedLine :: Glyph g =>
  LinearizationOpts ->          -- ^ options
  ([g] -> T.Text) ->            -- ^ linearization function
  (LineCategory g) ->           -- ^ categorized lines
  T.Text
linearizeCategorizedLine opts f (FirstOfParagraph glyphs) =
  _linopts_prePar opts  <> linearizeLine f glyphs <> "\n"
linearizeCategorizedLine opts f (DefaultLine glyphs) =
  linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_custos = Drop2} f (Custos glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_custos = Keep2, _linopts_preCustos = pre}
  f (Custos glyphs) =
  pre <> linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_sheetSig = Drop2} f (SheetSignature glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_sheetSig = Keep2, _linopts_preSheetSig = pre}
  f (SheetSignature glyphs) =
  pre <> linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_head = Drop3} f (Headline glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_head = Keep3} f (Headline glyphs) =
  linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_head = Part3,
                                 _linopts_prePage = pre,
                                 _linopts_postPage = post}
  f (Headline glyphs) =
  -- FIXME: filtering may not be enough, because there might be
  -- section numbers in the headline
  pre <> (T.filter (`elem` ['0'..'9']) $ linearizeLine f glyphs) <> post <> " "
linearizeCategorizedLine LinOpts{_linopts_foot = Drop3} f (Footline glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_foot = Keep3} f (Footline glyphs) =
  linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_foot = Part3,
                                 _linopts_prePage = pre,
                                 _linopts_postPage = post}
  f (Footline glyphs) =
  pre <> (T.filter (`elem` ['0'..'9']) $ linearizeLine f glyphs) <> post <> "\n"
  -- There must be a newline before form feed, doesn't it?


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
