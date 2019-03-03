module Pdf.Extract.Linearize where

-- | Linearize the glyphs of a line

import qualified Data.Text as T
import Data.List
import Data.Maybe

import Pdf.Extract.Glyph

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


