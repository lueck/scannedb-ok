module Pdf.Extract.PdfToolBox
  ( -- do not export x and y
  ) where

-- | Make Pdf.Content.Processor.Glyph form pdf-toolbox-content an
-- instance of Glyph, so that we can use pdf-toolbox as a parser.


import Pdf.Extract.Glyph
import Pdf.Extract.Linearize (Linearizable, linearize, linearizeGlyph)

import qualified Pdf.Content.Processor as P
import Pdf.Content.Transform

-- | In the pdf-toolbox, the naming is buggy: topLeft is really the
-- bottom left angle of the bbox and bottomRight is really top right
-- angle of the bbox.
instance Glyph P.Glyph where
  text = P.glyphText
  bbox g = BBox (x bottomLeft) (y bottomLeft) (x topRight) (y topRight)
    where
      bottomLeft = P.glyphTopLeft g
      topRight = P.glyphBottomRight g
  font _ = Nothing
  code = P.glyphCode
  xLeft = x . P.glyphTopLeft
  yBottom = y . P.glyphTopLeft
  size g = (y $ P.glyphBottomRight g) - (y $ P.glyphTopLeft g)
  width g = (x $ P.glyphBottomRight g) - (x $ P.glyphTopLeft g)

instance Eq P.Glyph where
  (P.Glyph aCode (Vector atlx atly) (Vector ablx ably) at) ==
    (P.Glyph bCode (Vector btlx btly) (Vector bblx bbly) bt)
    = aCode == bCode &&
      atlx == btlx &&
      atly == btly &&
      ablx == bblx &&
      ably == bbly &&
      at == bt

x :: Vector a -> a
x (Vector a _) = a

y :: Vector a -> a
y (Vector _ b) = b

instance Linearizable P.Glyph where
  linearize g = linearizeGlyph g
