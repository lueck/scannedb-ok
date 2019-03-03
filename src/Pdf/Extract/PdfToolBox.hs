module Pdf.Extract.PdfToolBox
  ( -- do not export x and y
  ) where

-- | Make Pdf.Content.Processor.Glyph form pdf-toolbox-content an
-- instance of Glyph, so that we can use pdf-toolbox as a parser.


import Pdf.Extract.Glyph
import qualified Pdf.Content.Processor as P
import Pdf.Content.Transform

instance Glyph P.Glyph where
  text = P.glyphText
  bbox g = BBox (x topLeft) (y topLeft) (x bottomRight) (y bottomRight)
    where
      topLeft = P.glyphTopLeft g
      bottomRight = P.glyphBottomRight g
  font _ = Nothing
  code = P.glyphCode
  xLeft = x . P.glyphTopLeft
  yBottom = y . P.glyphBottomRight
  size g = abs((y $ P.glyphTopLeft g) - (y $ P.glyphBottomRight g))
  width g = abs((x $ P.glyphTopLeft g) - (x $ P.glyphBottomRight g))

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
