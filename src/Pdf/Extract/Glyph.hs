module Pdf.Extract.Glyph where

import Prelude
import qualified Data.Text as T


data BBox = BBox
  { _bbox_xTopLeft :: Double
  , _bbox_yTopLeft :: Double
  , _bbox_xBottomRight :: Double
  , _bbox_yBottomRight :: Double
  }
  deriving (Eq, Show)


class Glyph a where
  text :: a -> Maybe T.Text
  bbox :: a -> BBox
  code :: a -> Int
  font :: a -> Maybe String
  xLeft :: a -> Double
  xLeft a = _bbox_xTopLeft $ bbox a
  yBottom :: a -> Double
  yBottom a = _bbox_yBottomRight $ bbox a
  size :: a -> Double
  size a = abs((_bbox_yTopLeft $ bbox a) - (_bbox_yBottomRight $ bbox a))
  width :: a -> Double
  width a = abs((_bbox_xTopLeft $ bbox a) - (_bbox_xBottomRight $ bbox a))
  

-- instance Eq Glyph where
--   a == b = ((text a) == (text b) &&
--             (bbox a) == (bbox b) &&
--             (code a) == (code b) &&
--             (font a) == (font b))
