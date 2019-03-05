{-# LANGUAGE DeriveGeneric #-}

module Pdf.Extract.Glyph where

import Prelude
import qualified Data.Text as T
import GHC.Generics

data BBox = BBox
  { _bbox_xBottomLeft :: Double
  , _bbox_yBottomLeft :: Double
  , _bbox_xTopRight :: Double
  , _bbox_yTopRight :: Double
  }
  deriving (Eq, Show, Generic)


class Glyph a where
  text :: a -> Maybe T.Text
  bbox :: a -> BBox
  code :: a -> Int
  font :: a -> Maybe String
  xLeft :: a -> Double
  xLeft a = _bbox_xBottomLeft $ bbox a
  yBottom :: a -> Double
  yBottom a = _bbox_yBottomLeft $ bbox a
  size :: a -> Double
  size a = abs((_bbox_yTopRight $ bbox a) - (_bbox_yBottomLeft $ bbox a))
  width :: a -> Double
  width a = abs((_bbox_xTopRight $ bbox a) - (_bbox_xBottomLeft $ bbox a))
  

-- instance Eq Glyph where
--   a == b = ((text a) == (text b) &&
--             (bbox a) == (bbox b) &&
--             (code a) == (code b) &&
--             (font a) == (font b))
