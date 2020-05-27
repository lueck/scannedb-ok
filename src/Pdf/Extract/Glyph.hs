{-# LANGUAGE DeriveGeneric, GADTs #-}

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
  xLeft = _bbox_xBottomLeft . bbox
  xRight :: a -> Double
  xRight = _bbox_xTopRight . bbox
  yBottom :: a -> Double
  yBottom = _bbox_yBottomLeft . bbox
  yTop :: a -> Double
  yTop = _bbox_yTopRight . bbox
  size :: a -> Double
  size a = abs((_bbox_yTopRight $ bbox a) - (_bbox_yBottomLeft $ bbox a))
  width :: a -> Double
  width a = abs((_bbox_xTopRight $ bbox a) - (_bbox_xBottomLeft $ bbox a))
  

-- instance Eq Glyph where
--   a == b = ((text a) == (text b) &&
--             (bbox a) == (bbox b) &&
--             (code a) == (code b) &&
--             (font a) == (font b))


-- | A GADT that can be wrapped around data types that are instances
-- of 'Glyph' (and 'Show'). This enables us to use a single function
-- for getting glyphs from various input formats.
data GlyphType
  where
    MkGlyphType :: (Glyph g, Show g, Eq g) => g -> GlyphType

instance Show GlyphType where
  show (MkGlyphType g) = show g

instance Eq GlyphType where
  (==) = eqGlyphType
  (/=) = neGlyphType

eqGlyphType, neGlyphType :: GlyphType -> GlyphType -> Bool
eqGlyphType (MkGlyphType g1) (MkGlyphType g2) =
  -- g1 == g2 -- does not work, since they may be of different types
  (text g1 == text g1) &&
  (bbox g1 == bbox g2) &&
  (code g1 == code g2) &&
  (font g1 == font g2)
neGlyphType (MkGlyphType g1) (MkGlyphType g2) =
  -- g1 /= g2
  (text g1 /= text g1) &&
  (bbox g1 /= bbox g2) &&
  (code g1 /= code g2) &&
  (font g1 /= font g2)

instance Glyph GlyphType where
  text (MkGlyphType g) = text g
  bbox (MkGlyphType g) = bbox g
  code (MkGlyphType g) = code g
  font (MkGlyphType g) = font g
