{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric #-}
module Pdf.Extract.Spacing
where

-- | Analyse the inter-glyph spacing of a document. 

import qualified Data.Text as T
import Data.List
import Data.Maybe
import GHC.Generics
import Control.Lens
import qualified Data.Csv as Csv
import Data.Char

import Pdf.Extract.Glyph


data Spacing = Spacing
  { _sp_char :: T.Text
  , _sp_next :: T.Text
  , _sp_dist :: Double
  , _sp_width :: Double
  , _sp_size :: Double
  , _sp_left :: Double
  , _sp_bottom :: Double        -- ^ For mining for lines on a single page
  , _sp_font :: T.Text
  , _sp_line :: Maybe Int
  , _sp_page :: Maybe Int
  } deriving (Eq, Show, Generic)

makeLenses ''Spacing


-- * Exporting

-- | 'Spacing' ready for CSV export.
instance Csv.ToRecord Spacing


-- * Getting spacing information

-- | Get the inter-glyph spacings of a line of glyphs. The line is
-- required to be sorted by xLeft value of the glyphs.
spacingsInLine :: Glyph g => Maybe Int -> Maybe Int -> [g] -> [Spacing]
spacingsInLine _ _ [] = []
spacingsInLine _ _ (_:[]) = []
spacingsInLine l p (g1:g2:gs) =
  (maybeToList $ Spacing
    <$> text g1
    <*> text g2
    <*> Just ((xLeft g2) - (xLeft g1))
    <*> (Just $ width g1)
    <*> (Just $ size g1)
    <*> (Just $ xLeft g1)
    <*> (Just $ yBottom g1)
    <*> (Just $ T.pack $ fromMaybe "unkown" $ font g1)
    <*> Just l
    <*> Just p)
  ++ spacingsInLine l p (g2:gs)


-- * ANN

-- | For the inter-word-spacing ANN we represent the characters of a
-- line without the spaces. Therefore we wrap each character in a
-- 'Spacing' record. Maybe we shoult make this record a functor. This
-- wrapping is done by the 'representSpacesAfter' function. The
-- resulting list can easily be parallelized with the list of glyphs
-- for the line.

-- | A wrapper for representing spaces.
data LeftSpacing a
  = SpaceBefore a
  | SpaceAfter a
  | NoSpace a
  deriving (Show, Eq)

withoutSpace :: LeftSpacing a -> a
withoutSpace (SpaceBefore a) = a
withoutSpace (SpaceAfter a) = a
withoutSpace (NoSpace a) = a

-- | Represent training data.
representSpacesAfter :: [Char] -> [LeftSpacing Char]
representSpacesAfter [] = []
representSpacesAfter (x:[])
  | x == ' ' = []
  | otherwise = (NoSpace x):[] 
representSpacesAfter (x:x2:xs)
  | x == ' ' = representSpacesAfter (x2:xs) -- drop leading space
  | x2 == ' ' = (SpaceAfter x) : (representSpacesAfter xs)
  | otherwise = (NoSpace x) : (representSpacesAfter (x2:xs))


-- | Linearize to 'String'.
leftSpacingToString :: [LeftSpacing Char] -> [Char]
leftSpacingToString [] = []
leftSpacingToString ((NoSpace a):xs) = a : (leftSpacingToString xs)
leftSpacingToString ((SpaceAfter a):xs) = a : ' ' : (leftSpacingToString xs)
leftSpacingToString ((SpaceBefore a):xs) = ' ' : a : (leftSpacingToString xs)


spacingVector :: Glyph g => g -> [Double]
spacingVector g =
  [ xLeft g
  , xRight g
  , width g
  , size g
  , charFeature ord g -- ordinal
  , charFeature (fromEnum . isUpper) g
  , charFeature (fromEnum . isLower) g
  , charFeature (fromEnum . (`elem` (",;:.!?"::[Char]))) g
  ]
  where
    charFeature f = fromIntegral . (fromMaybe 0 . (fmap (f . T.head))) . text
