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
