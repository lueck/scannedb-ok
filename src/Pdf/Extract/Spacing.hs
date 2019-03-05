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
  , _sp_size :: Double
  , _sp_font :: T.Text
  } deriving (Eq, Show, Generic)

makeLenses ''Spacing


-- * Exporting

-- | 'Spacing' ready for CSV export.
instance Csv.ToRecord Spacing


-- * Getting spacing information

-- | Get the inter-glyph spacings of a line of glyphs.
spacingsInLine :: Glyph g => [g] -> [Spacing]
spacingsInLine [] = []
spacingsInLine (_:[]) = []
spacingsInLine (g1:g2:gs) =
  (maybeToList $ Spacing
    <$> text g1
    <*> text g2
    <*> Just ((xLeft g2) - (xLeft g1))
    <*> (Just $ size g1)
    <*> (Just $ T.pack $ fromMaybe "unkown" $ font g1))
  ++ spacingsInLine (g2:gs)
