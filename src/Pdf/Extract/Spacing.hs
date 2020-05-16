{-# LANGUAGE OverloadedStrings
, TemplateHaskell
, DeriveGeneric
, DataKinds #-}
module Pdf.Extract.Spacing
where

-- | Analyse the inter-glyph spacing of a document. 

import qualified Data.Text as T
import Data.List
import Data.Maybe
import GHC.Generics hiding (S)
import Control.Lens
import qualified Data.Csv as Csv
import Data.Char
import qualified Data.Vector.Storable as V
import Data.Semigroup
import Grenade
import Grenade.Utils.OneHot
import GHC.TypeNats

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


-- | Linearize to 'LeftSpacing' to 'String'.
leftSpacingToString :: [LeftSpacing Char] -> [Char]
leftSpacingToString [] = []
leftSpacingToString ((NoSpace a):xs) = a : (leftSpacingToString xs)
leftSpacingToString ((SpaceAfter a):xs) = a : ' ' : (leftSpacingToString xs)
leftSpacingToString ((SpaceBefore a):xs) = ' ' : a : (leftSpacingToString xs)


-- | Generate a data vector from a 'Glyph'.
singleGlyphSpacingVector :: Glyph g => g -> V.Vector Double
singleGlyphSpacingVector g =
  V.fromList $
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

-- | Generate data vectors from a line of 'Glyph's by moving a window
-- over the line. The number of preceding and succeding glyphs,
-- i.e. the size of the window, is set by the first two arguments.
mkSpacingVectors
  :: Glyph g =>
     Int                             -- ^ number of preceding glyphs in window
  -> Int                             -- ^ number of succeding glyphs in window
  -> [g]                             -- ^ the line's glyphs
  -> [V.Vector Double]               -- ^ a vector of float values for each glyph
mkSpacingVectors pre succ gs =
  take (length gs) $ drop pre $
  fst $
  foldl' moveWindow ([], (take pre $ repeat Nothing)) $
  ((take pre $ repeat Nothing) ++ (map Just gs) ++ (take succ $ repeat Nothing))
  where
    moveWindow :: Glyph g =>
                  ([V.Vector Double], [Maybe g])
               -> Maybe g
               -> ([V.Vector Double], [Maybe g])
    moveWindow (vecs, oldWindow) curr = ((mkSpacingVector newWindow):vecs, newWindow)
      where
        newWindow = take (pre + succ + 1) (curr:oldWindow)
    mkSpacingVector :: Glyph g =>
                       [Maybe g]    -- ^ the glyphs in the moving window
                    -> V.Vector Double
    mkSpacingVector [] = V.empty
    mkSpacingVector (g:[]) = V.concat
      [ V.singleton $ fromIntegral $ fromEnum $ isJust g
      , V.singleton $ fromIntegral $ fromEnum $ isNothing g
      , fromMaybe (V.fromList $ take lenSingleVector $ repeat 0.0) $
        fmap singleGlyphSpacingVector g
      ]
      where
        -- adjust to length of singleGlyphSpacingVector
        lenSingleVector = 8
    mkSpacingVector (g1:g2:gs) = V.concat
      [ mkSpacingVector (g1:[])
      , (V.singleton $ fromMaybe 0.0 $ spaceBetween <$> g1 <*> g2)
      , mkSpacingVector (g2:gs)
      ]

-- | Calculate the horizontal space in between two glyphs.
spaceBetween :: (Glyph g) => g -> g -> Double
spaceBetween g1 g2 = (xRight g1) - (xLeft g2)


-- | Generate training data (or testing data) for a grenade network
-- using 3 preceding and 3 succeding glyphs.
mkTrainingShapes33
  :: Glyph g =>
     [LeftSpacing Char]
  -> [g]
  -> [(S ('D1 76), S('D1 2))]
mkTrainingShapes33 chars glyphs =
  catMaybes $
  map trainingShape $
  zip chars $
  mkSpacingVectors 3 3 glyphs


-- | Generate a shape of data for training or testing an ANN from a
-- pair of 'Glyph' vector and 'LeftSpacing' character.
--
-- old non-generic signature:
-- trainingShape :: (LeftSpacing Char, V.Vector Double) -> Maybe (S ('D1 76), S ('D1 2))
trainingShape :: GHC.TypeNats.KnownNat n =>
                 (LeftSpacing Char, V.Vector Double)
              -> Maybe (S ('D1 n), S ('D1 2))
trainingShape (SpaceAfter _, v) = (,) <$> (fromStorable v) <*> oneHot 1
trainingShape (SpaceBefore _, v) = (,) <$> (fromStorable v) <*> oneHot 1
trainingShape (NoSpace _, v) = (,) <$> (fromStorable v) <*> oneHot 0
