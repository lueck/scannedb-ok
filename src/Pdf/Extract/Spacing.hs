{-# LANGUAGE OverloadedStrings
, TemplateHaskell
, DeriveGeneric
, DataKinds
, BangPatterns
, TupleSections
, TypeFamilies
, FlexibleContexts #-}
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
import Control.Monad.Random
import Numeric.LinearAlgebra ( maxIndex )
import qualified Numeric.LinearAlgebra.Static as SA
import System.IO

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


-- * Artificial neural network for inserting inter-word spaces

-- | For training the inter-word-spacing ANN, the characters of a line
-- must be represented without the spaces. Therefore each character is
-- wrapped in a 'LeftSpacing' record. This wrapping is done by the
-- 'representSpacesAfter' function. The resulting list can easily be
-- parallelized with the list of glyphs for the line.
--
-- The record is generic an can be reused to represent spaces between
-- 'Glyph's, too.

-- | A wrapper for representing spaces. Actually only 'SpaceAfter' and
-- 'NoSpace' is used, the others are for making it conceptionally
-- complete.
data LeftSpacing a
  = SpaceBefore a               -- ^ Represents a character or 'Glyph'
                                -- with a space before it.
  | SpaceAfter a                -- ^ Represents a character or 'Glyph'
                                -- with a space after it.
  | SpaceAround a               -- ^ Represents a character or 'Glyph'
                                -- with spaces around it.
  | NoSpace a                   -- ^ Represents a character or 'Glyph'
                                -- with neither space before nor after
                                -- it.
  deriving (Show, Eq)

-- | Unwrap a 'LeftSpacing' item.
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


-- | Helper function for cleaning plain text for training.
cleanForSpaceTraining :: T.Text -> [T.Text]
cleanForSpaceTraining =
  map (T.filter (/=chr 12)) .    -- remove form feeds
  filter (/="") .                -- remove empty lines
  filter (/=(T.pack [chr 12])) . -- remove lines containing form feed
                                 -- only, e.g. last line
  T.lines                        -- split into lines


-- | Generate a data vector from a 'Glyph'.
singleGlyphSpacingVector :: Glyph g => g -> V.Vector Double
singleGlyphSpacingVector g =
  V.fromList $
  [ (width g) / (size g)
  , (size g) / (width g)
  , fromIntegral $ fromEnum $ (size g) < (width g)
  , fromIntegral $ fromEnum $ (size g) == (width g)
  , fromIntegral $ fromEnum $ (size g) > (width g)
  -- , charFeature ord g -- ordinal, causes noise in data and bad results
  , charFeature (fromEnum . isUpper) g
  , charFeature (fromEnum . isLower) g
  , charFeature (fromEnum . (`elem` (",;:.!?"::[Char]))) g
  ]
  where
    charFeature f = fromIntegral . (fromMaybe 0 . (fmap (f . T.head))) . text

-- | Generate data vectors from a line of 'Glyph's by moving a window
-- over the line. The number of preceding and succeeding glyphs,
-- i.e. the size of the window, is set by the first two
-- arguments.
--
-- Missing predecessors (in the beginning of a line) or missing
-- successors (at the end of a line) are represented as missing by
-- one-hot encoded presence/missing vector entries. See
-- 'mkSpacingVector' for details.
mkSpacingVectors
  :: Glyph g =>
     Int                             -- ^ number of preceding glyphs in window
  -> Int                             -- ^ number of succeeding glyphs in window
  -> [g]                             -- ^ the line's glyphs
  -> [V.Vector Double]               -- ^ a vector of float values for each glyph
mkSpacingVectors pre succ gs =
  take (length gs) $ drop pre $
  fst $
  foldr moveWindow ([], (take pre $ repeat Nothing)) $
  ((take pre $ repeat Nothing) ++ (map Just gs) ++ (take succ $ repeat Nothing))
  where
    moveWindow :: Glyph g =>
                  Maybe g
               -> ([V.Vector Double], [Maybe g])
               -> ([V.Vector Double], [Maybe g])
    moveWindow curr (vecs, oldWindow) = ((mkSpacingVector newWindow):vecs, newWindow)
      where
        newWindow = take (pre + succ + 1) (curr:oldWindow)

-- | Make a data vector from a window on some glyphs.
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
    -- SHAPE: adjust to length of singleGlyphSpacingVector
    lenSingleVector = 8
mkSpacingVector (g1:g2:gs) = V.concat
  [ mkSpacingVector (g1:[])
  , (V.singleton $ fromMaybe 0.0 $ (/) <$> dist <*> fmap width g1)
  , mkSpacingVector (g2:gs)
  ]
  where
    dist = spaceBetween <$> g1 <*> g2

-- | Calculate the horizontal space in between two glyphs.
spaceBetween :: (Glyph g) => g -> g -> Double
spaceBetween g1 g2 = abs $ (xLeft g2) - (xRight g1) -- FIXME?


-- | Generate a shape of data for training or testing an ANN from a
-- pair of 'Glyph' vector and 'LeftSpacing' character.
trainingShape :: (LeftSpacing Char, V.Vector Double)
              -> Maybe SpacingRow
trainingShape (SpaceAfter _, v) = (,) <$> (fromStorable v) <*> oneHot 1
trainingShape (SpaceBefore _, v) = (,) <$> (fromStorable v) <*> oneHot 1
trainingShape (NoSpace _, v) = (,) <$> (fromStorable v) <*> oneHot 0


-- | Verify training data (or testing data) and generate shaped data
-- for a grenade network using 3 preceding and 3 succeeding
-- glyphs. This takes a line of text from the set of training data and
-- the corresponding line of glyphs as arguments. The verification
-- tests if both lines contain the same sequence of characters. It is
-- also verified, that a shape was generated for each input glyph.
--
-- If the verification fails, a 'Left' value is returned with an error
-- string.
mkTrainingShapes
  :: (Glyph g) =>
     T.Text                     -- ^ a line of text from the training data
  -> [g]                        -- ^ a line of glyphs
  -> Either String [SpacingRow]
mkTrainingShapes txtLine glyphs' =
  join $
  fmap ((ifEitherP
         ((== (length unspacedLine)) . length)
         (const "Error while generating training data: input vector does not fit into shape")
         id) .
        catMaybes .
        (map trainingShape) .
        (zip unspacedLine) .
        (mkSpacingVectors pre succ)) $
  ifEitherP
  ((==(map withoutSpace unspacedLine)) . T.unpack . glyphsTxt)
  (T.unpack .
   T.concat .
   ([ "Error: Line of training data does not match PDF line\n"
       , "Training data:\n"
       , txtLine
       , "\nPDF data:\n"] ++) .
   (:[]) .
   glyphsTxt)
  id
  glyphs
  where
    ifEitherP p l r val
      | p val = Right $ r val
      | otherwise = Left $ l val
    unspacedLine = representSpacesAfter $ T.unpack txtLine
    glyphs = sortOn xLeft glyphs'
    glyphsTxt = T.concat . mapMaybe text -- glyphs
    -- SHAPE: adjust to 'SpacingShape'
    pre = 0
    succ = 1


-- | The shape of data input into the ANN.
--
-- Note that this must be determined from the length of the vector
-- returned by 'singleGlyphSpacingVector', 'mkSpacingVector' and the
-- number of preceding and succeeding glyphs in 'mkTrainingShapes'.
type SpacingShape = S ('D1 21)

-- | The shape of data output from the ANN.
type SpacingOutput = S ('D1 2)

-- | The shape of training data for the ANN.
type SpacingRow = (SpacingShape, SpacingOutput)

-- | The ANN for inserting inter-word spaces.
type SpacingNet
  = Network
    '[ FullyConnected 21 92, Logit,
       FullyConnected 92 92, Logit,
       FullyConnected 92 92, Logit,
       -- FullyConnected 92 92, Logit,
       FullyConnected 92 2, Logit]
    '[ 'D1 21, 'D1 92, 'D1 92,
       'D1 92, 'D1 92,
       'D1 92, 'D1 92,
       -- 'D1 92, 'D1 92,
       'D1 2, 'D1 2]


-- | A net with random weights.
randomSpacingNet :: MonadRandom m => m SpacingNet
randomSpacingNet = randomNetwork


-- | Run a single iteration learning the network.
runSpacingIteration
  :: Handle                     -- ^ a file handle for logging
  -> [SpacingRow]               -- ^ training data
  -> [SpacingRow]               -- ^ testing data
  -> LearningParameters         -- ^ learning rate
  -> SpacingNet                 -- ^ the current network
  -> Int                        -- ^ iteration number
  -> IO SpacingNet              -- ^ returns a new network
runSpacingIteration log trainRows validateRows rate net i = do
  let trained' = foldl' (trainEach ( rate { learningRate = learningRate rate * 0.9 ^ i} )) net trainRows
  let res = fmap (\(rowP,rowL) -> (rowL,) $ runNet trained' rowP) validateRows
  let res' = fmap (\(S1D label, S1D prediction) -> (maxIndex (SA.extract label), maxIndex (SA.extract prediction))) res
      truePos = length $ filter ((&&) <$> (==1) . fst <*> (==1) . snd) res'
      falsePos = length $ filter ((<) <$> fst <*> snd) res'
      trueNegs = length $ filter ((&&) <$> (==0) . fst <*> (==0) . snd) res'
      falseNegs = length $ filter ((>) <$> fst <*> snd) res'
      precision = (fromIntegral truePos) / (fromIntegral $ truePos + falsePos)
      recall = (fromIntegral truePos) / (fromIntegral $ truePos + falseNegs)
  hPutStrLn log $ "Iteration " ++
    show i ++ ": " ++
    show (length (filter ((==) <$> fst <*> snd) res')) ++ " of " ++ show (length res') ++
    ", false negatives: " ++ show falseNegs ++
    ", false positives: " ++ show falsePos ++
    ", precision: " ++ show precision ++
    ", recall: " ++ show recall
  --print trained'
  return trained'
  where
    trainEach rate' !network (i, o) = train rate' network i o



-- | Run the trained network a line of 'Glyph's.
runSpacingNetOnLine :: Glyph g => SpacingNet -> [g] -> Either String T.Text
runSpacingNetOnLine network glyphs' =
  fmap (T.pack .
        leftSpacingToString .
        (map (uncurry (runSpacingNet network))) .
        (zip (T.unpack glyphsTxt)) .
        (map fst)) $
  mkTrainingShapes glyphsTxt glyphs
  where
    glyphs = sortOn xLeft glyphs'
     -- A Text is needed for reusing mkTrainingShapes, but thrown
     -- away:
    glyphsTxt = T.concat $ mapMaybe text glyphs


-- | Run the trained network on input data.
runSpacingNet :: SpacingNet -> Char -> SpacingShape -> LeftSpacing Char
runSpacingNet net char input =
  spaceFromLabel char $ runNet net input


-- | Evaluate the output from the network.
spaceFromLabel :: a -> SpacingOutput -> LeftSpacing a
spaceFromLabel char (S1D label) = mkSpace char $ maxIndex $ SA.extract label
  where
    mkSpace char 0 = NoSpace char
    mkSpace char 1 = SpaceAfter char


