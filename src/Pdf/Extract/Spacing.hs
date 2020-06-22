{-# LANGUAGE OverloadedStrings
, DataKinds
, BangPatterns
, TupleSections
, TypeFamilies
, FlexibleContexts #-}
module Pdf.Extract.Spacing
where

-- | This module provides functions for inserting inter-word spaces. A
-- simple rule-based insertion of spaces will work for many texts. But
-- in the case of spaced letters for accentuation, like found in older
-- printings, rule-based insertion might fail. So this module provides
-- an artificial neural network, which can be trained with such cases.

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


-- * Rule-based insertion of inter-word spaces

-- | Insert spaces on the basis of the size of a glyph. If the
-- distance to the next glyph exceeds the product of the size and a
-- spacing factor, insert a space. In Antiqua scripts (english print)
-- a space was traditionally (at least) a third of the size of a
-- letter. So 1/3 is a good factor to start with.
sizeSpacingFactor :: Glyph g => Double -> [g] -> T.Text
sizeSpacingFactor _ [] = T.empty
sizeSpacingFactor _ (g:[]) = fromMaybe T.empty $ text g
sizeSpacingFactor spacing (g:gn:gs)
  | dist > spacing * (size g) = T.append (fromMaybe T.empty $ text g) $
                                T.append (T.pack " ") $
                                sizeSpacingFactor spacing (gn:gs)
  | otherwise = T.append (fromMaybe T.empty $ text g) $
                sizeSpacingFactor spacing (gn:gs)
    where
      dist = abs((xLeft gn) - (width g) - (xLeft g))


-- | Insert spaces on the basis of the width of a glyph. If the
-- distance to the next glyph exceeds the product of the width and a
-- spacing factor, insert a space.
widthSpacingFactor :: Glyph g => Double -> [g] -> T.Text
widthSpacingFactor _ [] = T.empty
widthSpacingFactor _ (g:[]) = fromMaybe T.empty $ text g
widthSpacingFactor spacing (g:gn:gs)
  | dist > spacing * (width g) = T.append (fromMaybe T.empty $ text g) $
                                  T.append (T.pack " ") $
                                  widthSpacingFactor spacing (gn:gs)
  | otherwise = T.append (fromMaybe T.empty $ text g) $
                widthSpacingFactor spacing (gn:gs)
    where
      dist = abs((xLeft g) - (xLeft gn))


-- * Representation of spaces

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


-- | Linearize to 'LeftSpacing' to 'T.Text'.
leftSpacingToText :: [LeftSpacing T.Text] -> T.Text
leftSpacingToText [] = ""
leftSpacingToText ((NoSpace a):xs) = a <> (leftSpacingToText xs)
leftSpacingToText ((SpaceAfter a):xs) = a <> " " <> (leftSpacingToText xs)
leftSpacingToText ((SpaceBefore a):xs) = " " <> a <> (leftSpacingToText xs)


-- | Helper function for cleaning plain text for training.
cleanForSpaceTraining :: T.Text -> [T.Text]
cleanForSpaceTraining =
  map (T.filter (/=chr 12)) .    -- remove form feeds
  filter (/="") .                -- remove empty lines
  filter (/=(T.pack [chr 12])) . -- remove lines containing form feed
                                 -- only, e.g. last line
  T.lines                        -- split into lines


-- * Artificial neural network for inserting inter-word spaces


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
  -- FIXME: make fit for french punctuation
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


-- | Bring data vectors into shape for training or testing an
-- ANN. This shapes a pair of 'Glyph' vector and 'LeftSpacing'
-- character.
trainingShape :: KnownNat n =>
                 LeftSpacing Char -- ^ label data, i.e. a character wrapped into 'LeftSpacing'
              -> V.Vector Double  -- ^ a data vector
              -> Maybe (S ('D1 n), SpacingOutput)
trainingShape (NoSpace _) v = (,) <$> (fromStorable v) <*> oneHot 0
trainingShape (SpaceAfter _) v = (,) <$> (fromStorable v) <*> oneHot 1
trainingShape (SpaceBefore _) v = (,) <$> (fromStorable v) <*> oneHot 1 -- FIXME: if ever used
trainingShape (SpaceAround _) v = (,) <$> (fromStorable v) <*> oneHot 1 -- FIXME: if ever used


-- | Verify training data (or testing data) and generate shaped data
-- for a grenade network using n preceding and m succeeding glyphs,
-- where n and m are given as arguments. This takes a line of text
-- from the set of training data and the corresponding line of glyphs
-- as arguments. The verification tests if both lines contain the same
-- sequence of characters. It is also verified, that a shape was
-- generated for each input glyph.
--
-- If the verification fails, a 'Left' value is returned with an error
-- string.
--
-- Note that this is a generic function. You should use a closure with
-- a type signature like follows to make your shapes of data for a
-- defined number of preceding and succeeding glyphs:
--
-- @myTrainingShapes :: (Glyph g) = T.Text -> [g] -> Either String [(S ('D1 65), SpacingOutput)]@
-- @myTrainingShapes = mkTrainingShapes 0 5@
mkTrainingShapes
  :: (KnownNat n, Glyph g) =>
     Int                    -- ^ number of predecessors in window
  -> Int                    -- ^ number of successors in window
  -> T.Text                 -- ^ a line of text from the training data
  -> [g]                    -- ^ a line of glyphs
  -> Either String [(S ('D1 n), SpacingOutput)]
mkTrainingShapes pre succ txtLine glyphs' =
  join $
  fmap ((ifEitherP
         ((== (length unspacedLine)) . length)
         (const "Error while generating training data: input vector does not fit into shape")
         id) .
        catMaybes .
        (map (uncurry trainingShape)) .
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


-- | Generate shaped training data with a window showing 2
-- predecessors and 2 successor.
mkTrainingShapes22
  :: (Glyph g) =>
     T.Text                 -- ^ a line of text from the training data
  -> [g]                    -- ^ a line of glyphs
  -> Either String [SpacingRow]
mkTrainingShapes22 = mkTrainingShapes 2 2


-- | Generate shaped data to be run through the network. This is a
-- generic function for generating shaped data with n preceeding and m
-- succeeding glyphs.
mkRunningShapes
  :: (KnownNat n, Glyph g) =>
     Int                    -- ^ number of predecessors in window
  -> Int                    -- ^ number of successors in window
  -> [g]                    -- ^ a line of glyphs
  -> Either String [S ('D1 n)]
mkRunningShapes pre succ glyphs =
  ifEitherP
  ((== (length glyphs)) . length)
  (const "Error while generating data: input vector does not fit into shape")
  id $
  catMaybes $
  map fromStorable $
  mkSpacingVectors pre succ glyphs
  where
    ifEitherP p l r val
      | p val = Right $ r val
      | otherwise = Left $ l val


-- | Generate shaped data for a network with 2 predecessors and 2
-- successors.
mkRunningShapes22 :: Glyph g => [g] -> Either String [SpacingShape]
mkRunningShapes22 = mkRunningShapes 2 2


-- | The shape of data input into the ANN.
--
-- Note that this must be determined from the length of the vector
-- returned by 'singleGlyphSpacingVector', 'mkSpacingVector' and the
-- number of preceding and succeeding glyphs in 'mkTrainingShapes'.
type SpacingShape = S ('D1 54)

-- | The shape of data output from the ANN.
type SpacingOutput = S ('D1 2)

-- | The shape of training data for the ANN.
type SpacingRow = (SpacingShape, SpacingOutput)

-- | The ANN for inserting inter-word spaces.
type SpacingNet
  = Network
    '[ FullyConnected 54 216, Logit,
       FullyConnected 216 216, Logit,
       FullyConnected 216 216, Logit,
       -- FullyConnected 216 216, Logit,
       FullyConnected 216 2, Logit]
    '[ 'D1 54, 'D1 216, 'D1 216,
       'D1 216, 'D1 216,
       'D1 216, 'D1 216,
       -- 'D1 216, 'D1 216,
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
      tRes = fmap (\(rowP,rowL) -> (rowL,) $ runNet trained' rowP) trainRows
      tRes' = fmap (\(S1D label, S1D prediction) -> ( maxIndex (SA.extract label)
                                                    , maxIndex (SA.extract prediction))) tRes
      vRes = fmap (\(rowP,rowL) -> (rowL,) $ runNet trained' rowP) validateRows
      vRes' = fmap (\(S1D label, S1D prediction) -> ( maxIndex (SA.extract label)
                                                    , maxIndex (SA.extract prediction))) vRes
  hPutStrLn log $ "Iteration " ++ show i ++ ": " ++
    "\nTraining data: " ++
    show (length (filter ((==) <$> fst <*> snd) tRes')) ++ " of " ++ show (length tRes') ++
    "\n " ++ spacingPrecision tRes' ++
    "\nValidation data: " ++
    show (length (filter ((==) <$> fst <*> snd) vRes')) ++ " of " ++ show (length vRes') ++
    "\n " ++ spacingPrecision vRes' ++
    "\n"
  return trained'
  where
    trainEach rate' !network (i, o) = train rate' network i o


-- | Calculate precision measures of a labeling result.
spacingPrecision :: (Ord b, Num b) => [(b, b)] -> String
spacingPrecision res =
  "false positives: " ++ show falsePos ++
  ", false negatives: " ++ show falseNegs ++
  ", precision: " ++ show precision ++
  ", recall: " ++ show recall
  where
    truePos = length $ filter ((&&) <$> (==1) . fst <*> (==1) . snd) res
    falsePos = length $ filter ((<) <$> fst <*> snd) res
    trueNegs = length $ filter ((&&) <$> (==0) . fst <*> (==0) . snd) res
    falseNegs = length $ filter ((>) <$> fst <*> snd) res
    precision = (fromIntegral truePos) / (fromIntegral $ truePos + falsePos)
    recall = (fromIntegral truePos) / (fromIntegral $ truePos + falseNegs)


-- | Run the trained network a line of 'Glyph's. The glyphs should be
-- sorted like in 'linearizeLine'.
runSpacingNetOnLine
  :: (Glyph g) =>
     SpacingNet
  -> [g]
  -> Either String T.Text
runSpacingNetOnLine network glyphs =
  fmap (leftSpacingToText .
        (map (uncurry (runSpacingNet network))) .
        (zip glyphsTxt)) $
  mkRunningShapes22 glyphs
  where
    glyphsTxt = map ((fromMaybe missingGlyph) . text) glyphs
    missingGlyph = "_"


-- | Run the trained network on input data.
runSpacingNet :: SpacingNet -> a -> SpacingShape -> LeftSpacing a
runSpacingNet net char input =
  spaceFromLabel char $ runNet net input


-- | Evaluate the output from the network.
spaceFromLabel :: a -> SpacingOutput -> LeftSpacing a
spaceFromLabel char (S1D label) = mkSpace char $ maxIndex $ SA.extract label
  where
    mkSpace char 0 = NoSpace char
    mkSpace char 1 = SpaceAfter char
