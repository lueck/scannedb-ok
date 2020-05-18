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
-- over the line. The number of preceding and succeeding glyphs,
-- i.e. the size of the window, is set by the first two arguments.
mkSpacingVectors
  :: Glyph g =>
     Int                             -- ^ number of preceding glyphs in window
  -> Int                             -- ^ number of succeeding glyphs in window
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
    -- adjust to 'SpacingShape'
    pre = 3
    succ = 3


-- | The shape of data input into the ANN.
--
-- Note that this must be determined from the length of the vector
-- returned by 'singleGlyphSpacingVector', 'mkSpacingVector' and the
-- number of preceding and succeeding glyphs in 'mkTrainingShapes'.
type SpacingShape = S ('D1 76)

type SpacingOutput = S ('D1 2)

type SpacingRow = (SpacingShape, SpacingOutput) 

type SpacingNet
  = Network
    '[ FullyConnected 76 152, Logit,
       FullyConnected 152 152, Logit,
       FullyConnected 152 2, Logit]
    '[ 'D1 76, 'D1 152, 'D1 152,
       'D1 152, 'D1 152,
       'D1 2, 'D1 2]


randomSpacingNet :: MonadRandom m => m SpacingNet
randomSpacingNet = randomNetwork


spaceLearningParams :: LearningParameters
spaceLearningParams = LearningParameters 0.01 0.9 0.0005


-- trainSpacingRow :: LearningParameters -> SpacingNet -> SpacingRow -> SpacingNet
-- trainSpacingRow lp net (input, output) = train lp net input output

-- testSpacingRow :: SpacingNet -> SpacingRow -> (S ('D1 2), S ('D1 2))
-- testSpacingRow net (rowInput, wishedOutput) = (wishedOutput, runNet net rowInput)

-- getSpacingLabels :: (SpacingOutput, SpacingOutput) -> (Int, Int)
-- getSpacingLabels (S1D wishedLabel, S1D actualOutput) =
--   (maxIndex (extract wishedLabel), maxIndex (extract actualOutput))


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
  hPutStrLn log $ "Iteration " ++ show i ++ ": " ++ show (length (filter ((==) <$> fst <*> snd) res')) ++ " of " ++ show (length res')
  --print trained'
  return trained'
  where
    trainEach rate' !network (i, o) = train rate' network i o


runSpacingNet :: SpacingNet -> Char -> SpacingShape -> LeftSpacing Char
runSpacingNet net char input =
  spaceFromLabel char $ runNet net input


spaceFromLabel :: a -> SpacingOutput -> LeftSpacing a
spaceFromLabel char (S1D label) = mkSpace char $ maxIndex $ SA.extract label
  where
    mkSpace char 0 = NoSpace char
    mkSpace char 1 = SpaceAfter char


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
    glyphsTxt = T.concat $ mapMaybe text glyphs
