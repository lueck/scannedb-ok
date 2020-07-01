{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
module Pdf.Extract.Precision where

import GHC.Generics (Generic)
import qualified Data.Csv as Csv
import Control.Lens


-- | See https://en.wikipedia.org/wiki/Precision_and_recall
data Precision = Precision
  { _prec_total :: Int
  , _prec_relevant :: Int
  , _prec_truePositives :: Int
  , _prec_falseNegatives :: Int
  , _prec_trueNegatives :: Int
  , _prec_falsePositives :: Int
  , _prec_precision :: Double
  , _prec_recall :: Double
  -- meta data
  , _prec_inputData :: Maybe String
  , _prec_goldStandard :: Maybe String
  , _prec_method :: Maybe String
  } deriving (Show, Generic)

makeLenses ''Precision

instance Csv.ToRecord Precision


-- | Calculate precision and recall of a labelled result. The first
-- element in the tuples should be the label of the gold standard the
-- second the result of the labeling process.
precisionLabel :: Eq b => b -> [(b, b)] -> Precision
precisionLabel label res = Precision
  { _prec_total = length res
  , _prec_relevant = length $ filter (==label) $ map fst res 
  , _prec_truePositives = truePos
  , _prec_falseNegatives = falseNeg
  , _prec_trueNegatives = trueNeg
  , _prec_falsePositives = falsePos
  , _prec_precision = (fromIntegral truePos) / (fromIntegral $ truePos + falsePos)
  , _prec_recall = (fromIntegral truePos) / (fromIntegral $ truePos + falseNeg)
  , _prec_inputData = Nothing
  , _prec_goldStandard = Nothing
  , _prec_method = Nothing
  }
  where
    truePos = length $ filter ((&&) <$> (==label) . fst <*> (==label) . snd) res
    falsePos = length $ filter ((&&) <$> (/=label) . fst <*> (==label) . snd) res
    trueNeg = length $ filter ((&&) <$> (/=label) . fst <*> (/=label) . snd) res
    falseNeg = length $ filter ((&&) <$> (==label) . fst <*> (/=label) . snd) res
