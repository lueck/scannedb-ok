{-# LANGUAGE OverloadedStrings #-}
module Pdf.Extract.Syllable where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Lazy as M
import System.IO
import Data.Char


-- * Detecting and repairing syllable divisions

-- | Detect and repair syllable division on a list of lines.
repair :: (T.Text -> Bool)      -- ^ test if unigramm is in vocabulary
       -> [T.Text]              -- ^ the list of lines
       -> [T.Text]              -- ^ accumulator, init with []
       -> IO [T.Text]           -- ^ returned list of lines
repair kown acc [] = return $ reverse acc
repair kown acc (l:[]) = do
  repair kown (l:acc) []
repair kown acc (l:nl:ls)
  | endsWithPunctuation $ lastWord l = do
      repair kown (l:acc) (nl:ls)
  | (kown $ lastWord l) && (kown $ firstWord nl) = do
      repair kown (l:acc) (nl:ls)
  | ((not $ kown $ lastWord l) || (not $ kown $ firstWord nl)) &&
    (kown (lastWord l <> firstWord nl)) = do
      logging $ "Joining \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\"."
      repair kown ((appendToLine l (firstWord nl)):acc) ((restOf nl):ls)
  | (not $ kown $ lastWord l) || (not $ kown $ firstWord nl) &&
    (not $ kown (lastWord l <> firstWord nl)) = do
      logging $ "Unable to repair: \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\"."
      repair kown (l:acc) (nl:ls)


logging :: T.Text -> IO ()
logging = T.hPutStrLn stderr


appendToLine :: T.Text -> T.Text -> T.Text
appendToLine "" w = w <> "\n"
appendToLine l w
  | T.last l == '\n' = (T.dropEnd 1 l) <> w <> "\n"
  | T.takeEnd 2 l == "\n\f" =
    (T.dropEnd 2 l) <> w <> "\n\f"
  | otherwise = l <> w -- What's happing?


endsWithPunctuation :: T.Text -> Bool
endsWithPunctuation w = isPunctuation $ T.last $ "." <> w -- assert w is non empty


firstWord :: T.Text -> T.Text
firstWord = fst . T.breakOn " " . T.stripStart


lastWord :: T.Text -> T.Text
lastWord = snd . T.breakOnEnd " " . T.stripEnd


restOf :: T.Text -> T.Text
restOf = T.stripStart . snd . T.breakOn " " . T.stripStart


-- * Tokenization of the middle of the lines


tokenizeMiddle :: T.Text -> [T.Text]
tokenizeMiddle = map (T.dropAround ((`elem` stripCats) . generalCategory)) .
                 T.words .
                 (snd . T.breakOn " ") .
                 (fst . T.breakOnEnd " ")


-- | You don't want to see them hanging around with tokens!
stripCats :: [GeneralCategory]
stripCats =
  [ Control
  , Space
  , ConnectorPunctuation
  , DashPunctuation
  , OtherPunctuation
  , OpenPunctuation
  , ClosePunctuation
  , InitialQuote
  , FinalQuote]


-- * Load a pool of tokens

loadTokens :: FilePath -> IO (M.HashMap T.Text Int)
loadTokens fname = do
  toks <- T.readFile fname
  return $ M.fromListWith (+) $ zip (T.words toks) (repeat 1)
