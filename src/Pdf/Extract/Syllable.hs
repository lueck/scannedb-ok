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
       -> Bool                  -- ^ division required
       -> [T.Text]              -- ^ the list of lines
       -> [T.Text]              -- ^ accumulator, init with []
       -> IO [T.Text]           -- ^ returned list of lines
repair kown mark acc [] = return $ reverse acc
repair kown mark acc (l:[]) = do
  repair kown mark (l:acc) []
repair kown True acc (l:nl:ls)
  -- Devision mark required:
  -- no mark
  | (not $ endsWithDash $ lastWord l) = do
      repair kown True (l:acc) (nl:ls)
  -- mark present and joint token without dash known
  | (endsWithDash $ lastWord l) &&
    (kown ((stripEndDash $ lastWord l) <> firstWord nl)) = do
      logging $ "Joining \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\". A"
      repair kown True ((appendToLine stripEndDash l (firstWord nl)):acc) ((restOf nl):ls)
  -- mark present and joint token with dash known
  | (endsWithDash $ lastWord l) &&
    (kown (lastWord l <> firstWord nl)) = do
      logging $ "Joining \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\". B"
      repair kown True ((appendToLine id l (firstWord nl)):acc) ((restOf nl):ls)
  -- mark present and joint token unknown
  | otherwise = do
      logging $ "Unable to repair: \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\". C"
      repair kown True (l:acc) (nl:ls)
repair kown False acc (l:nl:ls)
  -- Devision mark not required:
  -- line ends with punctuation: do not repair
  | endsWithPunctuation $ lastWord l = do
      repair kown False (l:acc) (nl:ls)
  -- both tokens known: do not repair. FIXME: stripEndDash on (lastWord l)?
  | (kown $ lastWord l) && (kown $ firstWord nl) =
      repair kown False (l:acc) (nl:ls)
  -- ends with dash or one tokens unkown and joint token without dash known
  | ((endsWithDash $ lastWord l) ||
     (not $ kown $ stripEndDash $ lastWord l) || (not $ kown $ firstWord nl)) &&
    (kown ((stripEndDash $ lastWord l) <> firstWord nl)) = do
      logging $ "Joining \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\"."
      repair kown False ((appendToLine stripEndDash l (firstWord nl)):acc) ((restOf nl):ls)
  -- ends with dash or one tokens unkown and joint token with dash known
  | ((endsWithDash $ lastWord l) ||
     (not $ kown $ stripEndDash $ lastWord l) || (not $ kown $ firstWord nl)) &&
    (kown ((lastWord l) <> firstWord nl)) = do
      logging $ "Joining \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\"."
      repair kown False ((appendToLine id l (firstWord nl)):acc) ((restOf nl):ls)
  -- joint token unkown
  | otherwise = do
      logging $ "Unable to repair: \"" <> lastWord l <> "\" and \"" <> firstWord nl <> "\"."
      repair kown False (l:acc) (nl:ls)


logging :: T.Text -> IO ()
logging = T.hPutStrLn stderr


appendToLine :: (T.Text -> T.Text) -> T.Text -> T.Text -> T.Text
appendToLine f "" w = (f w) <> "\n"
appendToLine f l w = (f $ T.dropWhileEnd isSpace l) <> w <> (T.takeWhileEnd isSpace l)


endsWithPunctuation :: T.Text -> Bool
endsWithPunctuation w = (\c -> isPunctuation c &&
                               (not $ isDivisionMark c)) $
                        T.last $ "." <> w -- assert w is non empty

endsWithDash :: T.Text -> Bool
endsWithDash w = isDivisionMark $ T.last $ "." <> w -- assert w is non empty

stripEndDash :: T.Text -> T.Text
stripEndDash = T.dropWhileEnd isDivisionMark

isDivisionMark :: Char -> Bool
isDivisionMark c = generalCategory c == DashPunctuation || c == '='

firstWord :: T.Text -> T.Text
--firstWord = fst . T.breakOn " " . T.stripStart
firstWord = (T.takeWhile (not . isSpace)) . T.stripStart


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
