{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Pdf.Extract.PyPdfMiner
  ( parseXml
  , PdfMinerGlyph(..)
  ) where

import Xeno.SAX
import System.IO
import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T 
import qualified Data.Text.Encoding as T

data PdfMinerGlyph = PdfMinerGlyph
  { _pmg_text :: Maybe T.Text
  , _pmg_bbox :: ByteString
  , _pmg_font :: ByteString
  } deriving (Eq, Show)

data PdfMinerState = PdfMinerState
  { _tag :: Maybe ByteString
  , _bbox :: Maybe ByteString
  , _font :: Maybe ByteString
  , _code :: Maybe ByteString
  , _text :: Maybe T.Text
  , _glyphs :: [PdfMinerGlyph]
  } deriving (Eq, Show)

makeLenses ''PdfMinerState

initState = PdfMinerState Nothing Nothing Nothing Nothing Nothing []


mkGlyph :: PdfMinerState -> [PdfMinerGlyph]
mkGlyph s = maybeToList $ PdfMinerGlyph <$> Just (s^.text) <*> (s^.bbox) <*> (s^.font)

-- | Parse xml rendered from pdfminers @pdf2txt.py -t xml@. We use the
-- fast sax parser `process` from `Xeno.SAX` package.
parseXml :: ByteString -> IO ([PdfMinerGlyph])
parseXml bs = do
  (_, s) <- runStateT
    (process open attr doNothing txt close doNothing bs)
    initState
  return $ s^.glyphs

open :: ByteString -> StateT PdfMinerState IO ()
open tg = do
  s <- get
  put $ s & tag .~ (Just tg)
  return ()

close :: ByteString -> StateT PdfMinerState IO ()
close tg = do
  s <- get
  when ((tg == "text") && (s^.tag == Just tg) && (isJust $ s^.bbox)) $
    put $ s
    & glyphs %~ (++ (mkGlyph s))
    & tag .~ Nothing
    & bbox .~ Nothing
    & font .~ Nothing
    & text .~ Nothing
    & code .~ Nothing
  return ()

attr :: ByteString -> ByteString -> StateT PdfMinerState IO ()
attr ky val = do
  s <- get
  case ky of
    "bbox" -> do { put $ s & bbox .~ Just val; return ()}
    "font" -> do { put $ s & font .~ Just val; return ()}
    _ -> return ()

txt :: ByteString -> StateT PdfMinerState IO ()
txt t = do
  s <- get
  put $ s & text .~ (rightToMaybe $ T.decodeUtf8' t)
  return ()
  
doNothing _ = pure ()
doNothing2 _ _ = pure ()

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
{-# INLINE rightToMaybe #-}
