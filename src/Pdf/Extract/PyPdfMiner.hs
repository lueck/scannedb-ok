{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Pdf.Extract.PyPdfMiner
  ( parseXml
  , PdfMinerGlyph(..)
  ) where

-- | Parse the XML representation of glyphs rendered from pdfminer
-- with @pdf2txt.py -t xml ...@.


import Xeno.SAX
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad.State
import Control.Lens
import Data.Maybe
import Control.Applicative
import qualified Data.Text as T 
import qualified Data.Text.Encoding as T
import Text.Read (readMaybe)

import qualified Pdf.Extract.Glyph as Gl


data PdfMinerGlyph = PdfMinerGlyph
  { _pmg_text :: Maybe T.Text
  , _pmg_bbox :: Gl.BBox
  , _pmg_font :: ByteString
  } deriving (Eq, Show)


instance Gl.Glyph PdfMinerGlyph where
  text = _pmg_text
  bbox = _pmg_bbox
  code = const 0
  font = Just . C.unpack . _pmg_font
  xLeft = Gl._bbox_xBottomLeft . _pmg_bbox
  yBottom = Gl._bbox_yBottomLeft . _pmg_bbox


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
mkGlyph s = maybeToList $ PdfMinerGlyph
  <$> Just (s^.text)
  <*> (join $ fmap readBbox $ s^.bbox)
  <*> (s^.font)

-- | Parse xml rendered from pdfminers. We use the fast sax parser
-- `process` from `Xeno.SAX` package.
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

readBbox :: ByteString -> Maybe Gl.BBox
readBbox b = Gl.BBox <$> readCoord 0 <*> readCoord 1 <*> readCoord 2 <*> readCoord 3
  where
    bs = C.split ',' b
    -- FIXME: Howto do conversion to Double directly from ByteString?
    readCoord n = join $ fmap (readMaybe . C.unpack) (bs ^? element n)

doNothing :: Monad m => ByteString -> m ()
doNothing _ = return ()
{-# INLINE doNothing #-}

doNothing2 :: Monad m => ByteString -> ByteString -> m ()
doNothing2 _ _ = return ()
{-# INLINE doNothing2 #-}

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just
{-# INLINE rightToMaybe #-}
