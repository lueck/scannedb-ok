{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Pdf.Extract.PyPdfMiner
  ( parseXml
  , PdfMinerGlyph(..)
  ) where

-- | Parse the XML representation of glyphs rendered from PDFMiner
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
import qualified Data.Range.Range as R

import qualified Pdf.Extract.Glyph as Gl
import Pdf.Extract.Linearize (Linearizable, linearize, linearizeGlyph)

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


instance Linearizable PdfMinerGlyph where
  linearize g = linearizeGlyph g


data PdfMinerState = PdfMinerState
  { _tag :: Maybe ByteString
  , _toBeExtracted :: Bool
  , _bbox :: Maybe ByteString
  , _font :: Maybe ByteString
  , _code :: Maybe ByteString
  , _text :: Maybe T.Text
  , _glyphs :: [PdfMinerGlyph]
  , _pages :: [[PdfMinerGlyph]]
  } deriving (Eq, Show)

makeLenses ''PdfMinerState

initState = PdfMinerState Nothing False Nothing Nothing Nothing Nothing [] []


mkGlyph :: PdfMinerState -> [PdfMinerGlyph]
mkGlyph s = maybeToList $ PdfMinerGlyph
  <$> Just (s^.text)
  <*> (join $ fmap readBbox $ s^.bbox)
  <*> (s^.font)

-- | Parse xml rendered from pdfminers. We use the fast sax parser
-- `process` from `Xeno.SAX` package.
parseXml :: [R.Range Int] -> ByteString -> IO ([[PdfMinerGlyph]])
parseXml ranges bs = do
  (_, s) <- runStateT
    (process open (attr ranges) doNothing txt close doNothing bs)
    initState
  return $ s^.pages -- [s^.glyphs]

open :: ByteString -> StateT PdfMinerState IO ()
open tg = do
  s <- get
  put $ s & tag .~ (Just tg)
  return ()
{-# INLINE open #-}

close :: ByteString -> StateT PdfMinerState IO ()
close tg = do
  s <- get
  case tg of
    "text" -> do
      if ((s^.toBeExtracted) && (s^.tag == Just "text") && (isJust $ s^.bbox))
        then put $ s
             & glyphs %~ (++ (mkGlyph s))
             & tag .~ Nothing
             & bbox .~ Nothing
             & font .~ Nothing
             & text .~ Nothing
             & code .~ Nothing
        else put $ s
             & tag .~ Nothing
             & bbox .~ Nothing
             & font .~ Nothing
             & text .~ Nothing
             & code .~ Nothing
      return ()
    "page" -> do
      if (s^.toBeExtracted)
        then put $ s
             & pages %~ (++[s^.glyphs])
             & glyphs .~ []
             & toBeExtracted .~ False
        else put $ s & glyphs .~ []
      return ()
    _ -> return ()

attr :: [R.Range Int] -> ByteString -> ByteString -> StateT PdfMinerState IO ()
attr ranges ky val = do
  s <- get
  case ky of
    "id" -> do
      when ((s^.tag == Just "page") && (inPageRange $ readMaybe $ C.unpack val)) $
        put $ s & toBeExtracted .~ True
      return ()
    "bbox" -> do { put $ s & bbox .~ Just val; return ()}
    "font" -> do { put $ s & font .~ Just val; return ()}
    _ -> return ()
  where
    inPageRange Nothing = False
    inPageRange (Just i) = R.inRanges ranges i
{-# INLINE attr #-}

txt :: ByteString -> StateT PdfMinerState IO ()
txt t = do
  s <- get
  put $ s & text .~ (rightToMaybe $ T.decodeUtf8' t)
  return ()
{-# INLINE txt #-}

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
