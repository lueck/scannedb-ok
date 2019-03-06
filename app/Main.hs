{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Data.Char

import System.Environment
import qualified Pdf.Document as P
import qualified Pdf.Content.Processor as P
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Range.Parser as R
import qualified Data.Range.Range as R
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy.Char8 as C


import Pdf.Extract.Lines
import Pdf.Extract.Linearize
import Pdf.Extract.PdfToolBox
import Pdf.Extract.PyPdfMiner
import Pdf.Extract.Glyph
import Pdf.Extract.Spacing

data PdfToText = PdfToText
  { inputMethod :: Maybe InputMethod
  , outputMethod :: Maybe OutputMethod
  , pages :: String -- Maybe (Range Int)
  , linesPerPage :: Int
  , fixedSpacingFactor :: Double
  , inputFile :: String
  }

data OutputMethod = Text | NoSpaces | Info | Glyphs | Spacing'

data InputMethod = PdfInput | PdfMinerXml

-- | A parser for the command line arguments.
pdfToText_ :: Parser PdfToText
pdfToText_ = PdfToText
  <$> optional ((flag' PdfInput
                 (short 'p'
                  <> long "pdf"
                  <> help "PDF input data. (Default)"))
                 <|>
                 (flag' PdfMinerXml
                  (short 'x'
                   <> long "xml"
                   <> help "XML input data. An XML representation of the glyphs of a PDF file, like produced with PDFMiner's \"pdf2txt.py -t xml ...\" command.")))
  <*> optional ((flag' Text
                 (short 't'
                  <> long "text"
                  <> help "Extract text. (Default)"))
                 <|>
                 (flag' NoSpaces
                  (short 'S'
                   <> long "no-spaces"
                   <> help "Extract text without spaces. (Yes, PDF really does not even know the concept of spaces..."))
                 <|>
                 (flag' Info
                  (short 'i'
                   <> long "statistics"
                   <> help "Show statistics about the text."))
                 <|>
                 (flag' Spacing'
                  (short 's'
                   <> long "spacings"
                   <> help "Extract inter-glyph distances for statistical analysis. This generates CSV output."))
                 <|>
                 (flag' Glyphs
                  (short 'g'
                   <> long "glyphs"
                   <> help "Show the information about the glyphs found in the document.")))
  <*> strOption (short 'r'
                 <> long "pages"
                 <> help "Ranges of pages to extract. Defaults to all. Examples: 3-9 or -10 or 2,4,6,20-30,40- or \"*\" for all. Except for all do not put into quotes."
                 <> value "*"
                 <> metavar "PAGES")
  <*> option auto (short 'l'
                   <> long "lines-per-page"
                   <> help "Lines per page. Lines of a vertically filled page. Defaults to 42. Needed be exact."
                   <> value 42
                   <> metavar "LINES")
  <*> option auto (short 'f'
                   <> long "spacing-factor"
                   <> help "A fixed spacing factor. If the distance between two glyphs exceeds the product of the first glyphs width and this factor, a space is inserted. Defaults to 1.7, but for Gothic letter values down to 1 are promising, too."
                   <> value 1.7
                   <> metavar "SPACING")
  <*> argument str (metavar "INPUTFILE"
                    <> help "Path to the input file.")


main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> pdfToText_)
          ( fullDesc
            <> progDesc "googleb-ok extracts the text from a PDF and was written to work for scanned books from books.google.com. There are options for stripping of page headers and footers in order to make the pure text ready for text mining and NLP."
            <> header "googleb-ok - extract text from a PDF, even scanned books." )


-- | Run the extractor with the parsed command line arguments.
run :: PdfToText -> IO ()
run (PdfToText (Just PdfMinerXml) outputMethod pages lines' spacing' inputFile) = do
  case (R.parseRanges pages)::(Either R.ParseError [R.Range Int]) of
    Left err -> do
      print err
      return ()
    Right ranges -> do
      glyphs <- B.readFile inputFile >>= parseXml ranges
      mapM (extract outputMethod lines' spacing') glyphs
      return ()
run (PdfToText _ outputMethod pages lines' spacing' inputFile) = do
  case (R.parseRanges pages)::(Either R.ParseError [R.Range Int]) of
    Left err -> do
      print err
      return ()
    Right ranges -> do
      withBinaryFile inputFile ReadMode $ \handle -> do
        pdf <- P.pdfWithHandle handle
        doc <- P.document pdf
        catalog <- P.documentCatalog doc
        rootNode <- P.catalogPageNode catalog
        count <- P.pageNodeNKids rootNode
        let pages = map (\n -> n - 1) $ filter (R.inRanges ranges) [1 .. count]
        mapM (extractPdfPage outputMethod lines' spacing' rootNode) pages
        return ()

-- | extract a single page using the pdf-toolbox
extractPdfPage :: Maybe OutputMethod -> Int -> Double -> P.PageNode -> Int -> IO ()
extractPdfPage outputMethod lines' spacing' rootNode n = do
  page <- P.pageNodePageByNum rootNode n
  spans <- P.pageExtractGlyphs page
  extract outputMethod lines' spacing' $ concatMap P.spGlyphs spans
  return ()


extract :: (Show g, Eq g, Glyph g) => Maybe OutputMethod -> Int -> Double -> [g] -> IO ()
extract (Just Glyphs) lines' spacing' glyphs = do
  mapM (putStrLn . show) glyphs
  return ()
extract (Just Info) lines' spacing' glyphs = do
  putStr "#Glyphs: "
  print $ length glyphs
  putStr "Top: "
  print $ glyphsTop glyphs
  putStr "Bottom: "
  print $ glyphsBottom glyphs
  let lines = findLinesWindow lines' 5 2 True glyphs
  putStr "#Lines: "
  print $ length $ lines
  putStr "Glyphs per Lines: "
  print $ map length lines
  return ()
extract (Just NoSpaces) lines' spacing' glyphs = do
  let lines = findLinesWindow lines' 5 2 True glyphs
  mapM (T.putStrLn . (linearizeLine (T.concat . mapMaybe text))) lines
  return ()
extract (Just Spacing') lines' spacing' glyphs = do
  let lines = findLinesWindow lines' 5 2 True glyphs
      csvOpts = Csv.defaultEncodeOptions {
        Csv.encDelimiter = fromIntegral $ ord ','
        }
  mapM (C.putStr . (Csv.encodeWith csvOpts) . spacingsInLine) lines
  return ()
extract _ lines' spacing' glyphs = do
  let lines = findLinesWindow lines' 5 2 True glyphs
  mapM (T.putStrLn . (linearizeLine (spacingFactor spacing'))) lines
  return ()



-- import qualified Data.ByteString.Char8 as C
-- import qualified Data.Serialize as S
-- import Text.Read (readMaybe)

-- mainO :: IO ()
-- mainO = do
--   [file] <- getArgs
--   b <- B.readFile file
--   print b
--   print $ readBbox b
--   let bs = C.split ',' b
--   print $ length bs
--   let d :: Maybe Double = readMaybe $ C.unpack b
--   print d
--   return ()