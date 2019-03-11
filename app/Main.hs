{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Data.Char
import Data.List
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
  , headlines :: Int
  , footlines :: Int
  , lineCategorizer :: LineCategorizer
  , inputFile :: String
  }

data OutputMethod = Text | NoSpaces | Info | Glyphs | Spacing'


data InputMethod = PdfInput | PdfMinerXml


data LineCategorizer
  = ByIndent
    Double                      -- ^ paragraph indentation
    Double                      -- ^ indentation of custos
    Double                      -- ^ indentation of sheet signature
    Double                      -- ^ line filling of sheet signature
  | AsDefault


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
                  (long "no-spaces"
                   <> help "Extract text without spaces. (Yes, PDF really does not even know the concept of spaces..."))
                 <|>
                 (flag' Info
                  (long "statistics"
                   <> help "Show statistics about the text."))
                 <|>
                 (flag' Spacing'
                  (long "spacing"
                   <> help "Extract inter-glyph distances for statistical analysis. This generates CSV output."))
                 <|>
                 (flag' Glyphs
                  (long "glyphs"
                   <> help "Show the information about the glyphs found in the document.")))
  <*> strOption (short 'r'
                 <> long "pages"
                 <> help "Ranges of pages to extract. Defaults to all. Examples: 3-9 or -10 or 2,4,6,20-30,40- or \"*\" for all. Except for all do not put into quotes."
                 <> value "*"
                 <> metavar "PAGES")
  <*> option auto (short 'l'
                   <> long "lines-per-page"
                   <> help "Lines per page. Lines of a vertically filled page. This does not need to be exact."
                   <> value 42
                   <> showDefault
                   <> metavar "LINES")
  <*> option auto (short 'f'
                   <> long "spacing-factor"
                   <> help "A fixed spacing factor. If the distance between two glyphs exceeds the product of the first glyphs width and this factor, a space is inserted. For Gothic letter scanned by google values down to 1 are promising."
                   <> value 1.3
                   <> showDefault
                   <> metavar "SPACING")
  <*> option auto (short 'H'
                   <> long "headlines"
                   <> help "Count of lines in the page header to be dropped."
                   <> value 0
                   <> showDefault
                   <> metavar "HEADLINES")
  <*> option auto (short 'F'
                   <> long "footlines"
                   <> help "Count of lines in the page footer to be dropped."
                   <> value 0
                   <> showDefault
                   <> metavar "FOOTLINES")
  <*> ((flag ByIndent ByIndent
        (short 'i'
         <> long "by-indent"
         <> help "Categorize lines by their indentation.")
        <*> option auto
        (long "par-indent"
          <> help "Minimal indentation of the first line of a new the paragraph. In portion of the page width."
          <> value 0.05
          <> showDefault
          <> metavar "PARINDENT")
        <*> option auto
        (long "custos-indent"
          <> help "Minimal indentation of the custos (dt. Kustode), i.e. the first syllable of the next page in the bottom line. In portion of the page width."
          <> value 0.667
          <> showDefault
          <> metavar "CUSTOSINDENT")
        <*> option auto
        (long "sig-indent"
          <> help "Minimal indentation of the sheet signature in portion of the page width."
          <> value 0.05
          <> showDefault
          <> metavar "SIGINDENT")
        <*> option auto
        (long "sig-filling"
          <> help "Maximal filling of the bottom line if it's a sheet signature."
          <> value 0.333
          <> showDefault
          <> metavar "SIGFILL"))
       <|>
       (flag' AsDefault
        (short 'C'
         <> long "no-categorization"
         <> help "Do not categorize the lines at all.")))
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")


main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> pdfToText_)
          ( fullDesc
            <> progDesc "googleb-ok extracts the text from a PDF and was written to work for scanned books from books.google.com. There are options for stripping of page headers and footers in order to make the pure text ready for text mining and NLP."
            <> header "googleb-ok - extract text from a PDF, even scanned books." )


-- | Run the extractor with the parsed command line arguments.
run :: PdfToText -> IO ()
run (PdfToText (Just PdfMinerXml) outputMethod pages lines' spacing' headlines' footlines' lineCategorizer' inputFile) = do
  case (R.parseRanges pages)::(Either R.ParseError [R.Range Int]) of
    Left err -> do
      print err
      return ()
    Right ranges -> do
      glyphs <- B.readFile inputFile >>= parseXml ranges
      mapM (extract outputMethod lines' spacing' headlines' footlines' lineCategorizer') glyphs
      return ()
run (PdfToText _ outputMethod pages lines' spacing' headlines' footlines' lineCategorizer' inputFile) = do
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
        mapM (extractPdfPage outputMethod lines' spacing' headlines' footlines' lineCategorizer' rootNode) pages
        return ()

-- | extract a single page using the pdf-toolbox
extractPdfPage :: Maybe OutputMethod -> Int -> Double -> Int -> Int -> LineCategorizer -> P.PageNode -> Int -> IO ()
extractPdfPage outputMethod lines' spacing' headlines' footlines' lineCategorizer' rootNode n = do
  page <- P.pageNodePageByNum rootNode n
  spans <- P.pageExtractGlyphs page
  extract outputMethod lines' spacing' headlines' footlines' lineCategorizer' $ concatMap P.spGlyphs spans
  return ()


extract :: (Show g, Eq g, Glyph g) =>
  Maybe OutputMethod ->         -- ^ output method
  Int ->                        -- ^ count of lines
  Double ->                     -- ^ spacing factor
  Int ->                        -- ^ headlines
  Int ->                        -- ^ footlines
  LineCategorizer ->            -- ^ config of line categorizer
  [g] ->                        -- ^ glyphs on this page
  IO ()
extract (Just Glyphs) _ _ _ _ _ glyphs = do
  mapM (putStrLn . show) glyphs
  return ()
extract (Just Info) lines' _ _ _ _ glyphs = do
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
extract (Just NoSpaces) lines' _ _ _ _ glyphs = do
  let lines = findLinesWindow lines' 5 2 True glyphs
  mapM (T.putStrLn . (linearizeLine (T.concat . mapMaybe text))) lines
  return ()
extract (Just Spacing') lines' _ _ _ _ glyphs = do
  let lines = findLinesWindow lines' 5 2 True glyphs
      csvOpts = Csv.defaultEncodeOptions {
        Csv.encDelimiter = fromIntegral $ ord ','
        }
  mapM (C.putStr . (Csv.encodeWith csvOpts) . spacingsInLine . (sortOn xLeft)) lines
  return ()
extract _ lines' spacing' headlines' footlines' (ByIndent pi ci si sf) glyphs = do
  let lines = findLinesWindow lines' 5 2 True glyphs
  mapM (T.putStrLn . (linearizeCategorizedLine (spacingFactor spacing'))) $
    categorizeLines (byIndent pi ci si sf) $
    (drop headlines') $ dropFoot footlines' lines
  T.putStr(T.singleton $ chr 12) -- add form feed at end of page
  return ()
extract _ lines' spacing' headlines' footlines' AsDefault glyphs = do
  let lines = findLinesWindow lines' 5 2 True glyphs
  mapM (T.putStrLn . (linearizeLine (spacingFactor spacing'))) $
    (drop headlines') $ dropFoot footlines' lines
  T.putStr(T.singleton $ chr 12) -- add form feed at end of page
  return ()


dropHead :: Glyph g => Int -> [[g]] -> [[g]]
dropHead 0 = id
dropHead n = drop n

dropFoot :: Glyph g => Int -> [[g]] -> [[g]]
dropFoot 0 = id
dropFoot n = reverse . drop n . reverse
