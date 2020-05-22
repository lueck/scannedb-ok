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
import Data.Tuple.Extra
import qualified Data.HashMap.Lazy as M
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Grenade

import Pdf.Extract.Lines
import Pdf.Extract.Linearize
import Pdf.Extract.PdfToolBox
import Pdf.Extract.PyPdfMiner
import Pdf.Extract.Glyph
import Pdf.Extract.Spacing
import Pdf.Extract.Syllable


-- * Parsing command line arguments

-- | A record for command line arguments
data ExtractionCommand
  = PdfToText
  { inputMethod :: InputMethod
  , outputMethod :: OutputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , fixedSpacingFactor :: Double
  , lineCategorizer :: LineCategorizer
  , nlpOut :: Bool
  , inputFile :: String
  }
  | TrainSpacing
  { inputMethod :: InputMethod
  , lineOpts :: LineOptions
  , iterations :: Int
  , learningParameters :: LearningParameters
  , trainingPDF :: FilePath     -- ^ file with glyphs (PDF, XML etc.) for training
  , trainingTxt :: FilePath     -- ^ plaintext file with spaces for training
  , validationPDF :: FilePath   -- ^ file with glyphs (PDF, XML, etc.) for validation
  , validationTxt :: FilePath   -- ^ plaintext file with spaces for validation
  }

-- | A record for the output command line argument 
data OutputMethod = Text | NoSpaces | Info | Glyphs | Spacing' | Tokens


-- | A record for the input type command line argument 
data InputMethod = PdfInput | PdfMinerXml


-- | A record for the command line arguments on categorizing lines
data LineCategorizer
  = ByIndent
    ByIndentOpts
    LinearizationOpts
    SyllableRepair
  | AsDefault
    Int                         -- ^ count of headlines to drop
    Int                         -- ^ count of footlines to drop


-- | A record for the output command line arguments on repairing
-- syllable division
data SyllableRepair = SylRepair
  { tokensFile :: Maybe FilePath
  --, divisionMark :: Char
  , markRequired :: Bool
  }

inputMethod_ :: Parser InputMethod
inputMethod_ =
  (flag PdfInput PdfInput
   (short 'p'
    <> long "pdf"
    <> help "PDF input data. (Default)"))
  <|>
  (flag' PdfMinerXml
   (short 'x'
    <> long "xml"
    <> help "XML input data. An XML representation of the glyphs of a PDF file, like produced with PDFMiner's \"pdf2txt.py -t xml ...\" command."))

outputMethod_ :: Parser OutputMethod
outputMethod_ =
  (flag Text Text
   (short 't'
    <> long "text"
    <> help "Extract text. (Default)"))
  <|>
  (flag' NoSpaces
   (long "no-spaces"
    <> help "Extract text without spaces. (PDF really does not even know the concept of spaces!)"))
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
    <> help "Show the information about the glyphs found in the document."))
  <|>
  (flag' Tokens
   (long "tokens"
    <> help "Generate a list of tokens from the document."))

pages_ :: Parser String
pages_ =
  strOption
  (short 'r'
   <> long "pages"
   <> help "Ranges of pages to extract. Defaults to all. Examples: 3-9 or -10 or 2,4,6,20-30,40- or \"*\" for all. Except for all do not put into quotes."
   <> value "*"
   <> metavar "PAGES")

lineCategorizer_ :: Parser LineCategorizer
lineCategorizer_ =
  (flag ByIndent ByIndent
   (short 'i'
    <> long "by-indent"
    <> help "Categorize lines by their indentation. (Default)")
   <*> byIndentOpts_
   <*> linOpts_
   <*> syllableRepair_)
  <|>
  ((flag' AsDefault
    (short 'C'
     <> long "no-categorization"
     <> help "Do not categorize the lines at all."))
    <*> option auto
    (long "headlines"
     <> help "Count of lines in the page header to be dropped."
     <> value 0
     <> showDefault
     <> metavar "HEADLINES")
    <*> option auto
    (long "footlines"
     <> help "Count of lines in the page footer to be dropped."
     <> value 0
     <> showDefault
     <> metavar "FOOTLINES"))


nlpOut_ :: Parser Bool
nlpOut_ = switch
  (long "nlp"
   <> help "Convient toggle for NLP-friendly output when categorizing lines by-indent (see -i) and drop page signature, drop custos, no indentation of categorized lines. This sets PAR to newline \"\\n\" and BLOCKQUOTE to the empty string \"\".")


-- | A parser for the command line arguments.
pdfToText_ :: Parser ExtractionCommand
pdfToText_ = PdfToText
  <$> inputMethod_
  <*> outputMethod_
  <*> pages_
  <*> lineOpts_
  <*> option auto (short 'f'
                   <> long "spacing-factor"
                   <> help "A fixed spacing factor. If the distance between two glyphs exceeds the product of the first glyphs width and this factor, a space is inserted. For Gothic letter scanned by google values down to 1 are promising."
                   <> value 1.3
                   <> showDefault
                   <> metavar "SPACING")
  <*> lineCategorizer_
  <*> nlpOut_
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")

trainSpacing_ :: Parser ExtractionCommand
trainSpacing_ = TrainSpacing
  <$> inputMethod_
  <*> lineOpts_
  <*> option auto (long "iterations"
                   <> help "number of learning iterations"
                   <> value 100
                   <> showDefault)
  <*> (LearningParameters
       <$> option auto (long "rate"
                        <> help "Learning rate"
                        <> value 0.01
                        <> showDefault)
       <*> option auto (long "momentum"
                        <> help "Learning momentum"
                        <> value 0.9
                        <> showDefault)
       <*> option auto (long "l2"
                        <> help "Learning regulizer"
                        <> value 0.0005
                        <> showDefault))
  <*> argument str (metavar "TRAININGPDF"
                    <> help "Path to PDF (or XML) with training text.")
  <*> argument str (metavar "TRAININGTEXT"
                    <> help "Path to plaintext with correct spaces which corresponds exactly to TRAININGPDF.")
  <*> argument str (metavar "VALIDATIONPDF"
                    <> help "Path to PDF (or XML) with validation text.")
  <*> argument str (metavar "VALIDATIONTEXT"
                    <> help "Path to plaintext with correct spaces which corresponds exactly to VALIDATIONPDF.")


command_ :: Parser ExtractionCommand
command_ = subparser
  (command "extract"
   (info
    (helper <*> pdfToText_)
    (fullDesc
     <> progDesc "scannedb-ok extract   extracts the text from a PDF and was written to work for scanned books from books.google.com. There are options for stripping of page headers and footers in order to make the pure text ready for text mining and NLP.\n\nTake care of the parentheses, square brackets and pipes in the usage note. Parentheses group options together, the pipe divides (groups of options) into alternatives. Square brackets mean that the option is optional.\n\nThere are two input formats, pdf and xml. There are six output formats: --text which is the default, --no-spaces for scriptura continua, --statistics for information on each page, --spacing for csv output with information on glyphs an inter-glyph spacing, --glyphs for an internal representation of glyphs, --tokens for output of a list of words which can be reused for repairing syllable divisions. There are two modes of line detection: -i for categorization of lines into headline, footline etc., -C for no such categorization at all. These modes have various options.\n\nThe order of the command line arguments does not matter."
     <> header "scannedb-ok extract - extract text from a PDF, even scanned books." ))
   <> command "trainSpacing"
   (info
    (helper <*> trainSpacing_)
    (fullDesc
     <> progDesc "scannedb-ok trainSpacing   trains an ANN for inter-glyph spacing. This requires four input files: TRAININGPDF and TRAININGTXT must contain exactly the same text, once in PDF format (or some other format conaining glyph descriptions, like pdfminers xml), and once as plaintext with correct spaces. This pair of files is used for training the neurons. VALIDATIONPDF and VALIDATIONTXT have to be exactly corresponding texts, too. They are used to validate the trained network after each learning iteration and the validation result is logged, so that the progression can be observed. For a start, you can simply reuse the first pair of files as validation texts."
     <> header "scannedb-ok trainSpacing - train spacing module.")))


lineOpts_ :: Parser LineOptions
lineOpts_ = LineOptions
  <$> option auto
  (short 'l'
   <> long "lines-per-page"
   <> help "Lines per page. Lines of a vertically filled page. This does not need to be exact."
   <> value 42
   <> showDefault
   <> metavar "LINES")
  <*> option auto
  (long "threshold"
   <> help "A threshold value important for the identification of lines by the internal clustering algorithm: Fail-OCRed glyphs between the lines may disturb the separation of lines. Instead of 0, the threshold value of is used to separate the clusters. If set to high, short lines may be dropped of."
   <> value 2
   <> showDefault
   <> metavar "THRESHOLD")
  <*> fmap not (switch
  (short 'k'
   <> long "keep-single-glyphs-lines"
   <> help "Do not drop glyphs found between the lines. By default, lines with a count of glyphs under THRESHOLD are dropped."))
  <*> option auto
  (long "steps-per-line"
   <> help "With the STEPS per line you may tweak the clustering algorithm for the indentification of lines."
   <> value 5
   <> showDefault
   <> metavar "STEPS")


-- | Parser for linearization command line options
linOpts_ :: Parser LinearizationOpts
linOpts_ = LinOpts
  <$> ((flag Part3 Part3
        (long "head-keep-page"
         <> help "Keep only the page number found in the headline. (Default)"))
       <|>
       (flag' Drop3
        (long "head-drop"
         <> help "Drop the whole headline."))
       <|>
       (flag' Keep3
        (long "head-keep"
         <> help "Keep the whole headline.")))
  <*> ((flag Part3 Part3
        (long "foot-keep-page"
         <> help "Keep only the page number found in the footline. (Default)"))
       <|>
       (flag' Drop3
        (long "foot-drop"
         <> help "Drop the whole footline."))
       <|>
       (flag' Keep3
        (long "foot-keep"
         <> help "Keep the whole footline.")))
  <*> ((flag Drop2 Drop2
        (long "custos-drop"
         <> help "Drop the custos, i.e. the bottom line which contains the first syllable of the next page. (Default)"))
       <|>
       (flag' Keep2
        (long "custos-keep"
         <> help "Keep the custos.")))
  <*> ((flag Drop2 Drop2
        (long "sig-drop"
         <> help "Drop the sheet signature in the bottom line. (Default)"))
       <|>
       (flag' Keep2
        (long "sig-keep"
         <> help "Keep the sheet signature.")))
  <*> strOption (long "page-pre"
                 <> help "The prefix for the page number if only the number is kept of a head- or footline."
                 <> value "[["
                 <> showDefault
                 <> metavar "PAGEPRE")
  <*> strOption (long "page-post"
                 <> help "The postfix for the page number if only the number is kept of a head- or footline."
                 <> value "]]"
                 <> showDefault
                 <> metavar "PAGEPOST")
  <*> strOption (long "par"
                 <> help "The prefix for linearizing the first line of a paragraph."
                 <> value "\n\t"
                 <> showDefault
                 <> metavar "PAR")
  <*> strOption (long "custos"
                 <> help "The prefix for linearizing the custos."
                 <> value "\t\t\t\t\t"
                 <> showDefault
                 <> metavar "CUSTOS")
  <*> strOption (long "sig"
                 <> help "The prefix for linearizing the sheet signature."
                 <> value "\t\t\t"
                 <> showDefault
                 <> metavar "SIG")
  <*> strOption (long "blockquote"
                 <> help "The prefix for linearizing a block quote."
                 <> value "\t\t$$"
                 <> showDefault
                 <> metavar "BLOCKQUOTE")

-- | A parser for command line arguments for line categorization
byIndentOpts_ :: Parser ByIndentOpts
byIndentOpts_ = ByIndentOpts
  <$> option auto
  (long "par-indent"
   <> help "Minimal indentation of the first line of a new the paragraph. In portion of a quad or \'em\' (dt. Geviert). This is the most important parameter to tinker with."
   <> value 3
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
   <> value 0.0333
   <> showDefault
   <> metavar "SIGINDENT")
  <*> option auto
  (long "sig-filling"
   <> help "Maximal filling of the bottom line if it's a sheet signature."
   <> value 0.333
   <> showDefault
   <> metavar "SIGFILL")
  <*> switch
  (long "quote-parsing"
   <> help "Use this option if you want to parse for block quotes. (Experimental) This might interfere with the parsing for new paragraphs. The difference is that a block quote's font size is assumed to be a few smaller. But clustering for the base font size is still experimental and has no good results for gothic script.")
  <*> switch
  (short 'M'
   <> long "drop-margin"
   <> help "Drop glyphs found outside of the type area. The type area is determined by a clustering algorithm which assumes that the most lines completely fill the type area horizontally. Do not use this switch, if this is not the case for your text. It may produce errors on pages with only one or two lines.")

-- | A parser for command line options for repairing syllable divisions
syllableRepair_ :: Parser SyllableRepair
syllableRepair_ = SylRepair
  <$> optional (strOption
  (short 'w'
   <> long "word-pool"
   <> help "If a path to file with a pool of words (tokens) is given, syllable division is repaired in the text output, but only when line categorization is turned on."
   <> metavar "WORDPOOL"))
  <*> switch
  (short 'D'
   <> long "no-division-mark-required"
   <> help "Use this switch, if syllable division is to be repaired for lines without dash mark.")
  -- <*> option auto
  -- (long "syllable-sep"
  --  <> help "The division mark for syllable division at the line end."
  --  <> value '-'
  --  <> showDefault
  --  <> hidden)


nlpOutput :: Bool -> LineCategorizer -> LineCategorizer
nlpOutput False o = o
nlpOutput True (ByIndent byIndOpts linOpts sylOpts) =
  ByIndent byIndOpts (nlpLike linOpts) sylOpts
nlpOutput True o = o


main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> command_)
               (fullDesc
                <> progDesc "scannedb-ok extracts the text from a PDF and was written to work for scanned books from books.google.com. "
                <> header "scannedb-ok extracts text from a PDF, even scanned books.")


-- | Run the extractor with the parsed command line arguments.
run :: ExtractionCommand -> IO ()
run (PdfToText PdfMinerXml outputMethod pages lineOpts spacing' lineCategorizer' nlp' inputFile) = do
  case (R.parseRanges pages)::(Either R.ParseError [R.Range Int]) of
    Left err -> do
      print err
      return ()
    Right ranges -> do
      glyphs <- B.readFile inputFile >>= parseXml ranges
      extract outputMethod lineOpts spacing' (nlpOutput nlp' lineCategorizer') $
        zip [1..] glyphs
      return ()
run (PdfToText _ outputMethod pages lineOpts spacing' lineCategorizer' nlp' inputFile) = do
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
        glyphs :: [[P.Glyph]] <- mapM (extractPdfPageGlyphs rootNode) pages
        extract outputMethod lineOpts spacing' (nlpOutput nlp' lineCategorizer') $ zip pages glyphs
        return ()
run (TrainSpacing PdfMinerXml lineOpts iterations rate trainingPdf trainingTxt validationPdf validationTxt) = do
  ranges <- parseRanges "*"
  spaced <- T.readFile trainingTxt
  glyphs <- B.readFile trainingPdf >>= parseXml ranges
  validationSpaced <- T.readFile validationTxt
  validationGlyphs <- B.readFile validationPdf >>= parseXml ranges
  let glyphLines = map (findLinesWindow lineOpts) glyphs
      txtLines = cleanForSpaceTraining spaced
      linesByLines = zip txtLines (concat glyphLines)
      validationGlyphLines = map (findLinesWindow lineOpts) validationGlyphs
      validationTxtLines = cleanForSpaceTraining validationSpaced
      validationLinesByLines = zip validationTxtLines (concat validationGlyphLines)
  hPutStr stderr "Verifying training data..."
  td <- mapM reportErrors $ map (uncurry mkTrainingShapes22) linesByLines
  hPutStrLn stderr " done"
  hPutStr stderr "Verifying validation data..."
  vd <- mapM reportErrors $ map (uncurry mkTrainingShapes22) validationLinesByLines
  hPutStrLn stderr " done"
  hPutStr stderr "Training..."
  initialNet <- randomSpacingNet
  trained <- foldM (runSpacingIteration stderr (concat td) (concat vd) rate) initialNet [1..iterations]
  hPutStrLn stderr " done"
  hPutStrLn stderr "Trained network run on validation data:"
  -- print td
  mapM_ (\gs -> do
            l <- reportErrors $ runSpacingNetOnLine trained gs
            T.putStrLn l) (concat validationGlyphLines)


reportErrors :: Either String a -> IO a
reportErrors (Right r) = return r
reportErrors (Left err) = fail err


parseRanges :: String -> IO [R.Range Int]
parseRanges pages = do
  case (R.parseRanges pages)::(Either R.ParseError [R.Range Int]) of
    Left err -> do
      fail $ show err
    Right ranges -> return ranges



-- | get all the glyphs from a page using the pdf-toolbox
extractPdfPageGlyphs :: P.PageNode -> Int -> IO ([P.Glyph])
extractPdfPageGlyphs  rootNode n = do
  page <- P.pageNodePageByNum rootNode n
  spans <- P.pageExtractGlyphs page
  return $ concatMap P.spGlyphs spans


extract :: (Show g, Eq g, Glyph g) =>
  OutputMethod ->               -- ^ output method
  LineOptions ->                -- ^ count of lines etc.
  Double ->                     -- ^ spacing factor
  LineCategorizer ->            -- ^ config of line categorizer
  [(Int, [g])] ->               -- ^ list of tuples of page number and
                                -- glyphs on this page
  IO ()
extract Glyphs _ _ _ pages = do
  mapM_ (putStrLn . show) $ concatMap snd pages
extract Info lineOpts _ _ pages = do
  forM_ pages (\(page, glyphs) -> do
                  putStr "Page: "
                  print $ show page
                  putStr "#Glyphs: "
                  print $ length glyphs
                  putStr "Top: "
                  print $ glyphsTop glyphs
                  putStr "Bottom: "
                  print $ glyphsBottom glyphs
                  let lines = findLinesWindow lineOpts glyphs
                  printLineInfo $ genLineInfo lines)
extract NoSpaces lineOpts _ _ pages = do
  forM_ pages (\(page, glyphs) -> do
                  let lines = findLinesWindow lineOpts glyphs
                  mapM_ (T.putStrLn . (linearizeLine (T.concat . mapMaybe text))) lines)
extract Spacing' lineOpts _ _ pages = do
  forM_ pages (\(page, glyphs) -> do
                  let lines = findLinesWindow lineOpts glyphs
                      lines_ = zip3 (map Just [1..])
                               (repeat (Just page))
                               $ map (sortOn xLeft) lines
                      csvOpts = Csv.defaultEncodeOptions {
                        Csv.encDelimiter = fromIntegral $ ord ','
                        }
                  mapM (C.putStr .
                         (Csv.encodeWith csvOpts) .
                         (uncurry3 spacingsInLine)) lines_)
  where
    getGlyph :: Glyph g => (a, b, [g]) -> [g]
    getGlyph (_, _, g) = g
extract Tokens lineOpts spacing' (ByIndent byIndOpts linOpts sylOpts) pages = do
  forM_ pages (\(page, glyphs) -> do
                  let tokens = concatMap (tokenizeMiddle) $
                               map (linearizeCategorizedLine linOpts (spacingFactor spacing')) $
                               categorizeLines (byIndent byIndOpts) $
                               findLinesWindow lineOpts glyphs
                  mapM T.putStrLn tokens)
extract Tokens lineOpts spacing' (AsDefault headlines' footlines') pages = do
  forM_ pages (\(page, glyphs) -> do
                  let tokens = concatMap (tokenizeMiddle) $
                               map (linearizeLine (spacingFactor spacing')) $
                               drop headlines' $
                               dropFoot footlines' $
                               findLinesWindow lineOpts glyphs
                  mapM T.putStrLn tokens)
extract _ lineOpts spacing' (ByIndent byIndOpts linOpts sylOpts) pages = do
  forM_ pages (\(page, glyphs) -> do
                  let lines = map (linearizeCategorizedLine linOpts (spacingFactor spacing')) $
                              categorizeLines (byIndent byIndOpts) $
                              findLinesWindow lineOpts glyphs
                  linearized <- if (isJust $ tokensFile sylOpts)
                    then loadTokens (fromMaybe "/dev/null" $ tokensFile sylOpts) >>=
                         \ws -> repair (flip M.member ws) (markRequired sylOpts) [] lines
                         -- /dev/null is never loaded, because of if condition
                    else return lines
                  mapM T.putStr linearized
                  -- add form feed at end of page
                  T.putStr(T.singleton $ chr 12))
extract _ lineOpts spacing' (AsDefault headlines' footlines') pages = do
  forM_ pages (\(page, glyphs) -> do
                  let lines = findLinesWindow lineOpts glyphs
                  mapM (T.putStrLn . (linearizeLine (spacingFactor spacing'))) $
                    (drop headlines') $ dropFoot footlines' lines
                  T.putStr(T.singleton $ chr 12) -- add form feed at end of page
                  return ())


dropHead :: Glyph g => Int -> [[g]] -> [[g]]
dropHead 0 = id
dropHead n = drop n

dropFoot :: Glyph g => Int -> [[g]] -> [[g]]
dropFoot 0 = id
dropFoot n = reverse . drop n . reverse
