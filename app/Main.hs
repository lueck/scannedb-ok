{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import System.IO
import Options.Applicative
import Data.Monoid ((<>))
import Data.Char
import Data.List
import Data.Either
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
import Data.Serialize as S
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (argument)

import Pdf.Extract.Lines
import Pdf.Extract.Linearize hiding (LineCategory(..), ByIndentOpts(..), byIndent)
import Pdf.Extract.PdfToolBox
import Pdf.Extract.PyPdfMiner
import Pdf.Extract.Glyph
import Pdf.Extract.Spacing
import Pdf.Extract.Block
import Pdf.Extract.Syllable
import Pdf.Extract.Precision


-- * Parsing command line arguments

-- | A record for all subcommands and their arguments
data ExtractionCommand
  = ExtractText
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , spaceMethod :: SpaceInsertion
  , lineCategorizer :: LineCategorizer
  , linearOpts :: LinearizationOpts
  , nlpOut :: Bool
  , sylRepair :: SyllableRepair
  , inputFile :: String
  }
  | NoSpaces
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , inputFile :: String
  }
  | Info
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , inputFile :: String
  }
  | ExtractGlyphs
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , inputFile :: String
  }
  | SpacingStats
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , inputFile :: String
  }
  | ExtractWords
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , spaceMethod :: SpaceInsertion
  , lineCategorizer :: LineCategorizer
  , linearOpts :: LinearizationOpts
  , nlpOut :: Bool
  , inputFile :: String
  }
  | TrainSpacing
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , lineCategorizer :: LineCategorizer
  , iterations :: Int
  , learningParameters :: LearningParameters
  , trainingPDF :: FilePath     -- ^ file with glyphs (PDF, XML etc.) for training
  , trainingTxt :: FilePath     -- ^ plaintext file with spaces for training
  , netFile :: FilePath         -- ^ file to dump the trained network in
  }
  | ValidateSpacing
  { inputMethod :: InputMethod
  , pages :: String
  , lineOpts :: LineOptions
  , spaceMethod :: SpaceInsertion
  , lineCategorizer :: LineCategorizer
  , validationTxt :: FilePath   -- ^ plaintext file with spaces for validation
  , inputFile :: String
  }
  | ValidateForeignSpacing
  { inputFile :: String
  , goldStandard :: String
  }


-- | A record for the input type command line argument 
data InputMethod = PdfInput | PdfMinerXml

-- | A record for the mechanism for inserting spaces
data SpaceInsertion
  = WidthSpacingFactor Double
  | SizeSpacingFactor Double
  | ANNSpacing FilePath

-- | A record for the command line arguments on categorizing lines
data LineCategorizer
  = ByIndent
    ByIndentOpts
    -- LinearizationOpts
    -- SyllableRepair
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


spaceInsertion_ :: Parser SpaceInsertion
spaceInsertion_ =
  (SizeSpacingFactor
   <$> option auto (short 's'
                    <> long "size-spacing-factor"
                    <> help "Use a fixed-spacing-factor rule for inserting inter-word spaces. If the distance between two glyphs exceeds the product of the first glyphs size and this factor, a space is inserted. For Antiqua fonts a space was traditionally 1/3 of the fonts height (size), but should never be less than 1/6. So 1/6 is a good value to start with."
                    <> value (1/6)
                    <> showDefault
                    <> metavar "SPACING"))
  <|>
  (WidthSpacingFactor
   <$> option auto (short 'f'
                    <> long "width-spacing-factor"
                    <> help "Use a fixed-spacing-factor rule for inserting inter-word spaces. If the distance between two glyphs exceeds the product of the first glyphs width and this factor, a space is inserted. For Gothic letter scanned by google values down to 1 are promising."
                    <> value 1.3
                    -- <> showDefault
                    <> metavar "SPACING"))
  <|>
  (ANNSpacing
   <$> strOption (short 'n'
                  <> long "spacing-net"
                  <> help "Use a trained artificial neural network for inserting inter-word spaces."
                  <> metavar "NETFILE"))


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
   -- <*> linOpts_
   -- <*> syllableRepair_
  )
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


-- | A parser for the __extract__ command and its arguments.
extractText_ :: Parser ExtractionCommand
extractText_ = ExtractText
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> spaceInsertion_
  <*> lineCategorizer_
  <*> linOpts_
  <*> nlpOut_
  <*> syllableRepair_
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")

-- | A parser for the __info__ command and its arguments.
info_ :: Parser ExtractionCommand
info_ = Info
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")

-- | A parser for the __nospaces__ command and its arguments.
noSpaces_ :: Parser ExtractionCommand
noSpaces_ = NoSpaces
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")

-- | A parser for the __glyphs__ command and its arguments.
glyphs_ :: Parser ExtractionCommand
glyphs_ = ExtractGlyphs
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")


-- | A parser for the __glyphstats__ command and its arguments.
spacingStats_ :: Parser ExtractionCommand
spacingStats_ = SpacingStats
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")


-- | A parser for the __words__ command and its arguments.
extractWords_ :: Parser ExtractionCommand
extractWords_ = ExtractWords
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> spaceInsertion_
  <*> lineCategorizer_
  <*> linOpts_
  <*> nlpOut_
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")


trainSpacing_ :: Parser ExtractionCommand
trainSpacing_ = TrainSpacing
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> lineCategorizer_
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
  <*> argument str (metavar "NETFILE"
                    <> help "Path to the file where to dump the trained network in. The file will be in a binary format, so \".dat\" is a reasonable suffix.")


-- | A parser for the __validateSpacing__ command and its arguments.
validateSpacing_ :: Parser ExtractionCommand
validateSpacing_ = ValidateSpacing
  <$> inputMethod_
  <*> pages_
  <*> lineOpts_
  <*> spaceInsertion_
  <*> lineCategorizer_
  <*> argument str (metavar "VALIDATIONTEXT"
                    <> help "Path to plaintext with correct spaces which corresponds exactly to the read portion of the INFILE.")
  <*> argument str (metavar "INFILE"
                    <> help "Path to the input file.")


-- | A parser for the __validateForeignSpacing__ command.
validateForeignSpacing_ :: Parser ExtractionCommand
validateForeignSpacing_ = ValidateForeignSpacing
  <$> argument str (metavar "INFILE"
                    <> help "Path to the plain text input file.")
  <*> argument str (metavar "GOLDSTANDARD"
                    <> help "Path to the verified gold standard.")


formatHint :: String
formatHint = "\n\nTake care of the parentheses, square brackets and pipes in the usage note. Parentheses group options together, the pipe divides (groups of options) into alternatives. Square brackets mean that the option is optional. "


command_ :: Parser ExtractionCommand
command_ = subparser
  (command "text"
   (info
    (helper <*> extractText_)
    (fullDesc
     <> progDesc "scannedb-ok text   extracts the text from a PDF. There are options for stripping of page headers and footers in order to make the pure text ready for text mining and NLP.\n\nThere are two input formats, pdf and xml."
     <> header "scannedb-ok text - extract text from a PDF." ))
   --
   <>
   command "words"
    (info
     (helper <*> extractWords_)
     (fullDesc
      <> progDesc "scannedb-ok words  extracts a list of all non-divided words from the document. The first an the last word or word-part of a line is left from the list, since they may be parts of word due to syllable division."
      <> header "scannedb-ok words - Generate a list of words from the document."))
   --
   <> command "trainSpacing"
   (info
    (helper <*> trainSpacing_)
    (fullDesc
     <> progDesc "scannedb-ok trainSpacing   trains an artificial neural network for inter-word space insertion. This requires two input files: TRAININGPDF and TRAININGTXT must contain exactly the same text, once in PDF (or XML) format, and once as plaintext with verified spaces. This pair of files is used for training the neurons. For validating the network see the validateSpacing command."
     <> header "scannedb-ok trainSpacing - Train spacing module."))   --
   --
   <>
   command "validateSpacing"
   (info
    (helper <*> validateSpacing_)
    (fullDesc
     <> progDesc "scannedb-ok validateSpacing   validates inter-word space insertion by one of scannedb-ok's algorithms against a gold standard given in a plain text file. Precision and recall are calculated."
     <> header "scannedb-ok validateSpaces - validate inter-word space insertion against a gold standard." ))
   --
   <>
   command "validateForeign"
   (info
    (helper <*> validateForeignSpacing_)
    (fullDesc
     <> progDesc "scannedb-ok validateForeign   validates inter-word space insertion in a foreign plain text file against a gold standard given in an other plain text file. Precision and recall are calculated."
     <> header "scannedb-ok validateForeign - validate foreign inter-word space insertion against a gold standard." ))
   --
   <>
   command "nospaces"
    (info
     (helper <*> noSpaces_)
     (fullDesc
      <> progDesc "scannedb-ok nospaces  extracts the text from a PDF without inserting spaces, i.e. it produces scriptura continua."
      <> header "scannedb-ok nospaces - extract text without spaces."))
   --
   <>
   command "stats"
    (info
     (helper <*> info_)
     (fullDesc
      <> progDesc "scannedb-ok stats  shows statistics about the text."
      <> header "scannedb-ok stats - Show statistics about the text."))
   --
   <>
   command "spacing"
    (info
     (helper <*> spacingStats_)
     (fullDesc
      <> progDesc "scannedb-ok spacing  shows the inter-glyph distances for statistical analysis. This generates CSV output."
      <> header "scannedb-ok spacing - Extract inter-glyph distances for statistical analysis."))
   --
   <>
   command "glyphs"
    (info
     (helper <*> glyphs_)
     (fullDesc
      <> progDesc "scannedb-ok glyphs  shows information about the glyphs found in the document."
      <> header "scannedb-ok glyphs - Show information about the glyphs found in the document."))
  )


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
  -- <$> ((flag Part3 Part3
  --       (long "head-keep-page"
  --        <> help "Keep only the page number found in the headline. (Default)"))
  --      <|>
  --      (flag' Drop3
  --       (long "head-drop"
  --        <> help "Drop the whole headline."))
  --      <|>
  --      (flag' Keep3
  --       (long "head-keep"
  --        <> help "Keep the whole headline.")))
  -- <*> ((flag Part3 Part3
  --       (long "foot-keep-page"
  --        <> help "Keep only the page number found in the footline. (Default)"))
  --      <|>
  --      (flag' Drop3
  --       (long "foot-drop"
  --        <> help "Drop the whole footline."))
  --      <|>
  --      (flag' Keep3
  --       (long "foot-keep"
  --        <> help "Keep the whole footline.")))
  -- <*> ((flag Drop2 Drop2
  --       (long "custos-drop"
  --        <> help "Drop the custos, i.e. the bottom line which contains the first syllable of the next page. (Default)"))
  --      <|>
  --      (flag' Keep2
  --       (long "custos-keep"
  --        <> help "Keep the custos.")))
  -- <*> ((flag Drop2 Drop2
  --       (long "sig-drop"
  --        <> help "Drop the sheet signature in the bottom line. (Default)"))
  --      <|>
  --      (flag' Keep2
  --       (long "sig-keep"
  --        <> help "Keep the sheet signature.")))
  <$> switch (long "keep-head"
              <> help "Keep headline.")
  <*> switch (long "keep-foot"
              <> help "Keep footline.")
  <*> switch (long "keep-custos"
              <> help "Keep the custos, i.e. the bottom line which contains the first syllable of the next page.")
  <*> switch (long "keep-sheet-signature"
              <> help "Keep the sheet signature.")
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
   <> help "If a path to file with a pool of words (tokens) is given, syllable division is repaired in the text output, but only when line categorization is turned on. The \"words\" command may be used to generate word pools."
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


nlpOutput :: Bool -> LinearizationOpts -> LinearizationOpts
nlpOutput False o = o
nlpOutput True o = nlpLike o


-- * Read PDF or XML input

-- | Get glyphs from various input formats. The returned glyphs are
-- wrapped in the 'MkGlyphType' GADT constructor, because Haskell does
-- not allow to return a class. So the actual type of the parsed
-- glyphs is completely unaccessible, but the API defined for 'Glyph'
-- and 'Show' and 'Eq' is, because 'GlyphType' is an instance of these
-- classes. See 'GlyphType'.
--
-- Return type: A list of tuples, the first component of which is an
-- integer representing the page number and the second the list of
-- glyphs on that page.
getGlyphs :: InputMethod        -- ^ type of input file
          -> String             -- ^ range
          -> FilePath           -- ^ path
          -> IO [(Int, [GlyphType])]
getGlyphs PdfMinerXml pages inputFile = do
  pages <- getPdfMinerGlyphs pages inputFile
  return $ map (\(p, gs) -> (p, (map MkGlyphType gs))) pages
getGlyphs PdfInput pages inputFile = do
  pages <- getPdfGlyphs pages inputFile
  return $ map (\(p, gs) -> (p, (map MkGlyphType gs))) pages


-- | Get pages of glyphs from PdfMiner XML input.
getPdfMinerGlyphs :: String     -- ^ range
             -> FilePath        -- ^ path
             -> IO [(Int, [PdfMinerGlyph])]
getPdfMinerGlyphs pages inputFile = do
  ranges <- parseRanges pages
  glyphs <- B.readFile inputFile >>= parseXml ranges
  return $ zip [1..] glyphs -- FIXME: zip with ranges


-- | Get pages of glyphs from PDF input.
getPdfGlyphs :: String             -- ^ range
             -> FilePath           -- ^ path
             -> IO [(Int, [P.Glyph])]
getPdfGlyphs pages inputFile = do
  -- hPutStrLn stderr "PDF input does not work good at the moment, due to a third-party PDF-parsing library. Please, consider using the -x option!"
  ranges <- parseRanges pages
  P.withPdfFile inputFile $ \pdf -> do
    doc <- P.document pdf
    catalog <- P.documentCatalog doc
    rootNode <- P.catalogPageNode catalog
    count <- P.pageNodeNKids rootNode
    let pages = map (\n -> n - 1) $ filter (R.inRanges ranges) [1 .. count]
    glyphs :: [[P.Glyph]] <- mapM (extractPdfPageGlyphs rootNode) pages
    return $ zip pages glyphs

-- | get all the glyphs from a page using the pdf-toolbox
extractPdfPageGlyphs :: P.PageNode -> Int -> IO ([P.Glyph])
extractPdfPageGlyphs  rootNode n = do
  page <- P.pageNodePageByNum rootNode n
  spans <- P.pageExtractGlyphs page
  return $ concatMap P.spGlyphs spans


-- | Make integer ranges from the \"pages\" argument.
parseRanges :: String -> IO [R.Range Int]
parseRanges pages = do
  case (R.parseRanges pages)::(Either R.ParseError [R.Range Int]) of
    Left err -> do
      fail $ show err
    Right ranges -> return ranges


-- * Get the space insertion mechanism

-- | This returns a function for inserting inter-word spaces.
getSpaceInserter :: Glyph g => SpaceInsertion -> IO ([g] -> Either String [LeftSpacing g])
getSpaceInserter (WidthSpacingFactor fac) = do
  -- hPutStrLn stderr "Rule-based insertion of inter-word spaces based on glyph widths."
  return (Right . widthSpacingFactor fac)
getSpaceInserter (SizeSpacingFactor fac) = do
  -- hPutStrLn stderr "Rule-based insertion of inter-word spaces based on font sizes."
  return (Right . sizeSpacingFactor fac)
getSpaceInserter (ANNSpacing netFile) = do
  c <- B.readFile netFile
  trained :: SpacingNet <- reportErrors $ S.runGet S.get c
  hPutStrLn stderr ("Parsed ANN: " ++ (filter (/='\n') $ show trained))
  let fun = runSpacingNetOnLine trained
  return fun


-- | Linearize line of glyphs from spacing algorithm or error message.
toSpacedText :: Glyph g => Either String [LeftSpacing g] -> T.Text
toSpacedText (Left err) = "ERROR: " <> T.pack err
toSpacedText (Right glyphs) = leftSpacingGlyphsToText T.empty glyphs


-- * Get block categorization mechanism

getBlockCategorizer :: Glyph g => LineCategorizer -> IO (BlockCategorizer g a)
getBlockCategorizer (ByIndent opts) = do
  return (byIndent opts)
getBlockCategorizer (AsDefault top bottom) = do
  return defaultBlock


-- * Get linearization config

getLinearizationConfig :: LineCategorizer -> LinearizationOpts -> IO LinearizationOptions
getLinearizationConfig (ByIndent _) opts = do
  return $ plaintextLinearizationOptions
    & lo_Headline %~ (setOutput $ _linopts_head opts)
    & lo_Footline %~ (setOutput $ _linopts_foot opts)
    & lo_Custos %~ ((setOutput $ _linopts_custos opts) .
                    (setPre $ _linopts_preCustos opts))
    & lo_SheetSignature %~ ((setOutput $ _linopts_sheetSig opts) .
                            (setPre $ _linopts_preSheetSig opts))
    & lo_BlockQuote %~ (setPre $ _linopts_preQuote opts)
    & lo_FirstOfParagraph %~ (setPre $ _linopts_prePar opts)
    & lo_PageNumber %~ ((setPre $ _linopts_prePage opts) .
                        (setPost $ _linopts_postPage opts))
getLinearizationConfig (AsDefault top bottom) _ = do
  return plaintextLinearizationOptions



-- * Run the program

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> command_)
               (fullDesc
                <> progDesc "scannedb-ok is a tool for extracting the text from a PDF and was written to work for scanned books from books.google.com. It was designed to always correctly extract the LINES of the text, to insert SPACES even in complicated cases like emphasis with spaced letters and to repair SYLLABLE DIVISION at line breaks. There are commands for extracting text, training an artificial neural network, printing statistics etc. Type \"scannedb-ok COMMAND -h\" for information about a command."
                <> header "scannedb-ok -- A tool for extracting text from a PDF that even works for scanned books."
                <> footer "See also: https://github.com/lueck/scannedb-ok#readme")


-- | Run the extractor with the parsed command line arguments.
run :: ExtractionCommand -> IO ()

-- for performance reasons we do not use getGlyphs here
run (ExtractText PdfInput ranges lineOpts spacing lineCategorizer linearOpts nlp wp inFile) = do
  pages <- getPdfGlyphs ranges inFile
  spaceFun <- getSpaceInserter spacing
  blockFun <- getBlockCategorizer lineCategorizer
  linearizationConfig <- getLinearizationConfig lineCategorizer (nlpOutput nlp linearOpts)
  let processed = map (pageMap (map (fmap spaceFun))) $ -- insert spaces
                  blocksOfDoc blockFun id $   -- categorize blocks
                  map (findLinesWindowOnPage lineOpts) $ -- find lines
                  pages
  runStateT (runReaderT (linearize processed) linearizationConfig) []
  return ()
run (ExtractText PdfMinerXml ranges lineOpts spacing lineCategorizer linearOpts nlp wp inFile) = do
  pages <- getPdfMinerGlyphs ranges inFile
  spaceFun <- getSpaceInserter spacing
  blockFun <- getBlockCategorizer lineCategorizer
  linearizationConfig <- getLinearizationConfig lineCategorizer (nlpOutput nlp linearOpts)
  let processed = map (pageMap (map (fmap spaceFun))) $ -- insert spaces
                  blocksOfDoc blockFun id $   -- categorize blocks
                  map (findLinesWindowOnPage lineOpts) $ -- find lines
                  pages
  runStateT (runReaderT (linearize processed) linearizationConfig) []
  return ()

-- run (ExtractWords inMeth ranges lineOpts spacing lineCategorizer' nlp' inFile) = do
--   pages <- getGlyphs inMeth ranges inFile
--   spaceFun <- getSpaceInserter spacing
--   extractWords lineOpts (toSpacedText . spaceFun) (nlpOutput nlp' lineCategorizer') pages

run (NoSpaces inMeth ranges lineOpts inFile) = do
  pages <- getGlyphs inMeth ranges inFile
  let linearizationConfig = plaintextLinearizationOptions
      processed = map (findLinesWindowOnPage lineOpts) -- find lines
                  pages
  runStateT (runReaderT (linearize processed) linearizationConfig) []
  return ()

run (Info inMeth ranges lineOpts inFile) = do
  pages <- getGlyphs inMeth ranges inFile
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
                  printPageFeatures $ pageFeatures id lines)

run (ExtractGlyphs inMeth ranges lineOpts inFile) = do
  pages <- getGlyphs inMeth ranges inFile
  putStrLn "page,line,character,x-left,y-bottom,width,size"
  forM_ pages (\(page, glyphs) -> do
                  let lines = findLinesWindow lineOpts glyphs
                      lines_ = zip ([1..]::[Int]) $ map (sortOn xLeft) lines
                      glyphTuple = (,,,,,,) <$> const page <*> fst <*> (text . snd) <*> (xLeft .snd) <*> (yBottom .snd) <*> (width . snd) <*> (size . snd)
                      propagateNum num l = zip (repeat num) l
                  mapM_ (C.putStr . Csv.encode . (map glyphTuple) . (uncurry propagateNum)) lines_)

run (SpacingStats inMeth ranges lineOpts inFile) = do
  pages <- getGlyphs inMeth ranges inFile
  putStrLn "page,line,character,successor,x-dist,width-character,size-character,width-successor,size-successor"
  forM_ pages (\(page, glyphs) -> do
                  let lines = findLinesWindow lineOpts glyphs
                      lines_ = zip ([1..]::[Int]) $ map (sortOn xLeft) lines
                  mapM (C.putStr . Csv.encode . (uncurry (spacingsInLine page))) lines_)
    where
      spacingsInLine _ _ [] = []
      spacingsInLine _ _ (_:[]) = []
      spacingsInLine p l (g1:g2:gs) =
        ( p
        , l
        , (text g1)
        , (text g2)
        , ((xLeft g2) - (xLeft g1))
        , (width g1)
        , (size g1)
        , (width g2)
        , (size g2)):(spacingsInLine p l (g2:gs))

run (TrainSpacing inMeth ranges lineOpts lineCat iterations rate trainingPdf trainingTxt netFile) = do
  spaced <- T.readFile trainingTxt
  pages <- getGlyphs inMeth ranges trainingPdf
  blockFun :: (BlockCategorizer GlyphType GlyphType) <- getBlockCategorizer lineCat
  let glyphLines =
        map unwrapBlock $         -- drop block info
        filter keepBlock $        -- filter blocks
        concat $                  -- join page and line levels
        map snd $                 -- drop page numbers
        blocksOfDoc blockFun id $ -- categorize blocks
        map (findLinesWindowOnPage lineOpts) $ -- find lines
        pages
      txtLines = cleanForSpaceTraining spaced
      linesByLines = zip txtLines glyphLines
  hPutStr stderr "# Verifying training data..."
  td <- mapM reportErrors $ map (uncurry mkTrainingShapes22) linesByLines
  hPutStrLn stderr " done"
  hPutStrLn stderr "# Training..."
  initialNet <- randomSpacingNet
  trained <- foldM (runSpacingIteration stderr (concat td) Nothing rate) initialNet [1..iterations]
  -- hPutStrLn stderr " done (Training)"
  hPutStr stderr $ "# Dumping trained network to " ++ netFile ++ " ..."
  B.writeFile netFile $ S.runPut $ S.put trained
  hPutStrLn stderr " done"

run (ValidateSpacing inMeth ranges lineOpts spacing lineCategorizer txtFile inFile) = do
  pages <- getGlyphs inMeth ranges inFile
  spaceFun :: ([GlyphType] -> Either String [LeftSpacing GlyphType]) <-
    getSpaceInserter spacing
  blockFun <- getBlockCategorizer lineCategorizer
  -- linearizationConfig <- getLinearizationConfig lineCategorizer
  validationSpaced <- T.readFile txtFile
  let blockLines =
        map unwrapBlock $         -- drop block info
        filter keepBlock $        -- filter blocks
        concat $                  -- join page and line levels
        map snd $                 -- drop page numbers
        blocksOfDoc blockFun id $ -- categorize blocks
        map (findLinesWindowOnPage lineOpts) $ -- find lines
        pages
      validationLines = map (representSpacesAfter .
                             T.unpack) $
                        cleanForSpaceTraining validationSpaced
      processed = map (fmap (map leftSpacingToLabel) . spaceFun) blockLines -- insert spaces
  case (lefts processed) of
    [] -> return ()
    _ -> fail $ "ERROR: while inserting inter-word spaces:\n" ++
         (intercalate "\n" $ lefts processed)
  mapM_ (\(glyphs, gold) -> do
            case validateSameText glyphs gold of
              True -> do
                return ()
              False -> do
                fail $ "ERROR: input data does not match gold standard\n" ++
                  (T.unpack $ T.concat $ map (fromMaybe "" . text) glyphs) ++
                  "\n" ++
                  (map withoutSpace gold)
        ) $ zip blockLines validationLines
  C.putStr $
    Csv.encodeWith csvOptions $ (:[]) $
    (\d -> precisionLabel 1 d
           & prec_inputData .~ (Just inFile)
           & prec_goldStandard .~ (Just txtFile)
           & prec_method .~ (Just $ spacingMeta spacing)
    ) $
    zip (concat $ map (map leftSpacingToLabel) validationLines) (concat $ rights processed)
  where
    csvOptions = Csv.defaultEncodeOptions {
      Csv.encIncludeHeader = True
      }
    spacingMeta (WidthSpacingFactor _) = "width"
    spacingMeta (SizeSpacingFactor _) = "size"
    spacingMeta (ANNSpacing _) = "ANN"

run (ValidateForeignSpacing silverFile goldFile) = do
  silverSpaced <- T.readFile silverFile
  goldSpaced <- T.readFile goldFile
  let goldLines = map (representSpacesAfter . T.unpack) $
                  cleanForSpaceTraining goldSpaced
      silverLines = map (representSpacesAfter . T.unpack) $
                    cleanForSpaceTraining silverSpaced
  mapM_ (\(silver, gold) -> do
            let plainSilver = map withoutSpace silver
                plainGold = map withoutSpace gold
            case plainSilver == plainGold of
              True -> do
                return ()
              False -> do
                fail $ "ERROR: input data does not match gold standard\n" ++
                  plainSilver ++ "\n" ++ plainGold
        ) $ zip silverLines goldLines
  C.putStr $
    Csv.encodeWith csvOptions $ (:[]) $
    (\d -> precisionLabel 1 d
           & prec_inputData .~ (Just silverFile)
           & prec_goldStandard .~ (Just goldFile)
           & prec_method .~ (Just "foreign")
    ) $
    zip
    (concat $ map (map leftSpacingToLabel) goldLines)
    (concat $ map (map leftSpacingToLabel) silverLines)
  where
    csvOptions = Csv.defaultEncodeOptions {
      Csv.encIncludeHeader = True
      }

run _ = do
  fail "This command is not defined for this input type."


-- | Report the error for an 'Either' functor.
reportErrors :: Either String a -> IO a
reportErrors (Right r) = return r
reportErrors (Left err) = fail err

keepBlock :: BlockCategory a -> Bool
keepBlock (DefaultBlock a) = True
keepBlock (FirstOfParagraph a) = True
keepBlock (BlockQuote a) = True
keepBlock _ = False





-- extractWords :: (Show g, Eq g, Glyph g) =>
--   LineOptions ->                -- ^ count of lines etc.
--   ([g] -> T.Text) ->            -- ^ space insertion function
--   LineCategorizer ->            -- ^ config of line categorizer
--   [(Int, [g])] ->               -- ^ list of tuples of page number and
--                                 -- glyphs on this page
--   IO ()
-- extractWords lineOpts spaceFun (ByIndent byIndOpts linOpts sylOpts) pages = do
--   forM_ pages (\(page, glyphs) -> do
--                   let tokens = concatMap (tokenizeMiddle) $
--                                map (linearizeCategorizedLine linOpts spaceFun) $
--                                categorizeLines (byIndent byIndOpts) $
--                                findLinesWindow lineOpts glyphs
--                   mapM T.putStrLn tokens)
-- extractWords lineOpts spaceFun (AsDefault headlines' footlines') pages = do
--   forM_ pages (\(page, glyphs) -> do
--                   let tokens = concatMap (tokenizeMiddle) $
--                                map (linearizeLine spaceFun) $
--                                drop headlines' $
--                                dropFoot footlines' $
--                                findLinesWindow lineOpts glyphs
--                   mapM T.putStrLn tokens)


dropHead :: Glyph g => Int -> [[g]] -> [[g]]
dropHead 0 = id
dropHead n = drop n

dropFoot :: Glyph g => Int -> [[g]] -> [[g]]
dropFoot 0 = id
dropFoot n = reverse . drop n . reverse
