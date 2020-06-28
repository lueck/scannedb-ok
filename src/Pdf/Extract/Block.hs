module Pdf.Extract.Block where

-- | This module provides categorization classes and categorizers for
-- tagging content on a block-level, i.e. lines. The term block-level
-- structure elements is used in sec. 14.8 of the PDF 1.7 reference as
-- an abstraction from western scripts. Here, it is used for lines as
-- a whole, in contrast to inline-level portions of a line.

import qualified Data.Text as T
import Data.Char
import Data.Maybe
import Data.List

import Pdf.Extract.Glyph
import Pdf.Extract.Utils
import Pdf.Extract.Linearize
import Pdf.Extract.Clustering
import Pdf.Extract.Lines


-- * Representation of categorized blocks

-- | Classes for categorizing blocks (lines).
data BlockCategory a
  = DefaultBlock a              -- ^ somewhere in the middle of a paragraph
  | FirstOfParagraph a          -- ^ first line of a paragraph
  | Custos a                    -- ^ bottom line with first syllable
                                -- of next page, cf.
                                -- https://www.typografie.info/3/wiki.html/k/kustode-r328/
  | SheetSignature a            -- ^ bottom line with sheet a short
                                -- title and number of the sheet (or
                                -- custos)
  | Headline a                  -- ^ first line of a page that matches
                                -- some features, e.g. presence of a
                                -- page number
  | Footline a                  -- ^ last line that matches some features
  | BlockQuote a                -- ^ an indented quotatio

instance Functor BlockCategory where
  fmap f (DefaultBlock a) = DefaultBlock (f a)
  fmap f (FirstOfParagraph a) = FirstOfParagraph (f a)
  fmap f (Custos a) = Custos (f a)
  fmap f (SheetSignature a) = SheetSignature (f a)
  fmap f (Headline a) = Headline (f a)
  fmap f (Footline a) = Footline (f a)
  fmap f (BlockQuote a) = BlockQuote (f a)

instance Linearizable a => Linearizable (BlockCategory a) where
  linearize (DefaultBlock a) = linearizeWithState _lo_DefaultBlock a
  linearize (FirstOfParagraph a) = linearizeWithState _lo_FirstOfParagraph a
  linearize (Custos a) = linearizeWithState _lo_Custos a
  linearize (SheetSignature a) = linearizeWithState _lo_SheetSignature a
  linearize (Headline a) = linearizeWithState _lo_Headline a
  linearize (Footline a) = linearizeWithState _lo_Footline a
  linearize (BlockQuote a) = linearizeWithState _lo_BlockQuote a


-- | Get the item from the labelled block.
unwrapBlock :: BlockCategory a -> a
unwrapBlock (DefaultBlock a) = a
unwrapBlock (FirstOfParagraph a) = a
unwrapBlock (Custos a) = a
unwrapBlock (SheetSignature a) = a
unwrapBlock (Headline a) = a
unwrapBlock (Footline a) = a
unwrapBlock (BlockQuote a) = a


-- * Calling categorizers

-- | A 'BlockCategorizer' is a categorization function with a uniform
-- signature.
--
-- A categorizer function is to be used in a fold (see 'blocksOfDoc'
-- and 'blocksOfPage') over the lines over a page. It has to
-- categorize a single line. For categorizing in context, there are
-- multiple features accessible: global document and page-wide
-- features. The context produced while categorizing in block
-- processing direction (cf. sec. 14.8.3 of the PDF 1.7
-- specification), i.e. the categorization result from *before*, is
-- accessible from within the categorizer function via the
-- accumulator. Also the processed pages from before are
-- accessible. For a look-ahead categorization, the next line and the
-- next page are accessible, too.
--
-- To keep the stack of categorization processes for text extraction
-- flexible, a glyph accessor function has to be passed. Imagine that
-- insertion of inter-word spaces has been processed before. In this
-- case 'withoutSpace' would be a way to access the glyph and keep the
-- spacing intact.
type BlockCategorizer g a =
  ((a -> g)                -- ^ glyph accessor function, e.g. 'id'
  -> DocFeatures           -- ^ overall features of the document
  -> PageFeatures          -- ^ overall features of the page
  -> [Page (BlockCategory [a])] -- ^ pages before
  -> Maybe [[a]]           -- ^ next page (look ahead)
  -> [BlockCategory [a]]   -- ^ categorized lines from before (accumulator)
  -> [a]                   -- ^ the current line
  -> [[a]]                 -- ^ rest of lines of page (look ahead)
  -> [BlockCategory [a]])

-- | Categorize lines of multiple pages.
--
-- Usage:
--
-- @blocksOfDoc (yourCategorizerFunction options) pages@
blocksOfDoc :: Glyph g =>
               BlockCategorizer g a -- ^ categorizer function
            -> (a -> g)             -- ^ glyph accessor, e.g. 'id'
            -> [Page [a]]              -- ^ list of pages
            -> [Page (BlockCategory [a])]
blocksOfDoc f getGlyph pages =
  foldlWithNext' (blocksOfPage f getGlyph (docFeatures getGlyph $ map snd pages)) [] pages

-- | Categorize lines of single page.
blocksOfPage :: Glyph g =>
                BlockCategorizer g a  -- ^ categorizer function
             -> (a -> g)              -- ^ glyph accessor, e.g. 'id'
             -> DocFeatures           -- ^ global features
             -> [Page (BlockCategory [a])] -- ^ categorized pages from before
             -> Page [a]              -- ^ lines of page
             -> Maybe (Page [a])      -- ^ next page
             -> [Page (BlockCategory [a])]
blocksOfPage f getGlyph doc done lines nextpage =
  done ++
  [pageMap
   (foldlWithRest'
    (f getGlyph doc (pageFeatures getGlyph $ snd lines) done (fmap snd nextpage))
    [])
   lines]


-- * Feature extraction

-- | Global features of a document that are relevant for block
-- categorization.
data DocFeatures = DocFeatures
  { dfGlyphSizeLowerBound :: Double -- ^ lower bound of predominant glyph size (font size)
  , dfGlyphSizeUpperBound :: Double -- ^ upper bound of predominant glyph size
  , dfGlyphSizeClusters :: [[Double]] -- ^ glyph size clusters
  }

-- | Get global features of a document.
docFeatures :: Glyph g => (a -> g) -> [[[a]]] -> DocFeatures
docFeatures getGlyph pages = DocFeatures
  { dfGlyphSizeLowerBound = glyphSizeUpperBound
  , dfGlyphSizeUpperBound = glyphSizeLowerBound
  , dfGlyphSizeClusters = glyphSizeClusters
  }
  where
    glyphFeatures = map (size . getGlyph) $ concat $ concat pages
    tallestGlyph = ceiling $ foldl max 0 glyphFeatures
    -- Clustering glyph size:
    glyphSizeClusters = slidingWindow1D (tallestGlyph * 10) 0 False id 0 (fromIntegral tallestGlyph) glyphFeatures
    glyphSizeCluster = longest glyphSizeClusters
    glyphSizeUpperBound = foldl max 0 glyphSizeCluster
    glyphSizeLowerBound = foldl min glyphSizeUpperBound glyphSizeCluster


-- | Global features of a page that are relevant for block
-- categorization.
data PageFeatures = PageFeatures
  { pfLinesCount :: Int
  , pfMaxGlyphs :: Int
  , pfLeftBorderLowerBound :: Double
  , pfLeftBorderUpperBound :: Double
  , pfLinesAtLeftBorder :: Int
  , pfLeftBorderClusters :: [[Double]]
  , pfRightBorderLowerBound :: Double
  , pfRightBorderUpperBound :: Double
  , pfLinesAtRightBorder :: Int
  , pfLineWidth :: Double
  , pfRightBorderClusters :: [[Double]]
  , pfGlyphSizeLowerBound :: Double
  , pfGlyphSizeUpperBound :: Double
  , pfGlyphSizeClusters :: [[Double]]
  }

-- | Get global features of a page.
pageFeatures :: Glyph g =>
                (a -> g) -- ^ glyph accessor
             -> [[a]]    -- ^ lines of the page
             -> PageFeatures
pageFeatures getGlyph lines = PageFeatures
  { pfLinesCount = length lines
  , pfMaxGlyphs = mostGlyphs
  , pfLeftBorderLowerBound = leftBorderLowerBound
  , pfLeftBorderUpperBound = leftBorderUpperBound
  , pfLinesAtLeftBorder = leftBorderSize
  , pfLeftBorderClusters = leftBorderClusters
  , pfRightBorderLowerBound = foldl min rightBorderUpperBound rightBorderCluster
  , pfRightBorderUpperBound = rightBorderUpperBound
  , pfLinesAtRightBorder = rightBorderSize
  , pfRightBorderClusters = rightBorderClusters
  , pfLineWidth = rightBorderUpperBound - leftBorderLowerBound
  , pfGlyphSizeLowerBound = glyphSizeUpperBound
  , pfGlyphSizeUpperBound = glyphSizeLowerBound
  , pfGlyphSizeClusters = glyphSizeClusters
  }
  where
    lineTuples = map (mkLineTuple getGlyph) lines
    getLeft (l, _, _, _) = l
    getRight (_, r, _, _) = r
    getSize (_, _, s, _) = s
    getCount (_, _, _, c) = c
    mostLeft = foldl1 min $ map getLeft lineTuples
    mostRight = foldl1 max $ map getRight lineTuples
    mostGlyphs = foldl max 0 $ map getCount lineTuples
    tallestGlyph = ceiling $ foldl max 0 $ map getSize lineTuples
    -- Left Border: We assume that the non-indented lines make the
    -- biggest cluster. And the lower (right) bound of this cluster is
    -- assumed to be the left border and the upper bound is used to
    -- identify indented lines.
    leftBorderClusters = slidingWindow1D (4 * mostGlyphs) 0 False id 0 mostRight $
                         map getLeft lineTuples
    (leftBorderSize, leftBorderCluster) = longest' leftBorderClusters
    leftBorderUpperBound = foldl max 0 leftBorderCluster
    leftBorderLowerBound = foldl min leftBorderUpperBound leftBorderCluster
    -- Right Border:
    rightBorderClusters = slidingWindow1D (4 * mostGlyphs) 0 False id 0 mostRight $
                          map getRight lineTuples
    (rightBorderSize, rightBorderCluster) = longest' rightBorderClusters
    rightBorderUpperBound = foldl max 0 rightBorderCluster
    -- Clustering glyph size:
    glyphSizeClusters = slidingWindow1D (tallestGlyph * 10) 0 False id 0 (fromIntegral tallestGlyph) $
                        map getSize lineTuples
                        -- map (\l -> getSize l / (fromIntegral $ getCount l)) lineTuples
    glyphSizeCluster = longest glyphSizeClusters
    glyphSizeUpperBound = foldl max 0 glyphSizeCluster
    glyphSizeLowerBound = foldl min glyphSizeUpperBound glyphSizeCluster

-- | Map glyphs of a line to a feature tuple.
mkLineTuple :: Glyph g => (a -> g) -> [a] -> (Double, Double, Double, Int)
mkLineTuple getGlyph =
  foldl' accGlyphs (1000, 0, 0, 0) . map (glyphTriple . getGlyph)

glyphTriple :: Glyph g => g -> (Double, Double, Double)
glyphTriple = (,,) <$> xLeft <*> xRight <*> size
{-# INLINE glyphTriple #-}

accGlyphs :: (Double, Double, Double, Int)
          -> (Double, Double, Double)
          -> (Double, Double, Double, Int)
accGlyphs (left, right, size, count) (l, r, s) =
  ( min left l
  , max right r
  , max size s
  , count + 1)
{-# INLINE accGlyphs #-}

-- | Print a list of 'PageFeatures' in the IO monad.
printPageFeatures :: PageFeatures -> IO ()
printPageFeatures infos = do
  putStr "#Lines: "
  print $ pfLinesCount infos
  --putStr "Glyphs per Lines: "
  --print $ map _line_glyphsInLine infos
  --
  putStr "Left border (lower bound): "
  print $ show $ pfLeftBorderLowerBound infos
  putStr "Left border (upper bound): "
  print $ show $ pfLeftBorderUpperBound infos
  putStr "Count of lines starting there: "
  print $ pfLinesAtLeftBorder infos
  putStr "Left border clusters: "
  print $ show $ pfLeftBorderClusters infos
  -- putStr "Left most glyph of lines: "
  -- print $ map _line_left infos
  --
  putStr "Right border, lines starting there: "
  print $ show $ pfLinesAtRightBorder infos
  putStr "Right border (upper bound): "
  print $ show $ pfRightBorderUpperBound infos
  putStr "Right border clusters: "
  print $ show $ pfRightBorderClusters infos
  --
  putStr "Glyph Size (lower bound of biggest cluster): "
  print $ show $ pfGlyphSizeLowerBound infos
  putStr "Glyph size clusters: "
  print $ show $ pfGlyphSizeClusters infos
  -- putStr "Glyph sizes of lines: "
  -- print $ map _line_glyphSize infos


-- * Rule based block categorizers

-- | Do not categorize blocks at all, i.e. make all lines a
-- 'DefaultBlock'.
--
-- Usage for a whole document:
--
-- @blocksOfDoc defaultBlock pages@
defaultBlock
  :: Glyph g =>
     (a -> g)              -- ^ glyph accessor function, e.g. 'id'
  -> DocFeatures           -- ^ overall features of the document
  -> PageFeatures          -- ^ overall features of the page
  -> [Page (BlockCategory [a])] -- ^ pages from before
  -> Maybe [[a]]           -- ^ next page
  -> [BlockCategory [a]]   -- ^ categorized lines from before
  -> [a]                   -- ^ the line
  -> [[a]]                 -- ^ lines after
  -> [BlockCategory [a]]
defaultBlock _ _ _ _ _ before line _ = before ++ [DefaultBlock line]


-- | Categorize blocks by indentation, horizontal filling etc. This is
-- a rule-based categorization algorithm.
--
-- 'byIndent' categorizes a single line. Usage for a whole document:
--
-- @blocksOfDoc (byIndent options) id pages@
byIndent
  :: Glyph g =>
     ByIndentOpts          -- ^ user-defined options
  -> (a -> g)              -- ^ glyph accessor function, e.g. 'id'
  -> DocFeatures           -- ^ overall features of the document
  -> PageFeatures          -- ^ overall features of the page
  -> [Page (BlockCategory [a])] -- ^ pages from before
  -> Maybe [[a]]           -- ^ next page
  -> [BlockCategory [a]]   -- ^ categorized lines from before
  -> [a]                   -- ^ the line
  -> [[a]]                 -- ^ lines after
  -> [BlockCategory [a]]
byIndent opts getGlyph doc page pagesBefore nextPage linesBefore line linesAfter
  | (count == 1) &&
    containsNumbersP =
    -- TODO: head skip exceeds baseline skip
    linesBefore ++ [Headline line]
  | (count == lastLine) &&
    indent > custInd * pageWidth &&
    -- we also use custInd for a filling criterion:
    (lineFill < (custFill * maxLineFill)) =
    linesBefore ++ [Custos line]
  | (count == lastLine) &&
    indent > (_byInd_sigInd opts) * pageWidth &&
    (lineFill < (_byInd_sigFill opts * maxLineFill)) =
    linesBefore ++ [SheetSignature line]
  | (count == lastLine) &&
    containsNumbersP =
    -- TODO: foot skip exceeds baseline skip
    linesBefore ++ [Footline line]
  | (_byInd_parseQuote opts) &&
    -- TODO: definitively more context needed
    indent > 0 &&
    size < (pfGlyphSizeLowerBound page) =
    linesBefore ++ [BlockQuote line]
  | indent > 0 =
    linesBefore ++ [FirstOfParagraph line]
  | otherwise =
    linesBefore ++ [DefaultBlock line]
  where
    count = 1 + length linesBefore
    glyphs' = map getGlyph line
    glyphs = if _byInd_dropMargin opts
             then filter inTypeArea glyphs'
             else glyphs'
    lineText = T.concat $ mapMaybe text glyphs
    containsNumbersP = 0 < longestSubstringWith isNumber lineText ||
                       0 < longestRomanNumber lineText
    inTypeArea = (\g -> (xLeft g >= pfLeftBorderLowerBound page) &&
                        (xRight g <= pfRightBorderUpperBound page))
    (left, right, size, glyphsInLine) = mkLineTuple id glyphs
    indent = left - pfLeftBorderUpperBound page
    pageWidth = pfLineWidth page
    custFill = 1.2 - custInd -- 1 + 0.2 for secure matching
    custInd = _byInd_custInd opts
    lineFill = fromIntegral glyphsInLine
    maxLineFill = fromIntegral $ pfMaxGlyphs page
    lastLine = pfLinesCount page


-- | Config for 'byIndent'.
data ByIndentOpts = ByIndentOpts
  { _byInd_parInd :: Double    -- ^ paragraph indent
  , _byInd_custInd :: Double   -- ^ indent of the custos in partion of the pagewidth
  , _byInd_sigInd :: Double    -- ^ indent of the sheet signature in partion of the pagewidth
  , _byInd_sigFill :: Double   -- ^ line filling of the sheet signature
  , _byInd_parseQuote :: Bool  -- ^ parse for block quotes
  , _byInd_dropMargin :: Bool  -- ^ drop glyphs outside of the type area
  }


-- categorizeByIndent :: Glyph g => ByIndentOpts -> [[[g]]] -> [[BlockCategory [g]]]
-- categorizeByIndent opts pages = blocksOfDoc (byIndent opts) id pages
