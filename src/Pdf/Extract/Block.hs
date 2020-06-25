{-# LANGUAGE OverloadedStrings #-}
module Pdf.Extract.Block where

-- | This module provides categorization classes and categorizers for
-- tagging content on a block-level, i.e. lines. The term block-level
-- structure elements is used in sec. 14.8 of the PDF 1.7 reference as
-- an abstraction from western scripts. Here, it is used for lines as
-- a whole, in contrast to inline-level portions of a line.


import Pdf.Extract.Glyph
import Pdf.Extract.Utils
import Pdf.Extract.Linearize (ByIndentOpts) -- deprecated


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
  | BlockQuote a                -- ^ an indented quotation

instance Functor BlockCategory where
  fmap f (DefaultBlock a) = DefaultBlock (f a)
  fmap f (FirstOfParagraph a) = FirstOfParagraph (f a)
  fmap f (Custos a) = Custos (f a)
  fmap f (SheetSignature a) = SheetSignature (f a)
  fmap f (Headline a) = Headline (f a)
  fmap f (Footline a) = Footline (f a)
  fmap f (BlockQuote a) = BlockQuote (f a)


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
-- case 'fmap' would be a way to access the glyph and keep the spacing
-- intact.
newtype Glyph g => BlockCategorizer g a = BlockCategorizer
  ((a -> g)                -- ^ glyph accessor function, e.g. 'id' or a 'fmap'
  -> DocFeatures           -- ^ overall features of the document
  -> PageFeatures          -- ^ overall features of the page
  -> [[BlockCategory [a]]] -- ^ pages before
  -> Maybe [[a]]           -- ^ next page (look ahead)
  -> [BlockCategory [a]]   -- ^ categorized lines from before (accumulator)
  -> [a]                   -- ^ the current line
  -> Maybe [a]             -- ^ next line and number (look ahead)
  -> [BlockCategory [a]])

-- | Categorize lines of multiple pages.
--
-- Usage:
--
-- @blocksOfDoc (BlockCategorizer (yourCategorizerFunction options)) pages@
blocksOfDoc :: Glyph g =>
               BlockCategorizer g a -- ^ categorizer function
            -> (a -> g)             -- ^ glyph accessor, e.g. 'id' or a 'fmap'
            -> [[[a]]]              -- ^ list of pages
            -> [[BlockCategory [a]]]
blocksOfDoc f getGlyph pages =
  foldlWithNext (blocksOfPage f getGlyph (docFeatures getGlyph pages)) [] pages

-- | Categorize lines of single page.
blocksOfPage :: Glyph g =>
                BlockCategorizer g a  -- ^ categorizer function
             -> (a -> g)              -- ^ glyph accessor, e.g. 'id' or a 'fmap'
             -> DocFeatures           -- ^ global features
             -> [[BlockCategory [a]]] -- ^ categorized pages from before
             -> [[a]]                 -- ^ lines of page
             -> Maybe [[a]]           -- ^ next page
             -> [[BlockCategory [a]]]
blocksOfPage (BlockCategorizer f) getGlyph doc done lines nextpage =
  (foldlWithNext (f getGlyph doc (pageFeatures getGlyph lines) done nextpage) [] lines):done


-- * Feature extraction

-- | Global features of a document that are relevant for block
-- categorization.
data DocFeatures = DocFeatures

-- | Get global features of a document.
docFeatures :: Glyph g => (a -> g) -> [[[a]]] -> DocFeatures
docFeatures getGlyph pages = DocFeatures

-- | Global features of a page that are relevant for block
-- categorization.
data PageFeatures = PageFeatures
  { pfLinesCount :: Int
  }

-- | Get global features of a page.
pageFeatures :: Glyph g =>
                (a -> g) -- ^ glyph accessor
             -> [[a]]    -- ^ lines of the page
             -> PageFeatures
pageFeatures getGlyph lines = PageFeatures
  { pfLinesCount = length lines
  }


-- * Rule based block categorizers

-- | Do not categorize blocks at all, i.e. make all lines a
-- 'DefaultBlock'.
--
-- Usage for a whole document:
--
-- @blocksOfDoc (BlockCategorizer defaultBlock) pages@
defaultBlock
  :: Glyph g =>
     (a -> g)              -- ^ glyph accessor function, e.g. 'id' or a 'fmap'
  -> DocFeatures           -- ^ overall features of the document
  -> PageFeatures          -- ^ overall features of the page
  -> [[BlockCategory [a]]] -- ^ pages from before
  -> Maybe [[a]]           -- ^ next page
  -> [BlockCategory [a]]   -- ^ categorized lines from before
  -> [a]                   -- ^ the line
  -> Maybe [a]             -- ^ next line
  -> [BlockCategory [a]]
defaultBlock _ _ _ _ _ before line _ = (DefaultBlock line):before


-- | Categorize blocks by indentation, horizontal filling etc. This is
-- a rule-based categorization algorithm.
--
-- 'blockByIndent' categorizes a single line. Usage for a whole
-- document:
--
-- @blocksOfDoc (BlockCategorizer (blockByIndent options)) fmap pages@
blockByIndent
  :: Glyph g =>
     ByIndentOpts          -- ^ user-defined options
  -> (a -> g)              -- ^ glyph accessor function, e.g. 'id' or a 'fmap'
  -> DocFeatures           -- ^ overall features of the document
  -> PageFeatures          -- ^ overall features of the page
  -> [[BlockCategory [a]]] -- ^ pages from before
  -> Maybe [[a]]           -- ^ next page
  -> [BlockCategory [a]]   -- ^ categorized lines from before
  -> [a]                   -- ^ the line
  -> Maybe [a]             -- ^ next line
  -> [BlockCategory [a]]
blockByIndent opts getGlyph doc page pagesBefore nextPage linesBefore line nextline =
  (DefaultBlock line) : linesBefore
  where
    lineNum = 1 + length linesBefore


-- categorizeByIndent :: Glyph g => ByIndentOpts -> [[[g]]] -> [[BlockCategory [g]]]
-- categorizeByIndent opts pages = blocksOfDoc (BlockCategorizer (blockByIndent opts)) id pages
