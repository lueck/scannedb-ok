{-# LANGUAGE OverloadedStrings #-}
module Pdf.Extract.Block where

-- | This module provides categorization classes and categorizers for
-- tagging content on a block-level, i.e. lines. The term block-level
-- structure elements is used in sec. 14.8 of the PDF 1.7
-- reference. Here, it is used for lines as a whole, in contrast to
-- inline-level portions of a line.

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Lens

import Pdf.Extract.Glyph
import Pdf.Extract.Lines


-- * Representation of categorized lines

-- | A wrapper for categorizing lines.
data LineCategory a
  = DefaultLine a               -- ^ somewhere in the middle of a paragraph
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

instance Functor LineCategory where
  fmap f (DefaultLine a) = DefaultLine (f a)
  fmap f (FirstOfParagraph a) = FirstOfParagraph (f a)
  fmap f (Custos a) = Custos (f a)
  fmap f (SheetSignature a) = SheetSignature (f a)
  fmap f (Headline a) = Headline (f a)
  fmap f (Footline a) = Footline (f a)
  fmap f (BlockQuote a) = BlockQuote (f a)


-- * Collecting features

