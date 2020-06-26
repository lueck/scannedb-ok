{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Pdf.Extract.Linearize where

-- | This module provides functions for linearizing processed
-- (categorized) data.

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import System.IO

import Pdf.Extract.Glyph
import Pdf.Extract.Lines


-- | A linearizing app can access the configuration and has a state.
type LinearizationApp a = ReaderT LinearizationOptions (StateT [Maybe LinearizationSymbol] IO) a


-- | A class for linearizable types.
class Linearizable a where
  linearize :: a -> LinearizationApp ()


instance Linearizable a => Linearizable [a] where
  linearize a = mapM_ linearize a


-- | Uniform structure of a config element for the linearization of a
-- categorized item.
type LinearizationTuple =
  ( Bool                      -- ^ whether or not to output at all
  , T.Text                    -- ^ opening text in output
  , T.Text                    -- ^ closing text in output
  , Maybe LinearizationSymbol -- ^ stack symbol to push on stack at opening
  , Maybe LinearizationSymbol -- ^ stack symbol to pull from stack at closing
  )

-- | Uniform structure of a config element for the linearization of a
-- categorized item.
type StatelessLinearizationTuple =
  ( T.Text                    -- ^ opening text in output
  , T.Text                    -- ^ closing text in output
  )

-- | Linearize (multiply stacked) categorizations.
linearizeWithState
  :: Linearizable a =>
     (LinearizationOptions -> LinearizationTuple) -- ^ access config field
  -> a                                            -- ^ linearizable inner content
  -> LinearizationApp ()
linearizeWithState getOption innerContent = do
  symStack <- get
  lOpts <- ask
  let (display, pre, post, preSym, postSym) = getOption lOpts
      output = loOutputHandle lOpts
      mode = loStackMode lOpts
  case display of
    False -> do
      return ()
    True -> do
      liftIO $ T.hPutStr output pre
      if isJust preSym
        then put (preSym:symStack)
        else put symStack -- FIXME: do nothing
      linearize innerContent
      (lastSym:symStack') <- get
      case mode of
        CFG -> do
          if isJust postSym && lastSym == postSym
            then put symStack'
            else fail "Error in linearization"
        otherwise -> do
          if isJust postSym && lastSym == postSym
            then put symStack'
            else put (lastSym:symStack') -- FIXME: do nothing
      liftIO $ T.hPutStr output post

-- | Same as 'linearizeWithState', but do not access state. This is
-- faster for categorizations, that can be linearized without stateful
-- processing, e.g. spacing.
linearizeWithoutState
  :: Linearizable a =>
     (LinearizationOptions -> StatelessLinearizationTuple)
  -> a
  -> LinearizationApp ()
linearizeWithoutState getOption innerContent = do
  lOpts <- ask
  let (pre, post) = getOption lOpts
      output = loOutputHandle lOpts
  liftIO $ T.hPutStr output pre
  linearize innerContent
  liftIO $ T.hPutStr output post

-- | Same as 'linearizeWithState', but for glyphs where it is neither
-- necessary to access the state nor pre and post strings.
linearizeGlyph :: Glyph g => g -> LinearizationApp ()
linearizeGlyph glyph = do
  lOpts <- ask
  let output = loOutputHandle lOpts
      missing = loMissingGlyphText lOpts
  liftIO $ T.hPutStr output $ fromMaybe missing $ text glyph


-- | Symbols for the linearization stack.
data LinearizationSymbol
  = DefaultBlockSymbol
  | ParagraphSymbol
  | CustosSymbol
  | SheetSignatureSymbol
  | HeadlineSymbol
  | FootlineSymbol
  | BlockQuoteSymbol
  deriving (Eq, Show)

data LinearizationStackMode
  = CFG   -- ^ output must conform to a context free grammar
  | Loose -- ^ loose output

-- | A configuration record for the linearization.
data LinearizationOptions = LinearizationOptions
  { loOutputHandle :: Handle
  , loStackMode :: LinearizationStackMode
  -- glyphs
  , loMissingGlyphText :: T.Text
  -- spacing classes
  -- , loNoSpace :: StatelessLinearizationTuple
  -- , loSpaceAfter :: StatelessLinearizationTuple
  -- , loSpaceBefore :: StatelessLinearizationTuple
  -- , loSpaceAround :: StatelessLinearizationTuple
  -- block classes
  , loDefaultBlock :: LinearizationTuple
  , loFirstOfParagraph :: LinearizationTuple
  , loCustos :: LinearizationTuple
  , loSheetSignature :: LinearizationTuple
  , loHeadline :: LinearizationTuple
  , loFootline :: LinearizationTuple
  , loBlockQuote :: LinearizationTuple
  }

-- | Config for linearization to plain text.
plaintextLinearizationOptions :: LinearizationOptions
plaintextLinearizationOptions = LinearizationOptions
  { loOutputHandle = stdout
  , loStackMode = Loose
  , loMissingGlyphText = "?"
  -- spacing classes
  -- , loNoSpace = ("", "")
  -- , loSpaceAfter = ("", " ")
  -- , loSpaceBefore = (" ", "")
  -- , loSpaceAround = (" ", " ")
  -- block classes
  , loDefaultBlock = (True, "", "\n", Nothing, Nothing)
  , loFirstOfParagraph = (True, "\t", "\n", Just ParagraphSymbol, Nothing)
  , loCustos = (False, "\t\t\t", "\n", Just CustosSymbol, Just CustosSymbol)
  , loSheetSignature = (False, "\t\t\t", "\n", Just SheetSignatureSymbol, Just SheetSignatureSymbol)
  , loHeadline = (False, "", "\n", Just HeadlineSymbol, Just HeadlineSymbol)
  , loFootline = (False, "", "\n", Just FootlineSymbol, Just FootlineSymbol)
  , loBlockQuote = (True, "\t\t", "\n", Just BlockQuoteSymbol, Just BlockQuoteSymbol)
  }

-- | Turn the output of a category ON.
outputOn :: LinearizationTuple -> LinearizationTuple
outputOn (_, pre, post, preSym, postSym) = (True, pre, post, preSym, postSym)

-- | Turn the output of a category OFF.
outputOff :: LinearizationTuple -> LinearizationTuple
outputOff (_, pre, post, preSym, postSym) = (False, pre, post, preSym, postSym)



-- * Categorize Lines

-- | A wrapper for categorizing lines of glyphs.
data (Glyph g) => LineCategory g
  = DefaultLine [g]             -- ^ somewhere in the middle of a paragraph
  | FirstOfParagraph [g]        -- ^ first line of a paragraph
  | Custos [g]                  -- ^ bottom line with first syllable
                                -- of next page, cf.
                                -- https://www.typografie.info/3/wiki.html/k/kustode-r328/
  | SheetSignature [g]          -- ^ bottom line with sheet a short
                                -- title and number of the sheet (or
                                -- custos)
  | Headline [g]                -- ^ first line of a page that matches
                                -- some features, e.g. presence of a
                                -- page number
  | Footline [g]                -- ^ last line that matches some features
  | BlockQuote [g]              -- ^ an indented quotation


data ByIndentOpts = ByIndentOpts
  { _byInd_parInd :: Double    -- ^ paragraph indent
  , _byInd_custInd :: Double   -- ^ indent of the custos in partion of the pagewidth
  , _byInd_sigInd :: Double    -- ^ indent of the sheet signature in partion of the pagewidth
  , _byInd_sigFill :: Double   -- ^ line filling of the sheet signature
  , _byInd_parseQuote :: Bool  -- ^ parse for block quotes
  , _byInd_dropMargin :: Bool  -- ^ drop glyphs outside of the type area
  }

-- | Categorize lines by applying a categorization function.
categorizeLines :: Glyph g =>
                   ([([g], Int, LineData)] -> [LineCategory g])
                -> [[g]]
                -> [LineCategory g]
categorizeLines f lines = f $ zip3 lines [1..] $ genLineInfo lines


-- | Categorize lines by indent and some other features.
byIndent :: Glyph g =>
            ByIndentOpts           -- ^ Options
         -> [([g], Int, LineData)] -- ^ the lines and line data
         -> [LineCategory g]
byIndent _ [] = []
byIndent opts ((line', count, ldata):ls)
  | (count == 1) &&
    (containsNumbers line) =
    -- TODO: head skip exceeds baseline skip
    (Headline line):[] ++ byIndent opts ls
  | (count == lastLine) &&
    indent > custInd * pageWidth &&
    -- we also use custInd for a filling criterion:
    (lineFill < (custFill * maxLineFill)) =
    (Custos line):[] ++ byIndent opts ls
  | (count == lastLine) &&
    indent > (_byInd_sigInd opts) * pageWidth &&
    (lineFill < (_byInd_sigFill opts * maxLineFill)) =
    (SheetSignature line):[] ++ byIndent opts ls
  | (count == lastLine) &&
    (containsNumbers line) =
    -- TODO: foot skip exceeds baseline skip
    (Footline line):[] ++ byIndent opts ls
  | (_byInd_parseQuote opts) &&
    -- TODO: definitively more context needed
    indent > 0 &&
    (_line_glyphSize ldata) < (_line_glyphSizeLowerBound ldata) =
    (BlockQuote line):[] ++ byIndent opts ls
  | indent > 0 =
    (FirstOfParagraph line):[] ++ byIndent opts ls
  | otherwise =
    (DefaultLine line):[] ++ byIndent opts ls
  where
    line = if _byInd_dropMargin opts
           then filter inTypeArea line'
           else line'
    inTypeArea = (\g -> (xLeft g >= _line_leftBorderLowerBound ldata) &&
                        (xRight g <= _line_rightBorderUpperBound ldata))
    indent = _line_left ldata - _line_leftBorderUpperBound ldata
    pageWidth = _line_rightBorderUpperBound ldata - _line_leftBorderLowerBound ldata
    custFill = 1.2 - custInd -- 1 + 0.2 for secure matching
    custInd = _byInd_custInd opts
    lineFill = fromIntegral $ _line_glyphsInLine ldata
    maxLineFill = fromIntegral $ _line_maxGlyphs ldata
    lastLine = _line_linesOnPage ldata


-- | Returns True if the given line of glyphs contains at least one number.
containsNumbers :: Glyph g => [g] -> Bool
containsNumbers [] = False
containsNumbers (g:gs)
  -- | (T.head $ text g) `elem` ['0'..'9'] = True
  | fromMaybe False $ fmap ((flip elem ['0'..'9']) . T.head) $ text g = True
  | otherwise = containsNumbers gs


-- | Treat every line as a line from the middle of a paragraph. Use
-- this if you don't want to categorize the lines at all.
asDefault :: Glyph g => [([g], Int, LineData)] -> [LineCategory g]
asDefault [] = []
asDefault ((line, count, ldata):ls) = (DefaultLine line):[] ++ asDefault ls


-- * Linearize line.

data KeepDrop = Keep2 | Drop2

data KeepDropPart = Keep3 | Drop3 | Part3

data LinearizationOpts = LinOpts
  { _linopts_head :: KeepDropPart
  , _linopts_foot :: KeepDropPart
  , _linopts_custos :: KeepDrop
  , _linopts_sheetSig :: KeepDrop
  , _linopts_prePage :: T.Text
  , _linopts_postPage :: T.Text
  , _linopts_prePar :: T.Text
  , _linopts_preCustos :: T.Text
  , _linopts_preSheetSig :: T.Text
  , _linopts_preQuote :: T.Text
  }

makeLenses ''LinearizationOpts

-- | Convient settings for NLP.
nlpLike :: LinearizationOpts -> LinearizationOpts
nlpLike opts = opts
               -- & linopts_head .~ Part3
               -- & linopts_foot .~ Part3
               & linopts_custos .~ Drop2
               & linopts_sheetSig .~ Drop2
               & linopts_prePar .~ "\n"
               & linopts_preSheetSig .~ ""
               & linopts_preQuote .~ ""


-- | Linearize a categorized line.
--
-- This requires a function for inserting spaces as the first
-- argument.
--
-- Use @(T.concat . mapMaybe (glyphText))@ for no spacing at all.
--
-- Use `spacingFactor` @(spacingFactor 1.8)@ for spacing on the basis
-- of the width of a glyph.
linearizeCategorizedLine :: Glyph g =>
  LinearizationOpts ->          -- ^ options
  ([g] -> T.Text) ->            -- ^ linearization function
  (LineCategory g) ->           -- ^ categorized lines
  T.Text
linearizeCategorizedLine opts f (FirstOfParagraph glyphs) =
  _linopts_prePar opts  <> linearizeLine f glyphs <> "\n"
linearizeCategorizedLine opts f (DefaultLine glyphs) =
  linearizeLine f glyphs <> "\n"
linearizeCategorizedLine opts f (BlockQuote glyphs) =
  _linopts_preQuote opts  <> linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_custos = Drop2} f (Custos glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_custos = Keep2, _linopts_preCustos = pre}
  f (Custos glyphs) =
  pre <> linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_sheetSig = Drop2} f (SheetSignature glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_sheetSig = Keep2, _linopts_preSheetSig = pre}
  f (SheetSignature glyphs) =
  pre <> linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_head = Drop3} f (Headline glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_head = Keep3} f (Headline glyphs) =
  linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_head = Part3,
                                 _linopts_prePage = pre,
                                 _linopts_postPage = post}
  f (Headline glyphs) =
  -- FIXME: filtering may not be enough, because there might be
  -- section numbers in the headline
  pre <> (T.filter (`elem` ['0'..'9']) $ linearizeLine f glyphs) <> post <> " "
linearizeCategorizedLine LinOpts{_linopts_foot = Drop3} f (Footline glyphs) = ""
linearizeCategorizedLine LinOpts{_linopts_foot = Keep3} f (Footline glyphs) =
  linearizeLine f glyphs <> "\n"
linearizeCategorizedLine LinOpts{_linopts_foot = Part3,
                                 _linopts_prePage = pre,
                                 _linopts_postPage = post}
  f (Footline glyphs) =
  pre <> (T.filter (`elem` ['0'..'9']) $ linearizeLine f glyphs) <> post <> "\n"
  -- There must be a newline before form feed, doesn't it?


-- | Linearize the glyphs of a line using a serialization function.
--
-- This requires a function for inserting spaces as the first
-- argument.
--
-- Use @(T.concat . mapMaybe (glyphText))@ for no spacing at all.
--
-- Use `spacingFactor` @(spacingFactor 1.8)@ for spacing on the basis
-- of the width of a glyph.
linearizeLine :: Glyph g => ([g] -> T.Text) -> [g] -> T.Text
linearizeLine f = f . sortOn xLeft
