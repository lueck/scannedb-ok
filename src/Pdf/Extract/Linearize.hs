{-# LANGUAGE
OverloadedStrings,
TemplateHaskell,
TypeSynonymInstances,
FlexibleInstances #-}
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
import Control.Lens

import Pdf.Extract.Glyph
import Pdf.Extract.Lines


-- | A linearizing app can access the configuration and has a state.
type LinearizationApp a = ReaderT LinearizationOptions (StateT [Maybe LinearizationSymbol] IO) a


-- | A class for linearizable types.
class Linearizable a where
  linearize :: a -> LinearizationApp ()


instance Linearizable a => Linearizable [a] where
  linearize a = mapM_ linearize a

instance (Linearizable a, Show b) => Linearizable (Either b a) where
  linearize (Left err) = liftIO $ fail $ show err
  linearize (Right a) = linearize a

instance (Linearizable a) => Linearizable (Page a) where
  linearize (_, xs) = linearizeWithState _lo_Page xs

instance Linearizable GlyphType where
  linearize (MkGlyphType g) = do
    lOpts <- ask
    let missing = _lo_MissingGlyphText lOpts
        output = _lo_OutputHandle lOpts
    liftIO $ T.hPutStr output $ fromMaybe missing $ text g


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
      output = _lo_OutputHandle lOpts
      mode = _lo_StackMode lOpts
      top [] = Nothing
      top (x:_) = x
  case display of
    False -> do
      return ()
    True -> do
      liftIO $ T.hPutStr output pre
      if isJust preSym
        then put (preSym:symStack)
        else put symStack -- FIXME: do nothing
      linearize innerContent
      symStack' <- get
      let lastSym = top symStack'
      case mode of
        CFG -> do
          if isJust postSym && lastSym == postSym
            then put (tail symStack')
            else fail "Error in linearization"
        otherwise -> do
          if isJust postSym && lastSym == postSym
            then put (tail symStack')
            else put symStack' -- FIXME: do nothing
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
      output = _lo_OutputHandle lOpts
  liftIO $ T.hPutStr output pre
  linearize innerContent
  liftIO $ T.hPutStr output post

-- | Same as 'linearizeWithState', but for glyphs where it is neither
-- necessary to access the state nor pre and post strings.
linearizeGlyph :: Glyph g => g -> LinearizationApp ()
linearizeGlyph glyph = do
  lOpts <- ask
  let output = _lo_OutputHandle lOpts
      missing = _lo_MissingGlyphText lOpts
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
  | PageNumberSymbol
  deriving (Eq, Show)

data LinearizationStackMode
  = CFG   -- ^ output must conform to a context free grammar
  | Loose -- ^ loose output

-- | A configuration record for the linearization.
data LinearizationOptions = LinearizationOptions
  { _lo_OutputHandle :: Handle
  , _lo_StackMode :: LinearizationStackMode
  -- glyphs
  , _lo_MissingGlyphText :: T.Text
  -- page
  , _lo_Page :: LinearizationTuple
  -- spacing classes
  -- , _lo_NoSpace :: StatelessLinearizationTuple
  -- , _lo_SpaceAfter :: StatelessLinearizationTuple
  -- , _lo_SpaceBefore :: StatelessLinearizationTuple
  -- , _lo_SpaceAround :: StatelessLinearizationTuple
  -- block classes
  , _lo_DefaultBlock :: LinearizationTuple
  , _lo_FirstOfParagraph :: LinearizationTuple
  , _lo_Custos :: LinearizationTuple
  , _lo_SheetSignature :: LinearizationTuple
  , _lo_Headline :: LinearizationTuple
  , _lo_Footline :: LinearizationTuple
  , _lo_BlockQuote :: LinearizationTuple
  -- inline classes
  , _lo_PageNumber :: LinearizationTuple
  }

makeLenses ''LinearizationOptions

-- | Config for linearization to plain text.
plaintextLinearizationOptions :: LinearizationOptions
plaintextLinearizationOptions = LinearizationOptions
  { _lo_OutputHandle = stdout
  , _lo_StackMode = Loose
  , _lo_MissingGlyphText = ""
  , _lo_Page = (True, "", "\xc", Nothing, Nothing)
  -- spacing classes
  -- , _lo_NoSpace = ("", "")
  -- , _lo_SpaceAfter = ("", " ")
  -- , _lo_SpaceBefore = (" ", "")
  -- , _lo_SpaceAround = (" ", " ")
  -- block classes
  , _lo_DefaultBlock = (True, "", "\n", Nothing, Nothing)
  , _lo_FirstOfParagraph = (True, "\t", "\n", Just ParagraphSymbol, Nothing)
  , _lo_Custos = (False, "\t\t\t", "\n", Just CustosSymbol, Just CustosSymbol)
  , _lo_SheetSignature = (False, "\t\t\t", "\n", Just SheetSignatureSymbol, Just SheetSignatureSymbol)
  , _lo_Headline = (False, "", "\n", Just HeadlineSymbol, Just HeadlineSymbol)
  , _lo_Footline = (False, "", "\n", Just FootlineSymbol, Just FootlineSymbol)
  , _lo_BlockQuote = (True, "\t\t", "\n", Just BlockQuoteSymbol, Just BlockQuoteSymbol)
  -- inline classes
  , _lo_PageNumber = (True, "[[", "]]", Just PageNumberSymbol, Just PageNumberSymbol)
  }

simpleLinerizationTuple :: LinearizationTuple
simpleLinerizationTuple = (True, "", "", Nothing, Nothing)

simpleStatelessLinearizationTuple :: StatelessLinearizationTuple
simpleStatelessLinearizationTuple = ("", "")

-- | Turn the output of a category on or off.
setOutput :: Bool -> LinearizationTuple -> LinearizationTuple
setOutput display (_, pre, post, preSym, postSym) = (display, pre, post, preSym, postSym)

-- | Set prefix of a category.
setPre :: T.Text -> LinearizationTuple -> LinearizationTuple
setPre pre (display, _, post, preSym, postSym) = (display, pre, post, preSym, postSym)

-- | Set suffix of a category.
setPost :: T.Text -> LinearizationTuple -> LinearizationTuple
setPost post (display, pre, _, preSym, postSym) = (display, pre, post, preSym, postSym)


-- * Linearization options for the UI

data KeepDrop = Keep2 | Drop2

data KeepDropPart = Keep3 | Drop3 | Part3

-- | User options
data LinearizationOpts = LinOpts
  { _linopts_head :: Bool -- KeepDropPart
  , _linopts_foot :: Bool -- KeepDropPart
  , _linopts_custos :: Bool -- KeepDrop
  , _linopts_sheetSig :: Bool -- KeepDrop
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
               & linopts_head .~ False -- Part3
               & linopts_foot .~ False -- Part3
               & linopts_custos .~ False -- Drop2
               & linopts_sheetSig .~ False -- Drop2
               & linopts_prePar .~ "\n"
               & linopts_preSheetSig .~ ""
               & linopts_preQuote .~ ""


-- * Deprecated

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
