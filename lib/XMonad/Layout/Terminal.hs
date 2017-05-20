{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Terminal
-- Copyright   :  (c) Cliff L. Biffle 2012
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  code@cliffle.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- A layout for terminal windows, when one cares about the width of each in
-- columns.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Terminal
    ( Terminal(..)
    , terminal80
    ) where

import Data.Maybe (fromMaybe)

import Graphics.X11.Xlib (Window, Rectangle(..), Dimension)
import Graphics.X11.Xlib.Extras ( getWMNormalHints
                                , getWindowAttributes
                                , sh_base_size
                                , sh_resize_inc
                                , wa_border_width)
import XMonad.Core (X, LayoutClass(..), fromMessage, io, withDisplay)
import XMonad.Layout (Resize(..), splitVertically)
import qualified XMonad.StackSet as W

-- | Produces a compact Terminal layout: 80 columns, no padding.
terminal80 :: Terminal a
terminal80 = Terminal 80 5 10

-- | Lays out windows from left to right, setting each window's width to show
-- the specified number of columns.  If a program doesn't provide information
-- about its current column width, we assume a certain fixed width.
--
-- If there isn't enough horizontal screen real estate to lay out all windows
-- at the requested widths, the remainder are stacked vertically in whatever
-- space remains to the right.
data Terminal a = Terminal !Int -- ^ Width of full-height windows, in columns.
                           !Int -- ^ Number of columns to add/remove on resize.
                           !Dimension -- ^ Column width to assume, when a
                                      -- program doesn't specify one.
                        deriving (Read, Show)

instance LayoutClass Terminal Window where

    doLayout (Terminal ncol _ fallback) r s = do
        let ws = W.integrate s
        fws <- mapM (widthCols fallback ncol) ws
        return (zip ws (fillScreen 0 r fws), Nothing)

    pureMessage (Terminal ncol delta fallback) m = fmap resize (fromMessage m)
        where resize Shrink
                  = Terminal (max 0 $ ncol - delta) delta fallback
              resize Expand
                  = Terminal (ncol + delta) delta fallback

    description (Terminal ncol _ _) =
        "Terminal " ++ show ncol

-- | Determine the width of @w@ given that we would like it to be @n@
--   columns wide, using @inc@ as a resize increment for windows that
--   don't have one
widthCols :: Dimension -> Int -> Window -> X Dimension
widthCols inc n w = withDisplay $ \d -> io $ do
    sh <- getWMNormalHints d w
    bw <- (fromIntegral . wa_border_width) <$> getWindowAttributes d w
    -- (SizeHints -> Maybe (Int, b)) -> Maybe Real
    let widthHint f = fst <$> f sh
        oneCol      = fromMaybe inc $ widthHint sh_resize_inc
        base        = fromMaybe 0 $ widthHint sh_base_size
    return $ 2 * bw + base + fromIntegral n * oneCol

-- | Fills the screen with as many windows as possible at their requested
-- widths (as computed by widthCols).  Shoves all other windows off to the
-- right.
fillScreen :: Dimension    -- ^ Space between windows.
           -> Rectangle    -- ^ Screen area to fill.
           -> [Dimension]  -- ^ Each window's preferred width.
           -> [Rectangle]  -- ^ Each window's assigned area.
fillScreen sp
           r@(Rectangle left top screenWidth screenHeight)
           w@(winWidth:winWidths)
        | winWidth + sp <= screenWidth =
                Rectangle left top winWidth screenHeight
                  : fillScreen sp
                               (Rectangle (left + fromIntegral (winWidth + sp))
                                          top
                                          (screenWidth - winWidth - sp)
                                          screenHeight)
                               winWidths
        | otherwise = splitVertically (length w) r

fillScreen _ _ [] = []
