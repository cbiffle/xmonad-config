module XMonad.Hooks.Multibar
  ( xmobars
  , multiPP
  , multiPP'
  ) where

import XMonad

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import XMonad.Hooks.DynamicLog (PP(..), dynamicLogString)

import System.IO (Handle, hPutStrLn)
import Control.Applicative (liftA2, (<*), (<$>))
import Graphics.X11.Xinerama (getScreenInfo)

import Data.Traversable (for)
import XMonad.Util.Run (spawnPipe)

--------------------------------------------------------------------------------
-- Status Bar Management

-- | Map from ScreenId to the Handle associated with that screen's status bar.
type HandleMap = M.Map ScreenId Handle

-- | Spawns an xmobar instance for each screen, collecting their Handles into
-- a HandleMap.
xmobars :: IO HandleMap
xmobars = do
  alist <- getScreens >>= mapM (\s -> (,) (S s) <$> xmobarScreen s)
  return $ M.fromList alist

-- | Fires up an xmobar instance on a particular screen, using my convention
-- for per-screen xmobar configurations, and returns the corresponding pipe.
xmobarScreen :: Int -> IO Handle
xmobarScreen n = spawnPipe $ "xmobar -x " ++ s ++ " ~/.xmonad/xmobarrc." ++ s
  where s = show n

-- | Find the subset of Xmobar-bedecked screens that also have a visible
-- workspace.  Yes, it is possible for screens not to have a visible
-- workspace in Xmonad... though I admit I don't understand how to achieve
-- this.
xmobarVisibleWorkspaces :: HandleMap -> X (M.Map ScreenId WorkspaceId)
xmobarVisibleWorkspaces hmap =
  M.mapMaybe id <$> M.traverseWithKey (\k _ -> screenWorkspace k) hmap


--------------------------------------------------------------------------------
-- Multi-way pretty printing

-- | Multiplex two PPs, one for the focused screen and one for the others.
-- This uses the stock dynamicLogString formatter; to use a custom formatter
-- see multiPP', below.
multiPP :: PP         -- ^ PP for focused screen.
        -> PP         -- ^ PP for unfocused screen(s).
        -> HandleMap  -- ^ Access to the status bar handles for each screen.
        -> X ()
multiPP = multiPP' dynamicLogString

-- | Multiplex two PPs as in multiPP (above), but with a custom log string
-- formatter.
multiPP' :: (PP -> X String)  -- ^ log string function
         -> PP                -- ^ PP for focused workspace
         -> PP                -- ^ PP for other workspaces
         -> HandleMap         -- ^ Per-screen xmobar handles
         -> X ()

multiPP' dynlStr focusPP unfocusPP hmap = do
  wset <- gets windowset
  wsmap <- xmobarVisibleWorkspaces hmap

  -- msgs :: M.Map ScreenId String
  msgs <- for wsmap $ \ws -> forkState $ do
    let isFoc = W.tag (W.workspace $ W.current wset) == ws
    modify $ \s -> s { windowset = W.view ws wset }
    dynlStr $ if isFoc then focusPP else unfocusPP

  -- Correlate messages with handles by ScreenId, allowing omissions,
  -- and send the messages through the handles.
  io $ mapM_ (uncurry hPutStrLn) $ M.elems $ zipMaps hmap msgs


--------------------------------------------------------------------------------
-- Utilities

-- | For each key present in both input maps, the result map contains a pair
-- of the values.  Any key missing from either input map is absent from the
-- result.
zipMaps :: (Ord k) => M.Map k a -> M.Map k b -> M.Map k (a, b)
zipMaps = M.mergeWithKey (\_ x1 x2 -> Just (x1, x2))
                         (const M.empty)
                         (const M.empty)

-- | Perform an action that may affect XState, restoring its initial
-- value afterwards.  I am still a little boggled that this isn't a standard
-- operation in Control.Monad.State (or at least I can't find it).
forkState :: X a -> X a
forkState action = do
  s <- get
  r <- action
  put s
  return r

getScreens :: IO [Int]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
  where f = fmap (zipWith const [0..]) . getScreenInfo
