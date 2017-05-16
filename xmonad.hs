import XMonad
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W

import XMonad.Actions.FocusNth (focusNth)
import XMonad.Hooks.DynamicLog (xmobarColor, shorten, PP(..), defaultPP, wrap)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.Multibar (xmobars, multiPP)

import XMonad.Layout.IM (withIM, Property(Title, Role, And, ClassName))
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(..))
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.Terminal (terminal80)

import XMonad.Util.EZConfig (removeKeys, additionalKeys)

import System.Exit (exitWith, ExitCode(ExitSuccess))


modm = mod4Mask

cbiffleConfig base hmap = ewmh $ docks $ base
  { layoutHook = avoidStruts $ onWorkspace "3" imLayout $ cbiffleLayout
  , manageHook = manageDocks <+> manageHook base
  , handleEventHook = handleEventHook base <+> fullscreenEventHook

  , modMask = modm
  , normalBorderColor = "#0000dd"
  , focusedBorderColor = "#ff0000"
  , focusFollowsMouse = False
  , clickJustFocuses = False
  , logHook = cbiffleLogHook hmap
  } `removeKeys` cbiffleUnwantedKeys
    `additionalKeys` cbiffleKeys

cbiffleLogHook hmap = multiPP
  (cbifflePP' "yellow" "green" ("red", "yellow"))
  (cbifflePP' "grey" "grey" ("grey", "grey"))
  hmap

icon relpath = "<icon=/home/cbiffle/.xmonad/img/" ++ relpath ++ ".xbm/>"

cbiffleLayout =
  renamed [CutWordsLeft 1, PrependWords (icon "layout-terminal")] terminal80
  ||| rename (icon "layout-tabbed") simpleTabbed
  ||| rename (icon "layout-tall") (Tall 1 (3/100) (1/2))
  where rename s l = renamed [Replace s] l

imLayout = withIM (1/4) (And (ClassName "Pidgin") (Role "buddy_list")) cbiffleLayout

-- My workspaces are on the F-keys, not the numerals.
cbiffleUnwantedKeys = [(modm .|. shiftMask, n) | n <- [xK_1 .. xK_9]]

cbiffleKeys = actionKeys ++ workspaceKeys ++ windowKeys

actionKeys =
  [ ((modm,                 xK_r), spawn "dmenu_run -p Run: -b")
  , ((modm .|. shiftMask,   xK_q), restart "xmonad" False)
  , ((modm .|. controlMask, xK_q), io (exitWith ExitSuccess))
  , ((modm .|. controlMask, xK_l), spawn "light-locker-command -l")

  , ((0, xK_Print), spawn "scrot")
  , ((0, xF86XK_AudioRaiseVolume), raiseVolume)
  , ((0, xF86XK_AudioLowerVolume), lowerVolume)
  , ((0, xF86XK_AudioMute),        muteAudio)
  , ((0, xF86XK_AudioMicMute),     muteMic)
  , ((0, xF86XK_MonBrightnessUp),   raiseBrightness)
  , ((0, xF86XK_MonBrightnessDown), lowerBrightness)
  ]

setVolume s = spawn $ "amixer set Master " ++ s ++ " unmute"

raiseVolume = setVolume "5%+"
lowerVolume = setVolume "5%-"
muteAudio = spawn $ "amixer set Master toggle"
muteMic = spawn $ "amixer set Capture toggle"

raiseBrightness = spawn $ "xbacklight +10"
lowerBrightness = spawn $ "xbacklight -10"

workspaceKeys = [ ((modm .|. m, k), windows $ f i)
                | (i, k) <- zip cbiffleWorkspaces [xK_F1..]
                , (m, f) <- [ (0, W.view)
                            , (controlMask, W.greedyView)
                            , (shiftMask, W.shift)
                            ]
                ]

windowKeys = [((modm, k), focusNth w) | (k, w) <- zip [xK_1..xK_9] [0..]]

cbiffleWorkspaces = [show i | i <- [1..12]]

cbifflePP' currentC titleC (urgentFC, urgentBC) = defaultPP
  { ppCurrent = clickable (xmobarColor currentC "" . wrap "[" "]" . cook)
  , ppVisible = wrap "(" ")" . cook
  , ppHidden = wrap " " " " . cook
  , ppUrgent = xmobarColor urgentFC urgentBC
  , ppTitle = xmobarColor titleC "" . cook . shorten 150
  , ppWsSep = ""
  }

clickable :: (String -> String) -> String -> String
clickable f ws = "<action=`xdotool key super+F" ++ ws ++ "`>" ++ f ws
                  ++ "</action>"

cook :: String -> String
cook s = "<raw=" ++ show (length s) ++ ":" ++ s ++ "/>"

main = xmonad . cbiffleConfig defaultConfig =<< xmobars
