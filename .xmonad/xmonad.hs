import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import System.IO

import qualified XMonad.StackSet as W

manageHook' = composeAll
   [ resource  =? "emacs"         --> doShift "emacs"
   , className =? "Conkeror"      --> doShift "www"
   , title     =? "rtorrent"      --> doShift "rtorrent"
   --, isFullscreen                 --> doFullFloat
   , manageDocks
   ] <+> manageScratchpad

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook(W.RationalRect l t w h)
                where
                        h = 1 / 3
                        w = 1 / 3
                        t = 0.33
                        l = 0.33


backgroundColor, textColor, color3, color4, color5 :: [Char]
backgroundColor = "#073642"
textColor = "#93a1a1"
color3 = "#b58900"
color4 = "#dc322f"
color5 = "#2aa198"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor color3 backgroundColor . pad
      , ppVisible           =   dzenColor color5 backgroundColor . pad
      , ppHidden            =   dzenColor textColor backgroundColor . pad . noScratchpad
      , ppHiddenNoWindows   =   dzenColor textColor backgroundColor . pad . noScratchpad
      , ppUrgent            =   dzenColor color4 backgroundColor . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor color3 backgroundColor
      , ppTitle             =   (" " ++) . dzenColor textColor backgroundColor . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
    where
        noScratchpad ws = if ws == "NSP" then "" else ws

myTerminal = "xfce4-terminal"
myXMonadBar = "dzen2 -dock -xs 1 -fn Inconsolata-10 -ta l -bg '" ++ backgroundColor ++ "' -w '840' -h '24'"
myStatusBar = "conky -c /home/rps/.xmonad/.conky_dzen | dzen2 -dock -fn Inconsolata-10 -x '840' -w '1080' -h '24' -ta 'r' -bg '" ++ backgroundColor ++ "' -y '0'"
layoutHook' = noBorders $ avoidStruts $ spacing 5 $ layoutHook defaultConfig

main = do
  dzenRightBar <- spawnPipe myStatusBar
  dzenLeftBar <- spawnPipe myXMonadBar
  xmonad $ ewmh defaultConfig
            { terminal = myTerminal
            , focusFollowsMouse = False
            , modMask = mod4Mask
            , layoutHook = layoutHook'
            , manageHook = manageHook' <+> manageHook defaultConfig 
            , workspaces = ["1:emacs", "2:www", "3:rtorrent", "4:mpd", "5:vmc", "6:win", "7:full"]
            , borderWidth = 1
            , normalBorderColor = "#268bd2"
            , focusedBorderColor = "#859900"
            , logHook = myLogHook dzenLeftBar
            }
            `additionalKeysP`
            [ ("M-s", scratchpad)
            , ("C-d q", spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
	    , ("C-d p", spawn "dmenu_run")
            , ("C-d f", windows W.focusDown)         -- Select next window.
            , ("C-d b", windows W.focusUp)           -- Select the previous window.
            , ("C-d <Return>", windows W.swapMaster) -- Swap master window and focused window.
            , ("C-h", sendMessage Shrink)          -- Shrink the master area.
            , ("C-l", sendMessage Expand)          -- Grow the master area.
            , ("C-d k", kill)                        -- Kill the selected window.
            , ("C-d c", spawn myTerminal)            -- Start terminal.
--            , ("C-d  
            ]
            where
                scratchpad = scratchpadSpawnActionTerminal myTerminal
