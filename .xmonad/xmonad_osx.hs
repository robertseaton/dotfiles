import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare

import System.IO

import qualified XMonad.StackSet as W

myManageHook = composeAll
   [ resource  =? "emacs"         --> doShift "emacs"
   , className =? "Conkeror"      --> doShift "www"
   , title     =? "rtorrent"      --> doShift "rtorrent"
   , manageDocks
   ] <+> manageScratchpad

manageScratchpad :: ManageHook
manageScratchpad = scratchpadManageHook(W.RationalRect l t w h)
                where
                        h = 1 / 3
                        w = 1 / 3
                        t = 0.33
                        l = 0.33

color1 = "#000"
color2 = "#268bd2"
color3 = "#cb4b16"
color4 = "#dc322f"
color5 = "#2aa198"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor color3 color1 . pad
      , ppVisible           =   dzenColor color5 color1 . pad
      , ppHidden            =   dzenColor color2 color1 . pad . noScratchpad
      , ppHiddenNoWindows   =   dzenColor color2 color1 . pad . noScratchpad
      , ppUrgent            =   dzenColor color4 color1 . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor color3 color1
      , ppTitle             =   (" " ++) . dzenColor color2 color1 . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
    where
        noScratchpad ws = if ws == "NSP" then "" else ws

myTerminal = "urxvt"
myStatusbar = "dzen2 -xs 1 -fn Inconsolata-8 -ta l -bg \"#000\""

main = do
     dzenBar <- spawnPipe myStatusbar
     xmonad $ defaultConfig
            { terminal = myTerminal
	--            , modMask = mod4Mask
            , layoutHook = avoidStruts $ layoutHook defaultConfig
            , manageHook = myManageHook <+> manageHook defaultConfig 
            , workspaces = ["emacs", "term", "www", "pdf", "rtorrent", "mpd", "vm"]
            , borderWidth = 1
            , normalBorderColor = "#000"
            , focusedBorderColor = "#000"
            , logHook = myLogHook dzenBar
            }
            `additionalKeysP`
            [ ("M-r", spawn "/usr/bin/emacs")
            , ("M-b", spawn "conkeror")
            , ("M-s", scratchpad)
            ]
            where
                scratchpad = scratchpadSpawnActionTerminal myTerminal
