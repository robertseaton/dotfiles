import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
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


backgroundColor, textColor, color3, color4, color5 :: [Char]
backgroundColor = "#181818"
textColor = "#d8d8d8"
color5 = "#DC9656"
color4 = "#A1B56C"
color3 = "#AB4642"


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

layout' = tall' ||| Full
  where
    tall' = Tall nmaster delta ratio
    nmaster = 1
    ratio = 0.618034 -- Golden ratio with a + b = 1.
    delta = 3/100

dmenu_cmd = "dmenu_run -h 24 -fn Inconsolata-10 -nb '" ++ backgroundColor ++ "' -nf '" ++ textColor ++ "' -sb '" ++ color3 ++ "' -sf '" ++ textColor ++ "'"

main = do
  dzenRightBar <- spawnPipe myStatusBar
  dzenLeftBar <- spawnPipe myXMonadBar
  xmonad $ ewmh defaultConfig
            { terminal = myTerminal
            , focusFollowsMouse = False
            , modMask = mod4Mask
            , layoutHook = smartBorders $ avoidStruts $ spacing 5 $ layout'
            , manageHook = myManageHook <+> manageHook defaultConfig 
            , workspaces = ["1:emacs", "2:www", "3:misc", "4:vmc", "5:win"]
            , borderWidth = 1
            , normalBorderColor = backgroundColor
            , focusedBorderColor = color3
            , logHook = myLogHook dzenLeftBar
            , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
            }
            `additionalMouseBindings`
            [ ((0, 8), \w -> prevWS ) -- Use mouse button to move to the previous workspace.
            , ((0, 9), \w -> nextWS ) -- Use mouse button to move to the next workspace.
            ]
            `additionalKeysP`
            [ ("M-s", scratchpad)
            , ("C-d q", spawn "killall conky dzen2 && xmonad --recompile && xmonad --restart")
	    , ("C-d s", spawn dmenu_cmd)
            , ("C-d f", windows W.focusDown)           -- Select next window.
            , ("C-d d", windows W.focusUp)             -- Select the previous window.
            , ("C-d <Return>", windows W.swapMaster)   -- Swap master window and focused window.
            , ("C-h", sendMessage Shrink)              -- Shrink the master area.
            , ("C-l", sendMessage Expand)              -- Grow the master area.
            , ("C-d x", kill)                          -- Kill the selected window.
            , ("C-d c", spawn myTerminal)              -- Start terminal.
            , ("C-d <Space>", sendMessage NextLayout)  -- Switch layout.
            , ("C-d n", windows W.swapDown)            -- Swap focused window with next window.
            , ("C-d p", windows W.swapUp)              -- Swap focused window with the previous window.
            , ("C-d +", sendMessage (IncMasterN 1))    -- Increment the number of windows in the master area.
            , ("C-d -", sendMessage (IncMasterN (-1))) -- Decrement the number of windows in the master area. 
            ]
            where
                scratchpad = scratchpadSpawnActionTerminal myTerminal
