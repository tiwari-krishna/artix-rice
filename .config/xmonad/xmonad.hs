-- Imports
import XMonad
import Data.Monoid
import System.Exit
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.NoBorders
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.ManageHook
import XMonad.Util.NamedScratchpad
import XMonad.Hooks.WindowSwallowing
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.CopyWindow
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.InsertPosition
import XMonad.Layout.Tabbed
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
import XMonad.Layout.Renamed
import XMonad.Actions.Minimize
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.FloatKeys
import XMonad.Util.ClickableWorkspaces
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.SpawnOnce

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Ratio ((%))

-- Setting Terminal and Browser
myTerminal      = "$TERMINAL"
myBrowser       = "$BROWSER"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

--Border Width
myBorderWidth   = 2

-- Modkey
myModMask       = mod4Mask

-- Scratchpads
scratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "mpc" spawnMpc findMpc manageMpc
                , NS "filemanager" spawnFileM findFileM manageFileM
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.7
                 w = 0.7
                 t = 0.85 -h
                 l = 0.85 -w
    spawnMpc  = myTerminal ++ " -t mpc -e ncmpcpp"
    findMpc   = title =? "mpc"
    manageMpc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.7
                 w = 0.7
                 t = 0.85 -h
                 l = 0.85 -w
    spawnFileM  = myTerminal ++ " --class filemanager -e ranger"
    findFileM   = className =? "filemanager"
    manageFileM = customFloating $ W.RationalRect l t w h
               where
                 h = 0.7
                 w = 0.7
                 t = 0.85 -h
                 l = 0.85 -w

-- Workspaces
myWorkspaces    = ["α","β","γ","δ","ε","ζ","η","θ","ι"]

-- Toggling Fullscreen Function
toggleFull = withFocused (\windowId -> do    
{       
   floats <- gets (W.floating . windowset);        
   if windowId `M.member` floats        
   then do 	   
       withFocused $ toggleBorder           
       withFocused $ windows . W.sink        
   else do 	   
       withFocused $ toggleBorder           
       withFocused $  windows . (flip W.float $ W.RationalRect 0 0 1 1)    }    )

-- Toggline Floating Function
toggleFloat = withFocused (\windowId ->do
{
  floats <- gets (W.floating . windowset);
  if windowId `M.member` floats
  then do
      withFocused $ windows . W.sink
  else do
      withFocused $ windows . (flip W.float $ W.RationalRect 0.15 0.15 0.7 0.7 )
}  )

-- Border Colors
myNormalBorderColor  = "#030000"
myFocusedBorderColor = "#ffffff"

-- Keybindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,           xK_space     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modm,       xK_q     ), kill)
    , ((modm,               xK_Tab ), sendMessage NextLayout)
    , ((modm, xK_t ), setLayout $ XMonad.layoutHook conf)
    , ((modm .|. shiftMask, xK_t), sendMessage $ JumpToLayout "Mirror Tall")
    , ((modm .|. controlMask, xK_t), sendMessage $ JumpToLayout "tabbed")
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((mod1Mask .|. controlMask,  xK_Return   ), windows W.focusMaster  )
    , ((modm .|. controlMask,      xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_y     ), withFocused $ windows . W.sink)
    , ((modm              , xK_p ), sendMessage (IncMasterN 1))
    , ((modm              , xK_o ), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm .|. controlMask, xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    , ((modm .|. shiftMask, xK_Return), namedScratchpadAction scratchpads "terminal")
    , ((modm .|. mod1Mask, xK_Return), namedScratchpadAction scratchpads "mpc")
    , ((mod1Mask .|. shiftMask, xK_Return), namedScratchpadAction scratchpads "filemanager")

    , ((modm,                xK_g    ), viewEmptyWorkspace)
    , ((modm .|. shiftMask,  xK_g    ), tagToEmptyWorkspace)
    , ((modm .|. shiftMask,    xK_f     ), toggleFull)
    , ((modm,                  xK_f     ), toggleFloat)

    , ((modm,                  xK_s     ), windows copyToAll) --Enable sticky
    , ((modm .|. shiftMask,    xK_s     ), killAllOtherCopies) --Remove sticky

    , ((modm,               xK_bracketright),  nextWS)
    , ((modm,               xK_bracketleft),  prevWS)
    , ((modm .|. shiftMask, xK_bracketright),  shiftToNext >> nextWS)
    , ((modm .|. shiftMask, xK_bracketleft),  shiftToPrev >> prevWS)
    , ((modm,               xK_period), nextScreen)
    , ((modm,               xK_comma),  prevScreen)
    , ((modm .|. shiftMask, xK_period), shiftNextScreen >> nextScreen)
    , ((modm .|. shiftMask, xK_comma),  shiftPrevScreen >> prevScreen)
    , ((modm .|. controlMask,  xK_Tab),     toggleWS)

    , ((modm,               xK_n     ), withFocused minimizeWindow      )
    , ((modm .|. shiftMask, xK_n     ), withLastMinimized maximizeWindow)

    , ((modm,               xK_Left     ), withFocused (keysResizeWindow (-10,-10) (1,1)))
    , ((modm,               xK_Right     ), withFocused (keysResizeWindow (10,10) (1,1)))
    , ((modm .|. shiftMask, xK_Left     ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
    , ((modm .|. shiftMask, xK_Right     ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))
    , ((modm .|. shiftMask, xK_a     ), withFocused (keysMoveWindowTo (512,384) (1%2,1%2)))

    -- Notification
    , ((modm,               xK_apostrophe ), spawn "notify-timedate")
    , ((modm .|. shiftMask, xK_apostrophe ), spawn "notify-battery")
    , ((modm .|. controlMask, xK_apostrophe  ), spawn "notify-wifi")

    -- Additional Keybindings (User Space Programs)
    , ((mod1Mask .|. shiftMask, xK_space  ), spawn "mpc toggle")
    , ((mod1Mask .|. shiftMask, xK_Left  ), spawn "mpc prev")
    , ((mod1Mask .|. shiftMask, xK_Right  ), spawn "mpc next")
    , ((mod1Mask,               xK_Up  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +3%")
    , ((mod1Mask,               xK_Down  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -3%")
    , ((mod1Mask,               xK_bracketleft  ), spawn "mpc seek -10")
    , ((mod1Mask .|. shiftMask, xK_bracketleft  ), spawn "mpc seek -60")
    , ((mod1Mask,               xK_bracketright  ), spawn "mpc seek +10")
    , ((mod1Mask .|. shiftMask, xK_bracketright  ), spawn "mpc seek +60")
    , ((mod1Mask,               xK_Right  ), spawn "mpc vol +5")
    , ((mod1Mask,               xK_Left  ), spawn "mpc vol -5")
    , ((mod1Mask,               xK_apostrophe  ), spawn "mpc seek 0%")
    , ((mod1Mask .|. controlMask,  xK_space  ), spawn "mpc single")
    , ((mod1Mask .|. shiftMask, xK_s  ), spawn "maim -s ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((0 .|. shiftMask,               xK_Print  ), spawn "maim -s ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((mod1Mask,                xK_Print  ), spawn "maim ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((mod1Mask,                xK_s  ), spawn "maim ~/Data/screenshots/$(date +%Y-%m-%d-%s).png")
    , ((0, xF86XK_AudioMute  ), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioRaiseVolume  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ +3%")
    , ((0, xF86XK_AudioLowerVolume  ), spawn "pactl set-sink-volume @DEFAULT_SINK@ -3%")
    , ((0, xF86XK_AudioPlay  ), spawn "mpc toggle")
    , ((0, xF86XK_AudioPrev  ), spawn "mpc prev")
    , ((0, xF86XK_AudioNext  ), spawn "mpc next")
    , ((0, xF86XK_MonBrightnessUp  ), spawn "xbacklight -inc 5")
    , ((mod1Mask .|. modm, xK_equal  ), spawn "xbacklight -inc 5")
    , ((mod1Mask .|. modm, xK_minus  ), spawn "xbacklight -dec 5")
    , ((0, xF86XK_MonBrightnessDown  ), spawn "xbacklight -dec 5")
    , ((0, xF86XK_HomePage  ), spawn (myBrowser))
    , ((mod1Mask , xK_F1  ), spawn "playerctl play-pause")
    , ((mod1Mask , xK_F2  ), spawn "playerctl pervious")
    , ((mod1Mask , xK_F3  ), spawn "playerctl next")
    , ((mod1Mask , xK_equal  ), spawn "playerctl volume 5+")
    , ((mod1Mask , xK_minus  ), spawn "playerctl volume 5-")
    , ((mod1Mask .|. shiftMask , xK_equal  ), spawn "playerctl position 30+")
    , ((mod1Mask .|. shiftMask , xK_minus  ), spawn "playerctl position 30-")

    , ((mod1Mask .|. controlMask, xK_v  ), spawn (myTerminal ++ " -e pulsemixer"))
    , ((modm,                     xK_a  ), spawn (myTerminal ++ " -e ranger"))
    , ((mod1Mask,                 xK_w  ), spawn (myTerminal ++ " -e nmtui"))
    , ((modm .|. shiftMask,       xK_e  ), spawn (myTerminal ++ " -e htop"))
    , ((mod1Mask,                 xK_r  ), spawn "radio-listen")
    , ((mod1Mask,                 xK_Menu  ), spawn "radio-listen")
    , ((modm .|. shiftMask,       xK_BackSpace ), spawn "power")
    , ((mod1Mask .|. shiftMask, xK_w     ), spawn "sxiv -q -o -t -r ~/Data/Media/wallpapers")
    , ((mod1Mask .|. shiftMask, xK_g     ), spawn "gimp")
    , ((modm,               xK_grave     ), spawn "alacritty")
    , ((modm .|. shiftMask, xK_grave     ), spawn "libreoffice")
    , ((modm .|. shiftMask, xK_d        ), spawn "rofi -show drun")
    , ((modm .|. shiftMask, xK_w        ), spawn "open-bookmarks")
    , ((modm, xK_semicolon        ), spawn "spellchk")
    , ((modm .|. shiftMask, xK_x     ), spawn "pcmanfm")
    , ((modm .|. controlMask, xK_r     ), spawn "mpv --untimed --no-cache --no-osc --no-input-default-bindings --profile=low-latency --input-conf=/dev/null --title=webcam /dev/video0")
    , ((modm,           xK_slash ), spawn "web-search")
    , ((modm .|. shiftMask, xK_slash ), spawn "browser-launch")
    , ((modm,           xK_Menu  ), spawn "rofi -show emoji")
    , ((modm,           xK_d     ), spawn "clipgrab")
    , ((modm,           xK_e     ), spawn "emacsclient -c || emacs")
    , ((modm,           xK_c     ), spawn "galculator")
    , ((modm,           xK_v     ), spawn "qbittorrent")
    , ((modm,           xK_w     ), spawn (myBrowser))
    , ((modm .|. shiftMask, xK_v     ), spawn "minitube")
    , ((modm .|. controlMask, xK_z     ), spawn "slock")
    ]
    ++

    -- Workspaces
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- Monitors
    [((m .|. modm .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


--Mouse bindings
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

-- Layouts
myLayout = smartBorders $ (tiled ||| Mirror tiled ||| tabbed shrinkText myTabTheme)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

-- Theme for Tabbed Layout
myTabTheme = def { fontName            = "xft:Noto Sans:regular:size=10:antialias=true:hinting=true"
                 , activeColor         = "#5c3469"
                 , inactiveColor       = "#1d191f"
                 , activeBorderColor   = "#ffffff"
                 , inactiveBorderColor = "#1c1616"
                 , activeTextColor     = "#ffffff"
                 , inactiveTextColor   = "#ffffff"
                 }

-- ManageHooks
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "mpv"            --> doFloat
    , className =? "confirm"        --> doFloat
    , className =? "file_progress"  --> doFloat
    , className =? "dialog"         --> doFloat
    , className =? "error"          --> doFloat
    , className =? "notification"   --> doFloat
    , className =? "splash"         --> doFloat
    , className =? "toolbar"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ] <+> namedScratchpadManageHook scratchpads

myEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "st-256color") (return True)

--myLogHook = return ()

myStartupHook = do
  spawnOnce "dunst"

{-myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = "  "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    --, ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    , ppLayout = (\ x -> case x of
           "Minimize Spacing Tall"                 -> "M&S"
           "Minimize Spacing Mirror Tall"          -> "Mir M&S"
           "Minimize Spacing Tabbed Simplest"      -> "Tabbed"
           _                                         -> x )
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 50

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    -}

main = do
  xmonad $ ewmhFullscreen 
         $ ewmh 
         -- $ withEasySB (statusBarProp "xmobar ~/.config/xmonad/xmobar.conf" (clickablePP myXmobarPP)) defToggleStrutsKey 
         $ defaults

defaults = docks def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = minimize . BW.boringWindows $ avoidStruts $ spacingWithEdge 5 $ myLayout,
        manageHook         = insertPosition End Newer <+> myManageHook <+> manageDocks,
        handleEventHook    = myEventHook <+> Hacks.windowedFullscreenFixEventHook <+> Hacks.trayerPaddingXmobarEventHook <+> Hacks.trayerAboveXmobarEventHook,
        logHook            = updatePointer (0.5, 0.5) (0, 0),
        startupHook        = myStartupHook
    }
