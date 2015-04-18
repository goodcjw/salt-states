import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Circle
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Themes

c2 = spacing 20 $ ResizableTall 1 (1/12) (1/2) []
c3 = spacing 20 $ ThreeCol 1 (1/12) (1/3)
cr = avoidStruts $ Circle
tb = noBorders $ tabbed shrinkText defaultTheme

myManageHooks = composeAll
    [ className =? "Firefox" --> doFloat
    , className =? "Vlc" --> doFloat
    , title =? "Hangouts" --> doFloat ]

main = do
    xmonad $ gnomeConfig
        {
        terminal = "terminator"
        , borderWidth = 4
        , normalBorderColor = "#02151B"
        , focusedBorderColor = "#b84130"
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , layoutHook = avoidStruts (c2 ||| c3 ||| cr ||| tb)
        , manageHook = (manageHook gnomeConfig) <+> myManageHooks
        }
