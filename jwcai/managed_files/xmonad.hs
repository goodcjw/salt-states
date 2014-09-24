import XMonad
import XMonad.Config.Gnome
import XMonad.Layout.ThreeColumns
import XMonad.Layout
import XMonad.Hooks.ManageDocks

myLayout = Tall 1 (1/12) (1/2) ||| ThreeCol 1 (1/12) (1/3) ||| Full

main = do
    xmonad $ gnomeConfig
        {
        terminal = "terminator"
        , borderWidth = 1
        , normalBorderColor = "#1C1C1C"
        , focusedBorderColor = "#C61915"
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , layoutHook = avoidStruts  $ myLayout
        }
