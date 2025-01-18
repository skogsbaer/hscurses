import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import UI.HSCurses.Widgets

import Control.Exception
import System.Exit

draw s = do
    (h, w) <- Curses.scrSize
    CursesH.gotoTop
    CursesH.drawLine w s
    Curses.refresh

done = return ()

forever :: (Monad m) => m t -> m t
forever x = x >> forever x

exit ew _ _ = return (Done ew)

options stys =
    let dsty = (mkDrawingStyle (stys !! 1)) {dstyle_active = stys !! 1}
     in defaultEWOptions {ewopt_style = dsty}

editWidget stys = newEditWidget (options stys) ""

edit ew = do
    (ew', s) <- activateEditWidget done (1, 10) (1, 10) ew
    Curses.wMove Curses.stdScr 5 0
    CursesH.drawLine 60 ("saved: " ++ s)
    Curses.refresh
    return ew'

loop ew = do
    c <- CursesH.getKey done
    ew' <- case c of
        Curses.KeyChar 'q' -> exitWith ExitSuccess
        Curses.KeyChar 'e' -> edit ew
        _ -> return ew
    loop ew'

styles =
    [ CursesH.defaultStyle
    , CursesH.Style CursesH.CyanF CursesH.PurpleB
    ]

main :: IO ()
main =
    do
        CursesH.start
        cstyles <- CursesH.convertStyles styles
        Curses.cursSet Curses.CursorInvisible
        CursesH.gotoTop
        CursesH.drawLine 20 "Hit 'e'!"
        Curses.wMove Curses.stdScr 1 0
        CursesH.drawLine 9 "Input: "
        Curses.refresh
        loop (editWidget cstyles)
        `finally` CursesH.end
