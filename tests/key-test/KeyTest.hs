import UI.HSCurses.Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Control.Exception
import Control.Monad (forever)
import Data.Char
import System.Exit

draw s = do
    (h, w) <- scrSize
    CursesH.gotoTop
    CursesH.drawLine w s
    refresh

done = return ()

main :: IO ()
main =
    finally
        ( do
            CursesH.start
            draw "Press any key, click the mouse, or use the mouse wheel"
            withAllMouseEvents $ do
                forever $ do
                    c <- CursesH.getKey done
                    case c of
                        KeyChar 'q' -> exitWith ExitSuccess
                        KeyMouse -> do
                            draw "mouse\n"
                            me <- getMouse
                            draw (show me)
                        x -> draw ("Last key: " ++ CursesH.displayKey x ++ " (" ++ show x ++ ")")
        )
        CursesH.end
