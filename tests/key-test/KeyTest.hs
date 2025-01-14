import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Control.Monad (forever)
import Data.Char
import System.Exit
import Control.Exception

draw s =
    do (h, w) <- Curses.scrSize
       CursesH.gotoTop
       CursesH.drawLine w s
       Curses.refresh

done = return ()

main :: IO ()
main =
    do CursesH.start
       draw ""
       forever (do c <- CursesH.getKey done
                   case c of
                     Curses.KeyChar 'q' -> exitWith ExitSuccess
                     x -> draw ("Last key: " ++ CursesH.displayKey x
                                ++ " (" ++ show x ++ ")")

               )
    `finally` CursesH.end
