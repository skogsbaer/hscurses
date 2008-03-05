import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

import Data.Char
import System.Exit
import Control.Exception

draw s =
    do (h, w) <- Curses.scrSize
       CursesH.gotoTop
       CursesH.drawLine w s
       Curses.refresh

done = return ()

forever x = do x
               forever x

controlCharsNonWs = [-- %x00-08
                     '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK',
                     '\BEL', '\BS',
                     -- %x0A-1F
                     '\LF', '\VT', '\FF', '\CR',
                     '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4',
                     '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC',
                     '\FS', '\GS', '\RS', '\US',
                     -- %x7F
                     '\DEL']

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