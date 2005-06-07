import qualified HSCurses.Curses as Curses
import qualified HSCurses.CursesHelper as CursesH
import HSCurses.Widgets
import Control.Exception
import System

twOptions sty = defaultTWOptions { twopt_style = mkDrawingStyle sty }

text sty  = newTextWidget (twOptions sty) (reverse "1\n12\n123\n1234\n12345\n123456\n1234567\n12345678\n123456789\n1234567890")

textSize = (4,4)

loop tw = 
    do drawTextWidget (0,0) textSize DHNormal tw
       c <- CursesH.getKey done
       case c of
         Curses.KeyChar 'q' -> exitWith ExitSuccess
         Curses.KeyChar ' ' -> loop $ textWidgetScrollDown textSize tw
         Curses.KeyChar '-' -> loop $ textWidgetScrollUp textSize tw
         Curses.KeyChar ',' -> loop $ textWidgetScrollLeft textSize tw
         Curses.KeyChar '.' -> loop $ textWidgetScrollRight textSize tw
         _   -> loop tw

done :: IO ()
done = return ()

styles = [CursesH.defaultStyle, CursesH.Style CursesH.WhiteF CursesH.PurpleB]

main :: IO ()
main = 
    do CursesH.start
       cstyles <- CursesH.convertStyles styles
       Curses.cursSet Curses.CursorInvisible
       CursesH.gotoTop
       loop (text (cstyles !! 1))
    `finally` CursesH.end