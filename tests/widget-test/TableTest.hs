import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import UI.HSCurses.Widgets
import Control.Exception
import System.Exit

row1 sty = map (TableCell . newTextWidget defaultTWOptions) ["1", "eins", "one"]
row2 sty = TableCell (newTextWidget
                      (defaultTWOptions { twopt_size =
                                          TWSizeFixed (2,10),
                                          twopt_style =
                                          mkDrawingStyle (sty!!2)}) "2")
           : map (TableCell . newTextWidget defaultTWOptions) ["zwei", "two"]
row3 sty = map (TableCell . newTextWidget defaultTWOptions) ["3", "drei"]
           ++ [ActiveTableCell $ newEditWidget
                    (defaultEWOptions {ewopt_style =
                                       mkDrawingStyle (sty!!1)}) ""]
row4 sty = map (TableCell . newTextWidget defaultTWOptions)
               ["4", "vier", "four"]
row5 sty = map (TableCell . newTextWidget defaultTWOptions) ["5", "fuenf"]
           ++ [TableCell (newTextWidget
                          (defaultTWOptions {twopt_size =
                                             TWSizeFixed (1,6),
                                             twopt_style =
                                             mkDrawingStyle (sty!!3)})
                          "five56XXXXX")]

rows sty = [row1 sty, row2 sty, row3 sty, row4 sty, row5 sty]

tableWidget sty = newTableWidget (TBWOptions (Just 1) [0,2] (10,10)) (rows sty)

tableSize = (2, 50)
tablePos = (1,0)

msgSize = (1, 40)
msgPos = (10, 0)

text = "0        1         2         3         4         5"

done = return ()

loop tbw msg =
    do drawTableWidget tablePos tableSize DHNormal tbw
       drawTextWidget msgPos msgSize DHNormal msg
       c <- CursesH.getKey done
       case c of
         Curses.KeyChar 'q' -> exitWith ExitSuccess
         Curses.KeyChar ' ' -> loop (tableWidgetScrollDown tableSize tbw) msg
         Curses.KeyChar '-' -> loop (tableWidgetScrollUp tableSize tbw) msg
         Curses.KeyRight -> loop (tableWidgetGoRight tableSize tbw) msg
         Curses.KeyLeft -> loop (tableWidgetGoLeft tableSize tbw) msg
         Curses.KeyUp -> loop (tableWidgetGoUp tableSize tbw) msg
         Curses.KeyDown -> loop (tableWidgetGoDown tableSize tbw) msg
         Curses.KeyChar '\r' ->
             do (new, res) <- tableWidgetActivateCurrent done tablePos
                                tableSize DHNormal tbw
                let msg' = case res of
                             Nothing -> textWidgetSetText msg
                                          "could not activate current cell"
                             Just s -> textWidgetSetText msg
                                         ("new content: <" ++ s ++ ">")
                loop new msg'
         _   -> loop tbw msg

styles = [CursesH.defaultStyle,
          CursesH.Style CursesH.WhiteF CursesH.PurpleB,
          CursesH.AttributeStyle [CursesH.Dim] CursesH.CyanF CursesH.WhiteB,
          CursesH.ColorlessStyle [CursesH.Bold]]

main :: IO ()
main =
    do CursesH.start
       cstyles <- CursesH.convertStyles styles
       Curses.cursSet Curses.CursorInvisible
       drawTextWidget (0, 0) (1, 60) DHFocus
                          (newTextWidget defaultTWOptions text)
       loop (tableWidget cstyles) (newTextWidget defaultTWOptions "")
       return ()
    `finally` CursesH.end
