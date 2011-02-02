-- Copyright (c) 2005-2011 Stefan Wehr (http://www.stefanwehr.de)
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-- Simple addressbook application to show the capabilities of the hscurses
-- library, especially its widget set.

module Main where

{-
TODO:

* save
* add
-}

import Control.Exception
import Control.Monad.State
import Data.List ( sort )
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Text.PrettyPrint.HughesPJ

import UI.HSCurses.Logging
import UI.HSCurses.Widgets
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

type Name = String
type Email = String
type Address = String
type ZIPCode = String
type City = String
type Province = String
type Country = String
type PhoneNumber = String

data Contact = Contact
    { lastName         :: Name
    , firstName        :: Name
    , emailAddress     :: Email
    , address          :: Address
    , zipCode          :: ZIPCode
    , city             :: City
    , province         :: Province
    , country          :: Country
    , phoneNumber      :: PhoneNumber
    }
    deriving (Show,Read,Eq,Ord)

emptyContact = Contact
               { lastName         = ""
               , firstName        = ""
               , emailAddress     = ""
               , address          = ""
               , zipCode          = ""
               , city             = ""
               , province         = ""
               , country          = ""
               , phoneNumber      = ""
               }

pprContact c =
    pprLine (combine (lastName c) ", " (firstName c)) $
    pprLine (address c) $
    pprLine (combine (zipCode c) " " (city c)) $
    pprLine (province c) $
    pprLine (country c) $
    pprLine (phoneNumber c) $
    pprLine (emailAddress c) $
    empty
    where
    pprLine :: String -> Doc -> Doc
    pprLine [] = (<>) empty
    pprLine s = ($$) (text s)
    combine [] _ s2 = s2
    combine s1 _ [] = s1
    combine s1 delim s2 = s1 ++ delim ++ s2

contactToLabelValueList :: Contact -> [(String, String)]
contactToLabelValueList c =
    [ ("Last Name", lastName c)
    , ("First Name", firstName c)
    , ("Email", emailAddress c)
    , ("Address", address c)
    , ("ZIP Code", zipCode c)
    , ("City", city c)
    , ("State/Province", province c)
    , ("Country", country c)
    , ("Phone", phoneNumber c) ]

readContacts :: FilePath -> IO [Contact]
readContacts f =
    do s <- readFile f
       case reads s of
         [(contacts, [])] -> return (sort contacts)
         _ -> error ("corrupt contact file: " ++ f)

writeContacts :: FilePath -> [Contact] -> IO ()
writeContacts f contacts =
    writeFile f (show contacts)

sampleContacts =
    [ emptyContact { lastName = "Wehr"
                   , firstName = "Stefan"
                   , emailAddress = "mail AT stefanwehr DOT de"
                   , address = "28 Loch Maree St"
                   , zipCode = "2032"
                   , city = "Kingsford"
                   , province = "NSW"
                   , country = "Australia" }
    , emptyContact { lastName = "Thorpe"
                   , firstName = "Ian"
                   , emailAddress = "ian@aol7.com.au"
                   , city = "Perth"
                   , country = "Australia" }
    , emptyContact { lastName = "Gates"
                   , firstName = "Bill"
                   , emailAddress = "billy@microsoft.com" }
    , emptyContact { lastName = "Stewart"
                   , firstName = "Don"
                   , address = "CSE, UNSW, 501-16, k17 building"
                   , city = "Sydney"
                   , country = "Australia" } ]


title = "contact-manager"

help = "q:quit, d:delete, a:add"

data CMState = CMState
    { cm_styles  :: [CursesH.CursesStyle]
    , cm_contacts :: [Contact]
    }
type CM = StateT CMState IO

runCM :: [CursesH.CursesStyle] -> [Contact] -> CM a -> IO a
runCM stys contacts cm = evalStateT cm (CMState { cm_styles = stys
                                                , cm_contacts = contacts })

nthStyle :: Int -> CM CursesH.CursesStyle
nthStyle n =
    do cs <- gets cm_styles
       return $ cs !! n

getSize = liftIO $ Curses.scrSize

styles = [ CursesH.defaultStyle
         , CursesH.AttributeStyle [CursesH.Bold] CursesH.GreenF CursesH.DarkBlueB
         ]


defStyle = nthStyle 0
lineStyle = nthStyle 1

lineDrawingStyle =
    do sty <- lineStyle
       return $ mkDrawingStyle sty

lineOptions =
    do sz <- getSize
       ds <- lineDrawingStyle
       return $ TWOptions { twopt_size = TWSizeFixed (1, getWidth sz),
                            twopt_style = ds,
                            twopt_halign = AlignLeft }

type ToplineWidget = TextWidget
type MidlineWidget = TextWidget
type BotlineWidget = TextWidget
type MsglineWidget = TableWidget
type ContactListWidget = TableWidget
type ContactDetailsWidget = TextWidget
type ContactEditWidget = TableWidget

mkToplineWidget =
    do opts <- lineOptions
       return $ newTextWidget (opts { twopt_halign = AlignCenter })
                  title

mkMidlineWidget :: ContactListWidget -> CM MidlineWidget
mkMidlineWidget listWidget =
    do opts <- lineOptions
       contacts <- gets cm_contacts
       let s = case tbw_pos listWidget of
                 Nothing -> show (length contacts)
                 Just (row, _) -> show (1+row) ++ "/" ++ show (length contacts)
       return $ newTextWidget  (opts { twopt_halign = AlignRight }) s

mkBotlineWidget =
    do opts <- lineOptions
       return $ newTextWidget opts help

-- We need to insert a dummy widget at the lower-right corner of the window,
-- i.e. at the lower-right corner of the message line. Otherwise, an
-- error occurs because drawing a character to this position moves the
-- cursor to the next line, which doesn't exist.
mkMsglineWidget =
    do sz <- getSize
       let width = getWidth sz
           opts = TWOptions { twopt_size = TWSizeFixed (1, width - 1),
                              twopt_style = defaultDrawingStyle,
                              twopt_halign = AlignLeft }
           tw = newTextWidget opts "msgline"
           row = [TableCell tw, TableCell $ EmptyWidget (1,1)]
           tabOpts = defaultTBWOptions { tbwopt_minSize = (1, width) }
       return $ newTableWidget tabOpts [row]

nlines = 4

contactListHeight (h, _) = (h - nlines) `div` 2

contactDetailsHeight (h, _) =
    let n = h - nlines
        in n `div` 2 + (n `mod` 2)

contactListOptions =
    do sz <- getSize
       return $ TBWOptions
                  { tbwopt_fillCol = Nothing,
                    tbwopt_fillRow = None,
                    tbwopt_activeCols = [0],
                    tbwopt_minSize = (contactListHeight sz, getWidth sz) }

contactDetailsOptions =
    do sz <- getSize
       return $ TWOptions { twopt_size = TWSizeFixed (contactDetailsHeight sz,
                                                         getWidth sz),
                            twopt_style = defaultDrawingStyle,
                            twopt_halign = AlignLeft }

mkContactListWidget :: CM ContactListWidget
mkContactListWidget =
    do contacts <- gets cm_contacts
       sz <- getSize
       let lines = alignRows (map contactLine contacts) ' ' "  "
           rows = map (contactRow $ getWidth sz) lines
       opts <- contactListOptions
       return $ newTableWidget opts rows
    where contactLine c = [lastName c, firstName c, emailAddress c]
          contactRow w s = [TableCell $ newTextWidget
                            (defaultTWOptions { twopt_size = TWSizeFixed (1, w) }) s]
          lastRow = [TableCell (EmptyWidget (0,0))]

mkContactDetailsWidget :: ContactListWidget -> CM ContactDetailsWidget
mkContactDetailsWidget listWidget =
    do contacts <- gets cm_contacts
       let contact = case tbw_pos listWidget of
                       Nothing -> ""
                       Just (row, _) ->
                           let c = contacts !! row
                               in show $ pprContact c
       opts <- contactDetailsOptions
       return $ newTextWidget opts contact

mkContactEditWidget :: Contact -> CM ContactEditWidget
mkContactEditWidget contact =
    let l = contactToLabelValueList contact
        rows = map mkRow l
        in do sz <- getSize
              let opts = TBWOptions
                         { tbwopt_fillCol = Just 1,
                           tbwopt_fillRow = None,
                           tbwopt_activeCols = [1],
                           tbwopt_minSize = (getHeight sz - 3, getWidth sz) }
              return $ newTableWidget opts rows
    where mkRow (label, value) =
              let labelW = newTextWidget defaultTWOptions label
                  valueW = newEditWidget defaultEWOptions value
                  in [TableCell labelW, ActiveTableCell valueW]

mkMainEditWidget contact =
    do tlw <- mkToplineWidget
       blw <- mkBotlineWidget
       msglw <- mkMsglineWidget
       ew <- mkContactEditWidget contact
       return $ MainEditWidget tlw blw msglw ew

data MainEditWidget = MainEditWidget
    { toplineEditWidget :: ToplineWidget
    , botlineEditWidget :: BotlineWidget
    , msglineEditWidget :: MsglineWidget
    , contactEditWidget :: ContactEditWidget }

mkMainWidget =
    do tlw <- mkToplineWidget
       clw <- mkContactListWidget
       mlw <- mkMidlineWidget clw
       cdw <- mkContactDetailsWidget clw
       blw <- mkBotlineWidget
       msglw <- mkMsglineWidget
       return $ MainWidget tlw mlw blw msglw clw cdw

instance Widget MainEditWidget where
    draw pos sz hint w = draw pos sz hint (mkRealMainEditWidget (Just sz) w)
    minSize w = minSize (mkRealMainEditWidget Nothing w)

mkRealMainEditWidget :: (Maybe Size) -> MainEditWidget -> TableWidget
mkRealMainEditWidget msz w =
    let cells = [ TableCell $ toplineEditWidget w
                , TableCell $ contactEditWidget w
                , TableCell $ botlineEditWidget w
                , TableCell $ msglineEditWidget w ]
        rows = map singletonRow cells
        opts = case msz of
                 Nothing -> defaultTBWOptions
                 Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
        in newTableWidget opts rows

data MainWidget = MainWidget
    { toplineWidget :: ToplineWidget
    , midlineWidget :: MidlineWidget
    , botlineWidget :: BotlineWidget
    , msglineWidget :: MsglineWidget
    , contactListWidget :: ContactListWidget
    , contactDetailsWidget :: ContactDetailsWidget }

instance Widget MainWidget where
    draw pos sz hint w = draw pos sz hint (mkRealMainWidget (Just sz) w)
    minSize w = minSize (mkRealMainWidget Nothing w)

mkRealMainWidget msz w =
    let cells = [ TableCell $ toplineWidget w
                , TableCell $ contactListWidget w
                , TableCell $ midlineWidget w
                , TableCell $ contactDetailsWidget w
                , TableCell $ botlineWidget w
                , TableCell $ msglineWidget w ]
        rows = map singletonRow cells
        opts = case msz of
                 Nothing -> defaultTBWOptions
                 Just sz -> defaultTBWOptions { tbwopt_minSize = sz }
        in newTableWidget opts rows

updateStateDependentWidgets :: MainWidget -> ContactListWidget -> CM MainWidget
updateStateDependentWidgets w listWidget =
    do detailsWidget <- mkContactDetailsWidget listWidget
       midlineWidget <- mkMidlineWidget listWidget
       return $ w { contactListWidget = listWidget
                  , contactDetailsWidget = detailsWidget
                  , midlineWidget = midlineWidget }

move :: Direction -> MainWidget -> CM MainWidget
move dir w =
    do sz <- getSize
       let listWidget = tableWidgetMove dir sz (contactListWidget w)
       updateStateDependentWidgets w listWidget

delete w =
    let lw = contactListWidget w
        in case tbw_pos lw of
             Nothing -> return w
             Just (row,_) ->
                 let lw' = tableWidgetDeleteRow row lw
                     in do modify (\s -> s { cm_contacts =
                                             deleteAt row (cm_contacts s) })
                           updateStateDependentWidgets w lw'
{-
editEventloop w ewm =
    do k <- CursesH.getKey (resize mkMainEditWidget)
       case k of
         Curses.KeyChar 'q' -> return w
         Curses.KeyChar '\r' ->
             do debug "editing..."
                sz <- getSize
                let ewm' = mkRealMainEditWidget (Just sz) ewm
                    (epos, esz) = getCellInfo (0,0) sz ewm' (1,0)
                    ew = contactEditWidget ewm
                (ew', res) <-
                    tableWidgetActivateCurrent (redraw ewm) epos esz DHFocus ew
                editEventloop w ewm
         _ -> editEventloop w ewm
-}

edit w =
    let lw = contactListWidget w
        in case tbw_pos lw of
             Nothing -> return w
             Just (row,_) ->
                 do contacts <- gets cm_contacts
                    let c = contacts !! row
                    ew <- mkMainEditWidget c
                    redraw ew
                    return w
                    --editEventloop w ew

resize :: Widget w => CM w -> CM ()
resize f =
    do liftIO $ do Curses.endWin
                   Curses.resetParams
                   Curses.cursSet Curses.CursorInvisible
                   Curses.refresh
       w <- f
       redraw w

redraw :: Widget w => w -> CM ()
redraw w =
    do sz <- getSize
       liftIO $ draw (0, 0) sz DHNormal w
       liftIO $ Curses.refresh

eventloop w =
    do k <- CursesH.getKey (resize mkMainWidget)
       case k of
         Curses.KeyChar 'q' -> return ()
         Curses.KeyChar 'd' -> process $ delete w
         Curses.KeyChar 'e' -> process $ edit w
         Curses.KeyUp       -> process $ move DirUp w
         Curses.KeyDown     -> process $ move DirDown w
         _ -> eventloop w
    where process f =
              do w' <- f
                 redraw w'
                 eventloop w'
cmMain :: CM ()
cmMain =
    do w <- mkMainWidget
       redraw w
       eventloop w


main :: IO ()
main =
    do args <- getArgs
       contacts <-
             if length args /= 1
                then do p <- getProgName
                        putStrLn ("Usage: " ++ p ++ " contact-file")
                        exitFailure
                else readContacts (args!!0)
       runCurses contacts `finally` CursesH.end
    where runCurses contacts =
              do CursesH.start
                 cstyles <- CursesH.convertStyles styles
                 Curses.cursSet Curses.CursorInvisible
                 runCM cstyles contacts cmMain
