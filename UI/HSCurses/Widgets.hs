{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}
-- glasgow-exts needed for existentials and multi-parameter type classes.

-- Copyright (c) 2005-2011 Stefan Wehr - http://www.stefanwehr.de
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

module UI.HSCurses.Widgets where

import Control.Exception (assert)
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Maybe

import UI.HSCurses.Logging
import UI.HSCurses.MonadException
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

type Pos = (Int, Int)
type Offset = (Int, Int)

type Size = (Int, -- height
             Int  -- width
            )

getHeight :: Size -> Int
getHeight = fst

getWidth :: Size -> Int
getWidth = snd

getYOffset :: Offset -> Int
getYOffset = fst

getXOffset :: Offset -> Int
getXOffset = snd

getYPos :: Pos -> Int
getYPos = fst

getXPos :: Pos -> Int
getXPos = snd

data Direction = DirLeft | DirRight | DirUp | DirDown
               deriving (Eq, Show, Ord)

data HAlignment = AlignLeft | AlignCenter | AlignRight
               deriving (Eq, Show)

data Cont a = Cont a | Done a

class Widget a where
    draw      :: Pos -> Size -> DrawingHint -> a -> IO ()
    minSize   :: a -> Size

class Widget a => ActiveWidget a where
    activate  :: MonadExcIO m => m () -> Pos -> Size -> a -> m (a, String)

type KeyHandler a = Pos -> Size -> a -> IO (Cont a)

mkKeyHandler :: (Pos -> Size -> a -> a)
                -> KeyHandler a
mkKeyHandler f pos sz w = return (Cont (f pos sz w))

--
-- Drawing
--

data DrawingHint = DHNormal
                 | DHFocus
                 | DHActive
                   deriving (Eq, Show, Ord)

data DrawingStyle = DStyle
    { dstyle_normal      :: CursesH.CursesStyle
    , dstyle_focus       :: CursesH.CursesStyle
    , dstyle_active      :: CursesH.CursesStyle
    } deriving (Eq, Show)


mkDrawingStyle :: CursesH.CursesStyle -> DrawingStyle
mkDrawingStyle defStyle =
    let revStyle = CursesH.changeCursesStyle defStyle [CursesH.Reverse]
        in DStyle { dstyle_normal = defStyle
                  , dstyle_focus = revStyle
                  , dstyle_active = revStyle
                  }

defaultDrawingStyle :: DrawingStyle
defaultDrawingStyle = mkDrawingStyle CursesH.defaultCursesStyle
_draw :: DrawingHint -> DrawingStyle -> IO a -> IO a
_draw DHActive sty io = CursesH.withStyle (dstyle_active sty) io
_draw DHNormal sty io = CursesH.withStyle (dstyle_normal sty) io
_draw DHFocus sty io = CursesH.withStyle (dstyle_focus sty) io

--
-- Helper functions for scrolling
--

scrollFactor :: Double
scrollFactor = 0.8

scrollBy :: Int -> Int
scrollBy displayLen =
    let amount = floor ((fromInteger . toInteger) displayLen * scrollFactor)
    in max (displayLen - 1) (min 1 amount)

-- returns the new offset for scrolling in forward direction
-- dataLen: total number of data items
-- offset: the index of the first data item shown on the current page
-- displayLen: the number of data items that is shown in one page
scrollForward :: Int -> Int -> Int -> Int
scrollForward dataLen offset displayLen =
    if offset + displayLen >= dataLen
       then offset
       else min (offset + scrollBy displayLen) (dataLen - displayLen)

-- returns the new offset for scrolling in backward direction.
-- parameters as for scrollForward
scrollBackward :: t -> Int -> Int -> Int
scrollBackward _ offset displayLen =
    if offset == 0
       then offset
       else max (offset - scrollBy displayLen) 0


--
-- EmptyWidget
--

data EmptyWidget = EmptyWidget Size

instance Widget EmptyWidget where
    draw _ _ _ _ = return ()
    minSize (EmptyWidget sz) = sz


--
-- An opaque widget
--

data OpaqueWidget = OpaqueWidget Size

instance Widget OpaqueWidget where
    draw (y,x) (h,w) _ _ =
        let draw' n =
                do Curses.wMove Curses.stdScr (y+n) x
                   CursesH.drawLine w ""
        in do mapM draw' (take h [0..])
              Curses.refresh
    minSize (OpaqueWidget sz) = sz

--
-- Widget for text input
--

data EditWidget = EditWidget
    { ew_content       :: String,
      ew_xoffset       :: Int, -- content!!xoffset is the 1st char shown
      ew_xcursor       :: Int, -- cursor position
      ew_history       :: [String],
      ew_historyIndex  :: Int,
      ew_historySavedContent :: Maybe String,
      ew_options       :: EditWidgetOptions
    }

ew_contentPos :: EditWidget -> Int
ew_contentPos ew = ew_xcursor ew + ew_xoffset ew

instance Widget EditWidget where
    draw = drawEditWidget
    minSize ew = (1, ewopt_minWidth $ ew_options ew)

instance ActiveWidget EditWidget where
    activate = activateEditWidget

data EditWidgetOptions = EWOptions
    { ewopt_keyHandlers    :: [(Curses.Key, KeyHandler EditWidget)],
      ewopt_minWidth       :: Int,
      ewopt_style          :: DrawingStyle
    }

defaultEWOptions :: EditWidgetOptions
defaultEWOptions = EWOptions
                   { ewopt_keyHandlers = editWidgetKeyHandlers,
                     ewopt_minWidth = 8,
                     ewopt_style = defaultDrawingStyle
                   }

newEditWidget :: EditWidgetOptions -> String -> EditWidget
newEditWidget opts =
    editWidgetSetContent
      (EditWidget
       { ew_content = "",
         ew_xoffset = 0,
         ew_xcursor = 0,
         ew_history = [],
         ew_historyIndex = -1,
         ew_historySavedContent = Nothing,
         ew_options = opts
       })



editWidgetGoLeft :: Pos
                                                -> Size
                                                -> EditWidget
                                                -> IO (Cont EditWidget)
editWidgetGoLeft = mkKeyHandler editWidgetGoLeft'
editWidgetGoRight :: Pos
                                                 -> Size
                                                 -> EditWidget
                                                 -> IO (Cont EditWidget)
editWidgetGoRight = mkKeyHandler editWidgetGoRight'
editWidgetDeleteLeft :: Pos
                                                    -> Size
                                                    -> EditWidget
                                                    -> IO (Cont EditWidget)
editWidgetDeleteLeft = mkKeyHandler editWidgetDeleteLeft'
editWidgetDeleteUnderCursor :: Pos
                                                           -> Size
                                                           -> EditWidget
                                                           -> IO (Cont EditWidget)
editWidgetDeleteUnderCursor = mkKeyHandler editWidgetDeleteUnderCursor'
editWidgetDeleteToEnd :: Pos
                                                     -> Size
                                                     -> EditWidget
                                                     -> IO (Cont EditWidget)
editWidgetDeleteToEnd = mkKeyHandler editWidgetDeleteToEnd'
editWidgetGoHome :: Pos
                                                -> Size
                                                -> EditWidget
                                                -> IO (Cont EditWidget)
editWidgetGoHome = mkKeyHandler editWidgetGoHome'
editWidgetGoEnd :: Pos
                                               -> Size
                                               -> EditWidget
                                               -> IO (Cont EditWidget)
editWidgetGoEnd = mkKeyHandler editWidgetGoEnd'
editWidgetHistoryUp :: Pos
                                                   -> Size
                                                   -> EditWidget
                                                   -> IO (Cont EditWidget)
editWidgetHistoryUp = mkKeyHandler editWidgetHistoryUp'
editWidgetHistoryDown :: Pos
                                                     -> Size
                                                     -> EditWidget
                                                     -> IO (Cont EditWidget)
editWidgetHistoryDown = mkKeyHandler editWidgetHistoryDown'

editWidgetKeyHandlers :: [(Curses.Key,
                                                       Pos
                                                       -> Size
                                                       -> EditWidget
                                                       -> IO (Cont EditWidget))]
editWidgetKeyHandlers =
    [(Curses.KeyLeft, editWidgetGoLeft),
     (Curses.KeyRight, editWidgetGoRight),
     (Curses.KeyBackspace, editWidgetDeleteLeft),
     (Curses.KeyChar '\^D', editWidgetDeleteUnderCursor),
     (Curses.KeyDC, editWidgetDeleteUnderCursor),
     (Curses.KeyChar '\^K', editWidgetDeleteToEnd),
     (Curses.KeyHome, editWidgetGoHome),
     (Curses.KeyChar '\^A', editWidgetGoHome),
     (Curses.KeyEnd, editWidgetGoEnd),
     (Curses.KeyChar '\^E', editWidgetGoEnd),
     (Curses.KeyChar '\r', editWidgetFinish),
     (Curses.KeyChar '\t', editWidgetFinish),
     (Curses.KeyUp, editWidgetHistoryUp),
     (Curses.KeyDown, editWidgetHistoryDown)
    ]

editWidgetGetContent :: EditWidget -> String
editWidgetGetContent ew = ew_content ew
editWidgetSetContent :: EditWidget
                                                    -> String
                                                    -> EditWidget
editWidgetSetContent ew s =
    addToHistory (ew { ew_content = s, ew_xoffset = 0, ew_xcursor = 0 }) s

editWidgetGetOptions :: EditWidget
                                                    -> EditWidgetOptions
editWidgetGetOptions ew = ew_options ew
editWidgetSetOptions :: EditWidget
                                                    -> EditWidgetOptions
                                                    -> EditWidget
editWidgetSetOptions ew opts = ew { ew_options = opts }

drawEditWidget :: Pos -> Size -> DrawingHint -> EditWidget -> IO ()
drawEditWidget (y, x) (_, width) hint ew =
    _draw hint (ewopt_style . ew_options $ ew) $
    do Curses.wMove Curses.stdScr y x
       CursesH.drawLine width (drop (ew_xoffset ew) $ ew_content ew)
       Curses.refresh

activateEditWidget :: MonadExcIO m => m () -> Pos -> Size
                   -> EditWidget -> m (EditWidget, String)
activateEditWidget refresh pos@(y, x) sz@(_, width) ew =
    CursesH.withCursor Curses.CursorVisible $ processKey ew
    where
    processKey ex =
        do liftIO $ drawLocal ex
           k <- CursesH.getKey refresh
           case lookup k (ewopt_keyHandlers $ ew_options ex) of
             Nothing ->
                 case k of
                   Curses.KeyChar c | isAscii c && isPrint c
                       -> processKey $ insertChar ex c
                   _   -> processKey ex
             Just f  ->
                 do x' <- liftIO $ f pos sz ex
                    case x' of
                      Cont ex' -> processKey ex'
                      Done ex' -> do liftIO $ drawEditWidget pos sz DHActive ex'
                                     return (ex', editWidgetGetContent ex')
    insertChar ew' c =
        let pos' = ew_contentPos ew'
            oldContent = ew_content ew'
            newContent = take pos' oldContent ++ (c : drop pos' oldContent)
            in editWidgetGoRight' pos' sz (ew' { ew_content = newContent })
    drawLocal ew' = _draw DHActive  (ewopt_style . ew_options $ ew') $
        do Curses.wMove Curses.stdScr y x
           CursesH.drawLine width (drop (ew_xoffset ew') $ ew_content ew')
           Curses.wMove Curses.stdScr y (x + ew_xcursor ew')
           Curses.refresh

editWidgetGoLeft' :: t -> t1 -> EditWidget -> EditWidget
editWidgetGoLeft' _ _ ew =
    let newXcursor = max (ew_xcursor ew - 1) 0
        newXoffset = if ew_xcursor ew == 0
                        then max (ew_xoffset ew - 1) 0
                        else ew_xoffset ew
        in ew { ew_xoffset = newXoffset,
                ew_xcursor = newXcursor }

editWidgetGoRight' :: t -> (t1, Int) -> EditWidget -> EditWidget
editWidgetGoRight' _ (_, width) ew =
    let len = length (ew_content ew)
        lastChar = len - ew_xoffset ew - 1
        newXcursor = minimum [ew_xcursor ew + 1, lastChar + 1, width - 1]
        newXoffset = if ew_xcursor ew == width - 1
                        then min (ew_xoffset ew + 1) (len - width + 1)
                        else ew_xoffset ew
        in ew { ew_xoffset = newXoffset,
                ew_xcursor = newXcursor }

editWidgetDeleteLeft' :: Pos -> Size -> EditWidget -> EditWidget
editWidgetDeleteLeft' pos sz ew =
    let cpos = ew_contentPos ew - 1
        oldContent = ew_content ew
        newContent = take cpos oldContent ++ drop (cpos+1) oldContent
        ew' = editWidgetGoLeft' pos sz (ew { ew_content = newContent })
        in if ew_xcursor ew == 0 && ew_xoffset ew /= 0
              then editWidgetGoRight' pos sz (editWidgetGoLeft' pos sz ew')
              else ew'

editWidgetDeleteUnderCursor' :: t -> t1 -> EditWidget -> EditWidget
editWidgetDeleteUnderCursor' _ _ ew =
    let pos = ew_contentPos ew
        oldContent = ew_content ew
        newContent = take pos oldContent ++ drop (pos+1) oldContent
        in ew { ew_content = newContent }

editWidgetDeleteToEnd' :: t -> t1 -> EditWidget -> EditWidget
editWidgetDeleteToEnd' _ _ ew =
    let pos = ew_contentPos ew
        oldContent = ew_content ew
        newContent = take pos oldContent
        in ew { ew_content = newContent }

editWidgetGoHome' :: t -> t1 -> EditWidget -> EditWidget
editWidgetGoHome' _ _ ew =
    ew { ew_xcursor = 0,
         ew_xoffset = 0 }

editWidgetGoEnd' :: Pos -> Size -> EditWidget -> EditWidget
editWidgetGoEnd' pos sz ew =
    let cpos = ew_contentPos ew
        len = length (ew_content ew)
        in if cpos == len
              then ew
              else editWidgetGoEnd' pos sz (editWidgetGoRight' pos sz ew)

editWidgetFinish :: (Monad m) => t -> t1 -> EditWidget -> m (Cont EditWidget)
editWidgetFinish _ _ ew =  return (Done (addToHistory ew (ew_content ew)))

maxHistoryLength :: Int
maxHistoryLength = 50

addToHistory :: EditWidget -> [Char] -> EditWidget
addToHistory ew s =
    let newHist = if not (null s)
                     then take maxHistoryLength (s : ew_history ew)
                     else ew_history ew
        in ew { ew_history = newHist, ew_historyIndex = -1,
                ew_historySavedContent = Nothing }

editWidgetHistoryUp' :: t -> t1 -> EditWidget -> EditWidget
editWidgetHistoryUp' _ _ ew = editWidgetHistory (+) ew

editWidgetHistoryDown' :: t -> t1 -> EditWidget -> EditWidget
editWidgetHistoryDown' _ _ ew = editWidgetHistory (-) ew

-- ew_historyList: list of history items, i.e. non-null strings which were
--   entered into the widget and confirmed with ENTER or which were added
--   via editWidgetSetContent.
-- ew_historyIndex: the index of the history item shown in the widget. The
--   value -1 means that the value saved in ew_historySavedContent should
--   be shown.
editWidgetHistory :: (Num t) => (Int -> t -> Int) -> EditWidget -> EditWidget
editWidgetHistory op ew =
    let i = ew_historyIndex ew
        l = ew_history ew
        j =  i `op` 1
        in if j >= 0 && j < length l
              then let savedContent =
                           case ew_historySavedContent ew of
                             Nothing -> Just (ew_content ew)
                             x -> x
                       in ew { ew_historyIndex = j, ew_content = l!!j,
                               ew_historySavedContent = savedContent,
                               ew_xcursor = 0, ew_xoffset = 0 }
              else if j == -1
                      then case ew_historySavedContent ew of
                             Nothing -> ew
                             Just x -> ew { ew_content = x,
                                            ew_historyIndex = j,
                                            ew_xcursor = 0,
                                            ew_xoffset = 0  }
                   else ew



--
-- Text widget
--

data TextWidget = TextWidget
    { tw_text           :: String,
      tw_yoffset        :: Int,
      tw_xoffset        :: Int,
      tw_options        :: TextWidgetOptions
    }
    deriving (Eq, Show)

instance Widget TextWidget where
    draw = drawTextWidget
    minSize tw =
        case twopt_size $ tw_options tw of
          TWSizeDefault -> let l = lines (tw_text tw)
                           in (length l, if null l then 0 else maximum (map length l))
          TWSizeFixed sz -> sz

data TextWidgetSize = TWSizeDefault    -- minimal size determined by content
                    | TWSizeFixed Size -- minimal size is fixed, content is
                                       -- possibly cut off
                      deriving (Eq, Show)
                                 {-
                    | Autowrap   -- minimal width determined by content,
                                 -- but lines are wrapped if necessary
                                 -}

data TextWidgetOptions = TWOptions
    { twopt_size   :: TextWidgetSize,
      twopt_style  :: DrawingStyle,
      twopt_halign :: HAlignment }
    deriving (Eq, Show)

defaultTWOptions :: TextWidgetOptions
defaultTWOptions = TWOptions
                { twopt_size = TWSizeDefault,
                  twopt_style = defaultDrawingStyle,
                  twopt_halign = AlignLeft }

newTextWidget :: TextWidgetOptions -> String -> TextWidget
newTextWidget opts s = TextWidget
                       { tw_text = s,
                         tw_yoffset = 0,
                         tw_xoffset = 0,
                         tw_options = opts
                       }

drawTextWidget :: Pos -> Size -> DrawingHint -> TextWidget -> IO ()
drawTextWidget (y, x) (height, width) hint tw =
    let ly = take height $ drop (tw_yoffset tw) (lines (tw_text tw))
        l = take height $ (map (drop (tw_xoffset tw)) ly ++ repeat [])
        l' = map (align (twopt_halign $ tw_options tw) width ' ') l
    in --trace ("drawing text widget at " ++ show pos ++ " with size " ++ show sz) $
       do _draw hint (twopt_style . tw_options $ tw)
                      (mapM drawLine $ zip l' [0..])
          Curses.refresh
    where drawLine (s, i) =
              do Curses.wMove Curses.stdScr (y + i) x
                 CursesH.drawLine width s

textWidgetGetText :: TextWidget -> String
textWidgetGetText = tw_text

textWidgetSetText :: TextWidget -> String -> TextWidget
textWidgetSetText tw s = tw { tw_text = s }

textWidgetScrollDown :: Size -> TextWidget -> TextWidget
textWidgetScrollDown (h, _) tw =
    let dataLen = length $ lines (tw_text tw)
        offset = tw_yoffset tw
        in tw { tw_yoffset = scrollForward dataLen offset h }

textWidgetScrollUp :: Size -> TextWidget -> TextWidget
textWidgetScrollUp (h, _) tw =
    let dataLen = length $ lines (tw_text tw)
        offset = tw_yoffset tw
        in tw { tw_yoffset = scrollBackward dataLen offset h }

textWidgetScrollLeft :: Size -> TextWidget -> TextWidget
textWidgetScrollLeft (_, w) tw =
    let dataLen = length $ lines (tw_text tw)
        offset = tw_xoffset tw
        in tw { tw_xoffset = scrollBackward dataLen offset w }

textWidgetScrollRight :: Size -> TextWidget -> TextWidget
textWidgetScrollRight (_, w) tw =
    let dataLen = length $ lines (tw_text tw)
        offset = tw_xoffset tw
        in tw { tw_xoffset = scrollForward dataLen offset w }


--
-- Table widget
--

data TableCell = forall w. Widget w => TableCell w
               | forall w. ActiveWidget w => ActiveTableCell w

isActive :: TableCell -> Bool
isActive (TableCell _) = False
isActive (ActiveTableCell _) = True

instance Widget TableCell where
    draw pos sz hint (TableCell w) = draw pos sz hint w
    draw pos sz hint (ActiveTableCell w) = draw pos sz hint w
    minSize (TableCell w) = minSize w
    minSize (ActiveTableCell w) = minSize w

_activateTableCell :: MonadExcIO m => m () -> Pos -> Size
                   -> TableCell -> m (TableCell, String)
_activateTableCell _ _ _ (TableCell _) =
    error "_activateTableCell: cannot activate non-active cell!"
_activateTableCell refresh pos sz (ActiveTableCell w) =
    do (new, res) <- activate refresh pos sz w
       return (ActiveTableCell new, res)

type Row = [TableCell]

singletonRow :: TableCell -> Row
singletonRow tc = [tc]

getCellWidget :: TableWidget -> (Int, Int) -> TableCell
getCellWidget tbw (row, col) = (tbw_rows tbw) !! row !! col

setCellWidget :: TableWidget -> (Int, Int) -> TableCell -> TableWidget
setCellWidget tbw (rowIndex, colIndex) w =
    let rows = tbw_rows tbw
        row = rows !! rowIndex
        newRow = listReplace row w colIndex
        newRows = listReplace rows newRow rowIndex
        in tbw { tbw_rows = newRows }

data TableWidget = TableWidget
    { tbw_rows     :: [Row],
      tbw_colOffset  :: Int,
      tbw_pos      :: Maybe Pos,
      tbw_options  :: TableWidgetOptions }

data FillRow = First | Last | None deriving (Eq,Show)

data TableWidgetOptions = TBWOptions
    { tbwopt_fillCol    :: Maybe Int,
      tbwopt_fillRow    :: FillRow,
      tbwopt_activeCols :: [Int],
      tbwopt_minSize    :: Size }
    deriving (Eq, Show)

defaultTBWOptions :: TableWidgetOptions
defaultTBWOptions = TBWOptions
                    { tbwopt_fillCol = Nothing,
                      tbwopt_fillRow = None,
                      tbwopt_activeCols = [],
                      tbwopt_minSize = (4, 10) }

instance Widget TableWidget where
    draw      = drawTableWidget
    minSize   = tbwopt_minSize . tbw_options

newTableWidget :: TableWidgetOptions -> [Row] -> TableWidget
newTableWidget opts rows = TableWidget
                              { tbw_rows = rows,
                                tbw_colOffset = 0,
                                tbw_pos = findFirstActiveCell rows opts,
                                tbw_options = opts }

data TableWidgetDisplayInfo =
    TBWDisplayInfo
    { tbwdisp_height     :: Int   -- height of the display area
    , tbwdisp_width      :: Int   -- width of the display area
    , tbwdisp_firstVis   :: Int   -- index of the first row visible
    , tbwdisp_lastVis    :: Int   -- index of the last row visible
    , tbwdisp_rows       :: [Row] -- the rows which are visible
    , tbwdisp_nrows      :: Int   -- the number of rows visible
    , tbwdisp_heights    :: [Int] -- the heights of the visible rows
    , tbwdisp_widths     :: [Int] -- the widths of the visible rows
      -- free space at the right side (xoffset, size)
    , tbwdisp_rightMargin :: Maybe (Int, Size)
    }

tableWidgetDisplayInfo :: Size -> TableWidget -> TableWidgetDisplayInfo
tableWidgetDisplayInfo (height, width) tbw =
    assert (isQuadratic (tbw_rows tbw)) $
    let allRows = tbw_rows tbw
        ncols = length (allRows!!0)
        colOffset = tbw_colOffset tbw
        allHeights = minSpaces getHeight allRows
        heights' = drop colOffset allHeights
        nrows = getNRows heights' 0 0
        rows = take nrows $ drop colOffset allRows
        (heights, heightDummy) =
            let hs = take nrows heights'
                s = sum hs
                d = height - s
                in case tbwopt_fillRow $ tbw_options tbw of
                     First -> (applyToFirst (+d) hs, 0)
                     Last -> (applyToLast (+d) hs, 0)
                     None -> (hs, d)
        widths' = minSpaces getWidth (transpose $ tbw_rows tbw)
        (widths, rightMargin) =
            if sum widths' > width
               then error ("table to wide: width=" ++ show (sum widths') ++
                           ", available width=" ++ show width)
               else case tbwopt_fillCol $ tbw_options tbw of
                      Just i | i >= 0 && i < ncols
                                 -> (take i widths' ++
                                     let rest = drop i widths'
                                     in (head rest + width - sum widths') : tail rest
                                    , Nothing)
                      _ -> let diff = width - sum widths'
                               msz = (height, diff)
                               m = if diff > 0 then Just (sum widths', msz)
                                   else Nothing
                           in (widths', m)
        dummyHeights = if heightDummy == 0 then [] else [heightDummy]
        dummyRows = if heightDummy == 0 then []
                    else [map (\w -> TableCell (OpaqueWidget (heightDummy, w)))
                          widths]
        in TBWDisplayInfo
               { tbwdisp_height = height
               , tbwdisp_width = width
               , tbwdisp_firstVis = colOffset
               , tbwdisp_lastVis = colOffset + nrows - 1
               , tbwdisp_rows = rows ++ dummyRows
               , tbwdisp_nrows = nrows + length dummyRows
               , tbwdisp_heights = heights ++ dummyHeights
               , tbwdisp_widths = widths
               , tbwdisp_rightMargin = rightMargin
               }
    where
        minSpaces f ls =
            snd $ mapAccumL
                    (\acc ws ->
                       (acc, acc + maximum (map (f . minSize) ws)))
                    0 ls
        getNRows (h:hs) n acc | h + n <= height = getNRows hs (h+n) (acc+1)
        getNRows _ _ acc = acc
        isQuadratic [] = True
        isQuadratic (x:xs) = isQuadratic' xs (length x)
        isQuadratic' (x:xs) n = length x == n && isQuadratic' xs n
        isQuadratic' [] _ = True
        applyToFirst _ [] = []
        applyToFirst f (x:xs) = f x : xs
        applyToLast _ [] = []
        applyToLast f l =
            let (h, t) = (head $ reverse l, tail $ reverse l)
                in reverse $ f h : t

getCellInfo :: Pos -> Size -> TableWidget -> (Int,Int) -> (Pos, Size)
getCellInfo (y,x) sz tbw (row, col) =
    let info = tableWidgetDisplayInfo sz tbw
        heights = tbwdisp_heights info
        widths = tbwdisp_widths info
        h = heights !! row
        w = widths !! col
        yoff = sum $ take row heights
        xoff = sum $ take col widths
        in ((y+yoff, x+xoff), (h, w))

drawTableWidget :: Pos -> Size -> DrawingHint -> TableWidget -> IO ()
drawTableWidget (y, x) sz hint tbw =
    let info = tableWidgetDisplayInfo sz tbw
        heights = tbwdisp_heights info
        widths = tbwdisp_widths info
        firstVis = tbwdisp_firstVis info
        rows = tbwdisp_rows info
        rightMargin = tbwdisp_rightMargin info
        in do drawRows rows heights widths 0 firstVis hint
              case rightMargin of
                Nothing -> return ()
                Just (xoff,s) -> draw (y,x+xoff) s hint (OpaqueWidget s)
              Curses.refresh
    where drawRows :: [Row] -> [Int] -> [Int] -> Int -> Int
                   -> DrawingHint -> IO ()
          drawRows [] _ _ _ _ _ = return ()
          drawRows (r:rs) (h:hs) widths yoffset rowIndex hint' =
            do drawCols r h widths yoffset 0 (rowIndex, 0) hint'
               drawRows rs hs widths (yoffset + h) (rowIndex + 1) hint'
          drawCols :: Row -> Int -> [Int] -> Int -> Int -> (Int, Int)
                   -> DrawingHint -> IO ()
          drawCols [] _ _ _ _ _ _ = return ()
          drawCols (c:cs) h (w:ws) yoffset xoffset (rowIndex, colIndex) hint' =
            let hint'' = case tbw_pos tbw of
                           Just (z, a) | z == rowIndex && a == colIndex
                               -> DHFocus
                           _ -> hint'
            in do draw (y+yoffset, x+xoffset) (h,w) hint'' c
                  drawCols cs h ws yoffset (xoffset + w)
                           (rowIndex, colIndex+1) hint'


tableWidgetScrollDown :: Size -> TableWidget -> TableWidget
tableWidgetScrollDown (h, _) tbw =
    let dataLen = length $ tbw_rows tbw
        offset = tbw_colOffset tbw
        newOffset = scrollForward dataLen offset h
        newTbw = tbw { tbw_colOffset = newOffset }
        in case tbw_pos newTbw of
             Nothing -> newTbw
             Just (y,x) -> newTbw { tbw_pos = Just (max newOffset y, x) }

tableWidgetScrollUp :: Size -> TableWidget -> TableWidget
tableWidgetScrollUp sz@(h,_) tbw =
    let dataLen = length $ tbw_rows tbw
        offset = tbw_colOffset tbw
        newOffset = scrollBackward dataLen offset h
        newTbw = tbw { tbw_colOffset = newOffset }
        newLastVis = tbwdisp_lastVis (tableWidgetDisplayInfo sz newTbw)
        in case tbw_pos newTbw of
             Nothing -> newTbw
             Just (y,x) ->
                 newTbw { tbw_pos = Just (min newLastVis y, x) }

tableWidgetActivateCurrent :: MonadExcIO m => m () -> Pos -> Size
                           -> DrawingHint -> TableWidget
                           -> m (TableWidget, Maybe String)
tableWidgetActivateCurrent refresh (y, x) sz _ tbw =
    case tbw_pos tbw of
      Nothing -> do debug "tableWidgetActivateCurrent: pos=Nothing"
                    return (tbw, Nothing)
      Just p -> let w = getCellWidget tbw p
                    in if not $ isActive w
                       then do debug "tableWidgetActivateCurrent: not active"
                               return (tbw, Nothing)
                       else activate' w p
    where
    activate' widget colyx@(coly, colx) =
        let info = tableWidgetDisplayInfo sz tbw
            vcol = colx
            vrow = coly - tbwdisp_firstVis info
            heights = tbwdisp_heights info
            widths = tbwdisp_widths info
            h = heights !! vrow
            w = widths !! vcol
            yoffset = sum (take vrow heights)
            xoffset = sum (take vcol widths)
        in do (new, res) <- _activateTableCell refresh (y+yoffset, x+xoffset)
                              (h, w) widget
              return (setCellWidget tbw colyx new, Just res)

tableWidgetGoLeft :: Size -> TableWidget -> TableWidget
tableWidgetGoLeft =  tableWidgetMove DirLeft

tableWidgetGoRight :: Size -> TableWidget -> TableWidget
tableWidgetGoRight =  tableWidgetMove DirRight

tableWidgetGoUp :: Size -> TableWidget -> TableWidget
tableWidgetGoUp =  tableWidgetMove DirUp

tableWidgetGoDown :: Size -> TableWidget -> TableWidget
tableWidgetGoDown =  tableWidgetMove DirDown

tableWidgetMove :: Direction
                                               -> (Int, Int)
                                               -> TableWidget
                                               -> TableWidget
tableWidgetMove dir sz tbw =
    let pos = tbw_pos tbw
        opts = tbw_options tbw
        nrows = length (tbw_rows tbw)
        in case pos of
             Nothing -> tbw
             Just p -> case findNextActiveCell opts nrows p dir of
                         Nothing -> tbw
                         newP@(Just (y, _)) ->
                             tableWidgetMakeVisible (tbw {tbw_pos=newP}) sz y

tableWidgetMakeVisible :: TableWidget
                                                      -> (Int, Int)
                                                      -> Int
                                                      -> TableWidget
tableWidgetMakeVisible tbw sz@(_,_) y =
    let info = tableWidgetDisplayInfo sz tbw
        firstVis = tbwdisp_firstVis info
        lastVis = tbwdisp_lastVis info
        in if y < firstVis
              then tableWidgetMakeVisible (tableWidgetScrollUp sz tbw) sz y
              else if y > lastVis
                      then tableWidgetMakeVisible
                               (tableWidgetScrollDown sz tbw) sz y
                      else tbw

findFirstActiveCell :: [Row] -> TableWidgetOptions -> Maybe Pos
findFirstActiveCell rows opts =
    let nrows = length rows
        firstActiveCells = map (\y -> findNextActiveCell opts nrows
                                                         (y, -1) DirRight)
                               [0..nrows-1]
        in case catMaybes firstActiveCells of
             [] -> Nothing
             (x:_) -> Just x

findNextActiveCell :: TableWidgetOptions -> Int  -> Pos -> Direction
                   -> Maybe Pos
findNextActiveCell opts nrows (y,x) dir =
--    trace ("findNextActiveCell (opts=" ++ show opts ++ ", nrows=" ++ show nrows
--           ++ ", pos=" ++ show pos ++ ", dir=" ++ show dir) $
    let rows = [0..(nrows - 1)]
        cols = sort (tbwopt_activeCols opts)
        horiz f = case f cols x rows y of
                    Nothing -> Nothing
                    Just z -> Just (y, z)
        vert f = case f rows y cols x of
                   Nothing -> Nothing
                   Just z -> Just (y, z)
        res = case dir of
                DirLeft-> horiz goLeft
                DirRight -> horiz goRight
                DirUp -> vert goLeft
                DirDown -> vert goRight
        in --trace ("result of findNextActiveCell: " ++ show res)
           res
    where goLeft _ _ rows a | not (a `elem` rows) = Nothing
          goLeft cols b _ _ =
              case reverse (takeWhile (<b) cols) of
                [] -> Nothing
                (c:_) -> Just c
          goRight _ _ rows a | not (a `elem` rows) = Nothing
          goRight cols a _ _ =
              case dropWhile (a>=) cols of
                [] -> Nothing
                (b:_) -> Just b

tableWidgetDeleteRow :: Int -> TableWidget -> TableWidget
tableWidgetDeleteRow n tbw =
    let rows = tbw_rows tbw
        rows' = deleteAt n rows
        pos' =
           case tbw_pos tbw of
             Nothing -> Nothing
             Just (row,col) ->
                 let row' = min row (length rows' - 1)
                     in if row' >= 0 then Just (row', col)
                        else Nothing
        in tbw { tbw_rows = rows', tbw_pos = pos' }

--
-- BorderWidget
--


--
-- Selection Widget
--

--
-- Utility functions
--

-- | Join a list by some delimiter
joinLists :: [[a]] -> [a] -> [a]
joinLists l s = if (null l) then [] else foldr1 (\x -> \y -> x ++ s ++ y) l

-- | Split a list by some delimiter
splitList :: Eq a => [a] -> [a] -> [[a]]
splitList d l =
    unfoldr (\x -> if (null x)
                      then Nothing
                      else Just $ nextToken d [] (snd $ splitAt (length d) x))
            (d++l)
  where nextToken _ r [] = (r, [])
        nextToken e r m@(h:t) | (e `isPrefixOf` m) = (r, m)
                              | otherwise = nextToken e (r++[h]) t

listReplace :: [a] -> a -> Int -> [a]
listReplace l a i =
    case splitAt i l of
      (_, []) -> error ("listReplace: index to large. index="++show i++
                        ", length="++show (length l))
      ([], _) | i < 0 -> error ("listReplace: negative index. index="++
                                show i)
      (xs,(_:ys)) -> xs ++ (a:ys)

--alignRows :: [[String]] -> Char -> String -> [String]
alignRows :: [[[a]]] -> a -> [a] -> [[a]]
alignRows rows fill delim =
    let widths = foldr maxWidths (repeat 0) rows
        in map (alignRow widths) rows
    where
    maxWidths ::  [[a]] -> [Int] -> [Int]
    maxWidths row acc = map (uncurry max) (zip acc (map length row))
    alignRow widths row = concatMap (uncurry alignCell) (zip widths row)
    alignCell width cell =
        let diff = width - length cell
            in cell ++ (take diff $ repeat fill) ++ delim


align :: HAlignment -> Int -> a -> [a] -> [a]
align a w f l =
    let space = w - length l
        in case a of
             AlignLeft -> l ++ (fill space)
             AlignRight -> (fill space) ++ l
             AlignCenter ->
                 let left = space `div` 2
                     right = left + (space `mod` 2)
                     in fill left ++ l ++ fill right
    where fill n = take n (repeat f)

deleteAt :: Int -> [a] -> [a]
deleteAt n l = if n >= 0 && n < length l
                  then let (a,b) = splitAt n l in a ++ (tail b)
                  else error ("deleteAt: illegal index: " ++ show n)
