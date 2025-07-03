{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- Copyright (c) 2002-2004 John Meacham (john at repetae dot net)
-- Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- | Binding to the [wn]curses library. From the ncurses man page:
--
-- >      The curses library routines give the user a terminal-inde-
-- >      pendent method of updating character screens with  reason-
-- >      able  optimization.
--
-- Sections of the quoted documentation are from the OpenBSD man pages, which
-- are distributed under a BSD license.
--
-- A useful reference is:
--        /Writing Programs with NCURSES/, by Eric S. Raymond and Zeyd
--        M. Ben-Halim, <http://dickey.his.com/ncurses/>
--
-- N.B attrs don't work with Irix curses.h. This should be fixed.
module UI.HSCurses.Curses (
    -- Basic Functions
    stdScr,
    initScr,
    initCurses,
    resetParams,
    endWin,
    scrSize,
    newTerm,
    delScreen,

    -- Windows, screens and Pads
    Screen,
    Window,
    Border (..),
    touchWin,
    newPad,
    pRefresh,
    delWin,
    newWin,
    wRefresh,
    wnoutRefresh,
    wBorder,
    defaultBorder,

    -- Refresh routines
    refresh,
    update,
    resizeTerminal,
    timeout,
    noqiflush,

    -- Navigation
    move,
    getYX,

    -- Input
    getCh,
    getch,
    decodeKey,
    ungetCh,
    keyResizeCode,

    -- * Input Options
    cBreak, -- :: Bool -> IO ()
    raw, -- :: Bool -> IO ()
    echo, -- :: Bool -> IO ()
    intrFlush, -- :: Bool -> IO ()
    keypad, -- :: Window -> Bool -> IO ()
    noDelay, -- :: Window -> Bool -> IO ()

    -- * Output
    wAddStr, -- :: Window -> String -> IO ()
    addLn, -- :: IO ()
    mvWAddStr,
    mvAddCh, -- :: Int -> Int -> ChType -> IO ()
    wMove,
    bkgrndSet, -- :: Attr -> Pair -> IO ()
    wbkgrndSet, -- :: Window -> Attr -> Pair -> IO ()
    erase, -- :: IO ()
    wclear, -- :: Window -> IO ()
    werase,
    clrToEol, -- :: IO ()
    wClrToEol,
    beep,
    waddch,
    winsch,
    waddchnstr, -- :: Window -> CString -> CInt -> IO CInt

    -- * Output Options
    clearOk,
    leaveOk,
    nl, -- :: Bool -> IO ()

    -- * Cursor Routines
    CursorVisibility (..),
    cursSet,

    -- * Color Support
    hasColors, -- :: IO Bool
    startColor, -- :: IO ()
    useDefaultColors, -- :: IO ()
    Pair (..), -- newtype Pair = Pair Int deriving (Eq, Ord, Ix)
    colorPairs, -- :: IO Int
    Color (..), -- newtype Color = Color Int deriving (Eq, Ord, Ix)
    colors, -- :: IO Int
    color, -- :: String -> Maybe Color
    --    black, red, green, yellow, blue, magenta, cyan, white, -- :: Color
    initPair, -- :: Pair -> Color -> Color -> IO ()
    pairContent, -- :: Pair -> IO (Color, Color)
    canChangeColor, -- :: IO Bool
    initColor, -- :: Color -> (Int, Int, Int) -> IO ()
    colorContent, -- :: Color -> IO (Int, Int, Int)
    defaultBackground,
    defaultForeground,

    -- * Attributes
    attrPlus,
    Attr,
    attr0, -- :: Attr
    isAltCharset,
    isBlink,
    isBold,
    isDim,
    isHorizontal,
    isInvis,
    isLeft,
    isLow,
    isProtect,
    isReverse,
    isRight,
    isStandout,
    isTop,
    isUnderline,
    isVertical,
    -- :: Attr -> Bool

    setAltCharset,
    setBlink,
    setBold,
    setDim,
    setHorizontal,
    setInvis,
    setLeft,
    setLow,
    setProtect,
    setReverse,
    setRight,
    setStandout,
    setTop,
    setUnderline,
    setVertical,
    -- :: Attr -> Bool -> Attr

    attrSet, -- :: Attr -> Pair -> IO ()
    attrOn,
    attrOff,
    standout,
    standend,
    attrDim,
    attrBold,
    attrDimOn,
    attrDimOff,
    attrBoldOn,
    attrBoldOff,
    wAttrOn,
    wAttrOff,
    wAttrSet,
    wAttrGet,

    -- * Mouse Routines
    getMouse,
    withMouseEventMask,
    withAllMouseEvents,
    ButtonEvent (..),
    MouseEvent (..),

    -- * Keys
    Key (..),
    cERR,
    cKEY_UP,
    cKEY_DOWN,
    cKEY_LEFT,
    cKEY_RIGHT,
    cTRUE,
    --    cACS_BLOCK,

    -- * Lines
    vline,
    ulCorner,
    llCorner,
    urCorner,
    lrCorner,
    rTee,
    lTee,
    bTee,
    tTee,
    hLine,
    vLine,
    plus,
    s1,
    s9,
    diamond,
    ckBoard,
    degree,
    plMinus,
    bullet,
    lArrow,
    rArrow,
    dArrow,
    uArrow,
    board,
    lantern,
    block,
    s3,
    s7,
    lEqual,
    gEqual,
    pi,
    nEqual,
    sterling,

    -- * Signals
    cursesSigWinch,

    -- * Misc
    cursesTest,
    throwIfErr,
    throwIfErr_,
    errI, -- :: IO CInt -> IO ()
    flushinp,
    recognize,
    ChType,
    NBool,
) where

#include <HSCurses.h>

#if HAVE_SIGNAL_H
#include <signal.h>
#endif

import UI.HSCurses.Logging

#if !(defined(HAVE_WCHAR_H) && (defined(HAVE_LIBNCURSESW) || defined(HAVE_LIBPDCURSESW)))
-- FIXME: this is needed for CI builds on macOS; we can probably replace the
-- whole CWString module with just the "new" function in Foreign.C.String
import UI.HSCurses.CWString (withLCStringLen)
#endif

import Data.Char
import Data.Ix (Ix)
import Prelude hiding (pi)

-- foldl' was put into Prelude in base 4.20
#if !MIN_VERSION_base(4,20,0)
import Data.List (foldl')
#endif

import System.IO.Unsafe (unsafePerformIO)

import Control.Concurrent
import Control.Monad (liftM, void, when)
import Control.Monad.Trans

#if !MIN_VERSION_base(4,7,0)
import Foreign hiding (unsafePerformIO, void)
#else
import Foreign hiding (void)
#endif

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

import GHC.IO.FD (FD (..))

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

#if __GLASGOW_HASKELL__ < 603
import Data.Bits
#endif


-- | 'initCurses' does all initialization necessary for a Curses application.
initCurses :: IO ()
initCurses = do
    initScr
    b <- hasColors
    when b $ startColor >> useDefaultColors

resetParams :: IO ()
resetParams = do
    raw True -- raw mode please
    echo False
    nl False
    intrFlush True
    leaveOk False
    keypad stdScr True
#ifdef NCURSES_EXT_FUNCS
    defineKey (# const KEY_UP) "\x1b[1;2A"
    defineKey (# const KEY_DOWN) "\x1b[1;2B"
    defineKey (# const KEY_SLEFT) "\x1b[1;2D"
    defineKey (# const KEY_SRIGHT) "\x1b[1;2C"
    defineKey (# const KEY_B2) "\x1b[E" -- xterm seems to emit B2, not BEG
    defineKey (# const KEY_END) "\x1b[F"
    defineKey (# const KEY_END) "\x1b[4~"
    defineKey (# const KEY_HOME) "\x1b[H"
    defineKey (# const KEY_HOME) "\x1b[1~"
#endif

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

throwIfErr :: (Eq a, Show a, Num a) => String -> IO a -> IO a
throwIfErr s = throwIf (== (# const ERR)) (\a -> "Curses[" ++ show a ++ "]:" ++ s)

throwIfErr_ :: (Eq a, Show a, Num a) => String -> IO a -> IO ()
throwIfErr_ name act = void $ throwIfErr name act

errI :: IO CInt -> IO ()
errI f = do
    r <- f
    if r == cERR
        then do
            _ <- endwin
            error "curses returned an error"
        else return ()

type WindowTag = ()
type Window = Ptr WindowTag
type ChType = #type chtype
type NBool = #type bool


-- | The standard screen
stdScr :: Window
stdScr = unsafePerformIO (peek stdscr)
{-# NOINLINE stdScr #-}

foreign import ccall "static HSCurses.h &stdscr"
    stdscr :: Ptr Window

-- | 'initScr' is normally the first curses routine to call when initializing a
-- program. curs_initscr(3):
--
-- > To initialize the routines, the routine initscr or newterm must be called
-- > before any of the other routines that deal with windows and screens are
-- > used.
-- >
-- > The initscr code determines the terminal type and initial- izes all curses
-- > data structures. initscr also causes the first call to refresh to clear the
-- > screen. If errors occur, initscr writes an appropriate error message to
-- > standard error and exits; otherwise, a pointer is returned to stdscr.
initScr :: IO Window
initScr = throwIfNull "initscr" initscr

foreign import ccall unsafe "HSCurses.h initscr" initscr :: IO Window

-- This seems like the easiest way to get a FILE (see: man FILE)
-- from an FD
type FILE = Ptr ()

foreign import ccall unsafe "fdopen"
    fdopen :: CInt -> CString -> IO FILE

fdOpen :: FD -> String -> IO FILE
fdOpen fd mode =
    withCString mode $ \mode' -> do
        fdopen (fdFD fd) mode'

type Screen = Ptr ()

-- | A program that outputs to more than one terminal should use the 'newTerm'
-- routine for each terminal instead of 'initScr'. A program that needs to
-- inspect capabilities, so it can continue to run in a line-oriented mode if
-- the terminal cannot support a screen-oriented program, would also use
-- 'newTerm'. curs_initscr(3X):
--
-- >    The routine newterm should be called once for each terminal. It returns
-- >    a variable of type SCREEN * which should be saved as a reference to that
-- >    terminal. newterm's arguments are
-- >
-- >    - the type of the terminal to be used in place of $TERM,
-- >
-- >    - an output stream connected to the terminal, and
-- >
-- >    - an input stream connected to the terminal
-- >
-- >    If the type parameter is NULL, $TERM will be used.
newTerm :: String -> FD -> FD -> IO Screen
newTerm typ out in' =
    withCString typ $ \typ' -> do
        fout <- fdOpen out "rw"
        fin <- fdOpen in' "r"
        throwIfNull "newterm" $ newterm typ' fout fin

foreign import ccall unsafe "HSCurses.h newterm"
    newterm :: CString -> FILE -> FILE -> IO Screen

foreign import ccall unsafe "HSCurses.h delscreen"
    delScreen :: Screen -> IO ()

-- | > The cbreak routine disables line buffering and erase/kill
--   > character-processing (interrupt and flow control characters are
--   > unaffected), making characters typed by the user immediately available to
--   > the program. The nocbreak routine returns the terminal to normal (cooked)
--   > mode.
cBreak :: Bool -> IO ()
cBreak True = throwIfErr_ "cbreak" cbreak
cBreak False = throwIfErr_ "nocbreak" nocbreak

foreign import ccall unsafe "HSCurses.h cbreak"     cbreak :: IO CInt
foreign import ccall unsafe "HSCurses.h nocbreak" nocbreak :: IO CInt

-- | > The raw and noraw routines place the terminal into or out of raw mode.
--   > Raw mode is similar to cbreak mode, in that characters typed are
--   > immediately passed through to the user program. The differences are that
--   > in raw mode, the interrupt, quit, suspend, and flow control characters
--   > are all passed through uninterpreted, instead of generating a signal. The
--   > behavior of the BREAK key depends on other bits in the tty driver that
--   > are not set by curses.
raw :: Bool -> IO ()
raw False = throwIfErr_ "noraw" noraw
raw True = throwIfErr_ "raw" raw_c

foreign import ccall unsafe "hscurses.h noraw" noraw :: IO CInt
foreign import ccall unsafe "HSCurses.h raw" raw_c :: IO CInt

-- | > The echo and noecho routines control whether characters typed by the user
--   > are echoed by getch as they are typed. Echoing by the tty driver is
--   > always disabled, but ini- tially getch is in echo mode, so characters
--   > typed are echoed. Authors of most interactive programs prefer to do their
--   > own echoing in a controlled area of the screen, or not to echo at all, so
--   > they disable echoing by calling noecho. [See curs_getch(3) for a
--   > discussion of how these routines interact with cbreak and nocbreak.]
echo :: Bool -> IO ()
echo False = throwIfErr_ "noecho" noecho
echo True = throwIfErr_ "echo" echo_c

foreign import ccall unsafe "HSCurses.h noecho" noecho :: IO CInt
foreign import ccall unsafe "HSCurses.h echo" echo_c :: IO CInt

-- | > The nl and nonl routines control whether the underlying display device
--   > translates the return key into newline on input, and whether it
--   > translates newline into return and line-feed on output (in either case,
--   > the call addch('\n') does the equivalent of return and line feed on the
--   > virtual screen). Initially, these translations do occur. If you disable
--   > them using nonl, curses will be able to make bet- ter use of the
--   > line-feed capability, resulting in faster cursor motion. Also, curses
--   > will then be able to detect the return key.
nl :: Bool -> IO ()
nl True = throwIfErr_ "nl" nl_c
nl False = throwIfErr_ "nonl" nonl

foreign import ccall unsafe "HSCurses.h nl" nl_c :: IO CInt
foreign import ccall unsafe "HSCurses.h nonl" nonl :: IO CInt

-- | > If the intrflush option is enabled, (bf is TRUE), when an interrupt key
--   > is pressed on the keyboard (interrupt, break, quit) all output in the tty
--   > driver queue will be flushed, giving the effect of faster response to the
--   > interrupt, but causing curses to have the wrong idea of what is on the
--   > screen. Disabling (bf is FALSE), the option prevents the flush.
intrFlush :: Bool -> IO ()
intrFlush bf =
    throwIfErr_ "intrflush" $ intrflush stdScr (if bf then 1 else 0)

foreign import ccall unsafe "HSCurses.h intrflush"
    intrflush :: Window -> (#type bool) -> IO CInt

-- | Enable the keypad of the user's terminal.
keypad :: Window -> Bool -> IO ()
keypad win bf = throwIfErr_ "keypad" $ keypad_c win (if bf then 1 else 0)

foreign import ccall unsafe "HSCurses.h keypad"
    keypad_c :: Window -> (#type bool) -> IO CInt

noDelay :: Window -> Bool -> IO ()
noDelay win bf =
    throwIfErr_ "nodelay" $ nodelay win (if bf then 1 else 0)

foreign import ccall unsafe "HSCurses.h nodelay"
    nodelay :: Window -> (#type bool) -> IO CInt

-- | Normally, the hardware cursor is left at the location of the window cursor
-- being refreshed. The leaveok option allows the cursor to be left wherever the
-- update happens to leave it. It is useful for applications where the cursor is
-- not used, since it reduces the need for cursor motions. If possible, the
-- cursor is made invisible when this option is enabled.
leaveOk :: Bool -> IO CInt
leaveOk True = leaveok_c stdScr 1
leaveOk False = leaveok_c stdScr 0

foreign import ccall unsafe "HSCurses.h leaveok"
    leaveok_c :: Window -> (#type bool) -> IO CInt

clearOk :: Bool -> IO CInt
clearOk True = clearok_c stdScr 1
clearOk False = clearok_c stdScr 0

foreign import ccall unsafe "HSCurses.h clearok"
    clearok_c :: Window -> (#type bool) -> IO CInt

foreign import ccall unsafe "HSCurses.h use_default_colors"
    useDefaultColors :: IO ()

defaultBackground, defaultForeground :: Color
defaultBackground = Color (-1)
defaultForeground = Color (-1)

#ifdef NCURSES_EXT_FUNCS
defineKey :: CInt -> String -> IO ()
defineKey k s = withCString s (\s' -> define_key s' k) >> return ()

foreign import ccall unsafe "HSCurses.h define_key"
    define_key :: Ptr CChar -> CInt -> IO ()
#endif

-- | >  The program must call endwin for each terminal being used before
--   >  exiting from curses.
endWin :: IO ()
endWin = throwIfErr_ "endwin" endwin

foreign import ccall unsafe "HSCurses.h endwin" endwin :: IO CInt

-- | Get the dimensions of the screen (lines, cols).
scrSize :: IO (Int, Int)
-- Note, per the documentation:
--    http://invisible-island.net/ncurses/ncurses-intro.html#caution
-- It is not recommended to peek at the LINES and COLS global variables.  This code
-- was previously doing exactly that, but now it is fixed to use getmaxyx.
--   -Ryan Newton [2013.03.31]
scrSize = do
    yfp <- mallocForeignPtr
    xfp <- mallocForeignPtr
    withForeignPtr yfp $ \yp ->
        withForeignPtr xfp $ \xp -> do
            getMaxYX stdScr yp xp
            y <- peek yp
            x <- peek xp
            return (fromIntegral y, fromIntegral x)
    
foreign import ccall "HSCurses.h getmaxyx_fun"
    getMaxYX ::
        Window -> Ptr CInt -> Ptr CInt -> IO ()

-- | Refresh curses windows and lines. curs_refresh(3)
refresh :: IO ()
refresh = throwIfErr_ "refresh" refresh_c

foreign import ccall unsafe "HSCurses.h refresh"
    refresh_c :: IO CInt

-- | Refresh the specified window, copying the data from the virtual screen to
-- the physical screen.
wRefresh :: Window -> IO ()
wRefresh w = throwIfErr_ "wrefresh" $ wrefresh_c w

foreign import ccall unsafe "HSCurses.h wrefresh"
    wrefresh_c :: Window -> IO CInt

-- | Stage an update to a window, but don't actually do the refresh until update
-- is called. This allows multiple windows to be updated together more smoothly.
wnoutRefresh :: Window -> IO ()
wnoutRefresh w = throwIfErr_ "wnoutrefresh" $ wnoutrefresh_c w

foreign import ccall safe "HSCurses.h wnoutrefresh"
    wnoutrefresh_c :: Window -> IO CInt

-- | Do an actual update. Used after endWin on linux to restore the terminal.
update :: IO ()
update = throwIfErr_ "update" update_c

foreign import ccall unsafe "HSCurses.h doupdate" update_c :: IO CInt

foreign import ccall unsafe "static curses.h timeout" timeout_c :: CInt -> IO ()

-- | Set a delay in milliseconds.
timeout :: Int -> IO ()
timeout = timeout_c . fromIntegral

hasColors :: IO Bool
hasColors = liftM (/= 0) has_colors

foreign import ccall unsafe "HSCurses.h has_colors" has_colors :: IO (#type bool)

-- | Initialise the color settings. Also sets the screen to the default colors
-- (white on black).
startColor :: IO ()
startColor = throwIfErr_ "start_color" start_color

foreign import ccall unsafe start_color :: IO CInt

newtype Pair = Pair Int deriving (Eq, Ord, Ix, Show)

-- | Defines the maximum number of color-pairs the terminal can support).
colorPairs :: IO Int
colorPairs = fmap fromIntegral $ peek colorPairsPtr

foreign import ccall "HSCurses.h &COLOR_PAIRS"
    colorPairsPtr :: Ptr CInt

newtype Color = Color Int deriving (Eq, Ord, Ix)

colors :: IO Int
colors = liftM fromIntegral $ peek colorsPtr

foreign import ccall "HSCurses.h &COLORS" colorsPtr :: Ptr CInt

color :: String -> Maybe Color
color "default" = Just $ Color (-1)
color "black" = Just $ Color (# const COLOR_BLACK)
color "red" = Just $ Color (# const COLOR_RED)
color "green" = Just $ Color (# const COLOR_GREEN)
color "yellow" = Just $ Color (# const COLOR_YELLOW)
color "blue" = Just $ Color (# const COLOR_BLUE)
color "magenta" = Just $ Color (# const COLOR_MAGENTA)
color "cyan" = Just $ Color (# const COLOR_CYAN)
color "white" = Just $ Color (# const COLOR_WHITE)
color _ = Nothing

-- | curses support color attributes on terminals with that capability. To use
-- these routines start_color must be called, usually right after initscr.
-- Colors are always used in pairs (referred to as color-pairs). A color-pair
-- consists of a foreground color (for characters) and a background color (for
-- the blank field on which the charac- ters are displayed). A programmer
-- initializes a color- pair with the routine init_pair. After it has been ini-
-- tialized, COLOR_PAIR(n), a macro defined in <curses.h>, can be used as a new
-- video attribute.
--
-- If a terminal is capable of redefining colors, the pro- grammer can use the
-- routine init_color to change the defi- nition of a color.
--
-- The init_pair routine changes the definition of a color- pair. It takes three
-- arguments: the number of the color- pair to be changed, the foreground color
-- number, and the background color number. For portable applications:
--
-- - The value of the first argument must be between 1 and COLOR_PAIRS-1.
--
-- - The value of the second and third arguments must be between 0 and COLORS
--   (the 0 color pair is wired to white on black and cannot be changed).
initPair :: Pair -> Color -> Color -> IO ()
initPair (Pair p) (Color f) (Color b) =
    throwIfErr_ "init_pair" $
        init_pair (fromIntegral p) (fromIntegral f) (fromIntegral b)

foreign import ccall unsafe
    init_pair :: CShort -> CShort -> CShort -> IO CInt

pairContent :: Pair -> IO (Color, Color)
pairContent (Pair p) =
    alloca $ \fPtr ->
        alloca $ \bPtr -> do
            throwIfErr "pair_content" $ pair_content (fromIntegral p) fPtr bPtr
            f <- peek fPtr
            b <- peek bPtr
            return (Color (fromIntegral f), Color (fromIntegral b))

foreign import ccall unsafe
    pair_content :: CShort -> Ptr CShort -> Ptr CShort -> IO CInt

canChangeColor :: IO Bool
canChangeColor = liftM (/= 0) can_change_color

foreign import ccall unsafe can_change_color :: IO (#type bool)

initColor :: Color -> (Int, Int, Int) -> IO ()
initColor (Color c) (r, g, b) =
    throwIfErr_ "init_color" $
        init_color (fromIntegral c) (fromIntegral r) (fromIntegral g) (fromIntegral b)

foreign import ccall unsafe
    init_color :: CShort -> CShort -> CShort -> CShort -> IO CInt

colorContent :: Color -> IO (Int, Int, Int)
colorContent (Color c) =
    alloca $ \rPtr ->
        alloca $ \gPtr ->
            alloca $ \bPtr -> do
                throwIfErr "color_content" $ color_content (fromIntegral c) rPtr gPtr bPtr
                r <- peek rPtr
                g <- peek gPtr
                b <- peek bPtr
                return (fromIntegral r, fromIntegral g, fromIntegral b)

foreign import ccall unsafe
    color_content :: CShort -> Ptr CShort -> Ptr CShort -> Ptr CShort -> IO CInt

foreign import ccall unsafe "HSCurses.h hs_curses_color_pair" colorPair :: Pair -> ChType

-----------------------------------------------------------------------------
-- * Attributes
-----------------------------------------------------------------------------

foreign import ccall unsafe "HSCurses.h attr_set"
    attr_set :: Attr -> CShort -> Ptr a -> IO Int

foreign import ccall unsafe "HSCurses.h wattr_set"
    wattr_set :: Window -> Attr -> CInt -> Ptr a -> IO CInt

foreign import ccall unsafe "HSCurses.h wattr_get"
    wattr_get :: Window -> Ptr Attr -> Ptr CShort -> Ptr a -> IO CInt

foreign import ccall "HSCurses.h attr_on" attr_on :: (#type attr_t) -> Ptr a -> IO Int
foreign import ccall "HSCurses.h attr_off" attr_off :: (#type attr_t) -> Ptr a -> IO Int
foreign import ccall "HSCurses.h attron" attron :: Int -> IO Int
foreign import ccall "HSCurses.h attroff" attroff :: Int -> IO Int
foreign import ccall unsafe "HSCurses.h wattron" wattron :: Window -> CInt -> IO CInt
foreign import ccall unsafe "HSCurses.h wattroff" wattroff :: Window -> CInt -> IO CInt
foreign import ccall standout :: IO Int
foreign import ccall standend :: IO Int

wAttrSet :: Window -> (Attr, Pair) -> IO ()
wAttrSet w (a, (Pair p)) =
    throwIfErr_ "wattr_set" $
        wattr_set w a (fromIntegral p) nullPtr

-- | Manipulate the current attributes of the named window. see curs_attr(3)
wAttrGet :: Window -> IO (Attr, Pair)
wAttrGet w =
    alloca $ \pa ->
        alloca $ \pp -> do
            throwIfErr_ "wattr_get" $ wattr_get w pa pp nullPtr
            a <- peek pa
            p <- peek pp
            return (a, Pair $ fromIntegral p)

newtype Attr = Attr (#type attr_t)
    deriving (Eq, Storable, Bits, Num, Show)

-- | Normal display (no highlight)
attr0 :: Attr
#ifdef WA_NORMAL
attr0 = Attr (#const WA_NORMAL)
#else
attr0 = Attr (#const A_NORMAL)
#endif

isAltCharset, isBlink, isBold, isDim, isHorizontal, isInvis, isLeft,
    isLow, isProtect, isReverse, isRight, isStandout, isTop,
    isUnderline, isVertical :: Attr -> Bool

isAltCharset = isAttr (#const WA_ALTCHARSET)
isBlink      = isAttr (#const WA_BLINK)
isBold       = isAttr (#const WA_BOLD)
isDim        = isAttr (#const WA_DIM)
isHorizontal = isAttr (#const WA_HORIZONTAL)
isInvis      = isAttr (#const WA_INVIS)
isLeft       = isAttr (#const WA_LEFT)
isLow        = isAttr (#const WA_LOW)
isProtect    = isAttr (#const WA_PROTECT)
isReverse    = isAttr (#const WA_REVERSE)
isRight      = isAttr (#const WA_RIGHT)
isStandout   = isAttr (#const WA_STANDOUT)
isTop        = isAttr (#const WA_TOP)
isUnderline  = isAttr (#const WA_UNDERLINE)
isVertical   = isAttr (#const WA_VERTICAL)

isAttr :: (#type attr_t) -> Attr -> Bool
isAttr b (Attr a) = a .&. b /= 0

setAltCharset, setBlink, setBold, setDim, setHorizontal, setInvis,
    setLeft, setLow, setProtect, setReverse, setRight, setStandout,
    setTop, setUnderline, setVertical :: Attr -> Bool -> Attr

setAltCharset = setAttr (#const WA_ALTCHARSET)
setBlink      = setAttr (#const WA_BLINK)
setBold       = setAttr (#const WA_BOLD)
setDim        = setAttr (#const WA_DIM)
setHorizontal = setAttr (#const WA_HORIZONTAL)
setInvis      = setAttr (#const WA_INVIS)
setLeft       = setAttr (#const WA_LEFT)
setLow        = setAttr (#const WA_LOW)
setProtect    = setAttr (#const WA_PROTECT)
setReverse    = setAttr (#const WA_REVERSE)
setRight      = setAttr (#const WA_RIGHT)
setStandout   = setAttr (#const WA_STANDOUT)
setTop        = setAttr (#const WA_TOP)
setUnderline  = setAttr (#const WA_UNDERLINE)
setVertical   = setAttr (#const WA_VERTICAL)

setAttr :: (#type attr_t) -> Attr -> Bool -> Attr
setAttr b (Attr a) False = Attr (a .&. complement b)
setAttr b (Attr a) True  = Attr (a .|. b)

attrPlus :: Attr -> Attr -> Attr
attrPlus (Attr a) (Attr b) = Attr (a .|. b)

attrSet :: Attr -> Pair -> IO ()
attrSet attr (Pair p) = throwIfErr_ "attrset" $
    attr_set attr (fromIntegral p) nullPtr

attrOn :: Attr -> IO ()
attrOn (Attr attr) = throwIfErr_ "attr_on" $
    attr_on attr nullPtr

attrOff :: Attr -> IO ()
attrOff (Attr attr) = throwIfErr_ "attr_off" $
    attr_off attr nullPtr

wAttrOn :: Window -> Int -> IO ()
wAttrOn w x = throwIfErr_ "wattron" $ wattron w (fi x)

wAttrOff :: Window -> Int -> IO ()
wAttrOff w x = throwIfErr_ "wattroff" $ wattroff w (fi x)

attrDimOn :: IO ()
attrDimOn  = throwIfErr_ "attron A_DIM" $
    attron (#const A_DIM)

attrDimOff :: IO ()
attrDimOff = throwIfErr_ "attroff A_DIM" $
    attroff (#const A_DIM)

attrBoldOn :: IO ()
attrBoldOn  = throwIfErr_ "attron A_BOLD" $
    attron (#const A_BOLD)

attrBoldOff :: IO ()
attrBoldOff = throwIfErr_ "attroff A_BOLD" $
    attroff (#const A_BOLD)

attrDim :: Int
attrDim = (#const A_DIM)
attrBold :: Int
attrBold = (#const A_BOLD)

-- | Raw NCurses routine.
foreign import ccall safe
    waddch :: Window -> ChType -> IO CInt

-- | Raw NCurses routine.
foreign import ccall safe
    winsch :: Window -> ChType -> IO CInt

-- | Raw NCurses routine.
foreign import ccall safe
    waddchnstr :: Window -> CString -> CInt -> IO CInt

foreign import ccall safe "static curses.h mvaddch"
    mvaddch_c :: CInt -> CInt -> ChType -> IO ()

mvWAddStr :: Window -> Int -> Int -> String -> IO ()
mvWAddStr w y x str = wMove w y x >> wAddStr w str

mvAddCh :: Int -> Int -> ChType -> IO ()
mvAddCh l m n = mvaddch_c (fromIntegral l) (fromIntegral m) (fromIntegral n)

addLn :: IO ()
addLn = wAddStr stdScr "\n"

#if defined(HAVE_WCHAR_H) && (defined(HAVE_LIBNCURSESW) || defined(HAVE_LIBPDCURSESW))

foreign import ccall unsafe
    waddnwstr :: Window -> CWString -> CInt -> IO CInt

wAddStr :: Window -> String -> IO ()
wAddStr win str = do
    let convStr f = case f [] of
            [] -> return ()
            s ->
                throwIfErr_ "waddnstr" $
                    withCWStringLen (s) (\(ws, len) -> (waddnwstr win ws (fi len)))
        loop [] acc = convStr acc
        loop (ch : str') acc =
            recognize
                ch
                (loop str' (acc . (ch :)))
                ( \ch' -> do
                    convStr acc
                    throwIfErr "waddch" $ waddch win ch'
                    loop str' id
                )
    loop str id

#else

-- This is heavily called, and does a lot of allocs.  We walk over all
-- the string accumulating a list of characters to be drawn.
--
-- Got it down to:
--
--      wAddStr Yi.Curses 20.0   38.1
--      wAddStr Yi.Curses 10.0   32.5
--
-- TODO make this way less expensive. That accum sucks.
-- use difference lists for O(1) append
wAddStr :: Window -> [Char] -> IO ()
wAddStr _ [] = return ()
wAddStr win s =
    throwIfErr_ ("waddnstr: <" ++ s ++ ">") $
        withLCStringLen (s) (\(ws, len) -> waddnstr win ws (fi len))

foreign import ccall safe
    waddnstr :: Window -> CString -> CInt -> IO CInt

#endif

foreign import ccall safe
    vline :: Char -> Int -> IO ()

--
-- what ?
--

#let translate_attr attr =                              \
    "(if a .&. %lu /= 0 then %lu else 0) .|.",          \
    (unsigned long) WA_##attr, (unsigned long) A_##attr

bkgrndSet :: Attr -> Pair -> IO ()
bkgrndSet (Attr a) p = bkgdset $
    fromIntegral (ord ' ') .|.
    #translate_attr ALTCHARSET
    #translate_attr BLINK
    #translate_attr BOLD
    #translate_attr DIM
    #translate_attr INVIS
    #translate_attr PROTECT
    #translate_attr REVERSE
    #translate_attr STANDOUT
    #translate_attr UNDERLINE
    colorPair p

foreign import ccall unsafe bkgdset :: ChType -> IO ()

wbkgrndSet :: Window -> Attr -> Pair -> IO ()
wbkgrndSet win (Attr a) p = wbkgdset win $
    fromIntegral (ord ' ') .|.
    #translate_attr ALTCHARSET
    #translate_attr BLINK
    #translate_attr BOLD
    #translate_attr DIM
    #translate_attr INVIS
    #translate_attr PROTECT
    #translate_attr REVERSE
    #translate_attr STANDOUT
    #translate_attr UNDERLINE
    colorPair p

foreign import ccall unsafe wbkgdset :: Window -> ChType -> IO ()

-- | Copy blanks to every position in the screen.
erase :: IO ()
erase = throwIfErr_ "erase" $ werase_c stdScr

foreign import ccall unsafe "werase" werase_c :: Window -> IO CInt

-- | Copy blanks to every position in a window.
werase :: Window -> IO ()
werase w = throwIfErr_ "werase" $ werase_c w

-- | Copy blanks to a window and set clearOk for that window.
wclear :: Window -> IO ()
wclear w = throwIfErr_ "wclear" $ wclear_c w

foreign import ccall unsafe "wclear" wclear_c :: Window -> IO CInt

clrToEol :: IO ()
clrToEol = throwIfErr_ "clrtoeol" clrtoeol

foreign import ccall unsafe clrtoeol :: IO CInt

-- | > move the cursor associated with the window to line y and column x. This
--   > routine does not move the physical cursor of the terminal until refresh
--   > is called. The position specified is relative to the upper left-hand
--   > corner of the window, which is (0,0).
--
-- Note that 'move_c' may be a macro.
move :: Int -> Int -> IO ()
move y x = throwIfErr_ "move" $ move_c (fromIntegral y) (fromIntegral x)

foreign import ccall unsafe "move"
    move_c :: CInt -> CInt -> IO CInt

-- | >    move the cursor associated with the window
--   >    to line y and column x.  This routine does  not  move  the
--   >    physical  cursor  of the terminal until refresh is called.
--   >    The position specified is relative to the upper  left-hand
--   >    corner of the window, which is (0,0).
wMove :: Window -> Int -> Int -> IO ()
wMove w y x = throwIfErr_ "wmove" $ wmove w (fi y) (fi x)

foreign import ccall unsafe
    wmove :: Window -> CInt -> CInt -> IO CInt

-----------------------------------------------------------------------------
-- * Cursor routines
-----------------------------------------------------------------------------

data CursorVisibility
    = CursorInvisible
    | CursorVisible
    | CursorVeryVisible

vis_c :: CursorVisibility -> CInt
vis_c vis = case vis of
    CursorInvisible -> 0
    CursorVisible -> 1
    CursorVeryVisible -> 2

c_vis :: CInt -> CursorVisibility
c_vis 0 = CursorInvisible
c_vis 1 = CursorVisible
c_vis 2 = CursorVeryVisible
c_vis n = error ("Illegal C value for cursor visibility: " ++ show n)

-- | Set the cursor state.
--
-- > The curs_set routine sets the cursor state is set to invisible, normal, or
-- > very visible for visibility equal to 0, 1, or 2 respectively. If the
-- > terminal supports the visibility requested, the previous cursor state is
-- > returned; otherwise, ERR is returned.
cursSet :: CursorVisibility -> IO CursorVisibility
cursSet CursorInvisible = do
    leaveOk True
    old <- curs_set 0
    return $ c_vis old
cursSet v = do
    leaveOk False
    old <- curs_set (vis_c v)
    return $ c_vis old

foreign import ccall unsafe "HSCurses.h curs_set"
    curs_set :: CInt -> IO CInt

-- | Get the current cursor coordinates.
getYX :: Window -> IO (Int, Int)
getYX w =
    alloca $ \py ->
        -- allocate two ints on the stack
        alloca $ \px -> do
            nomacro_getyx w py px -- writes current cursor coords
            y <- peek py
            x <- peek px
            return (fromIntegral y, fromIntegral x)

-- | Get the current cursor coords, written into the two argument ints.
--
-- > The getyx macro places the current cursor position of the given window in
-- > the two integer variables y and x.
foreign import ccall unsafe "HSCursesUtils.h hscurses_nomacro_getyx"
    nomacro_getyx :: Window -> Ptr CInt -> Ptr CInt -> IO ()

touchWin :: Window -> IO ()
touchWin w = throwIfErr_ "touchwin" $ touchwin w

foreign import ccall touchwin :: Window -> IO CInt

newPad :: Int -> Int -> IO Window
newPad nlines ncols =
    throwIfNull "newpad" $
        newpad (fromIntegral nlines) (fromIntegral ncols)

pRefresh :: Window -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
pRefresh pad pminrow pmincol sminrow smincol smaxrow smaxcol =
    throwIfErr_ "prefresh" $
        prefresh
            pad
            (fromIntegral pminrow)
            (fromIntegral pmincol)
            (fromIntegral sminrow)
            (fromIntegral smincol)
            (fromIntegral smaxrow)
            (fromIntegral smaxcol)

delWin :: Window -> IO ()
delWin w = throwIfErr_ "delwin" $ delwin w

data Border = Border
    { ls :: Char
    , rs :: Char
    , ts :: Char
    , bs :: Char
    , tl :: Char
    , tr :: Char
    , bl :: Char
    , br :: Char
    }

defaultBorder :: Border
defaultBorder = Border '\0' '\0' '\0' '\0' '\0' '\0' '\0' '\0'

-- | Draw a border around the edges of a window. 'defaultBorder' is a record
-- representing all 0 parameters to wrecord.
wBorder :: Window -> Border -> IO ()
wBorder w (Border ls rs ts bs tl tr bl br) =
    throwIfErr_ "wborder" $
        wborder w ls' rs' ts' bs' tl' tr' bl' br'
  where
    ls' = castCharToCChar ls
    rs' = castCharToCChar rs
    ts' = castCharToCChar ts
    bs' = castCharToCChar bs
    tl' = castCharToCChar tl
    tr' = castCharToCChar tr
    bl' = castCharToCChar bl
    br' = castCharToCChar br

foreign import ccall unsafe
    prefresh :: Window -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe
    newpad :: CInt -> CInt -> IO Window

foreign import ccall unsafe
    delwin :: Window -> IO CInt

foreign import ccall unsafe
    wborder :: Window -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> CChar -> IO CInt

newWin :: Int -> Int -> Int -> Int -> IO Window
newWin nlines ncolumn begin_y begin_x =
    throwIfNull "newwin" $
        newwin (fi nlines) (fi ncolumn) (fi begin_y) (fi begin_x)

foreign import ccall unsafe
    newwin :: CInt -> CInt -> CInt -> CInt -> IO Window

wClrToEol :: Window -> IO ()
wClrToEol w = throwIfErr_ "wclrtoeol" $ wclrtoeol w

foreign import ccall unsafe wclrtoeol :: Window -> IO CInt

-- | >      The getch, wgetch, mvgetch and mvwgetch, routines read a
--   >      character  from the window.
#if defined(HAVE_LIBPDCURSES) || defined (HAVE_LIBPDCURSESW)
foreign import ccall unsafe "HSCurses.h hscurses_nomacro_getch" getch :: IO CInt
#else
foreign import ccall unsafe "HSCurses.h getch" getch :: IO CInt
#endif

foreign import ccall unsafe flushinp :: IO CInt

foreign import ccall unsafe "HSCurses.h noqiflush" noqiflush :: IO ()

foreign import ccall unsafe "HSCurses.h beep" c_beep :: IO CInt
foreign import ccall unsafe "HSCurses.h flash" c_flash :: IO CInt

beep :: IO ()
beep = do
    br <- c_beep
    when (br /= (# const OK)) (c_flash >> return ())

-- | A mapping of curses keys to Haskell values.
data Key
    = KeyChar Char
    | KeyBreak
    | KeyDown
    | KeyUp
    | KeyLeft
    | KeyRight
    | KeyHome
    | KeyBackspace
    | KeyF Int
    | KeyDL
    | KeyIL
    | KeyDC
    | KeyIC
    | KeyEIC
    | KeyClear
    | KeyEOS
    | KeyEOL
    | KeySF
    | KeySR
    | KeyNPage
    | KeyPPage
    | KeySTab
    | KeyCTab
    | KeyCATab
    | KeyEnter
    | KeySReset
    | KeyReset
    | KeyPrint
    | KeyLL
    | KeyA1
    | KeyA3
    | KeyB2
    | KeyC1
    | KeyC3
    | KeyBTab
    | KeyBeg
    | KeyCancel
    | KeyClose
    | KeyCommand
    | KeyCopy
    | KeyCreate
    | KeyEnd
    | KeyExit
    | KeyFind
    | KeyHelp
    | KeyMark
    | KeyMessage
    | KeyMove
    | KeyNext
    | KeyOpen
    | KeyOptions
    | KeyPrevious
    | KeyRedo
    | KeyReference
    | KeyRefresh
    | KeyReplace
    | KeyRestart
    | KeyResume
    | KeySave
    | KeySBeg
    | KeySCancel
    | KeySCommand
    | KeySCopy
    | KeySCreate
    | KeySDC
    | KeySDL
    | KeySelect
    | KeySEnd
    | KeySEOL
    | KeySExit
    | KeySFind
    | KeySHelp
    | KeySHome
    | KeySIC
    | KeySLeft
    | KeySMessage
    | KeySMove
    | KeySNext
    | KeySOptions
    | KeySPrevious
    | KeySPrint
    | KeySRedo
    | KeySReplace
    | KeySRight
    | KeySRsume
    | KeySSave
    | KeySSuspend
    | KeySUndo
    | KeySuspend
    | KeyUndo
    | KeyResize
    | KeyMouse
    | KeyUnknown Int
    deriving (Eq, Show)

decodeKey :: CInt -> Key
decodeKey key = case key of
    _ | key >= 0 && key <= 255 -> KeyChar (chr (fromIntegral key))
    (#const KEY_BREAK)         -> KeyBreak
    (#const KEY_DOWN)          -> KeyDown
    (#const KEY_UP)            -> KeyUp
    (#const KEY_LEFT)          -> KeyLeft
    (#const KEY_RIGHT)         -> KeyRight
    (#const KEY_HOME)          -> KeyHome
    (#const KEY_BACKSPACE)     -> KeyBackspace
    _ | key >= (#const KEY_F0) && key <= (#const KEY_F(63))
                               -> KeyF (fromIntegral (key - #const KEY_F0))
    (#const KEY_DL)            -> KeyDL
    (#const KEY_IL)            -> KeyIL
    (#const KEY_DC)            -> KeyDC
    (#const KEY_IC)            -> KeyIC
    (#const KEY_EIC)           -> KeyEIC
    (#const KEY_CLEAR)         -> KeyClear
    (#const KEY_EOS)           -> KeyEOS
    (#const KEY_EOL)           -> KeyEOL
    (#const KEY_SF)            -> KeySF
    (#const KEY_SR)            -> KeySR
    (#const KEY_NPAGE)         -> KeyNPage
    (#const KEY_PPAGE)         -> KeyPPage
    (#const KEY_STAB)          -> KeySTab
    (#const KEY_CTAB)          -> KeyCTab
    (#const KEY_CATAB)         -> KeyCATab
    (#const KEY_ENTER)         -> KeyEnter
    (#const KEY_SRESET)        -> KeySReset
    (#const KEY_RESET)         -> KeyReset
    (#const KEY_PRINT)         -> KeyPrint
    (#const KEY_LL)            -> KeyLL
    (#const KEY_A1)            -> KeyA1
    (#const KEY_A3)            -> KeyA3
    (#const KEY_B2)            -> KeyB2
    (#const KEY_C1)            -> KeyC1
    (#const KEY_C3)            -> KeyC3
    (#const KEY_BTAB)          -> KeyBTab
    (#const KEY_BEG)           -> KeyBeg
    (#const KEY_CANCEL)        -> KeyCancel
    (#const KEY_CLOSE)         -> KeyClose
    (#const KEY_COMMAND)       -> KeyCommand
    (#const KEY_COPY)          -> KeyCopy
    (#const KEY_CREATE)        -> KeyCreate
    (#const KEY_END)           -> KeyEnd
    (#const KEY_EXIT)          -> KeyExit
    (#const KEY_FIND)          -> KeyFind
    (#const KEY_HELP)          -> KeyHelp
    (#const KEY_MARK)          -> KeyMark
    (#const KEY_MESSAGE)       -> KeyMessage
    (#const KEY_MOVE)          -> KeyMove
    (#const KEY_NEXT)          -> KeyNext
    (#const KEY_OPEN)          -> KeyOpen
    (#const KEY_OPTIONS)       -> KeyOptions
    (#const KEY_PREVIOUS)      -> KeyPrevious
    (#const KEY_REDO)          -> KeyRedo
    (#const KEY_REFERENCE)     -> KeyReference
    (#const KEY_REFRESH)       -> KeyRefresh
    (#const KEY_REPLACE)       -> KeyReplace
    (#const KEY_RESTART)       -> KeyRestart
    (#const KEY_RESUME)        -> KeyResume
    (#const KEY_SAVE)          -> KeySave
    (#const KEY_SBEG)          -> KeySBeg
    (#const KEY_SCANCEL)       -> KeySCancel
    (#const KEY_SCOMMAND)      -> KeySCommand
    (#const KEY_SCOPY)         -> KeySCopy
    (#const KEY_SCREATE)       -> KeySCreate
    (#const KEY_SDC)           -> KeySDC
    (#const KEY_SDL)           -> KeySDL
    (#const KEY_SELECT)        -> KeySelect
    (#const KEY_SEND)          -> KeySEnd
    (#const KEY_SEOL)          -> KeySEOL
    (#const KEY_SEXIT)         -> KeySExit
    (#const KEY_SFIND)         -> KeySFind
    (#const KEY_SHELP)         -> KeySHelp
    (#const KEY_SHOME)         -> KeySHome
    (#const KEY_SIC)           -> KeySIC
    (#const KEY_SLEFT)         -> KeySLeft
    (#const KEY_SMESSAGE)      -> KeySMessage
    (#const KEY_SMOVE)         -> KeySMove
    (#const KEY_SNEXT)         -> KeySNext
    (#const KEY_SOPTIONS)      -> KeySOptions
    (#const KEY_SPREVIOUS)     -> KeySPrevious
    (#const KEY_SPRINT)        -> KeySPrint
    (#const KEY_SREDO)         -> KeySRedo
    (#const KEY_SREPLACE)      -> KeySReplace
    (#const KEY_SRIGHT)        -> KeySRight
    (#const KEY_SRSUME)        -> KeySRsume
    (#const KEY_SSAVE)         -> KeySSave
    (#const KEY_SSUSPEND)      -> KeySSuspend
    (#const KEY_SUNDO)         -> KeySUndo
    (#const KEY_SUSPEND)       -> KeySuspend
    (#const KEY_UNDO)          -> KeyUndo
#ifdef KEY_RESIZE
    (#const KEY_RESIZE)        -> KeyResize
#endif
#ifdef KEY_MOUSE
    (#const KEY_MOUSE)         -> KeyMouse
#endif
    _                          -> KeyUnknown (fromIntegral key)

keyResizeCode :: Maybe CInt
#ifdef KEY_RESIZE
keyResizeCode = Just (#const KEY_RESIZE)
#else
keyResizeCode = Nothing
#endif

cERR :: CInt
cERR = #const ERR

cKEY_UP, cKEY_DOWN, cKEY_LEFT, cKEY_RIGHT :: ChType
cKEY_UP = #const KEY_UP
cKEY_DOWN = #const KEY_DOWN
cKEY_LEFT = #const KEY_LEFT
cKEY_RIGHT = #const KEY_RIGHT

cTRUE :: NBool
cTRUE = #const TRUE

-- ncurses ungetch and Haskell's threadWaitRead do not work together well.
-- So I decided to implement my own input queue.
ungetCh :: (Integral a) => a -> IO ()
ungetCh i = do
    debug "ungetCh called"
    writeChan inputBuf (BufDirect (fi i))

data BufData
    = -- | Data directly available
      BufDirect CInt
    | -- | Data can be obtained by calling getch
      DataViaGetch

inputBuf :: Chan BufData
inputBuf = unsafePerformIO newChan
{-# NOINLINE inputBuf #-}

getchToInputBuf :: IO ()
getchToInputBuf = do
    threadWaitRead (fi (0 :: Int))
    {- From the (n)curses manpage:
    Programmers  concerned  about portability should be prepared for either
    of two cases: (a) signal receipt does not interrupt getch;  (b)  signal
    receipt  interrupts getch and causes it to return ERR with errno set to
    EINTR.  Under the ncurses implementation, handled signals never  inter$B!>(B
    rupt getch.
    -}
    -- we only signalize that getch can now called without getting blocked.
    -- directly calling `getch' might result in losing the character just
    -- read (race condition).
    debug "now input available on stdin"
    writeChan inputBuf DataViaGetch

-- | Read a single character from the window.
getCh :: IO Key
getCh = do
  debug "getCh called"
  tid <- forkIO getchToInputBuf
  d <- readChan inputBuf
  -- we can kill the thread safely, because the thread does not read any data
  -- via getch
  killThread tid
  v <- case d of
         BufDirect x ->
           do debug "getCh: getting data directly from buffer"
              return x
         DataViaGetch ->
           do debug "getCh: getting data via getch"
              getch -- won't block!
  case v of
    (#const ERR) -> -- NO CODE IN THIS LINE
        do e <- getErrno
           if e `elem` [eAGAIN, eINTR]
              then do debug "Curses.getCh returned eAGAIN or eINTR"
                      getCh
              else throwErrno "HSCurses.Curses.getch"
    k -> let k' = decodeKey k
             in do debug ("getCh: result = " ++ show k')
                   return k'

resizeTerminal :: Int -> Int -> IO ()

#ifdef HAVE_RESIZETERM
resizeTerminal a b = throwIfErr_ "resizeterm"  $ resizeterm (fi a) (fi b)

foreign import ccall unsafe "HSCurses.h resizeterm"
    resizeterm :: CInt -> CInt -> IO CInt
#else
resizeTerminal _ _ = return ()
#endif

-- | The SIGWINCH signal is sent whenever the terminal size changes. This signal
-- is not available on all platforms, so it is a |Maybe| value.

#ifdef mingw32_HOST_OS
type Signal = CInt
#endif

cursesSigWinch :: Maybe Signal
#ifdef SIGWINCH
cursesSigWinch = Just (#const SIGWINCH)
#else
cursesSigWinch = Nothing
#endif

-- | A test case printing out some common attributes.
cursesTest :: IO ()
cursesTest = do
    initScr
    hc <- hasColors
    when hc startColor
    ccc <- canChangeColor
    (ys,xs) <- scrSize
    cp <- colorPairs
    cs <- colors
    endWin
    putStrLn $ "ScreenSize: " ++ show (xs,ys)
    putStrLn $ "hasColors: " ++ show hc
    putStrLn $ "canChangeColor: " ++ show ccc
    putStrLn $ "colorPairs: " ++ show cp
    putStrLn $ "colors: " ++ show cs

-----------------------------------------------------------------------------
-- * Mouse routines
-----------------------------------------------------------------------------

data MouseEvent = MouseEvent
    { mouseEventId :: CInt
    , mouseEventX :: CInt
    , mouseEventY :: CInt
    , mouseEventZ :: CInt
    , mouseEventButton :: [ButtonEvent]
    }
    deriving (Show)

instance Storable MouseEvent where
    sizeOf _ = (#size MEVENT)
    alignment _ = (#alignment MEVENT)
    peek ptr = do
        id' <- (#peek MEVENT, id) ptr
        x <- (#peek MEVENT, x) ptr
        y <- (#peek MEVENT, y) ptr
        z <- (#peek MEVENT, z) ptr
        bstate :: (#type mmask_t) <- (#peek MEVENT, bstate) ptr
        pure $! MouseEvent id' x y z (besFromMouseMask bstate)
    poke ptr (MouseEvent id' x y z bstate) = do
        (#poke MEVENT, id) ptr id'
        (#poke MEVENT, x) ptr x
        (#poke MEVENT, y) ptr y
        (#poke MEVENT, z) ptr z
        (#poke MEVENT, bstate) ptr (besToMouseMask bstate)

foreign import ccall unsafe "HSCurses.h getmouse"
    getmouse :: Ptr MouseEvent -> IO CInt

getMouse :: (MonadIO m) => m (Maybe MouseEvent)
getMouse = liftIO $ alloca $ \ptr -> do
    res <- getmouse ptr
    if res == (# const OK)
        then Just <$> peek ptr
        else pure Nothing

data ButtonEvent
    = ButtonPressed Int
    | ButtonReleased Int
    | ButtonClicked Int
    | ButtonDoubleClicked Int
    | ButtonTripleClicked Int
    | ButtonShift
    | ButtonControl
    | ButtonAlt
    deriving (Eq, Show)

withMouseEventMask :: (MonadIO m) => [ButtonEvent] -> m a -> m a
withAllMouseEvents :: (MonadIO m) => m a -> m a

#ifdef KEY_MOUSE

foreign import ccall unsafe "HSCurses.h mousemask"
    mousemask :: (#type mmask_t) -> Ptr (#type mmask_t) -> IO (#type mmask_t)

-- TODO: bracket instead?
withMouseEventMask bes action = do
    ov <- liftIO $ alloca (\a ->  mousemask (besToMouseMask bes) a >> peek a)
    r <- action
    liftIO $ mousemask ov nullPtr
    return r

withAllMouseEvents action = do
    ov <- liftIO $ alloca (\a ->  mousemask (#const ALL_MOUSE_EVENTS) a >> peek a)
    r <- action
    liftIO $ mousemask ov nullPtr
    return r

besToMouseMask :: [ButtonEvent] -> (#type mmask_t)
besToMouseMask bes = foldl' (.|.) 0 (map cb bes) where
    cb (ButtonPressed 1) = (#const BUTTON1_PRESSED)
    cb (ButtonPressed 2) = (#const BUTTON2_PRESSED)
    cb (ButtonPressed 3) = (#const BUTTON3_PRESSED)
    cb (ButtonPressed 4) = (#const BUTTON4_PRESSED)
    cb (ButtonReleased 1) = (#const BUTTON1_RELEASED)
    cb (ButtonReleased 2) = (#const BUTTON2_RELEASED)
    cb (ButtonReleased 3) = (#const BUTTON3_RELEASED)
    cb (ButtonReleased 4) = (#const BUTTON4_RELEASED)
    cb (ButtonClicked 1) = (#const BUTTON1_CLICKED)
    cb (ButtonClicked 2) = (#const BUTTON2_CLICKED)
    cb (ButtonClicked 3) = (#const BUTTON3_CLICKED)
    cb (ButtonClicked 4) = (#const BUTTON4_CLICKED)
    cb (ButtonDoubleClicked 1) = (#const BUTTON1_DOUBLE_CLICKED)
    cb (ButtonDoubleClicked 2) = (#const BUTTON2_DOUBLE_CLICKED)
    cb (ButtonDoubleClicked 3) = (#const BUTTON3_DOUBLE_CLICKED)
    cb (ButtonDoubleClicked 4) = (#const BUTTON4_DOUBLE_CLICKED)
    cb (ButtonTripleClicked 1) = (#const BUTTON1_TRIPLE_CLICKED)
    cb (ButtonTripleClicked 2) = (#const BUTTON2_TRIPLE_CLICKED)
    cb (ButtonTripleClicked 3) = (#const BUTTON3_TRIPLE_CLICKED)
    cb (ButtonTripleClicked 4) = (#const BUTTON4_TRIPLE_CLICKED)
#if NCURSES_MOUSE_VERSION > 1
    cb (ButtonPressed 5) = (#const BUTTON5_PRESSED)
    cb (ButtonReleased 5) = (#const BUTTON5_RELEASED)
    cb (ButtonClicked 5) = (#const BUTTON5_CLICKED)
    cb (ButtonDoubleClicked 5) = (#const BUTTON5_DOUBLE_CLICKED)
    cb (ButtonTripleClicked 5) = (#const BUTTON5_TRIPLE_CLICKED)
#endif
    cb ButtonShift = (#const BUTTON_SHIFT)
    cb ButtonAlt = (#const BUTTON_ALT)
#ifdef BUTTON_CTRL
    cb ButtonControl = (#const BUTTON_CTRL)
#else
    cb ButtonControl = (#const BUTTON_CONTROL)
#endif
    cb _ = 0

besFromMouseMask :: (#type mmask_t) -> [ButtonEvent]
besFromMouseMask mmask =
    foldl'
        (\evts (c, evt) -> if mmask .&. c /= 0 then evt : evts else evts)
        mempty
        mappings
  where
    mappings =
        [ ((#const BUTTON1_PRESSED), ButtonPressed 1)
        , ((#const BUTTON2_PRESSED), ButtonPressed 2)
        , ((#const BUTTON3_PRESSED), ButtonPressed 3)
        , ((#const BUTTON4_PRESSED), ButtonPressed 4)
        , ((#const BUTTON1_RELEASED), ButtonReleased 1)
        , ((#const BUTTON2_RELEASED), ButtonReleased 2)
        , ((#const BUTTON3_RELEASED), ButtonReleased 3)
        , ((#const BUTTON4_RELEASED), ButtonReleased 4)
        , ((#const BUTTON1_CLICKED), ButtonClicked 1)
        , ((#const BUTTON2_CLICKED), ButtonClicked 2)
        , ((#const BUTTON3_CLICKED), ButtonClicked 3)
        , ((#const BUTTON4_CLICKED), ButtonClicked 4)
        , ((#const BUTTON1_DOUBLE_CLICKED), ButtonDoubleClicked 1)
        , ((#const BUTTON2_DOUBLE_CLICKED), ButtonDoubleClicked 2)
        , ((#const BUTTON3_DOUBLE_CLICKED), ButtonDoubleClicked 3)
        , ((#const BUTTON4_DOUBLE_CLICKED), ButtonDoubleClicked 4)
        , ((#const BUTTON1_TRIPLE_CLICKED), ButtonTripleClicked 1)
        , ((#const BUTTON2_TRIPLE_CLICKED), ButtonTripleClicked 2)
        , ((#const BUTTON3_TRIPLE_CLICKED), ButtonTripleClicked 3)
        , ((#const BUTTON4_TRIPLE_CLICKED), ButtonTripleClicked 4)
#if NCURSES_MOUSE_VERSION > 1
        , ((#const BUTTON5_PRESSED), ButtonPressed 5)
        , ((#const BUTTON5_RELEASED), ButtonReleased 5)
        , ((#const BUTTON5_CLICKED), ButtonClicked 5)
        , ((#const BUTTON5_DOUBLE_CLICKED), ButtonDoubleClicked 5)
        , ((#const BUTTON5_TRIPLE_CLICKED), ButtonTripleClicked 5)
#endif
        , ((#const BUTTON_SHIFT), ButtonShift)
        , ((#const BUTTON_ALT), ButtonAlt)
#ifdef BUTTON_CTRL
        , ((#const BUTTON_CTRL), ButtonControl)
#else
        , ((#const BUTTON_CONTROL), ButtonControl)
#endif
        ]

#else

withMouseEventMask _ a = a
withAllMouseEvents = id

#endif

ulCorner :: Char
ulCorner = chr 0x250C

llCorner :: Char
llCorner = chr 0x2514

urCorner :: Char
urCorner = chr 0x2510

lrCorner :: Char
lrCorner = chr 0x2518

rTee :: Char
rTee = chr 0x2524

lTee :: Char
lTee = chr 0x251C

bTee :: Char
bTee = chr 0x2534

tTee :: Char
tTee = chr 0x252C

hLine :: Char
hLine = chr 0x2500

vLine :: Char
vLine = chr 0x2502

plus :: Char
plus = chr 0x253C

s1 :: Char
s1 = chr 0x23BA -- was: 0xF800

s9 :: Char
s9 = chr 0x23BD -- was: 0xF804

diamond :: Char
diamond = chr 0x25C6

ckBoard :: Char
ckBoard = chr 0x2592

degree :: Char
degree = chr 0x00B0

plMinus :: Char
plMinus = chr 0x00B1

bullet :: Char
bullet = chr 0x00B7

lArrow :: Char
lArrow = chr 0x2190

rArrow :: Char
rArrow = chr 0x2192

dArrow :: Char
dArrow = chr 0x2193

uArrow :: Char
uArrow = chr 0x2191

board :: Char
board = chr 0x2591

lantern :: Char
lantern = chr 0x256C

block :: Char
block = chr 0x2588

s3 :: Char
s3 = chr 0x23BB -- was: 0xF801

s7 :: Char
s7 = chr 0x23BC -- was: 0xF803

lEqual :: Char
lEqual = chr 0x2264

gEqual :: Char
gEqual = chr 0x2265

pi :: Char
pi = chr 0x03C0

nEqual :: Char
nEqual = chr 0x2260

sterling :: Char
sterling = chr 0x00A3

recognize :: Char -> IO a -> (ChType -> IO a) -> IO a
recognize _ch noConvert _convert = noConvert -- Handle the most common case first.
