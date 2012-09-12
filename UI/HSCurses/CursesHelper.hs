{-# LANGUAGE CPP #-}

--
-- Copyright (C) 2005-2011 Stefan Wehr
--
-- Derived from: yi/Curses/UI.hs
--      Copyright (C) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
--      Released under the GPL, granted permission to release this module
--      under the LGPL.
--
-- Derived from: riot/UI.hs
--      Copyright (c) Tuomo Valkonen 2004.
--      Released under the GPL, granted permission to release this module
--      under the LGPL.

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


module UI.HSCurses.CursesHelper (

        -- * UI initialisation
        start, end, suspend, resizeui,

        -- * Input
        getKey,

        -- * Drawing
        drawLine, drawCursor,

        -- * Navigation
        gotoTop,

        -- * Colors
        ForegroundColor(..), BackgroundColor(..),
        defaultColor, black, red, green, yellow, blue, magenta, cyan, white,

        -- * Attributes
        Attribute(..), convertAttributes,

        -- * Style
        Style(..), CursesStyle, mkCursesStyle, changeCursesStyle,
        setStyle, wSetStyle, resetStyle, wResetStyle, convertStyles,
        defaultStyle, defaultCursesStyle, withStyle, wWithStyle,

        -- * Keys
        displayKey,

        -- * Helpers
        withCursor, withProgram
  )   where

import UI.HSCurses.Curses hiding ( refresh, Window )
import UI.HSCurses.Logging
import UI.HSCurses.MonadException
import qualified UI.HSCurses.Curses as Curses

import Data.Char
import Data.Maybe
import Control.Monad.Trans

#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

--
--

--
-- | @start@ initializes the UI and grabs the keyboard.
--
-- This function installs a handler for the SIGWINCH signal
-- which writes the KEY_RESIZE key to the input queue (if KEY_RESIZE and
-- and SIGWINCH are both available).
--
start :: IO ()
start = do
    Curses.initCurses                   -- initialise the screen
    Curses.resetParams
    Curses.keypad Curses.stdScr True    -- grab the keyboard
    case (Curses.cursesSigWinch, Curses.keyResizeCode) of
#ifndef mingw32_HOST_OS
      (Just sig, Just key) ->
          do installHandler sig (Catch $ sigwinch sig key) Nothing
             return ()
#endif
      _ -> debug ("cannot install SIGWINCH handler: signal=" ++
                  show Curses.cursesSigWinch ++ ", KEY_RESIZE=" ++
                  show Curses.keyResizeCode)
#ifndef mingw32_HOST_OS
    where sigwinch sig key =
              do debug "SIGWINCH signal received"
                 Curses.ungetCh key
                 installHandler sig (Catch $ sigwinch sig key) Nothing
                 return ()
#endif



--
-- | Clean up and go home.
--
end :: IO ()
end = do Curses.endWin
-- Refresh is needed on linux. grr.
#if NCURSES_UPDATE_AFTER_END
         Curses.update
#endif

--
-- | Suspend the program.
--
suspend :: IO ()
#ifndef mingw32_HOST_OS
suspend = raiseSignal sigTSTP
#else
suspend = return ()
#endif

--
-- | @getKey refresh@ reads a key.
--
-- The @refresh@ function is used to redraw the screen when the terminal size
-- changes (see the documentatio of @start@ for a discussion of the problem).
--
getKey :: MonadIO m => m () -> m Key
getKey refresh = do
    k <- liftIO $ Curses.getCh
    debug ("getKey: " ++ show k)
    case k of
      KeyResize ->
          do refresh
             getKey refresh
      _ -> return k


--
-- | @drawLine n s@ draws @n@ characters of string @s@.
--
drawLine :: Int -> String -> IO ()
-- lazy version is faster than calculating length of s
drawLine w s = Curses.wAddStr Curses.stdScr $! take w (s ++ repeat ' ')

--
-- | Draw the cursor at the given position.
--
drawCursor :: (Int,Int) -> (Int, Int) -> IO ()
drawCursor (o_y,o_x) (y,x) = withCursor Curses.CursorVisible $ do
    gotoTop
    (h,w) <- scrSize
    Curses.wMove Curses.stdScr (min (h-1) (o_y + y)) (min (w-1) (o_x + x))

--
-- | Move cursor to origin of stdScr.
--
gotoTop :: IO ()
gotoTop = Curses.wMove Curses.stdScr 0 0


--
-- | Resize the window
-- From "Writing Programs with NCURSES", by Eric S. Raymond and
-- Zeyd M. Ben-Halim
--
--
resizeui :: IO (Int,Int)
resizeui = do
    Curses.endWin
    Curses.refresh
    Curses.scrSize



------------------------------------------------------------------------
--
-- | Basic colors.
--
defaultColor :: Curses.Color
defaultColor = fromJust $ Curses.color "default"

black, red, green, yellow, blue, magenta, cyan, white :: Curses.Color
black     = fromJust $ Curses.color "black"
red       = fromJust $ Curses.color "red"
green     = fromJust $ Curses.color "green"
yellow    = fromJust $ Curses.color "yellow"
blue      = fromJust $ Curses.color "blue"
magenta   = fromJust $ Curses.color "magenta"
cyan      = fromJust $ Curses.color "cyan"
white     = fromJust $ Curses.color "white"

--
-- | Converts a list of 'Curses.Color' pairs (foreground color and
--   background color) into the curses representation 'Curses.Pair'.
--
--   You should call this function exactly once, at application startup.
--
-- (not visible outside this module)
colorsToPairs :: [(Curses.Color, Curses.Color)] -> IO [Curses.Pair]
colorsToPairs cs =
    do p <- Curses.colorPairs
       let nColors = length cs
           blackWhite = p < nColors
       if blackWhite
          then trace ("Terminal does not support enough colors. Number of " ++
                      " colors requested: " ++ show nColors ++
                      ". Number of colors supported: " ++ show p)
                 return $ take nColors (repeat (Curses.Pair 0))
          else mapM toPairs (zip [1..] cs)
     where toPairs (n, (fg, bg)) =
               let p = Curses.Pair n
               in do Curses.initPair p fg bg
                     return p

------------------------------------------------------------------------
-- Nicer, user-visible color defs.
--
-- We separate colors into dark and bright colors, to prevent users
-- from erroneously constructing bright colors for dark backgrounds,
-- which doesn't work.

--
-- | Foreground colors.
--
data ForegroundColor
    = BlackF
    | GreyF
    | DarkRedF
    | RedF
    | DarkGreenF
    | GreenF
    | BrownF
    | YellowF
    | DarkBlueF
    | BlueF
    | PurpleF
    | MagentaF
    | DarkCyanF
    | CyanF
    | WhiteF
    | BrightWhiteF
    | DefaultF
    deriving (Eq, Show)

--
-- | Background colors.
--
data BackgroundColor
    = BlackB
    | DarkRedB
    | DarkGreenB
    | BrownB
    | DarkBlueB
    | PurpleB
    | DarkCyanB
    | WhiteB
    | DefaultB
    deriving (Eq, Show)

--
-- | Mapping abstract colours to ncurses attributes and colours
--
-- (not visible outside this module)

convertBg :: BackgroundColor -> ([Attribute], Curses.Color)
convertBg c = case c of
    BlackB      -> ([], black)
    DarkRedB    -> ([], red)
    DarkGreenB  -> ([], green)
    BrownB      -> ([], yellow)
    DarkBlueB   -> ([], blue)
    PurpleB     -> ([], magenta)
    DarkCyanB   -> ([], cyan)
    WhiteB      -> ([], white)
    DefaultB    -> ([], defaultColor)

convertFg :: ForegroundColor -> ([Attribute], Curses.Color)
convertFg c = case c of
    BlackF       -> ([], black)
    GreyF        -> ([Bold], black)
    DarkRedF     -> ([], red)
    RedF         -> ([Bold], red)
    DarkGreenF   -> ([], green)
    GreenF       -> ([Bold], green)
    BrownF       -> ([], yellow)
    YellowF      -> ([Bold], yellow)
    DarkBlueF    -> ([], blue)
    BlueF        -> ([Bold], blue)
    PurpleF      -> ([], magenta)
    MagentaF     -> ([Bold], magenta)
    DarkCyanF    -> ([], cyan)
    CyanF        -> ([Bold], cyan)
    WhiteF       -> ([], white)
    BrightWhiteF -> ([Bold], white)
    DefaultF     -> ([], defaultColor)


------------------------------------------------------------------------
--
-- | Abstractions for some commonly used attributes.
--
data Attribute = Bold
               | Underline
               | Dim
               | Reverse
               | Blink
               deriving (Eq, Show)

--
-- | Converts an abstract attribute list into its curses representation.
--
convertAttributes :: [Attribute] -> Curses.Attr
convertAttributes =
    foldr setAttrs Curses.attr0
    where setAttrs Bold = setBoldA
          setAttrs Underline = setUnderlineA
          setAttrs Dim = setDimA
          setAttrs Reverse = setReverseA
          setAttrs Blink = setBlinkA

setBoldA, setUnderlineA, setDimA,
  setReverseA, setBlinkA :: Curses.Attr -> Curses.Attr
setBoldA = flip Curses.setBold True
setUnderlineA = flip Curses.setUnderline True
setDimA = flip Curses.setDim True
setReverseA = flip Curses.setReverse   True
setBlinkA = flip Curses.setBlink True

------------------------------------------------------------------------
--
-- | A humand-readable style.
--
data Style = Style ForegroundColor BackgroundColor
           | AttributeStyle [Attribute] ForegroundColor BackgroundColor
           | ColorlessStyle [Attribute]
           deriving (Eq, Show)

defaultStyle :: Style
defaultStyle = Style DefaultF DefaultB

--
-- | A style which uses the internal curses representations for
--   attributes and colors.
--
data CursesStyle = CursesStyle Curses.Attr Curses.Pair
                 | ColorlessCursesStyle Curses.Attr
                 deriving (Eq, Show)

{-
instance Show CursesStyle where
    show (CursesStyle _ _) = "CursesStyle"
    show (ColorlessCursesStyle _) = "ColorlessCursesStyle"
-}

mkCursesStyle :: [Attribute] -> CursesStyle
mkCursesStyle attrs = ColorlessCursesStyle (convertAttributes attrs)

--
-- | Changes the attributes of the given CursesStyle.
--
changeCursesStyle :: CursesStyle -> [Attribute] -> CursesStyle
changeCursesStyle (CursesStyle _ p) attrs =
    CursesStyle (convertAttributes attrs) p
changeCursesStyle _ attrs = ColorlessCursesStyle (convertAttributes attrs)

defaultCursesStyle :: CursesStyle
defaultCursesStyle = CursesStyle Curses.attr0 (Curses.Pair 0)

--
-- | Reset the screen to normal values
--
resetStyle :: IO ()
resetStyle = wResetStyle Curses.stdScr

wResetStyle :: Curses.Window -> IO ()
wResetStyle = flip wSetStyle defaultCursesStyle

--
-- | Manipulate the current style of the standard screen
--
setStyle :: CursesStyle -> IO ()
setStyle = wSetStyle Curses.stdScr

wSetStyle :: Curses.Window -> CursesStyle -> IO ()
wSetStyle window (CursesStyle a p) = Curses.wAttrSet window (a, p)
wSetStyle window (ColorlessCursesStyle a) =
    do (_, p) <- Curses.wAttrGet window
       Curses.wAttrSet window (a, p)

withStyle :: MonadExcIO m => CursesStyle -> m a -> m a
withStyle = wWithStyle Curses.stdScr

wWithStyle :: MonadExcIO m => Curses.Window -> CursesStyle -> m a -> m a
wWithStyle window style action =
    bracketM
        (liftIO $ do old <- Curses.wAttrGet window    -- before
                     wSetStyle window style
                     return old)
        (\old -> liftIO $ Curses.wAttrSet window old) -- after
        (\_ -> action)                                -- do this

--
-- | Converts a list of human-readable styles into the corresponding
--   curses representation.
--
--   This function should be called exactly once at application startup
--   for all styles of the application.
convertStyles :: [Style] -> IO [CursesStyle]
convertStyles styleList =
    do let (attrs, cs) = unzip $ map convertStyle styleList
           cursesAttrs = map convertAttributes attrs
       cursesPairs <- colorsToPairs' cs
       let res = zipWith toCursesStyle cursesAttrs cursesPairs
       trace ("convertStyles: " ++ show (zip styleList res)) (return res)
    where convertStyle (Style fg bg) = convertStyle (AttributeStyle [] fg bg)
          convertStyle (AttributeStyle attrs fg bg) =
              let (afg, cfg) = convertFg fg
                  (abg, cbg) = convertBg bg
              in (afg ++ abg ++ attrs, Just (cfg, cbg))
          convertStyle (ColorlessStyle attrs) = (attrs, Nothing)
          colorsToPairs' cs =
              do pairs <- colorsToPairs (catMaybes cs)
                 return $ mergeNothing cs pairs
          mergeNothing (Just _:crest) (p:prest) = Just p
                                                  : mergeNothing crest prest
          mergeNothing (Nothing:crest) ps = Nothing : mergeNothing crest ps
          mergeNothing [] [] = []
          toCursesStyle cursesAttrs Nothing =
              ColorlessCursesStyle cursesAttrs
          toCursesStyle cursesAttrs (Just cursesPair) =
              CursesStyle cursesAttrs cursesPair

------------------------------------------------------------------------
--
-- | Converting keys to humand-readable strings
--

displayKey :: Key -> String
displayKey (KeyChar ' ') = "<Space>"
displayKey (KeyChar '\t') = "<Tab>"
displayKey (KeyChar '\r') = "<Enter>"
displayKey (KeyChar c)
    | isPrint c = [c]
displayKey (KeyChar c)  -- Control
    | ord '\^A' <= ord c && ord c <= ord '\^Z'
        = let c' = chr $ ord c - ord '\^A' + ord 'a'
              in '^':[toUpper c']
displayKey (KeyChar c) = show c
displayKey KeyDown = "<Down>"
displayKey KeyUp = "<Up>"
displayKey KeyLeft = "<Left>"
displayKey KeyRight = "<Right>"
displayKey KeyHome = "<Home>"
displayKey KeyBackspace = "<BS>"
displayKey (KeyF i) = 'F' : show i
displayKey KeyNPage = "<NPage>"
displayKey KeyPPage = "<PPage>"
displayKey KeyEnter = "<Return>"
displayKey KeyEnd = "<End>"
displayKey KeyIC = "<Insert>"
displayKey KeyDC = "<Delete>"
displayKey k = show k


------------------------------------------------------------------------
--
-- | Other helpers
--

--
-- | set the cursor, and do action
--
withCursor :: MonadExcIO m => CursorVisibility -> m a -> m a
withCursor nv action =
    bracketM
        (liftIO $ Curses.cursSet nv)             -- before
        (\vis -> liftIO $ Curses.cursSet vis)    -- after
        (\_ -> action)                           -- do this

withProgram :: MonadExcIO m => m a -> m a
withProgram action = withCursor CursorVisible $
    bracketM_ (liftIO endWin) (liftIO flushinp) action
