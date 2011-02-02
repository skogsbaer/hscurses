{-# LANGUAGE CPP #-}

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

module UI.HSCurses.Logging (trace,debug) where

import Control.Monad.Trans

#ifdef __DEBUG__

import Data.IORef
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Locale
import System.Time

#endif


trace :: String -> a -> a
debug :: MonadIO m => String -> m ()

#ifdef __DEBUG__

logFile :: Handle
logFile = unsafePerformIO $ do h <- openFile ".hscurses.log" AppendMode
                               debug_ h "logging initialized"
                               return h
{-# NOINLINE logFile #-}

formatTime :: IO String
formatTime =
    do let fmt = "%Y-%m-%d %H:%M:%S"
       clockT <- getClockTime
       calTime <- toCalendarTime clockT
       let maxSdecLen = 5
           sdec' = show $ ctPicosec calTime
           sdec = if length sdec' > maxSdecLen
                     then take maxSdecLen sdec'
                     else sdec'
       return (formatCalendarTime defaultTimeLocale fmt calTime
               ++ ":" ++ sdec)

trace s x =
    unsafePerformIO $ do debug s
                         return x

debug s = liftIO $ debug_ logFile s

debug_ f s =
    do ts <- formatTime
       hPutStrLn f ("[" ++ ts ++ "] " ++ s)
       hFlush f

#else

trace _ x = x

debug _ = return ()

#endif
