{-# OPTIONS -cpp -fno-cse #-}

-- Copyright (c) 2005 Stefan Wehr - http://www.stefanwehr.de
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

module HSCurses.Logging (trace) where

import IO
import Time
import Locale
import Data.IORef
import System.IO.Unsafe             ( unsafePerformIO )

trace :: String -> a -> a

#ifdef __DEBUG__

logFile :: Handle
logFile = unsafePerformIO $ openFile ".hscurses.log" AppendMode
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
    unsafePerformIO $ do ts <- formatTime
                         hPutStrLn logFile ("[" ++ ts ++ "] " ++ s)
                         hFlush logFile
                         return x

#else

trace _ x = x

#endif
