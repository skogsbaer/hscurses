{-# LANGUAGE ForeignFunctionInterface #-}

--
-- Copyright (c) 2004 Tuomo Valkonen <tuomov at iki dot fi>
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


-- | Iconv binding

#if HAVE_ICONV_H
# include <iconv.h>
#endif

module UI.HSCurses.IConv {-(
    IConv,
    iconv,
    iconv_,
    with_iconv,
    to_unicode,
    from_unicode,
    to_unicode_,
    from_unicode_
  )-} where

import UI.HSCurses.CWString          ( peekUTF8StringLen, withUTF8StringLen )

import System.IO.Unsafe ( unsafePerformIO )

import Foreign hiding ( unsafePerformIO )
import Foreign.C
import Control.Exception    ( Exception, try, bracket )

type IConv = Ptr () --(#type iconv_t)

err_ptr :: Ptr b -> Bool
err_ptr p = p == (plusPtr nullPtr (-1))

throw_if_not_2_big :: String -> IO CSize -> IO CSize
throw_if_not_2_big s r_ = do
    r <- r_
    if r == fromIntegral (-1 :: Int) then do
        errno <- getErrno
        if errno /= e2BIG then
	    throwErrno s
	  else
	    return r
      else
        return r

iconv_open :: String -> String -> IO IConv
iconv_open to from =
    withCString to $
        \cto -> withCString from $
            \cfrom -> do
	        throwErrnoIf err_ptr "iconv_open"
		    $ c_iconv_open cto cfrom


iconv_close :: IConv -> IO ()
iconv_close ic =
    throwErrnoIfMinus1_ "iconv_close" $ c_iconv_close ic

outbuf_size :: Int
outbuf_size = 1024

do_iconv :: ((Ptr a, Int) -> IO String) -> IConv -> (Ptr b, Int) -> IO String
do_iconv get_string_fn ic (inbuf, inbuf_bytes) =
    alloca $ \inbuf_ptr ->
        alloca $ \inbytesleft_ptr ->
            alloca $ \outbuf_ptr ->
                alloca $ \outbytesleft_ptr ->
                    allocaBytes outbuf_size $ \outbuf -> do
      poke (inbytesleft_ptr :: Ptr CSize) (fromIntegral inbuf_bytes)
      poke inbuf_ptr inbuf
      let loop acc = do
          poke (outbytesleft_ptr :: Ptr CSize) (fromIntegral outbuf_size)
          poke outbuf_ptr outbuf
          ret <- throw_if_not_2_big "c_iconv" $
              c_iconv ic inbuf_ptr inbytesleft_ptr
                         outbuf_ptr outbytesleft_ptr
          left <- peek outbytesleft_ptr
          res <- get_string_fn (castPtr outbuf, outbuf_size - fromIntegral left)
          if ret == fromIntegral (-1 :: Int) then
              loop (acc++res)
            else
              return (acc++res)
      loop []


with_iconv :: String -> String -> (IConv -> IO a) -> IO a
with_iconv to from fn =
    bracket (iconv_open to from) iconv_close fn

iconv_ :: String -> IConv -> IO String
iconv_ str ic =
    withCStringLen str $ do_iconv peekCStringLen ic

-- between 8-bit encodings only
iconv :: Exception e => String -> String -> String -> Either e String
iconv to from str =
    unsafePerformIO $ try $ with_iconv to from (iconv_ str)


#ifdef HAVE_WCHAR_H
{-
type CUni = (#type wchar_t)
cuni_size = (#size wchar_t)
unicode_charset = "WCHAR_T"

chartocuni :: Char -> CUni
chartocuni = fromIntegral . ord

cunitochar :: CUni -> Char
cunitochar = chr . fromIntegral
-}

cuni_charset :: [Char]
cuni_charset = "WCHAR_T"

peek_cuni :: (Ptr (#type wchar_t), Int) -> IO String
peek_cuni (buf, bytes) = do
    let (chars, rembytes) = bytes `divMod` (#size wchar_t)
    if rembytes /= 0 then
        error "Conversion result contains remainder bytes."
      else
        peekCWStringLen (buf, chars)
        --liftM (map cunitochar) $ peekArray chars buf

with_cuni :: String -> ((Ptr (#type wchar_t), Int) -> IO String) -> IO String
with_cuni str f =
    withCWStringLen str $ \(s, l) -> f (s, l*(#size wchar_t))
    --withArray (map chartocuni str) $ \s -> f (s, l*cuni_size)

#else
-- no CF_WCHAR_SUPPORT

-- Due to endianness problems, it is easiest to do this through UTF-8

cuni_charset :: [Char]
cuni_charset = "UTF-8"

peek_cuni :: CStringLen -> IO String
peek_cuni = peekUTF8StringLen

with_cuni :: [Char] -> (CStringLen -> IO a) -> IO a
with_cuni = withUTF8StringLen

#endif

to_unicode_ :: String -> String -> IO String
to_unicode_ from str =
     with_iconv cuni_charset from $
      \ic -> withCStringLen str $ do_iconv peek_cuni ic

to_unicode :: Exception e => String -> String -> Either e String
to_unicode from str =
    unsafePerformIO $ try $ to_unicode_ from str

from_unicode_ :: String -> String -> IO String
from_unicode_ to str =
     with_iconv to cuni_charset $
      \ic -> with_cuni str $ do_iconv peekCStringLen ic

from_unicode :: Exception e => String -> String -> Either e String
from_unicode from str =
    unsafePerformIO $ try $ from_unicode_ from str


#ifndef ICONV_LIB_PREFIX

foreign import ccall unsafe "iconv.h iconv_open" c_iconv_open
    :: CString -> CString -> IO IConv

foreign import ccall unsafe "iconv.h iconv_close" c_iconv_close
    :: IConv -> IO CInt

foreign import ccall unsafe "iconv.h iconv" c_iconv
    :: IConv -> Ptr a -> Ptr CSize -> Ptr b -> Ptr CSize -> IO CSize

#else

foreign import ccall unsafe "iconv.h libiconv_open" c_iconv_open
    :: CString -> CString -> IO IConv

foreign import ccall unsafe "iconv.h libiconv_close" c_iconv_close
    :: IConv -> IO CInt

foreign import ccall unsafe "iconv.h libiconv" c_iconv
    :: IConv -> Ptr a -> Ptr CSize -> Ptr b -> Ptr CSize -> IO CSize

#endif
