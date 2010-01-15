#ifndef HSCURSES_H
#define HSCURSES_H

#ifndef GHC_CONFIG_INCLUDED
#define GHC_CONFIG_INCLUDED
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include "ghcconfig.h"
#endif

#ifndef CONFIG_INCLUDED
#define CONFIG_INCLUDED
#include "config.h"
#endif

#if HAVE_NCURSESW_NCURSES_H
#include <ncursesw/ncurses.h>
#elif HAVE_NCURSES_H
#include <ncurses.h>
#else
#include <curses.h>
#endif

#if defined(initscr)
#undef initscr
#endif

#if defined(cbreak)
#undef cbreak
#endif

#if defined(clrtoeol)
#undef clrtoeol
#endif

#if defined(touchwin)
#undef touchwin
#endif

#if defined(beep)
#undef beep
#endif

#if defined(flash)
#undef flash
#endif

#if defined(wattr_set)
#undef wattr_set
#endif

#if defined(wattr_get)
#undef wattr_get
#endif

#endif  // HSCURSES_H
