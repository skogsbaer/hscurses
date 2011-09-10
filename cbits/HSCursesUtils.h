#ifndef HSCURSESUTILS_H
#define HSCURSESUTILS_H

#include "HSCurses.h"
#include "HsFFI.h"
#include <stdlib.h>

extern void hscurses_nomacro_getyx(WINDOW *win, int *y, int *x );

extern chtype hs_curses_color_pair(HsInt pair );

extern HsInt hs_get_mb_cur_max();

#if defined(HAVE_LIBPDCURSES) || defined (HAVE_LIBPDCURSESW)
extern int hscurses_nomacro_getch(void);
#endif

#endif  // HSCURSESUTILS_H
