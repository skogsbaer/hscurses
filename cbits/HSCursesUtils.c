
#include "HSCursesUtils.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in UI/HSCurses/Curses.hsc
 */
void hscurses_nomacro_getyx(WINDOW *win, int *y, int *x)
{
    getyx(win, *y, *x);
}

chtype hs_curses_color_pair(HsInt pair)
{
    return COLOR_PAIR (pair);
}

HsInt hs_get_mb_cur_max()
{
    return MB_CUR_MAX;
}

#if defined(HAVE_LIBPDCURSES) || defined (HAVE_LIBPDCURSESW)
int hscurses_nomacro_getch(void)
{
    return getch();
}
#endif
