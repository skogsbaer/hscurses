
#include "HSCursesUtils.h"

/*
 * A non-macro version of getyx(3), to make writing a Haskell binding
 * easier.  Called in UI/HSCurses/Curses.hsc
 */
void hscurses_nomacro_getyx(WINDOW *win, int *y, int *x)
{
    getyx(win, *y, *x);
}

