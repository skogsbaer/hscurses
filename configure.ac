# use autoreconf to generate configure script and cbits/config.h

# sanity
AC_INIT(UI/HSCurses/Curses.hsc)

# Prepare to generate the config.h.in file
AC_CONFIG_HEADERS([cbits/config.h])

# this is arbitrary
AC_PREREQ([2.52])

AC_ARG_WITH(compiler,
            [AC_HELP_STRING([--with-hc=ARG],
                            [ignored])],
            true, true)
#
# Curses. If you don't have wchar_t-aware ncurses (Debian package:
# libncursesw5-dev),  uncomment the first line and comment-out the
# two later.
#
# The shell variables should be added to, e.g., $LIBS
#
AC_CHECK_LIB(curses,    addnstr)
AC_CHECK_LIB(ncurses,   addnstr)
AC_CHECK_LIB(ncursesw,  waddnwstr)
AC_CHECK_LIB(pdcurses,  addnstr)
AC_CHECK_LIB(pdcursesw, waddnwstr)

AC_CHECK_LIB(iconv, iconv)
AC_CHECK_LIB(iconv, libiconv)

# Bit weird. On OpenBSD you need the 'lib' suffix to iconv functions, it seems
AC_CHECK_FUNCS(libiconv, AC_DEFINE( [ICONV_LIB_PREFIX],[1],
                [Define to 1 if you need a 'lib' prefix to iconv functions]))

# On linux we need to call doupdate() after endwin() to restore the term
if uname -s | grep Linux > /dev/null 2>&1 ; then
    AC_DEFINE( [NCURSES_UPDATE_AFTER_END],[1],
               [Define to 1 if you need to call doupdate after endwin])
fi

# some special libs need to be set for package.conf files
if echo "$LIBS" | grep iconv > /dev/null 2>&1 ; then
    ICONV=iconv
fi

if (echo "$LIBS" | grep ncursesw) >/dev/null 2>&1 ; then
    CURSES=ncursesw
elif (echo "$LIBS" | grep ncurses) >/dev/null 2>&1 ; then
    CURSES=ncurses
elif (echo "$LIBS" | grep pdcursesw) >/dev/null 2>&1 ; then
    CURSES=pdcursesw
elif (echo "$LIBS" | grep pdcurses) >/dev/null 2>&1 ; then
    CURSES=pdcurses
else
    CURSES=curses
fi
AC_SUBST(ICONV)
AC_SUBST(CURSES)

#
# Specific headers to check for
#
AC_CHECK_HEADERS([iconv.h curses.h ncurses.h ncursesw/ncurses.h locale.h langinfo.h wchar.h limits.h signal.h ])

AC_SUBST(SYMS)

current_directory=`pwd`
AC_SUBST(current_directory)

AC_CONFIG_FILES([hscurses.buildinfo])
AC_OUTPUT
