TOPDIR = .

# way = p
HCFLAGS = -D__DEBUG__

include $(TOPDIR)/mk/config.mk

# this rule must remain first
default: boot lib

ALL_DIRS = cbits HSCurses

PKG      = hscurses
LIBRARY  = libhscurses$(_way).a
LIBOBJS  = $(OBJS)

INSTALL_HEADERS = $(wildcard cbits/*.h)

include $(TOPDIR)/mk/rules.mk

# Dependency orders
ifeq ($(MAKECMDGOALS),clean)
#empty
else
ifeq ($(MAKECMDGOALS),distclean)
#empty
else
include $(TOPDIR)/depend
endif
endif
