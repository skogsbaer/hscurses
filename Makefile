TOPDIR = .

# way = p
# HCFLAGS = -D__DEBUG__

include $(TOPDIR)/mk/config.mk

# this rule must remain first
default: boot lib

ALL_DIRS = cbits HSCurses

PKG      = hscurses
LIBRARY  = libhscurses$(_way).a
LIBOBJS  = $(OBJS)

INSTALL_HEADERS = $(wildcard cbits/*.h)

include $(TOPDIR)/mk/rules.mk

.PHONY: srcdist
srcdist:
	@DIST=hscurses-ds-`date +%Y%m%d`; \
	CHANGELOG=CHANGELOG.$$DIST; \
	if [ -e $$DIST ] ; then echo "$$DIST already exists"; exit 1; fi; \
	darcs dist -d $$DIST > /dev/null && \
	gunzip $$DIST.tar.gz && \
	tar --delete --file=$$DIST.tar $$DIST/boring && \
	mkdir $$DIST && \
	darcs changes | sed 's/[[:alnum:].]*@[[:alnum:].]*/(Email address hidden)/g' > $$DIST/$$CHANGELOG && \
	tar --append --file=$$DIST.tar $$DIST/$$CHANGELOG && \
	mv $$DIST/$$CHANGELOG $$CHANGELOG && \
	rm -rf $$DIST && \
	gzip $$DIST.tar && \
	md5sum $$DIST.tar.gz > $$DIST.md5 && \
	echo "created dist as $$DIST.tar.gz"

CLEAN_FILES+=$(wildcard hscurses-ds-*.tar.gz hscurses-ds-*.md5 CHANGELOG.hscurses-ds-*)
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
