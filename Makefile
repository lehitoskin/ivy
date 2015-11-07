UNAME = $(shell uname)

ifeq ($(UNAME), Darwin)
PLATFORM = osx
else
PLATFORM = posix
endif

all:
	make -f Makefile.$(PLATFORM) all

%:
	make -f Makefile.$(PLATFORM) $@

