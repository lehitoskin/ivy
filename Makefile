UNAME = $(shell uname)

ifeq ($(UNAME), Darwin)
PLATFORM = osx
else
PLATFORM = posix
endif

all:
	$(MAKE) -f Makefile.$(PLATFORM) all

%:
	$(MAKE) -f Makefile.$(PLATFORM) $@

