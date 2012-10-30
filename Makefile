# Makefile for yaze

#	$Id: Makefile,v 1.5 2004/04/23 10:05:25 fdc Exp $	

VERSION		= 1.14

# CC must be an ANSI-C compiler
CC            =	gcc

#where you want the binaries and manual page
BINDIR	      = /usr/local/bin
MANDIR	      = /usr/local/man/man1
LIBDIR	      = /usr/local/lib

# full speed or debugging to taste
OPTIMIZE      = -O2
#OPTIMIZE       = -g

# -DUSE_GNU_READLINE for command recall/editing and filename completion
# -DBGii_BUG works around a problem in Backgrounder II
# -DBIOS to build a CP/M bios and monitor program into the emulator
#  (see also YAZE_OBJS, below)
# -DMMU compiles in support for bank-switched memory
# -DMEMSIZE <val> sets size of memory in KBytes (default 64)
# solaris2 needs -D__EXTENSIONS__
# linux needs -D_BSD_SOURCE
OPTIONS	      = -DBIOS -D_BSD_SOURCE

# Link with CP/M BIOS support,
YAZE_OBJS     = yaze.o simz80.o bios.o monitor.o
# or link as a naked Z80
#YAZE_OBJS    = yaze.o simz80.o io.o

# -lreadline -lcurses -liberty if you defined USE_GNU_READLINE
#  (you don't need -liberty on linux or BSD systems)
LIBS	      =

# a bsd-like install program (/usr/ucb/install on Solaris2)
INSTALL	      = install


###### you should not need to change anything below this line ######
CWARN	      = -ansi -pedantic -Wall -Wshadow \
		-Wpointer-arith -Wnested-externs -Winline
CFLAGS        =	$(CWARN) $(OPTIMIZE) $(OPTIONS) -DLIBDIR=\"$(LIBDIR)/\"

SRCS	      = yaze.c simz80.c io.c bios.c monitor.c cdm.c
DOC	      = README README-1.10 COPYING yaze.doc yaze.1 cdm.1 ChangeLog
TEST_SRC      =	test/prelim.z80 test/zexall.z80 \
		test/zexdoc.z80 test/savage.pas test/sys.azm
TEST_BIN      = test/prelim.com test/zexall.com test/zexdoc.com \
		test/savage.com test/sys.com test/timex.com

DISTRIB       =	Makefile simz80.h yaze.h bios.h simz80.pl .yazerc yaze.boot \
		host2cpm.tcl test/zexlax.pl \
		$(SRCS) $(DOC) $(TEST_SRC) $(TEST_BIN)

all:		yaze cdm

yaze:		$(YAZE_OBJS)
		$(CC) $(CFLAGS) $(YAZE_OBJS) $(LIBS) -o $@

simz80.c:	simz80.pl
		rm -f simz80.c
		perl -w simz80.pl >simz80.c
		chmod a-w simz80.c

cdm:		cdm.o
		$(CC) $(CFLAGS) cdm.o $(LIBS) -o $@

install:	all
		$(INSTALL) -s -c -m 755 yaze $(BINDIR)
		$(INSTALL) -s -c -m 755 cdm $(BINDIR)
		$(INSTALL) -c -m 644 yaze.boot $(LIBDIR)
		$(INSTALL) -c -m 644 yaze.1 $(MANDIR)
		$(INSTALL) -c -m 644 cdm.1 $(MANDIR)

tar:		$(DISTRIB)
		(rm -rf yaze-$(VERSION); \
		mkdir yaze-$(VERSION); \
		cd yaze-$(VERSION); \
		for f in Makefile simz80.h yaze.h bios.h simz80.pl \
			    .yazerc yaze.boot host2cpm.tcl $(SRCS) $(DOC); \
			do ln ../$$f .; done; \
		mkdir test; \
		for f in $(TEST_SRC); \
			do if test -z `tr -dc '\r' <../$$f`; \
				then sed 's/$$//' <../$$f >$$f; \
				else ln ../$$f $$f; fi; \
		done; \
		for f in test/zexlax.pl $(TEST_BIN); \
			do ln ../$$f $$f; done; \
		cd ..; \
		tar cf yaze-$(VERSION).tar yaze-$(VERSION); \
		gzip -9 yaze-$(VERSION).tar; \
		rm -rf yaze-$(VERSION))

clean:;		rm -f *.o *~ core

yaze.o:		yaze.c simz80.h yaze.h
bios.o:		bios.c simz80.h yaze.h
simz80.o:	simz80.c simz80.h
