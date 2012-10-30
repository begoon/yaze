/* Basic i/o system module.
   Copyright (C) 1995  Frank D. Cringle.

This file is part of yaze - yet another Z80 emulator.

Yaze is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#include <fcntl.h>
#include <termios.h>
#include <ctype.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/mman.h>

#include "simz80.h"
#include "yaze.h"
#include "bios.h"

/* 	$Id: bios.c,v 1.4 2004/04/23 09:50:15 fdc Exp $	 */

#ifndef lint
static char vcid[] = "$Id: bios.c,v 1.4 2004/04/23 09:50:15 fdc Exp $";
#endif /* lint */


/* Z80 registers */
#define AF	af[af_sel]
#define BC	regs[regs_sel].bc
#define DE	regs[regs_sel].de
#define HL	regs[regs_sel].hl

/* location on current disk */
static WORD	track;
static WORD	sector;
static WORD	dma;

typedef unsigned long COUNT;
static COUNT	bioscount;
static BYTE	old_iobyte;

static void
new_iobyte(int io)
{
    switch ((io >> 6) & 3) {
    case 0:	chn[CHNlst] = TTYout;
	break;
    case 1:	chn[CHNlst] = CRTout;
	break;
    case 2:	chn[CHNlst] = LPTout;
	break;
    case 3:	chn[CHNlst] = UL1out;
	break;
    }
    switch ((io >> 4) & 3) {
    case 0: chn[CHNpun] = TTYout;
	break;
    case 1: chn[CHNpun] = PUNout;
	break;
    case 2: chn[CHNpun] = UP1out;
	break;
    case 3: chn[CHNpun] = UP2out;
	break;
    }
    switch ((io >> 2) & 3) {
    case 0:	chn[CHNrdr] = TTYin;
	break;
    case 1:	chn[CHNrdr] = RDRin;
	break;
    case 2:	chn[CHNrdr] = UR1in;
	break;
    case 3:	chn[CHNrdr] = UR2in;
	break;
    }
    switch (io & 3) {
    case 0: chn[CHNconin] = TTYin;
	chn[CHNconout] = TTYout;
	break;
    case 1: chn[CHNconin] = CRTin;
	chn[CHNconout] = CRTout;
	break;
    case 2: chn[CHNconin] = chn[CHNrdr];
	chn[CHNconout] = chn[CHNlst];
	break;
    case 3: chn[CHNconin] = UC1in;
	chn[CHNconout] = UC1out;
	break;
    }
    old_iobyte = io;
}

static struct sio *
getsiop(int chan)
{
    if (ram[3] != old_iobyte)
	new_iobyte(ram[3]);
    return &siotab[chn[chan]];
}

/* gr: */
int isafifo(int desc)
{
    struct stat buf;
    
    return fstat(desc, &buf) == 0 && S_ISFIFO(buf.st_mode);
}

int
bios_init(const char *initfile)
{
    int go = 0;
    FILE *su;
    const char *fn = initfile;
    char *name;
    char buf[BUFSIZ];

#ifdef DEBUG
    signal(SIGINT, sighand);
#endif
    if (z3env && z3env > bios_top && z3env < dptable) {
	/* reserve space for a z-system environment page */
	WORD p = z3env-bios_top;
	int i = 1024;
	while (i--) {
	    global_alv[p>>3] |= (0x80 >> (p&7));
	    p++;
	}
    }
    siotab[CRTin].fp = stdin;
    name = ttyname(fileno(stdin));
    siotab[CRTin].filename = name ? newstr(name) : "(stdin)";
    siotab[CRTin].tty = isatty(fileno(stdin));
    /* gr: */
    siotab[CRTin].canselect = siotab[CRTin].tty || isafifo(fileno(stdin));
    siotab[CRTout].fp = stdout;
    name = ttyname(fileno(stdout));
    siotab[CRTout].filename = name ? newstr(name) : "(stdout)";
    siotab[CRTout].tty = isatty(fileno(stdout));
    /* gr: */
    siotab[CRTout].canselect = siotab[CRTout].tty || isafifo(fileno(stdout));
    if (siotab[CRTin].tty) {
	ttyflags = ISATTY;
	if (tcgetattr(fileno(stdin), &cookedtio) != 0) {
	    perror("tcgetattr");
	    exit(1);
	}
	rawtio = cookedtio;
	rawtio.c_iflag = 0;
	rawtio.c_oflag = 0;
	rawtio.c_lflag = interrupt ? ISIG : 0;
	memclr(rawtio.c_cc, NCCS);
	rawtio.c_cc[VINTR] = interrupt;
	rawtio.c_cc[VMIN] = 1;
    }
    if (*initfile != '/' && access(initfile, R_OK) != 0) {
	char *h = getenv("HOME");
	if (h) {
	    fn = buf;
	    sprintf((char *) fn, "%s/%s", h, initfile);
	}
    }
    if ((su = fopen(fn, "r")) == NULL)
	return 0;
    while (!go && fgets(buf, BUFSIZ-1, su))
	go = docmd(buf);
    fclose(su);
    atexit(ttycook);
    ttyraw();
    return go;
}

/* The constat() routine is complicated by the need to deal with CP/M programs
   that poll console status without doing any other bios calls.
   Examples are Wordstar and Backgrounder II.  In Wordstar the purpose is to
   act as a crude timer that can be interrupted by the user pressing a key
   (delay before a menu is shown).  In Backgrounder it's an artifact of the
   pseudo multitasking logic.

   A simple-minded constat() implementation leads to such CP/M programs soaking
   up 100% of the host CPU and getting confused about the system speed.

   The 2 constants CSTTIME and CSTCOUNT control the emulation.
   constat() normally returns the current console status immediately.
   However, after it has been called CSTCOUNT times without any other bios
   activity, it waits CSTTIME microseconds (or until a character is available)
   before returning.
*/

#define CSTTIME		100000
#define CSTCOUNT	128

static const struct timeval immediate = { 0, 0 };
static const struct timeval delay = { 0, CSTTIME };

static int
constat(void)
{
    static int consecutive;		/* number of consecutive calls */
    static COUNT lastcount;		/* previous call */
    struct timeval t = immediate;
    fd_set rdy;
    struct sio *s = getsiop(CHNconin);
    int fd;

    if (s->fp == NULL)		/* no file */
	return 0;
    if (s->canselect == 0)	/* disk files are not always ready! (gr) */
	return 1;
    if (bioscount != lastcount+1)
	consecutive = 0;
    else if (++consecutive == CSTCOUNT) {
	consecutive = 0;
	t = delay;
    }
    lastcount = bioscount;

    fd = fileno(s->fp);
    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    (void) select(fd+1, &rdy, NULL, NULL, &t);
    return FD_ISSET(fd, &rdy);
}

static int
lststat(void)
{
    static int consecutive;		/* number of consecutive calls */
    static COUNT lastcount;		/* previous call */
    struct timeval t = immediate;
    fd_set rdy;
    struct sio *s = getsiop(CHNlst);
    int fd;

    if (s->fp == NULL)			/* no file */
	return 0;
    if (s->tty == 0)			/* disk files are always ready */
	return 1;
    if (bioscount != lastcount+1)
	consecutive = 0;
    else if (++consecutive == CSTCOUNT) {
	consecutive = 0;
	t = delay;
    }
    lastcount = bioscount;

    fd = fileno(s->fp);
    FD_ZERO(&rdy);
    FD_SET(fd, &rdy);
    (void) select(fd+1, &rdy, NULL, NULL, &t);
    return FD_ISSET(fd, &rdy);
}

static int
serin(int chan)
{
    char c;
    int ch;
    struct sio *s = getsiop(chan);

    if (s->fp == NULL)
	return 0x1a;
    if (s->tty) {
	if (read(fileno(s->fp), &c, 1) == 0)
	    return 0x1a;
	else
	    return c;
    }
    if ((ch = getc(s->fp)) == EOF)
	return 0x1a;
    else
	return ch & 0xff;
}

static void
serout(int chan, char c)
{
    struct sio *s = getsiop(chan);

    if (s->fp == NULL)
	return;
    if (s->tty)
	(void) write(fileno(s->fp), &c, 1);
    else
	fwrite(&c, 1, 1, s->fp);
}

static WORD
seldsk(int disk)
{
    if (disk < 0 || disk > 15 || !(mnttab[disk].flags & MNT_ACTIVE))
	return 0;
    curdisk = mnttab + disk;
    return curdisk->dph;
}

#ifdef BGii_BUG
/* I cannot persuade Backgrounder ii to output the H at the end of a
   VT-100/xterm cursor-positioning sequence.  This kludges it. */
void
BGiiFix(int c)
{
    static const char cc[] = "\033[0;0";
    static const char *p = cc;

    if (c == 0)
	return;
    if ('0' <= c && c <= '9')
	return;
    if (*p == '0')
	p++;
    if (*p == c) {
	p++;
	return;
    }
    if (*p) {
	p = cc;
	return;
    }
    p = cc;
    if (c == 'H')
	return;
    serout(CHNconout, 'H');
}
#else
#define BGiiFix(x)
#endif

#define FCACHE_SIZE	4
static struct fc {
    FILE *f;
    struct mnt *disk;
    struct fdesc *fd;
} fcache[FCACHE_SIZE];

/* clear the file cache when disk is unmounted */
void
clearfc(struct mnt *dp)
{
    int i;

    for (i = 0; i < FCACHE_SIZE; i++) {
	struct fc *f = fcache+i;
	if (f->f && f->disk == dp) {
	    fclose(f->f);
	    f->f = NULL;
	}
    }
}

static int
readsec(void)
{
    unsigned long offset, blockno;

    if (curdisk == NULL || !(curdisk->flags & MNT_ACTIVE))
	return 1;
    offset = (track*GetWORD(curdisk->dph+16) + sector)*128;
    blockno = offset / BLOCK_SIZE;
    if ((curdisk->flags & MNT_UNIXDIR) == 0) {
	/* the 'disk image' case is easy */
	memcpy(ram + dma, curdisk->data + offset, 128);
	return 0;
    }

    /* handle a 'unix directory' disc */
    if (blockno < 16) {
	/* directory access */
	if (offset/32 < curdisk->nde)
	    memcpy(ram+dma, curdisk->data+offset, 128);
	else
	    memset(ram+dma, 0xe5, 128);
    }
    else {			/* file access */
	int i;
	for (i = 0; i < FCACHE_SIZE; i++) {
	    struct fc *f = fcache + i;
	    if ((f->f == NULL) ||
		(f->disk == curdisk && blockno >= f->fd->firstblock &&
		 blockno <= f->fd->lastblock))
		break;
	}
	if (i == FCACHE_SIZE) {
	    /* cache overflow */
	    fclose(fcache[FCACHE_SIZE-1].f);
	    memmove(fcache+1, fcache, (FCACHE_SIZE - 1) * sizeof(struct fc));
	    i = 0;
	    fcache[0].f = NULL;
	}
	if (fcache[i].f == NULL) {
	    struct fc *f = fcache + i;
	    struct fdesc *fd = curdisk->fds;
	    int j;
	    f->disk = curdisk;
	    for (j = 0; j < curdisk->nfds; j++, fd++)
		if (blockno >= fd->firstblock && blockno <= fd->lastblock)
		    break;
	    if (j >= curdisk->nfds) {
		/* cant happen */
		return 1;
	    }
	    if ((f->f = fopen(fd->fullname, "rb")) == NULL) {
		perror(fd->fullname);
		return 1;
	    }
	    f->fd = fd;
	}
	if (i != 0) {
	    /* sort least-recently-used to front */
	    struct fc temp;
	    temp = fcache[i];
	    memmove(fcache+1, fcache, i * sizeof(struct fc));
	    fcache[0] = temp;
	}
	if (fseek(fcache[0].f, offset-fcache[0].fd->firstblock*BLOCK_SIZE,
		  SEEK_SET) < 0)
	    return 1;
	if ((i = fread(ram+dma, 1, 128, fcache[0].f)) < 128)
	    memset(ram+dma+i, 0x1a, 128-i); /* EOF */
    }
    return 0;
}


static int
writesec(int stype)
{
    if (curdisk == NULL ||
	((curdisk->flags & (MNT_ACTIVE|MNT_RDONLY)) != MNT_ACTIVE))
	return 1;
    memcpy(curdisk->data +
	   (track*GetWORD(curdisk->dph+16) + sector)*128, ram + dma, 128);
    return 0;
}

/* This only works for single-byte sector numbers
   (but then, so does the CP/M 2.2 CBIOS code).
*/
static WORD
sectrans(WORD sec, WORD table)
{
    if (table == 0)
	return sec;
    return GetBYTE(table + sec);
}

static BYTE savcpm[CPM_LENGTH];		/* saved copy of cpm */

void
bios(int func)
{
    bioscount++;
    switch (func) {
    case 0:			/* cold boot */
	memcpy(savcpm, ram + ccp_base, CPM_LENGTH);
	goto gocpm;
    case 1:			/* warm boot */
	memcpy(ram + ccp_base, savcpm, CPM_LENGTH);
    gocpm:
	dma = 0x80;
	ram[0] = 0xc3;
	ram[1] = 0x03;
	ram[2] = hreg(bios_base);
	ram[5] = 0xc3;
	ram[6] = 0x06;
	ram[7] = hreg(bdos_base);
	Setlreg(BC, ram[4]);
	pc = ccp_base;
	return;
    case 2:			/* console status */
	Sethreg(AF, constat() ? 0xff : 0x00);
	break;
    case 3:			/* console input */
	Sethreg(AF, serin(CHNconin) & 0x7f);
	break;
    case 4:			/* console output */
	BGiiFix(lreg(BC));
	serout(CHNconout, lreg(BC));
	break;
    case 5:			/* list output */
	serout(CHNlst, lreg(BC));
	break;
    case 6:			/* punch output */
	serout(CHNpun, lreg(BC));
	break;
    case 7:			/* tape reader input */
	Sethreg(AF, serin(CHNrdr));
	break;
    case 8:			/* home disk */
	track = 0;
	break;
    case 9:			/* select disk */
	HL = seldsk(lreg(BC));
	break;
    case 10:			/* set track */
	track = BC;
	break;
    case 11:			/* set sector */
	sector = BC;
	break;
    case 12:			/* set dma */
	dma = BC;
	break;
    case 13:			/* read sector */
	Sethreg(AF, readsec());
	break;
    case 14:			/* write sector */
	Sethreg(AF, writesec(lreg(BC)));
	break;
    case 15:			/* list status */
	Sethreg(AF, lststat() ? 0xff : 0x00);
	break;
    case 16:			/* translate sector */
	HL = sectrans(BC, DE);
	break;
    case 253:			/* return from recursive call */
	return;
    case 254:
	++pc;			/* meta-level command */
	if (GetBYTE(pc) == 0)
	    monitor(0);
	else {
	    /* need a copy because docmd() scratches its argument */
	    char *sav = newstr((char *) ram+pc);
	    ttycook();
	    (void) docmd(sav);
	    ttyraw();
	    free(sav);
	    pc += strlen((char *) ram+pc);
	}
	break;
    case 255:			/* quit */
	exit(0);
	break;
    default:
	fprintf(stderr, "Invalid bios function: %d\r\n", func);
    }
    pc++;
}
