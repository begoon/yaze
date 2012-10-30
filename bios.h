/* Header file for the basic i/o system.
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

/* 	$Id: bios.h,v 1.2 2004/01/11 16:11:17 fdc Exp $	 */

/* TTY management */
extern struct termios cookedtio, rawtio;
#define ISATTY	1
#define ISRAW	2
extern int ttyflags;
extern int interrupt;
extern void ttyraw(void), ttycook(void);


/* Table of logical streams */
extern int chn[5];

#define CHNconin	0
#define CHNconout	1
#define CHNrdr		2
#define CHNpun		3
#define CHNlst		4

/* Table of physical streams */
#define	SIOno	-1
#define TTYin	0
#define TTYout	1
#define CRTin	2
#define CRTout	3
#define UC1in	4
#define UC1out	5
#define RDRin	6
#define UR1in	7
#define UR2in	8
#define PUNout	9
#define UP1out	10
#define UP2out	11
#define LPTout	12
#define UL1out	13
#define	MAXPSTR	14

#define ST_IN	0
#define ST_OUT	1
#define ST_IN2	2
#define ST_OUT2	3

extern struct sio {
    FILE *fp;
    char *filename;
    char *streamname;
    char tty;
    char canselect;	/* gr */
    const char strtype;
} siotab[];


/* Disk management */

/* There are two kinds of simulated disk:
   a unix file containing an image of a cp/m disk and
   a unix directory which is made to look like a cp/m disk on the fly */
#define MNT_ACTIVE	1
#define MNT_RDONLY	2
#define MNT_UNIXDIR	4

extern struct mnt {		/* mount table (simulated drives A..P) */
    WORD flags;
    WORD dph;			/* address of disk parameter header in
				   cp/m ram */
    BYTE *data;			/* disk image */
    char *filename;		/* filename of disk image or unix directory */
    union {
	struct {		/* details of disk image */
	    char *header;	/* header if it's a disk image */
	    size_t isize;	/* size of disk image */
	    int ifd;		/* file descriptor of disk image */
	} image;
	struct {		/* details of unix directory */
	    int nde;		/* number of entries in cp/m directory */
	    int nfds;		/* number of files */
	    struct fdesc {	/* descriptor for each file */
		char *fullname;
		unsigned long firstblock;
		unsigned long lastblock;
		unsigned long serial;	/* unique id to aid caching */
	    } *fds;
	} udir;
    } m;
} mnttab[], *curdisk;
#define	header	m.image.header
#define	isize	m.image.isize
#define	ifd	m.image.ifd
#define	nde	m.udir.nde
#define	nfds	m.udir.nfds
#define	fds	m.udir.fds

/* We always use a block size of 4k for simulated disks constructed from unix
   directories.  The maximum possible number of cp/m directory entries on such a
   disk is 2032. */
#define BLOCK_SIZE	4096
#define N_ENTRIES	2032
#define COVER(x,y)	(((x)-1)/(y)+1)

void clearfc(struct mnt *dp);
int isafifo(int desc); /* gr */

#ifdef DEBUG
void sighand(int sig);
#endif
