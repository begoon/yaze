/* System monitor.
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

/* 	$Id: monitor.c,v 1.3 2004/01/11 16:49:58 fdc Exp $	 */

#ifndef lint
static char vcid[] = "$Id: monitor.c,v 1.3 2004/01/11 16:49:58 fdc Exp $";
#endif /* lint */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#ifndef CLK_TCK
#define CLK_TCK CLOCKS_PER_SEC
#endif
#include <limits.h>
#include <fcntl.h>
#include <termios.h>
#include <ctype.h>
#include <signal.h>
#include <sys/times.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/mman.h>

#include "simz80.h"
#include "yaze.h"
#include "bios.h"

/* TTY management */

struct termios cookedtio, rawtio;
int ttyflags;
int interrupt;

void
ttyraw(void)
{
    if ((ttyflags & (ISATTY|ISRAW)) == ISATTY) {
	tcsetattr(fileno(stdin), TCSAFLUSH, &rawtio);
	ttyflags |= ISRAW;
    }
}

void
ttycook(void)
{
    if (ttyflags & ISRAW) {
	tcsetattr(fileno(stdin), TCSAFLUSH, &cookedtio);
	putc('\n', stdout);
	ttyflags &= ~ISRAW;
    }
}


/* memory management routines for disk descriptors
   (we need to allocate chunks of cp/m ram for these) */

/* inefficient but robust bit-wise algorithm (we're not doing this all day) */
#define bytefree(x)	(!(global_alv[(x-bios_top) >> 3] &		\
			   (0x80 >> ((x-bios_top) & 7))))

static WORD
cpmalloc(WORD len)
{
    WORD p = bios_top;

    while (p < dptable - len)
	if (!bytefree(p))
	    p++;
	else {
	    int i;
	    for (i = 1; i < len; i++)
		if (!bytefree(p + i))
		    break;
	    if (i == len) {
		WORD p1 = p - bios_top;
		while (len--) {
		    global_alv[p1>>3] |= (0x80 >> (p1&7));
		    p1++;
		}
		return p;
	    }
	    p += i;
	}
    return 0;
}


static void
cpmfree(WORD adr, WORD len)
{
    WORD p = adr - bios_top;

    while (len--) {
	global_alv[p>>3] &= ~(0x80 >> (p&7));
	p++;
    }
}


/* Disk management */

/* There are two kinds of simulated disk:
   a unix file containing an image of a cp/m disk and
   a unix directory which is made to look like a cp/m disk on the fly */

#define MNT_ACTIVE	1
#define MNT_RDONLY	2
#define MNT_UNIXDIR	4

struct mnt mnttab[16], *curdisk; 	/* mount table (simulated drives A..P) */

/* display a mount table entry */
static void
showdisk(int disk, int verbose)
{
    struct mnt *dp = mnttab + disk;

    printf("%c: ", disk+'A');
    if (!(dp->flags & MNT_ACTIVE)) {
	puts("not mounted");
	return;
    }
    printf(dp->flags & MNT_UNIXDIR ? "%s %s/\n": "%s %s\n",
	   dp->flags & MNT_RDONLY ? "r/o " : "r/w ", dp->filename);
    if (!verbose)
	return;
    printf("  dph=%04X, xlt=%04X, dirbuf=%04X, dpb=%04X, csv=%04X, alv=%04X, spt=%04X\n",
	   dp->dph, GetWORD(dp->dph), GetWORD(dp->dph+8), GetWORD(dp->dph+10),
	   GetWORD(dp->dph+12), GetWORD(dp->dph+14), GetWORD(dp->dph+16));
    printf("  bsh=%02X, blm=%02X, exm=%02X, dsm=%04X, drm=%04X, al=%04X, cks=%04X, off=%04X\n",
	   GetBYTE(dp->dph+18), GetBYTE(dp->dph+19), GetBYTE(dp->dph+20), GetWORD(dp->dph+21),
	   GetWORD(dp->dph+23), GetWORD(dp->dph+25), GetWORD(dp->dph+27), GetWORD(dp->dph+29));
}

/* unmount a disk */
static int
umount(int disk)
{
    struct mnt *dp = mnttab + disk;
    WORD xlt;

    if (!(dp->flags & MNT_ACTIVE))
	return 0;
    if (dp->flags & MNT_UNIXDIR) {
	int i;
	clearfc(dp);			/* clear the bios's file cache */
	for (i = 0; i < dp->nfds; i++)
	    free(dp->fds[i].fullname);
	free(dp->data);
	free(dp->fds);
    }
    else if (munmap(dp->header, dp->isize) == -1 ||
	     close(dp->ifd) == -1)
	perror(dp->filename);
    dp->flags = 0;
    free(dp->filename);
    if ((xlt = GetWORD(dp->dph)) != 0)
	cpmfree(xlt, GetWORD(dp->dph+16));
    cpmfree(GetWORD(dp->dph+14), (GetWORD(dp->dph+16+5) >> 3) + 1);
    return 0;
}

/* stash a string away on the heap */
char *
newstr(const char *str)
{
	char *p = xmalloc(strlen(str) + 1);
	(void) strcpy(p, str);
	return p;
}

/* Decide if a unix file is eligible to be included as a cp/m file in a
   simulated disk constructed from a unix directory.  Return the filesize in
   bytes if so, 0 if not. */
static off_t
cpmsize(const char *dirname, const char *filename, unsigned char *cpmname, char **unixname)
{
    int i, fd;
    const char *p = filename;
    unsigned char *p1 = cpmname;
    struct stat st;
    char *path;

    /* construct cpm filename, rejecting any that dont fit */
    for (i = 0; i < 8; i++) {
	if (*p == 0 || *p == '.')
	    break;
	if (*p <= ' ' || *p >= '{' || strchr("=_:,;<>", *p))
	    return 0;
	*p1++ = toupper(*p);
	p++;
    }
    for (; i < 8; i++)
	*p1++ = ' ';
    if (*p) {
	if (*p == '.')
	    p++;
	else
	    return 0;
    }
    for (; i < 11; i++) {
	if (*p == 0)
	    break;
	if (*p <= ' ' || *p >= '{' || strchr(".=_:,;<>", *p))
	    return 0;
	*p1++ = toupper(*p);
	p++;
    }
    if (*p)
	return 0;
    for (; i < 11; i++)
	*p1++ = ' ';

    /* construct unix filename */
    path = xmalloc(strlen(dirname) + strlen(filename) + 2);
    sprintf(path, "%s/%s", dirname, filename);

    /* check that file is readable, regular and non-empty */
    if ((fd = open(path, O_RDONLY)) < 0) {
	free(path);
	return 0;
    }
    if (fstat(fd, &st) < 0) {
	close(fd);
	free(path);
	return 0;
    }
    close (fd);
    if (((st.st_mode & S_IFMT) != S_IFREG) || st.st_size == 0) {
	free(path);
	return 0;
    }

    *unixname = path;
    return st.st_size;
}

/* mount a unix directory as a simulated cpm disk */
static int
mountdir(struct mnt *dp, const char *filename)
{
    DIR *dirp;
    struct dirent *direntp;
    unsigned char *cpmdir = xmalloc(N_ENTRIES*32);
    unsigned long blockno = COVER((N_ENTRIES*32), BLOCK_SIZE);
    int direno = 0;
    int nfiles = 0;
    WORD alv;
    static unsigned long serialno = 0;

    dp->fds = xmalloc(N_ENTRIES*sizeof(struct fdesc));
    if ((dirp = opendir(filename)) == NULL) {
	perror(filename);
	free(cpmdir);
	free(dp->fds);
	return 0;
    }
    while ((direntp = readdir(dirp)) != NULL) {
	char *fullname;
	off_t size;		/* file size in bytes */
	int ndirents;		/* number of directory entries required for file */
	int nlogexts;		/* number of full logical extents occupied by */
				/* file */
	int i;
	unsigned long nblocks;	/* number of blocks occupied by file */
	if ((size = cpmsize(filename, direntp->d_name, cpmdir+32*direno+1,
			    &fullname)) == 0)
	    continue;
	for (i = 0; i < direno; i++)
	    if (memcmp(cpmdir+32*i+1, cpmdir+32*direno+1, 11) == 0) {
		free(fullname);		/* discard case-collapsed duplicates */
		continue;
	    }
	ndirents = COVER(size, 8*BLOCK_SIZE);
	nlogexts = size/(16*1024);
	nblocks = COVER(size, BLOCK_SIZE);
	if ((direno + ndirents > N_ENTRIES) ||
	    ((blockno + nblocks) * BLOCK_SIZE > 0xffff * 128)) {
	    fprintf(stderr, "not all files in %s will fit on disk\n", filename);
	    free(fullname);
	    break;
	}
	dp->fds[nfiles].fullname = fullname;
	dp->fds[nfiles].firstblock = blockno;
	dp->fds[nfiles].lastblock = blockno + nblocks - 1;
	dp->fds[nfiles++].serial = serialno++;
	for (i = 0; i < ndirents; i++) {
	    unsigned char *cp = cpmdir + (direno+i)*32;
	    int ex = nlogexts < 2*i+1 ? nlogexts : 2*i+1;
	    int j;
	    *cp = 0;
	    if (i)
		memcpy(cp+1, cpmdir+direno*32+1, 11);
	    cp[12] = ex & 0x1f;
	    cp[13] = 0;
	    cp[14] = ex >> 5;
	    cp[15] = nblocks <= 8 ?
		COVER((size-16*1024*nlogexts), 128) : 128;
	    cp += 16;
	    for (j = 0; j < 8; j++) {
		if (nblocks > 0) {
		    *cp++ = blockno;
		    *cp++ = blockno >> 8;
		    ++blockno;
		    --nblocks;
		}
		else {
		    *cp++ = 0;
		    *cp++ = 0;
		}
	    }
	}
	direno += ndirents;
    }
    closedir(dirp);
    if (nfiles == 0) {
	fprintf(stderr, "no suitable files in %s\n", filename);
	free(cpmdir);
	free(dp->fds);
	return 0;
    }
    dp->nde = direno;
    while (direno & 3) {
	memset(cpmdir+direno*32, 0xe5, 32);
	++direno;
    }
    dp->nfds = nfiles;
    dp->fds = realloc(dp->fds, nfiles*sizeof(struct fdesc));
    dp->data = realloc(cpmdir, direno*32);

    /* set up disk parameter header */
    dp->dph = dptable + (16+15)*(dp-mnttab);
    memclr(ram + dp->dph, 16+15);
    PutWORD(dp->dph+8, dirbuff);	/* pointer to directory buffer */
    PutWORD(dp->dph+10, dp->dph+16);	/* pointer to dpb  */
    PutWORD(dp->dph+16, 256);		/* sectors per track */
    PutBYTE(dp->dph+18, 5);		/* block shift factor */
    PutBYTE(dp->dph+19, 31);		/* block mask */
    PutBYTE(dp->dph+20, 1);		/* extent mask */
    PutWORD(dp->dph+21, blockno > 256 ? blockno-1 : 256); /* DSM */
    PutWORD(dp->dph+23, N_ENTRIES-1);	/* DRM */
    PutWORD(dp->dph+25, 0xffff);	/* AL0,AL1 */

    alv = cpmalloc((GetWORD(dp->dph+16+5) >> 3) + 1);
    if (alv == 0) {
	int i;
	fprintf(stderr, "insufficient space to mount %s\n", filename);
	for (i = 0; i < dp->nfds; i++)
	    free(dp->fds->fullname);
	free(dp->data);
	free(dp->fds);
	return 0;
    }
    PutWORD(dp->dph+14, alv);		/* pointer to allocation vector  */
    dp->filename = newstr(filename);
    dp->flags = MNT_ACTIVE|MNT_RDONLY|MNT_UNIXDIR;

    return 1;
}

static struct {
    char magic[32];
    char dpb[15];
} sssd = {
    "<CPM_Disk>  Drive x",
    "\x1a\x00\x03\x07\x00\xf2\x00\x3f\x00\xc0\x00\x00\x00\x02\x00"
};

static char *xlt26 =
"\x00\x06\x0c\x12\x18\x04\x0a\x10\x16\x02\x08\x0e\x14"
"\x01\x07\x0d\x13\x19\x05\x0b\x11\x17\x03\x09\x0f\x15";

static int
mount(int disk, const char *filename, int readonly)
{
    struct mnt *dp = mnttab + disk;
    int prot = PROT_READ|PROT_WRITE;
    int doffs;
    WORD alv;
    BYTE buf[128];
    struct stat st;

    if (dp->flags & MNT_ACTIVE)
	umount(disk);

    dp->flags = 0;
    if (stat(filename, &st) < 0) {
	perror(filename);
	return 0;
    }

    if ((st.st_mode & S_IFMT) == S_IFDIR)
	return mountdir(dp, filename);

    if ((st.st_mode & S_IFMT) != S_IFREG) {
	fprintf(stderr, "%s is neither a regular file nor a directory\n", filename);
	return 0;
    }

    if (readonly || (dp->ifd = open(filename, O_RDWR)) < 0) {
	prot = PROT_READ;
	dp->flags |= MNT_RDONLY;
	if ((dp->ifd = open(filename, O_RDONLY)) < 0) {
	    perror(filename);
	    return 0;
	}
    }

    /* peek at descriptor page */
    if (read(dp->ifd, buf, 128) != 128) {
	perror(filename);
	close(dp->ifd);
	return 0;
    }
    if (memcmp(buf, "<CPM_Disk>", 10) != 0) {
	WORD xlt;
	if (st.st_size != 256256) {
	    fprintf(stderr, "%s is not a valid <CPM_Disk> file\n", filename);
	    close(dp->ifd);
	    return 0;
	}
	/* assume this is an image of a sssd floppy */
	memcpy(buf, &sssd, sizeof(sssd));
	dp->dph = dptable + (16+15)*disk;
	memclr(ram + dp->dph, 16+15);
	xlt = cpmalloc(26);		/* space for sector translation table */
	memcpy(ram+xlt, xlt26, 26);
	PutWORD(dp->dph, xlt);
	doffs = 0;
    }
    else {
	dp->dph = dptable + (16+15)*disk;
	memclr(ram + dp->dph, 16+15);
	doffs = 128;
    }
    PutWORD(dp->dph+8, dirbuff);	/* pointer to directory buffer */
    PutWORD(dp->dph+10, dp->dph+16);	/* pointer to dpb  */
    memcpy(ram + dp->dph+16, buf + 32, 15); /* copy dpb into cp/m ram */
    PutWORD(dp->dph+16+11, 0);		/* check vector size = 0 (fixed disk) */

    /* calculate memory requirement */
    /* (((DSM+1)<<BSH) + OFFS*SPT + 1)*128 */
    dp->isize = (((GetWORD(dp->dph+16+5) + 1) << GetBYTE(dp->dph+16+2)) +
		GetWORD(dp->dph+16+13) * GetWORD(dp->dph+16+0) + 1) * 128;

    alv = cpmalloc((GetWORD(dp->dph+16+5) >> 3) + 1);
    if (alv == 0) {
	fprintf(stderr, "insufficient space to mount %s\n", filename);
	close(dp->ifd);
	return 0;
    }
    PutWORD(dp->dph+14, alv);		/* pointer to allocation vector  */

#ifndef MAP_FILE
#define MAP_FILE 0
#endif

#ifdef __BOUNDS_CHECKING_ON
    /* absurd -1 return code blows bgcc's mind */
    dp->header = mmap(NULL, dp->isize, prot, MAP_FILE|MAP_SHARED, dp->ifd, 0);
#else
    if ((dp->header = mmap(NULL, dp->isize, prot, MAP_FILE|MAP_SHARED,
			   dp->ifd, 0)) == (char *)-1) {
	perror(filename);
	close(dp->ifd);
	return 0;
    }
#endif
    dp->filename = newstr(filename);
    dp->data = (BYTE *) dp->header + doffs;
    dp->flags |= MNT_ACTIVE;
    return 1;
}


static const char *white = " \t";

static int
domount(char *cmd)
{
    int d, v, r;
    char *tok = strtok(NULL, white);

    if ((v = tok && (strcmp(tok, "-v") == 0)))
	tok = strtok(NULL, white);
    if ((r = tok && (strcmp(tok, "-r") == 0)))
	tok = strtok(NULL, white);
    if (tok && !v) {
	d = *tok - 'A';
	if (d < 0 || d > 15)
	    d = *tok - 'a';
	if (d < 0 || d > 15 || tok[1]) {
	    fprintf(stderr, "illegal disk specifier: %s\n", tok);
	    return 0;
	}
	tok = strtok(NULL, white);
	mount(d, tok, r);
    }
    else for (d = 0; d < 16; d++)
	if (mnttab[d].flags & MNT_ACTIVE)
	    showdisk(d, v);
    return 0;
}

static int
doumount(char *cmd)
{
    int d;
    char *tok = strtok(NULL, white);

    if (tok) {
	d = *tok - 'a';
	if (d < 0 || d > 15 || tok[1]) {
	    fprintf(stderr, "illegal disk specifier: %s\n", tok);
	    return 0;
	}
	umount(d);
    }
    return 0;
}

struct sio siotab[MAXPSTR] = {
{ NULL, NULL, "ttyin", 0, ST_IN2 },
{ NULL, NULL, "ttyout", 0, ST_OUT2 },
{ NULL, NULL, "crtin", 0, ST_IN2 },
{ NULL, NULL, "crtout", 0, ST_OUT2 },
{ NULL, NULL, "uc1in", 0, ST_IN2 },
{ NULL, NULL, "uc1out", 0, ST_OUT2 },
{ NULL, NULL, "rdr", 0, ST_IN },
{ NULL, NULL, "ur1", 0, ST_IN },
{ NULL, NULL, "ur2", 0, ST_IN },
{ NULL, NULL, "pun", 0, ST_OUT },
{ NULL, NULL, "up1", 0, ST_OUT },
{ NULL, NULL, "up2", 0, ST_OUT },
{ NULL, NULL, "lpt", 0, ST_OUT },
{ NULL, NULL, "ul1", 0, ST_OUT } };


/* Table of logical streams */
int chn[5];

static int
doattach(char *cmd)
{
    int fd, i, opflags;
    struct sio *s;
    char *tok = strtok(NULL, white);

    if (tok) {
	char *p = tok + strlen(tok);
	if (p > tok && *--p == ':')
	    *p = 0;
	for (i = 0; i < MAXPSTR; i++) {
	    s = siotab + i;
	    if (strncmp(tok, s->streamname, 3) == 0)
		break;
	}
	if (i == MAXPSTR) {
	    fprintf(stderr, "stream not recognized: %s\n", tok);
	    return 0;
	}
	if (s->strtype == ST_IN2) {
	    if (strcmp(tok, (s+1)->streamname) == 0) {
		s++;
		opflags = O_WRONLY|O_CREAT|O_TRUNC;
	    }
	    else if (strcmp(tok, s->streamname) == 0)
		opflags = O_RDONLY;
	    else
		opflags = O_RDWR|O_CREAT;
	}
	else
	    opflags = s->strtype == ST_IN ? O_RDONLY : O_WRONLY|O_CREAT|O_TRUNC;
	tok = strtok(NULL, white);
	if (!tok || !*tok) {
	    fputs("need a filename\n", stderr);
	    return 0;
	}
	if (s->fp) {
	    fclose(s->fp);
	    s->fp = NULL;
	    free(s->filename);
	}
	if ((fd = open(tok, opflags, 0666)) < 0)
	    perror(tok);
	else {
	    char *mode = "rb";
	    if (opflags & O_WRONLY)
		mode = "wb";
	    else if (opflags & O_RDWR)
		mode = "r+b";
	    s->filename = newstr(tok);
	    s->fp = fdopen(fd, mode);
	    s->tty = isatty(fd);
	    s->canselect = s->tty || isafifo(fd);	/* gr */
	}
    }
    else for (i = 0; i < MAXPSTR; i++) {
	s = siotab + i;
	if (s->fp)
	    printf("%s:\t%s\n", s->streamname, s->filename);
    }
    return 0;
}

static int
dodetach(char *cmd)
{
    int i;
    struct sio *s;
    char *tok = strtok(NULL, white);

    if (tok) {
	char *p = tok + strlen(tok);
	if (p > tok && *--p == ':')
	    *p = 0;
	for (i = 0; i < MAXPSTR; i++) {
	    s = siotab + i;
	    if (strncmp(tok, s->streamname, 3) == 0)
		break;
	}
	if (i == MAXPSTR) {
	    fprintf(stderr, "stream not recognized: %s\n", tok);
	    return 0;
	}
	if (s->fp) {
	    fclose(s->fp);
	    s->fp = NULL;
	    free(s->filename);
	}
    }
    return 0;
}


static long
getval(char *s)
{
    char *tok = s+2;

    if (*tok == 0)
	tok = strtok(NULL, white);
    if (tok && *tok) {
	long unit = 1;
	char u = tok[strlen(tok)-1];
	switch (toupper(u)) {
	case 'K':
	    unit = 1024;
	    break;
	case 'M':
	    unit = 1024*1024;
	    break;
	}
	return unit*strtol(tok, NULL, 10);
    }
    else {
	fprintf(stderr, "option needs a value: %s\n", s);
	return -1;
    }
}

static void
checkval(int ok, long val, char *msg)
{
    if (!ok)
	fprintf(stderr, "bad %s value: %ld\n", msg, val);
}

/* count ones in a 16-bit value */
static int
popcount(long v)
{
    int total;

    total = ((v>>1) & 0x5555) + (v & 0x5555);
    total = ((total>>2) & 0x3333) + (total & 0x3333);
    total = ((total>>4) & 0x0f0f) + (total & 0x0f0f);
    return (total & 0xff) + (total>>8);
}

static void
makedisk(FILE *f, char *fn, long diroffs, long dirsize, long fullsize)
{
    long n;
    BYTE sector[128];

    memset(sector, 0xe5, sizeof sector);

    /* skip offset tracks */
    if (fseek(f, diroffs, SEEK_CUR) < 0) {
	fclose(f);
	perror(fn);
	return;
    }
    /* write empty directory */
    for (n = 0; n < dirsize; n += sizeof sector)
	if (fwrite(sector, sizeof sector, 1, f) == 0) {
	    fclose(f);
	    perror(fn);
	    return;
	}
    /* seek to end of disk and write last sector to define size */
    if (fseek(f, fullsize - sizeof sector, SEEK_SET) < 0 ||
	fwrite(sector, sizeof sector, 1, f) == 0 ||
	fclose(f) != 0)
	perror(fn);
}

/* create a new CP/M disk */
static int
docreate(char *tok)
{
    char *fn = NULL;
    FILE *f;
    long size = 1024*1024;
    char head[128];
    long dsm, spt = -1, bsize = -1, drm = -1, offs = -1;
    int dblocks;
    WORD al01;

    while ((tok = strtok(NULL, white)) != NULL) {
	if (*tok == '-')
	    switch (tok[1]) {
	    case 'b':
		bsize = getval(tok);
		break;
	    case 'd':
		drm = getval(tok);
		break;
	    case 'o':
		offs = getval(tok);
		break;
	    case 's':
		spt = getval(tok);
		break;
	    default:
		fprintf(stderr, "unrecognized option: %s\n", tok);
	    }
	else {
	    fn = tok;
	    break;
	}
    }

    if (fn == NULL) {
	fputs("need a filename\n", stderr);
	return 0;
    }
    if ((tok = strtok(NULL, white)) != NULL) {
	char unit = 'b';
	int n = sscanf(tok, "%ld%c", &size, &unit);
	if (n == 2)
	    switch (toupper(unit)) {
	    case 'B':
		break;
	    case 'K':
		size *= 1024;
		break;
	    case 'M':
		size *= 1024*1024;
		break;
	    default:
		fprintf(stderr, "units not recognized: %s\n", tok);
		return 0;
	    }
	else if (n != 1) {
	    fprintf(stderr, "need numeric size: %s\n", tok);
	    return 0;
	}
    }
    if ((f = fopen(fn, "w")) == NULL) {
	perror(fn);
	return 0;
    }
    if (size == 256256 && (spt == -1 || spt == 26) &&
	(bsize == -1 || bsize == 1024) &&
	(drm == -1 || drm == 63) &&
	(offs == -1 || offs == 2)) {
	/* raw standard sssd floppy format */
	spt = 26;
	drm = 63;
	offs = 2;
	/* we clear all tracks that might contain directory sectors,
	   thus avoiding messing with the sector translation table */
	makedisk(f, fn, 128*spt*offs, 128*(((drm+4)/4+spt-1)/spt)*spt, size);
	return 0;
    }
    else if (size < 256*1024) {
	if (bsize == -1)
	    bsize = 1024;
	if (drm == -1)
	    drm = 63;
	if (spt == -1)
	    spt = 26;
	if (offs == -1)
	    offs = 0;
    }
    else {
	if (bsize == -1)
	    bsize = 2048;
	if (drm == -1)
	    drm = 1023;
	if (spt == -1)
	    spt = 26;
	if (offs == -1)
	    offs = 0;
    }
    dsm = (size - offs*spt*128)/bsize - 1;
    checkval(spt <= 0xffff, spt, "sectors per track");
    checkval(size/(spt*128)+offs <= 0xffff, size/(spt*128)+offs, "tracks");
    checkval(((bsize&(bsize-1))==0) &&
	     (bsize >= ((dsm < 256) ? 1024 : 2048)) &&
	     bsize <= 16384, bsize, "block size");
    dblocks = ((drm+1)*32+bsize-1)/bsize;
    checkval(dblocks<=16 && dblocks < dsm, drm, "max directory entry");
    memclr(head, sizeof head);
    sprintf(head, "<CPM_Disk>");
    head[32] = spt;
    head[33] = spt>>8;
    head[34] = popcount(bsize-1)-7;	/* bsh */
    head[35] = (bsize/128-1);		/* blm */
    head[36] = dsm < 256 ? bsize/1024-1 : bsize/2048-1; /* exm */
    head[37] = dsm;
    head[38] = dsm>>8;
    head[39] = drm;
    head[40] = drm>>8;
    al01 = ~((1<<(16-dblocks))-1);
    head[41] = al01>>8;
    head[42] = al01;
    head[45] = offs;
    head[46] = offs>>8;

    if (fwrite(head, sizeof head, 1, f) == 0) {
	fclose(f);
	perror(fn);
	return 0;
    }
    makedisk(f, fn, 128*spt*offs, 128*(drm+4)/4, sizeof head + size);
    return 0;
}

static int
hexdig(char c)
{
    if ('0' <= c && c <= '9')
	return c - '0';
    if ('A' <= c && c <= 'F')
	return c - 'A' + 10;
    if ('a' <= c && c <= 'f')
	return c - 'a' + 10;
    return -1;
}

static int
doint(char *cmd)
{
    int d1, d2;
    char *tok = strtok(NULL, white);

    if (tok) {
	if (strlen(tok) != 2) {
	bad:
	    printf("%s invalid key specifier\n", tok);
	    return 0;
	}
	/* let's face it: this doesn't work if the host character set is not ascii */
	if (tok[0] == '^' && '@' <= tok[1])
	    interrupt = tok[1] & 0x1f;
	else {
	    if ((d1 = hexdig(tok[0])) < 0)
		goto bad;
	    if ((d2 = hexdig(tok[1])) < 0)
		goto bad;
	    interrupt = (d1<<4)+d2;
	}
	rawtio.c_lflag = interrupt ? ISIG : 0;
	rawtio.c_cc[VINTR] = interrupt;
    }
    else {
	fputs("interrupt key is ", stdout);
	if (interrupt == 0)
	    puts("disabled");
	else if (interrupt < 0x20)
	    printf("^%c\n", interrupt+'@');
	else
	    printf("%2x\n", interrupt);
    }
    return 0;
}

static int
dotime(char *cmd)
{
    static clock_t lastreal;
    clock_t now;
    static struct tms lastbuf;
    struct tms tbuf;
    long tickspersec = CLK_TCK;
    extern char *perl_params;

    now = times(&tbuf);

    printf("elapsed=%.3f, user=%.3f, sys=%.3f (%s)\n",
	   ((double)(now-lastreal))/tickspersec,
	   ((double)(tbuf.tms_utime-lastbuf.tms_utime))/tickspersec,
	   ((double)(tbuf.tms_stime-lastbuf.tms_stime))/tickspersec,
	   perl_params);
    lastreal = now;
    lastbuf = tbuf;
    return 0;
}

static int
dogo(char *cmd)
{
    return 1;
}

static int
doshell(char *cmd)
{
    char *shell = getenv("SHELL");
#ifdef DEBUG
    void (*sigint)(int);

    sigint = signal(SIGINT, SIG_IGN);
#endif
    if (shell == NULL)
	shell = "/bin/sh";
    if (cmd[1])
	system(cmd+1);
    else
	system(shell);
#ifdef DEBUG
    (void) signal(SIGINT, sigint);
#endif
    return 0;
}

static int
doquit(char *cmd)
{
    exit(0);
}

static int dohelp(char *cmd);

typedef struct {
    char *name;				/* User printable name of the function. */
    int (*func)(char *);		/* Function to call to do the job. */
    char *doc;				/* Short documentation.  */
    char *detail;			/* Long documentation. */
} COMMAND;

static COMMAND commands[] = {
{ "help",   dohelp,   "Display this text or give help about a command",
      "help <cmd>                 displays more information about <cmd>" },
{ "?",      dohelp,   "Synonym for `help'", NULL },
{ "attach", doattach, "Attach CP/M device to a unix file",
      "attach                     without arguments lists the current attachments\n"
      "attach <physdev> <file>    attaches <physdev> to the unix <file>,\n"
      "                           where <physdev> is one of ttyin, ttyout,\n"
      "                           crtin, crtout, uc1in, uc1out, rdr,\n"
      "                           ur1, ur2, pun, up1, up2, lpt, ul1" },
{ "detach", dodetach, "Detach CP/M device from file",
      "detach <physdev>           closes the file attached to <physdev>\n"
      "                           (see attach)" },
{ "mount",  domount,  "Mount a unix file or directory as a CP/M disk",
      "mount                      without arguments lists the mount table\n"
      "mount -v                   lists the mount table verbosely\n"
      "mount <drive> <file>       mounts <file> as CP/M disk <drive>\n"
      "                           (a letter from a..p).\n"
      "        If <file> is a plain file it must contain a CP/M filesystem.\n"
      "        If <file> is a unix directory its contents may be accessed\n"
      "           as a read-only CP/M disk\n"
      "mount -r <drive> <file>    mounts the <file> read/only." },
{ "umount", doumount, "Unmount a CP/M disk",
      "umount <drive>             closes the file associated with <drive>\n"
      "                           and frees the resources" },
{ "create", docreate, "Create a new disk",
      "create {flags} <file> {size}  creates a unix <file> initialized as a\n"
      "                              CP/M disk of size {size} (default 1MB).\n"
      "       -b <block size>        default 1024 if size < 256K, else 2048\n"
      "       -d <# dir entries - 1> default 1023\n"
      "       -o <track offset>      default 0\n"
      "       -s <sectors per track> default 26\n"
      "create <file> 256256          create a raw SSSD disk image" },
{ "interrupt", doint, "Set user interrupt key",
      "interrupt <key>            makes <key> interrupt CP/M back to the monitor\n"
      "        <key> may be a 2-digit hex number or ^x where x is one of a..z[\\]^_\n"
      "        ^@ makes CP/M uninterruptible (from the keyboard)\n"
      "interrupt                  without an argument displays the current setting" },
{ "go",     dogo,     "Start/Continue CP/M execution", NULL },
{ "!",      doshell,  "Execute a unix command",
      "!                          escape to a unix shell\n"
      "!cmd                       execute unix cmd" },
{ "quit",   doquit,   "Terminate yaze", NULL },
{ "time",   dotime,   "Display elapsed time since last `time' command",
      "displays elapsed, user and system time in seconds,\n"
      "         along with simulator options" },
{ NULL, NULL, NULL, NULL }
};

static int
dohelp(char *cmd)
{
    char *tok = strtok(NULL, white);
    int tlen;
    COMMAND *cp;

    if (tok) {
	for (tlen = strlen(tok), cp = commands; cp->name; cp++)
	    if (strncmp(tok, cp->name, tlen) == 0)
		break;
	if (cp->name) {
		puts(cp->detail ? cp->detail : cp->doc);
	    return 0;
	}
    }
    for (cp = commands; cp->name; cp++)
	printf("%-10s  %s\n", cp->name, cp->doc);
    return 0;
}

int
docmd(char *cmd)
{
    char *tok;
    int tlen;
    COMMAND *cp;
    int (*func)(char *) = NULL;

    if (cmd == NULL)
	return 0;
    while (*cmd == ' ' || *cmd == '\t' || *cmd == '\n')
	cmd++;
    for (tok = cmd + strlen(cmd) - 1; tok >= cmd; tok--)
	if (*tok == ' ' || *tok == '\t' || *tok == '\n')
	    *tok = 0;
	else
	    break;
    if (*cmd == 0)
	return 0;
    add_history(cmd);
    if (*cmd == '!') {
	/* special case */
	doshell(cmd);
	return 0;
    }
    tok = strtok(cmd, white);
    if (tok == NULL || *tok == 0)
	return 0;
    for (tlen = strlen(tok), cp = commands; cp->name; cp++)
	if (strncmp(tok, cp->name, tlen) == 0)
	    /* don't allow quit command to be abbreviated */
	    if (cp->func != doquit || strcmp(tok, cp->name) == 0) {
		if (func == NULL)
		    func = cp->func;
		else {
		    func = NULL;	/* ambiguous */
		    break;
		}
	    }
    if (func)
	return func(cmd);
    printf("%s ?\n", tok);
    return 0;
}

#ifdef DEBUG
void
sighand(int sig)
{
    stopsim = 1;
}
#endif

void
monitor(FASTWORK adr)
{
    static char *cmd = NULL;

    ttycook();
#ifdef DEBUG
    if (adr & 0x10000)
	printf("stopped at pc=0x%04x\n", adr & 0xffff);
    stopsim = 0;
    signal(SIGINT, sighand);
#endif
#ifdef USE_GNU_READLINE
    do {
	if (cmd) {
	    free(cmd);
	    cmd = NULL;
	}
	cmd = readline("$>");
	if (cmd == NULL)
	    if ((ttyflags & ISATTY) == 0)
		doquit(NULL);
	    else
		putchar('\n');
    } while (!docmd(cmd));
#else
    if (cmd == NULL)
	cmd = xmalloc(BUFSIZ);
    do {
	fputs("$>", stdout);
	fflush(stdout);
	if (fgets(cmd, BUFSIZ-1, stdin) == NULL) {
	    if ((ttyflags & ISATTY) == 0)
		doquit(NULL);
	    else {
		putchar('\n');
		cmd[0] = 0;
	    }
	}
    } while (!docmd(cmd));
#endif
    ttyraw();
}
