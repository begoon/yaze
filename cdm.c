/* cdm - CP/M Disk Manager
   Copyright (C) 1995  Frank D. Cringle.

cdm is free software; you can redistribute it and/or modify it under
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

/* 	$Id: cdm.c,v 1.3 2004/01/11 16:49:58 fdc Exp $	 */

#ifndef lint
static char vcid[] = "$Id: cdm.c,v 1.3 2004/01/11 16:49:58 fdc Exp $";
#endif /* lint */


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <signal.h>

#ifdef BSD
#if defined(sun)
#include <memory.h>
#include <string.h>
#endif
#ifndef strchr
#define strchr index
#endif
#ifndef strrchr
#define strrchr rindex
#endif
#define memclr(p,n)	bzero(p,n)
#define memcpy(t,f,n)	bcopy(f,t,n)
#define memcmp(p1,p2,n)	bcmp(p1,p2,n)
#define memset(p,v,n)							\
    do { size_t len = n;						\
	 char *p1 = p;							\
	 while (len--) *p1++ = v;					\
    } while (0)
#else
#include <string.h>
#define memclr(p,n)	(void) memset(p,0,n)
#endif

typedef unsigned char	BYTE;

#define VERSION	"1.10"

#ifdef CPM3
/* this is rudimentary and untested - just the obvious differences */
#define MAXUSER 15
#define S2MASK	0x3f
#else
/* MAXUSER = 15 for standard CP/M and 31 for ZRDOS */
#define MAXUSER	31
#define	S2MASK	0x0f
#endif

extern void *xmalloc(size_t size);
#ifdef USE_GNU_READLINE
#include <readline/readline.h>
void add_history(char *cmd);
#else
#define add_history(x)
#endif

#define PutWORD(a, v)							\
    do { *(a) = (BYTE)(v);						\
         *(a+1) = (v) >> 8;						\
     } while (0)


#define MNT_ACTIVE	1
#define MNT_RDONLY	2

static struct mnt {		/* mount table (drives A..P) */
    int flags;
    char *filename;		/* unix filename of disk */
    int fd;			/* file descriptor */
    size_t size;		/* file size */
    int *xlt;			/* pointer to sector translation table, if any */
    BYTE *alv;			/* pointer to allocation vector */
    int spt;			/* sectors per track */
    int bsh;			/* block shift factor */
    int exm;			/* extent mask */
    int dsm;			/* max block number */
    int drm;			/* max directory entry */
    int off;			/* track offset */
    BYTE *data;			/* disk image */
    BYTE *dir;			/* start of directory */
} mnttab[16];

int curdisk = -1;		/* current disk (0..15) */
int curuser;			/* current user (0..MAXUSER) */

/* stash a string away on the heap */
static char *
newstr(const char *str)
{
	char *p = xmalloc(strlen(str) + 1);
	(void) strcpy(p, str);
	return p;
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
    printf("%s %s\n", dp->flags & MNT_RDONLY ? "r/o " : "r/w ", dp->filename);
    if (!verbose)
	return;
    printf("  spt=%d, bsh=%d, exm=%x, dsm=%d, drm=%d, off=%d\n",
	   dp->spt, dp->bsh, dp->exm, dp->dsm, dp->drm, dp->off);
}

static int
umount(int disk)
{
    struct mnt *dp = mnttab + disk;

    if (!(dp->flags & MNT_ACTIVE))
	return 0;
    free(dp->alv);
    if (munmap((char *) dp->data, dp->size) == -1 || close(dp->fd) == -1)
	perror(dp->filename);
    dp->flags = 0;
    free(dp->filename);
    if (disk == curdisk)
	curdisk = -1;
    return 0;
}

/* Find and allocate a free block on dp.
 * Return block number or 0 if disk is full.
 */
static long
findfree(struct mnt *dp)
{
    BYTE *a = dp->alv;
    long b;

    for (b = 0; b <= dp->dsm; b++) {
	if (a[b>>3] == 0xff) {
	    b += 7;
	    continue;
	}
	if ((a[b>>3]&(0x80>>(b&7))) == 0) {
	    a[b>>3] |= 0x80>>(b&7);
	    return b;
	}
    }
    return 0;
}

/* allocate (set=1) or deallocate (set=0) the groups in a directory entry */
static long
alloc(struct mnt *dp, BYTE *p, int set)
{
    int i, small = dp->dsm < 256;
    BYTE *a = dp->alv;
    long total = 0;

    for (i = 16; i < 32; i++) {
	unsigned long blockno;
	if (small)
	    blockno = p[i];
	else {
	    blockno = p[i] + (p[i+1]<<8);
	    i++;
	}
	if (blockno == 0)
	    continue;
	if (blockno > dp->dsm) {
	    fprintf(stderr, "!!! bad block number on disk %s !!!\n",
		    dp->filename);
	    continue;
	}
	total++;
	if (set) {
	    if (a[blockno >> 3] & (0x80 >> (blockno & 7))) {
		fprintf(stderr,
			"!!! block %ld used more than once on disk %s !!!\n",
			blockno, dp->filename);
		continue;
	    }
	    a[blockno >> 3] |= 0x80 >> (blockno & 7);
	}
	else
	    a[blockno >> 3] &= ~(0x80 >> (blockno & 7));
    }
    return total;
}


static int
mount(int disk, const char *filename, int readonly)
{
    struct mnt *dp;
    int prot = PROT_READ|PROT_WRITE;
    int doffs, i, al01;
    long total;
    BYTE buf[128];
    struct stat st;
    caddr_t cp;

    if (disk > 15)
	return 0;
    dp = mnttab + disk;
    if (dp->flags & MNT_ACTIVE)
	umount(disk);

    dp->flags = 0;
    if (stat(filename, &st) < 0) {
	perror(filename);
	return 0;
    }

    if ((st.st_mode & S_IFMT) != S_IFREG) {
	fprintf(stderr, "%s is not a regular file\n", filename);
	return 0;
    }

    if (readonly || (dp->fd = open(filename, O_RDWR)) < 0) {
	prot = PROT_READ;
	dp->flags |= MNT_RDONLY;
	if ((dp->fd = open(filename, O_RDONLY)) < 0) {
	    perror(filename);
	    return 0;
	}
    }

    /* peek at descriptor page */
    if (read(dp->fd, buf, 128) != 128) {
	perror(filename);
	close(dp->fd);
	return 0;
    }
    dp->size = st.st_size;
    if (memcmp(buf, "<CPM_Disk>", 10) != 0) {
	fprintf(stderr, "%s is not a valid <CPM_Disk> file\n", filename);
	close(dp->fd);
	return 0;
    }
    dp->xlt = 0;
    dp->spt = buf[32]+(buf[33]<<8);
    dp->bsh = buf[34];
    dp->exm = buf[36];
    dp->dsm = buf[37]+(buf[38]<<8);
    dp->drm = buf[39]+(buf[40]<<8);
    dp->off = buf[45]+(buf[46]<<8);
    al01 = (buf[41]<<8)+buf[42];
    doffs = 128;
#ifdef __BOUNDS_CHECKING_ON
    /* absurd -1 return code blows bgcc's mind */
    cp = mmap(NULL, dp->size, prot, MAP_SHARED, dp->fd, 0);
#else
    if ((cp = mmap(NULL, dp->size, prot, MAP_SHARED, dp->fd, 0)) == (caddr_t)-1) {
	perror(filename);
	close(dp->fd);
	return 0;
    }
#endif
    dp->data = (BYTE *) cp;
    dp->dir = dp->data + doffs + dp->off*dp->spt*128;
    dp->filename = newstr(filename);
    dp->flags |= MNT_ACTIVE;
    dp->alv = xmalloc(dp->dsm/8+2);	/* keep length an even number */
    memclr(dp->alv, dp->dsm/8+2);
    dp->alv[0] = (BYTE) (al01>>8);
    dp->alv[1] = (BYTE) al01;
    total = popcount(al01);
    for (i = 0; i <= dp->drm; i++) {
	BYTE *p = dp->dir + 32*i;
	if (*p <= MAXUSER)
	    total += alloc(dp, p, 1);
    }
    printf("%s mounted as disk %c with %ld of %ld bytes allocated\n",
	   dp->filename, 'A'+disk, total<<(7+dp->bsh), (dp->dsm+1L)<<(7+dp->bsh));
    curdisk = disk;
    return 1;
}

static const char *white = " \t";

static int
domount(char *tok)
{
    int d, v, r;

    tok = strtok(NULL, white);
    if ((v = tok && (strcmp(tok, "-v") == 0)))
	tok = strtok(NULL, white);
    if ((r = tok && (strcmp(tok, "-r") == 0)))
	tok = strtok(NULL, white);
    if (tok && !v) {
	d = toupper(*tok) - 'A';
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
doumount(char *tok)
{
    int d;

    tok = strtok(NULL, white);
    if (tok) {
	d = toupper(*tok) - 'A';
	if (d < 0 || d > 15 || tok[1]) {
	    fprintf(stderr, "illegal disk specifier: %s\n", tok);
	    return 0;
	}
	umount(d);
    }
    return 0;
}

static int
diskuser(char *str, int *disk, int *user)
{
    int ch = toupper(*str);

    *disk = -1;
    *user = -1;
    if ('A' <= ch && ch <= 'P') {
	*disk = ch - 'A';
	str++;
    }
    if (*str && '0' <= *str && *str <= '9')
	*user = *str++ - '0';
    if (*str && '0' <= *str && *str <= '9')
	*user = *user*10 + *str++ - '0';
    return ((*str == 0 || *str == ':') && *user <= MAXUSER);
}

static int
makefcb(char *fn, BYTE *fcb)
{
    char *p;
    int d = -1;
    int u = -1;
    int ch, i;

    fcb[0] = curuser;
    if ((p = strchr(fn, ':')) == NULL)
	p = fn;
    else {
	p++;
	if (fn[0] == '*' || fn[0] == '?') {
	    /* wildcard user, current disk */
	    if (fn[1] != ':')
		return -1;
	    fcb[0] = '?';
	}
	else if (fn[1] == '*' || fn[1] == '?') {
	    /* wildcard user, specified disk */
	    if (fn[2] != ':' || (ch = toupper(fn[0])) < 'A' || ch > 'P')
		return -1;
	    d = ch - 'A';
	    fcb[0] = '?';
	} else if (!diskuser(fn, &d, &u))
	    return -1;
    }
    if (d < 0)
	d = curdisk;
    if (d < 0 || (mnttab[d].flags & MNT_ACTIVE) == 0)
	return -1;
    if (u >= 0)
	fcb[0] = u;
    for (i = 0; i < 8; i++) {	/* copy name */
	if (*p == '*')
	    *++fcb = '?';
	else if (*p == '.' || *p == 0)
	    *++fcb = ' ';
	else if (strchr("<>,;:=[]", *p))
	    return -1;
	else {
	    *++fcb = toupper(*p);
	    p++;
	}
    }
    while (*p && *p != '.')
	p++;
    if (*p == '.')
	p++;
    for (i = 0; i < 3; i++) {	/* copy extension */
	if (*p == '*')
	    *++fcb = '?';
	else if (*p == 0)
	    *++fcb = ' ';
	else if (strchr("<>.,;:=[]", *p))
	    return -1;
	else {
	    *++fcb = toupper(*p);
	    p++;
	}
    }
    for (i = 0; i < 4; i++)	/* clear ex, s1, s2, rc */
	*++fcb = 0;
    return d;
}

/* search for a directory entry on disk 'dp' matching 'len' bytes of the name
 * in 'fcb' starting at the 'start'th entry.  (len 1..15)
*/
static int
srchdir(struct mnt *dp, BYTE* fcb, int len, int start)
{
    BYTE *entry = dp->dir + start*32;

    /* note: real CP/M quits when it finds virgin entries */
    while (start <= dp->drm) {
	int n;
	for (n = 0; n < len; n++) {
	    int mask;
	    switch (n) {
	    case 0:			/* user number / delete flag */
		mask = 0xff;
		break;
	    case 12:			/* extent */
		mask = ~dp->exm & 0x1f;
		break;
	    case 13:			/* s1 */
		mask = 0;		/* ignore possible cpm3 bytecount */
		break;
	    case 14:			/* s2 */
		mask = S2MASK;
		break;
	    default:			/* filename */
		mask = 0x7f;
	    }
	    if (fcb[n] != '?' && fcb[n] != (entry[n]&mask))
		break;
	}
	if (n == len)
	    return start;
	entry += 32;
	start++;
    }
    return -1;
}

static struct mnt *sortdisk;
static char sortkeys[3];

static int
dircmp(const void *d1, const void *d2)
{
    BYTE *p1 = sortdisk->dir + *(int *)d1 * 32;
    BYTE *p2 = sortdisk->dir + *(int *)d2 * 32;
    int n, d, k;

    for (k = 0; k < 3; k++)
	switch (sortkeys[k]) {
	case 'u':			/* user number */
	    if ((d = (p1[0] - p2[0])))
		return d;
	    break;
	case 'n':			/* name */
	    for (n = 1; n < 9; n++)
		if ((d = (p1[n]&0x7f) - (p2[n]&0x7f)))
		    return d;
	    break;
	case 'e':			/* extension */
	    for (n = 9; n < 12; n++)
		if ((d = (p1[n]&0x7f) - (p2[n]&0x7f)))
		    return d;
	}
    return 0;
}

static int
pname(BYTE *p)
{
    int i;
    int width;

    for (i = 0, width = 0; i < 8; i++, p++)
	if (*p != ' ') {
	    putchar(*p&0x7f);
	    width++;
	}
    if (*p != ' ') {
	putchar('.');
	width++;
	for (i = 0; i < 3; i++, p++)
	    if (*p != ' ') {
		putchar(*p&0x7f);
		width++;
	    }
    }
    return width;
}

/* Find total file size (including unallocated holes, if any)
 * p points to an arbitrary directory entry of the file on disk dp.
 */
static long
sizetotal(BYTE *p, struct mnt *dp)
{
    int i;
    BYTE fcb[16];
    long res = 0, finalex = 0;

    for (i = 0; i < 12; i++)
	fcb[i] = p[i]&0x7f;
    i = 0;
    for (i = 0; (i = srchdir(dp, fcb, 12, i)) >= 0; i++) {
	BYTE *entry = dp->dir + i*32;
	long ex = (entry[12]&0x1f&~dp->exm) + ((entry[14]&S2MASK) << 5);
	if (ex >= finalex) {
	    finalex = ex;
	    res = ex*16*1024 +
		((dp->exm&entry[12])+1)*16*1024 - (128-entry[15])*128;
	}
    }
    return res;
}

/* Calculate allocated file size.
 * p points to an arbitrary directory entry of the file on disk dp.
 */
static long
sizealloc(BYTE *p, struct mnt *dp)
{
    int i;
    BYTE fcb[16];
    long res = 0;

    for (i = 0; i < 12; i++)
	fcb[i] = p[i]&0x7f;
    i = 0;
    for (i = 0; (i = srchdir(dp, fcb, 12, i)) >= 0; i++) {
	BYTE *entry = dp->dir + i*32;
	res += ((dp->exm&entry[12])+1)*16*1024 - (128-entry[15])*128;
    }
    return res;
}

/* Calculate number of bytes allocated on disk */
static long
inuse(struct mnt *dp)
{
    long blocks = 0;
    unsigned short *ap = (unsigned short *) dp->alv;
    int i;

    for (i = 0; i < dp->dsm/16+1; i++)
	blocks += popcount(*ap++);
    return blocks<<(dp->bsh-3);
}

/* test a file attribute */
#define attr(b,c)	((b&0x80)?c:'-')

static int
dodir(char *tok)
{
    int disk, dn = 0, nmatches;
    char *afn = NULL;
    BYTE fcb[16];
    BYTE *entry;
    struct mnt *dp;
    char wild[8];
    int *list;
    int longform = 0;
    int columns;
    char *def = "eun";			/* default: extension, user, name */
    int k = 0, rows, row;

    while ((tok = strtok(NULL, white)) != NULL) {
	if (*tok == '-')
	    switch (tok[1]) {
	    case 'e':
	    case 'n':
	    case 'u':
		if (k < 3)
		    sortkeys[k++] = tok[1];
		break;
	    case 'l':
		longform = 1;
		break;
	    default:
		fprintf(stderr, "unrecognized option: %s\n", tok);
	    }
	else
	    afn = tok;
    }
    while (k < 3) {
	int d;
	for (d = 0; d < 3; d++) {
	    int j;
	    for (j = 0; j < k; j++)
		if (sortkeys[j] == def[d])
		    break;
	    if (j == k) {
		sortkeys[k++] = def[d];
		break;
	    }
	}
    }
    if (afn == NULL)
	afn = "*.*";
    k = strlen(afn);
    if (k && k < 5 && afn[k-1] == ':') {
	sprintf(wild, "%s*.*", afn);
	disk = makefcb(wild, fcb);
    }
    else
	disk = makefcb(afn, fcb);
    if (disk < 0)
	return 0;
    fcb[13] = '?';			/* any s1 */
    dp = mnttab + disk;
    list = xmalloc(dp->drm * sizeof(dn));
    nmatches = 0;
    while ((dn = srchdir(dp, fcb, 15, dn)) >= 0) {
	if (dp->dir[32*dn] <= MAXUSER)
	    list[nmatches++] = dn;
	dn++;
    }
    sortdisk = dp;
    qsort(list, nmatches, sizeof(dn), dircmp);
    columns = longform ? 2 : 4;
    rows = (nmatches+columns-1)/columns;
    for (row = 0; row < rows; row++) {
	int column;
	for (column = 0; column < columns; column++) {
	    int col;
	    if ((dn = column*rows+row) >= nmatches) {
		putchar('\n');
		column = columns;
		break;
	    }
	    entry = dp->dir + 32*list[dn];
	    if (longform) {
		long t = sizetotal(entry, dp);
		long a = sizealloc(entry, dp);
		printf("%8ld%c %c%c%c%c ", t, t==a ? ' ' : '!',
		       attr(entry[8], 'w'), attr(entry[9], 'r'),
		       attr(entry[10], 's'), attr(entry[11], 'a'));
	    }
	    printf("%2d:", *entry);
	    col = 3 + pname(++entry);
	    if (column == columns-1)
		putchar('\n');
	    else while (col++ < 18)
		putchar(' ');
	}
    }
    free(list);
    printf("%4ldK of %4ldK in use\n", inuse(dp), (dp->dsm+1L)<<(dp->bsh-3));
    return 0;
}

/* CP/M file descriptor */
typedef struct {
    struct mnt *dp;			/* pointer to mount table entry */
    BYTE fcb[16];			/* file name suitable for srchdir() */
    int dirp;				/* index of most recently used extent */
    long pos;				/* current position in file */
    char *block;			/* pointer to data located at pos */
    long len;				/* length of data pointed to by *block */
    char *p;				/* work pointer for putcpm */
} CFILE;

/* Set pointer to memory-mapped location of data corresponding to a given
 * position in a CP/M file.  Sets cf->block to the address and cf->len to the
 * length of valid data at that address.  cf->block is set to NULL if there is
 * no data at that position (due to end of file or a hole in the file) and
 * in this case cf->len is set to the block size as a hint where to look next
 * in case it is a hole.
 * If make is 1, a new block (and extent if needed) is allocated if there is
 * currently no data at the given postion.
 * Returns 0 if ok, 1 if extent not found and 2 if block not found.
 */
static int
getblock(CFILE *cf, int make)
{
    int lex = cf->pos/(16*1024);	/* logical extent */
    int idx = cf->pos >> (cf->dp->bsh + 7); /* block index within file */
    long blockno;			/* block index on disk */
    long offs;				/* offset of pos from block boundary */
    BYTE *entry;			/* pointer to directory entry */
    int mex;				/* last extent contained in entry */

    cf->len = 1<<(cf->dp->bsh+7);	/* block size */
    cf->fcb[12] = lex&0x1f;
    cf->fcb[14] = lex>>5;
    if (cf->dirp >= 0) {		/* is dirp still valid? */
	entry = cf->dp->dir + cf->dirp*32;
	if (((cf->fcb[12] ^ entry[12])&~cf->dp->exm&0x1f) ||
	    ((cf->fcb[14] ^ entry[14])&S2MASK))
	    cf->dirp = -1;
    }
    if (cf->dirp < 0)			/* search for extent */
	cf->dirp = srchdir(cf->dp, cf->fcb, 15, 0);
    if (cf->dirp < 0 && make && ((lex>>5) <= S2MASK) &&
	(cf->dirp = srchdir(cf->dp, (BYTE *)"\xe5", 1, 0)) >= 0) {
	/* allocate a new physical extent */
	entry = cf->dp->dir + cf->dirp*32;
	memcpy(entry, cf->fcb, 15);
	entry[12] = lex&0x1f;
	entry[13] = 0;
	entry[14] = lex>>5;
	entry[15] = 0x80;		/* max record count */
	memclr(entry+16, 16);		/* no blocks yet */
    }
    if (cf->dirp < 0) {			/* eof or hole or directory full */
	cf->block = cf->p = NULL;
	return 1;
    }
    entry = cf->dp->dir + cf->dirp*32;
    mex = ((entry[14]&S2MASK)<<5)+(entry[12]&0x1f);
    if (lex > mex)
	blockno = 0;
    else
	blockno = (cf->dp->dsm < 256) ?
	    entry[16+(idx&0xf)] :
		entry[16+((idx<<1)&0xf)] +
		    (entry[16+((idx<<1)&0xf)+1] << 8);
    if (blockno == 0 && make) {		/* allocate a new block */
	int ip;
	blockno = findfree(cf->dp);
	if (blockno != 0) {
	    entry[12] = lex&0x1f;	/* grab the whole physical extent */
	    entry[13] = 0;
	    entry[14] = lex>>5;
	    entry[15] = 0x80;		/* max record count */
	    if (cf->dp->dsm < 256)
		entry[16+(idx&0xf)] = blockno;
	    else {
		BYTE* where = entry + 16+((idx<<1)&0xf);
		PutWORD(where, blockno);
	    }
	    ip = cf->pos & ((cf->dp->bsh<<7)|0x7f);
	    if (ip)			/* clear unused initial portion */
		memclr(cf->dp->dir + (blockno<<(cf->dp->bsh+7)) - ip, ip);
	}
    }
    if (blockno == 0) {			/* eof or hole or disk full */
	cf->block = cf->p = NULL;
	return 2;
    }
    offs = cf->pos & (cf->len-1);
    cf->len -= offs;
    if (lex == mex && ((cf->pos + cf->len) & 0x3fff) > (entry[15] << 7))
	/* in partial last block of logical extent */
	cf->len = (entry[15] << 7) - (cf->pos & 0x3fff);
    if (cf->len < 0) {
	/* just over the top */
	cf->len = 1<<(cf->dp->bsh+7);
	cf->block = cf->p = NULL;
    }
    else
	cf->block = cf->p = (char *) cf->dp->dir +
	    (blockno<<(cf->dp->bsh+7)) + offs;
    return 0;
}

static int
writeok(struct mnt *dp, int dirp, char *fn)
{
    if ((dp->flags & MNT_RDONLY) || (dp->dir[32*dirp+9]&0x80)) {
	fprintf(stderr, "read-only: %s\n", fn);
	return 0;
    }
    return 1;
}

static int
erase(struct mnt *dp, BYTE *fcb)
{
    int dirp;
    int n = 0;

    for (dirp = 0; (dirp = srchdir(dp, fcb, 12, dirp)) >= 0; dirp++) {
	BYTE *entry = dp->dir + dirp*32;
	alloc(dp, entry, 0);
	*entry = 0xe5;
	n++;
    }
    return n;
}

static int
doerase(char *tok)
{
    int disk, dirp;
    BYTE fcb[16];
    char *fn = strtok(NULL, white);

    if (fn == NULL) {
	fputs("need a filename\n", stderr);
	return 0;
    }
    disk = makefcb(fn, fcb);
    if (disk >= 0 &&
	(dirp = srchdir(mnttab+disk, fcb, 12, 0)) >= 0 &&
	!writeok(mnttab+disk, dirp, fn))
	return 0;
    if (disk < 0 || erase(mnttab+disk, fcb) == 0)
	fprintf(stderr, "no match: %s\n", fn);
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

/* create a new CP/M disk */
static int
docreate(char *tok)
{
    char *fn = NULL;
    FILE *f;
    long s, size = 1024*1024;
    char header[128];
    BYTE sector[128];
    long al01, dsm, spt = -1, bsize = -1, drm = -1, offs = -1;
    int dblocks;

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
    memset(sector, 0xe5, sizeof sector);

    if (size < 256*1024) {
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
    memclr(header, sizeof header);
    sprintf(header, "<CPM_Disk>");
    PutWORD(header+32, spt);
    header[34] = popcount(bsize-1)-7;	/* bsh */
    header[35] = (bsize/128-1);		/* blm */
    header[36] = dsm < 256 ? bsize/1024-1 : bsize/2048-1; /* exm */
    PutWORD(header+37, dsm);
    PutWORD(header+39, drm);
    al01 = ~((1<<(16-dblocks))-1);
    header[41] = al01>>8;
    header[42] = al01;
    PutWORD(header+45, offs);
    if (fwrite(header, sizeof header, 1, f) == 0) {
	perror(fn);
	return 0;
    }
    s = offs*spt*128 + (drm+1)*32;
    while (s > 0) {
	if (fwrite(sector, sizeof sector, 1, f) == 0) {
	    perror(fn);
	    return 0;
	}
	s -= sizeof sector;
    }
    if (fseek(f, sizeof header + size - sizeof sector, SEEK_SET) < 0 ||
	fwrite(sector, sizeof sector, 1, f) == 0 ||
	fclose(f) != 0)
	perror(fn);
    return 0;
}

/* Close out an extent.  Must be called with valid cf values */
static void
trunc(CFILE *cf)
{
    int lex = cf->pos/(16*1024);	/* logical extent */

    if (cf->dirp >= 0) {
	BYTE *entry = cf->dp->dir + cf->dirp*32;
	entry[12] = lex&0x1f;
	entry[14] = lex>>5;
	entry[15] = ((cf->pos+127)>>7)&0x7f;
    }
}

/* copy CP/M file to CP/M file */
static void
copycc(char *fn1, char *fn0)
{
    CFILE cf[2];			/* copy from [1] to [0] */
    int d0, d1;
    long size;

    if ((d1 = makefcb(fn1, cf[1].fcb)) < 0) {
	fprintf(stderr, "illegal name: %s\n", fn1);
	return;
    }
    if ((d0 = makefcb(fn0, cf[0].fcb)) < 0) {
	fprintf(stderr, "illegal name: %s\n", fn0);
	return;
    }
    cf[1].dp = mnttab + d1;
    cf[1].dirp = srchdir(cf[1].dp, cf[1].fcb, 15, 0);
    if (cf[1].dirp < 0) {
	fprintf(stderr, "no file: %s\n", fn1);
	return;
    }
    cf[0].dp = mnttab + d0;
    cf[0].dirp = srchdir(cf[0].dp, cf[0].fcb, 12, 0);
    if (cf[0].dirp >= 0 && !writeok(cf[0].dp, cf[0].dirp, fn0))
	return;
    if (cf[1].dp == cf[0].dp && cf[1].dirp == cf[0].dirp) {
	fprintf(stderr, "same file: %s %s\n", fn1, fn0);
	return;
    }
    if (cf[0].dirp >= 0)
	erase(cf[0].dp, cf[0].fcb);
    cf[1].pos = 0;
    cf[0].pos = cf[0].len = 0;
    cf[0].dirp = -1;
    for (size = sizetotal(cf[1].fcb, cf[1].dp); size > 0; ) {
	int minlen;
	getblock(&cf[1], 0);
	if (cf[1].block == NULL) {
	    cf[1].pos += 1024;
	    size -= 1024;
	    continue;
	}
	if (cf[0].pos != cf[1].pos)
	    trunc(&cf[0]);
	cf[0].pos = cf[1].pos;
	switch (getblock(&cf[0], 1)) {
	case 0:
	    break;
	case 1:
	    fprintf(stderr, "No space in directory for all of %s\n", fn0);
	    return;
	case 2:
	    fprintf(stderr, "No space on disk for all of %s\n", fn0);
	    return;
	}
	minlen = (cf[1].len < cf[0].len) ? cf[1].len : cf[0].len;
	memcpy(cf[0].block, cf[1].block, minlen);
	size -= minlen;
	cf[1].pos += minlen;
	cf[0].pos += minlen;
    }
    trunc(&cf[0]);
}

static int
putcpm(int ch, CFILE *cf)
{
    int r = 0;

    if (cf->dirp < 0 || cf->p >= cf->block + cf->len)
	r = getblock(cf, 1);
    if (r == 0) {
	*cf->p++ = ch;
	cf->pos++;
    }
    return r;
}

/* copy unix file to CP/M file */
static void
copyuc(char *fn1, char *fn0)
{
    FILE *uf;
    CFILE cf;
    int ch, d;
    int text = toupper(*fn1) == 'T';

    if ((uf = fopen(fn1+2, "r")) == NULL) {
	perror(fn1+2);
	return;
    }
    if ((d = makefcb(fn0, cf.fcb)) < 0) {
	fprintf(stderr, "illegal name: %s\n", fn0);
	return;
    }
    cf.dp = mnttab + d;
    cf.dirp = srchdir(cf.dp, cf.fcb, 12, 0);
    if (cf.dirp >= 0 && !writeok(cf.dp, cf.dirp, fn0))
	return;
    if (cf.dirp >= 0)
	erase(cf.dp, cf.fcb);
    cf.pos = 0;
    cf.dirp = -1;
    while ((ch = getc(uf)) != EOF) {
	int r;
	if ((text && ch == '\n' && (r = putcpm('\r', &cf)) != 0) ||
	    (r = (putcpm(ch, &cf) != 0))) {
	    fclose(uf);
	    if (r == 1)
		fprintf(stderr, "No space in directory for all of %s\n", fn0);
	    else
		fprintf(stderr, "No space on disk for all of %s\n", fn0);
	    return;
	}
    }
    fclose(uf);
    while (cf.pos & 0x7f)
	putcpm(0x1a, &cf);
    trunc(&cf);
}

/* copy CP/M file to unix file
	dirp >= 0 : skip srchdir() */
static void
copycu(char *fn1, char *fn0, int dirp)
{
    CFILE cf;
    FILE *uf;
    int d, len;
    long size;
    int text = toupper(*fn0) == 'T';

    if ((d = makefcb(fn1, cf.fcb)) < 0) {
	fprintf(stderr, "illegal name: %s\n", fn1);
	return;
    }
    cf.dp = mnttab + d;
    cf.dirp = (dirp >= 0) ? dirp : srchdir(cf.dp, cf.fcb, 15, 0);
    if (cf.dirp < 0) {
	fprintf(stderr, "no file: %s\n", fn1);
	return;
    }
    if ((uf = fopen(fn0+2, "w")) == NULL) {
	perror(fn0+2);
	return;
    }
    cf.pos = 0;
    for (size = sizetotal(cf.fcb, cf.dp); size > 0; ) {
	getblock(&cf, 0);
	len = cf.len;
	cf.pos += len;
	if (size < len)
	    len = size;
	size -= len;
	while (len-- > 0) {
	    if (cf.block == NULL)
		putc(0, uf);
	    else {
		int ch = *cf.block++;
		if (text && ch == 0x1a) {
		    size = 0;
		    break;
		}
		if (!text)
		    putc(ch, uf);
		else {
		    ch &= 0x7f;
		    if (ch != '\r')
			putc(ch, uf);
		}
	    }
	}
	if (ferror(uf))
	    break;
    }
    if (ferror(uf) || fclose(uf) != 0)
	perror(fn0);
}

#define BAD_FN	0
#define CPM_UFN	1
#define CPM_AFN	2
#define UX_RFN	3
#define UX_DIR	4

static int
checkname(char *name, int optional)
{
    char first = toupper(name[0]);
    int i;
    BYTE fcb[16];
    struct stat st;

    if ((first == 'T' || first == 'U') && name[1] == ':') {
	if (strchr(name, '?') || strchr(name, '*')) {
	    fprintf(stderr, "ambiguous: %s\n", name);
	    return BAD_FN;
	}
	if (stat(name+2, &st) < 0) {
	    if (optional)
		return UX_RFN;
	    else {
		perror(name);
		return BAD_FN;
	    }
	}
	return (st.st_mode & S_IFMT) == S_IFDIR ? UX_DIR : UX_RFN;
    }
    if (makefcb(name, fcb) < 0) {
	fprintf(stderr, "illegal name: %s\n", name);
	return BAD_FN;
    }
    for (i = 0; i < 16; i++)
	if (fcb[i] == '?')
	    return CPM_AFN;
    return CPM_UFN;
}

static int
spname(char *s, BYTE *p)
{
    int c;
    int i;
    char *start = s;

    for (i = 0; i < 8; i++, p++)
	if ((c = *p) != ' ') {
	    c &= 0x7f;
	    if (isupper(c))
		c = tolower(c);
	    *s++ = c;
	}
    if (*p != ' ') {
	*s++ = '.';
	for (i = 0; i < 3; i++, p++)
	    if ((c = *p) != ' ') {
		c &= 0x7f;
		if (isupper(c))
		    c = tolower(c);
		*s++ = c;
	    }
    }
    *s = '\0';
    return s - start;			/* strlen */
}

/* TODO: be more permissive about ambiguous names */
static int
docp(char *tok)
{
    char *fn1 = strtok(NULL, white);	/* full name 1 */
    char *fn1n;				/* name 1 without drive/user */
    char *fn2, *p = NULL;		/* full name 2 and stack copy */
    int t1, t2;				/* file types */

    if (fn1 == NULL) {
	fputs("need at least one filename\n", stderr);
	return 0;
    }
    if ((t1 = checkname(fn1, 0)) == BAD_FN)
	return 0;
    if ((fn1n = strchr(fn1, ':')) == NULL)
	fn1n = fn1;
    else
	fn1n++;
    if ((fn2 = strtok(NULL, white)) == NULL)
	fn2 = p = newstr(fn1n);
    else {
	int len2 = strlen(fn2);
	if (fn2[len2-1] == ':') {
	    p = xmalloc(len2+strlen(fn1n)+1);
	    sprintf(p, "%s%s", fn2, fn1n);
	    fn2 = p;
	}
    }
    t2 = checkname(fn2, 1);
    if (t2 == CPM_AFN || t1 == UX_DIR) {
	fprintf(stderr, "ambiguous: %s\n", fn1);
	if (p) free(p);
	return 0;
    }
    switch (t2) {
    case BAD_FN:
	break;
    case CPM_UFN:
	switch (t1) {
	case CPM_AFN:
	    fprintf(stderr, "ambiguous: %s\n", fn1);
	    break;
	case CPM_UFN:
	    copycc(fn1, fn2);
	    break;
	case UX_RFN:
	    copyuc(fn1, fn2);
	    break;
	}
	break;
    case UX_DIR:
	switch (t1) {
	case CPM_AFN:
	{
	    int disk, dn = 0, nmatches;
	    char *afn;
	    BYTE fcb[16];
	    BYTE *entry;
	    struct mnt *dp;
	    char wild[8];
	    int k = 0;
	    int len1;
	    char mfn1[18];			/* matching [duu:]filename */
	    char *mfn1n;			/* matching filename */
	    char *mfn2;				/* UNIX dst dir + mfn1n */
	    char *p1;

	    strcpy(mfn1, fn1);
	    mfn1n = mfn1 + (fn1n - fn1);
	    afn = fn1;
	    k = strlen(afn);
	    if (k && k < 5 && afn[k-1] == ':') {
		sprintf(wild, "%s*.*", afn);
		disk = makefcb(wild, fcb);
	    }
	    else
		disk = makefcb(afn, fcb);
	    if (disk < 0)
		return 0;
	    dp = mnttab + disk;
	    nmatches = 0;
	    while ((dn = srchdir(dp, fcb, 15, dn)) >= 0) {
		entry = dp->dir + 32*dn;
		if (*entry <= MAXUSER) {

		    /* convert fcb name/type to lc string */

		    len1 = spname(mfn1n, entry+1);

		    /* append filename string to UNIX dirname */

		    p1 = xmalloc(strlen(fn2)+len1+2);
		    sprintf(p1, "%s/%s", fn2, mfn1n);
		    if (p) free(p);
		    mfn2 = p = p1;

		    /* make sure dst is not a UNIX directory */

		    if (checkname(mfn2, 1) == UX_DIR)
			fprintf(stderr, "dest is dir, exists: %s\n", mfn2);
		    else
			copycu(mfn1, mfn2, dn);
		    nmatches++;
		}
		dn++;
	    }
	    if (nmatches == 0)
		fprintf(stderr, "no matches: %s\n", fn1);
	    break;
	}
	case CPM_UFN:
	{
	    char *p1 = xmalloc(strlen(fn2)+strlen(fn1n)+2);

	    sprintf(p1, "%s/%s", fn2, fn1n);
	    if (p) free(p);
	    fn2 = p = p1;
	    if (checkname(fn2, 1) == UX_DIR) {
		fprintf(stderr, "dest is dir, exists: %s\n", fn2);
		if (p) free(p);
		return 0;
	    }
	    copycu(fn1, fn2, -1);
	    break;
	}
	case UX_RFN:
	    fputs("use !cp\n", stderr);
	    break;
	}
	break;
    case UX_RFN:
	switch (t1) {
	case CPM_AFN:
	    fprintf(stderr, "ambiguous: %s\n", fn1);
	    break;
	case CPM_UFN:
	    copycu(fn1, fn2, -1);
	    break;
	case UX_RFN:
	    fputs("use !cp\n", stderr);
	    break;
	}
    }
    if (p) free(p);
    return 0;
}

static int
dotype(char *tok)
{
    char *fn = strtok(NULL, white);
    CFILE cf;
    FILE *pf;
    int d, len;
    long size;
    char *pager;
    void (*sigint)(int);

    if (fn == NULL) {
	fputs("need a filename\n", stderr);
	return 0;
    }
    if ((d = makefcb(fn, cf.fcb)) < 0) {
	fprintf(stderr, "illegal name: %s\n", fn);
	return 0;
    }
    cf.dp = mnttab + d;
    cf.dirp = srchdir(cf.dp, cf.fcb, 15, 0);
    if (cf.dirp < 0) {
	fprintf(stderr, "no file: %s\n", fn);
	return 0;
    }
    size = sizetotal(cf.fcb, cf.dp);
    if (size == 0)
	return 0;
    if ((pager = getenv("PAGER")) == NULL)
	pager = "more";
    (void) signal(SIGPIPE, SIG_IGN);
    sigint = signal(SIGINT, SIG_IGN);
    if ((pf = popen(pager, "w")) == NULL) {
	fprintf(stderr, "can't execute %s\n", pager);
	return 0;
    }
    cf.pos = 0;
    while (size > 0) {
	getblock(&cf, 0);
	len = cf.len;
	cf.pos += len;
	if (size < len)
	    len = size;
	size -= len;
	while (len-- > 0) {
	    if (cf.block == NULL)
		putc(0, pf);
	    else {
		int ch = *cf.block++;
		if (ch == 0x1a) {
		    size = 0;
		    break;
		}
		if (ch != '\r')
		    putc(ch, pf);
	    }
	}
	if (ferror(pf))
	    break;
    }
    (void) pclose(pf);
    (void) signal(SIGINT, sigint);
    (void) signal(SIGPIPE, SIG_DFL);
    return 0;
}

static int
dorename(char *tok)
{
    char *new = strtok(NULL, white);
    char *old;
    int dnew, dold, dirp, i;
    BYTE fcbnew[16], fcbold[16];
    struct mnt *dp;

    if ((old = strchr(new, '=')) != NULL)
	*old++ = 0;
    else {
	char *t = strtok(NULL, white);
	if (t) {
	    old = new;
	    new = t;
	}
	else {
	    fputs("need 2 filenames\n", stderr);
	    return 0;
	}
    }
    dnew = makefcb(new, fcbnew);
    dold = makefcb(old, fcbold);
    if (dnew < 0 || dold < 0 ||
	strchr((char *) fcbnew, '?') || strchr((char *) fcbold, '?')) {
	fputs("bad name\n", stderr);
	return 0;
    }
    if (dold != dnew && strchr(new, ':') && strchr(old, ':')) {
	fputs("can't rename to different disk\n", stderr);
	return 0;
    }
    if (strchr(new, ':'))
	dold = dnew;
    if (memcmp(fcbold, fcbnew, 12) == 0) {
	fputs("same name\n", stderr);
	return 0;
    }
    dp = mnttab + dold;
    if ((dirp = srchdir(dp, fcbnew, 12, 0)) >= 0 &&
	!writeok(dp, dirp, new))
	return 0;
    if (dirp >= 0)
	erase(dp, fcbnew);
    for (dirp = 0; dirp <= dp->drm; dirp++) { /* for each directory entry */
	BYTE *entry = dp->dir + dirp*32;
	for (i = 0; i < 12; i++)	/* does it match the old file? */
	    if ((entry[i] ^ fcbold[i]) & 0x7f)
		break;
	if (i != 12)
	    continue;
	for (i = 0; i < 12; i++)	/* change the name, leave the attributes */
	    entry[i] = fcbnew[i] | (entry[i] & 0x80);
    }
    return 0;
}

static int
doshell(char *cmd)
{
    void (*sigint)(int);
    char *shell = getenv("SHELL");

    if (shell == NULL)
	shell = "/bin/sh";
    sigint = signal(SIGINT, SIG_IGN);
    if (cmd[1])
	system(cmd+1);
    else
	system(shell);
    (void) signal(SIGINT, sigint);
    return 0;
}

static int
doquit(char *tok)
{
    if (strcmp(tok, "quit") == 0)
	exit(0);
    else
	fprintf(stderr, "command not recognized: %s\n", tok);
    return 0;
}

static int dohelp(char *tok);

typedef struct {
    char *name;				/* User printable name of the function. */
    int (*func)(char *);		/* Function to call to do the job. */
    char *doc;				/* Short documentation.  */
    char *detail;			/* Long documentation. */
} COMMAND;

COMMAND commands[] = {
{ "help",   dohelp,   "Display this text or give help about a command",
      "help <cmd> displays more information about <cmd>" },
{ "?",      dohelp,   "Synonym for `help'", NULL },
{ "mount",  domount,  "Mount a unix file as a CP/M disk",
      "mount without arguments lists the mount table\n"
      "mount -v  lists the mount table verbosely\n"
      "mount <drive> <file> mounts <file> as CP/M disk <drive> (a letter from a..p)\n"
      "mount -r <drive> <file> mounts the <file> as a read/only disk" },
{ "umount", doumount, "Unmount a CP/M disk",
      "umount <drive> closes the file associated with <drive> and frees the resources" },
{ "dir",    dodir,    "Display directory",
      "dir       lists all files on the current disk / user number\n"
      "dir <afn> lists files matching the (possibly) ambiguous file name\n"
      "dir -l    (with or without <afn>) lists the files with their size\n"
      "          and attributes\n"
      "dir -eun  sort list by extension / user number / name (choose any order)" },
{ "ls",     dodir,    "Synonym for `dir'", NULL },
{ "era",    doerase,  "Erase CP/M file(s)",
      "era <afn>     erase files matching <afn>" },
{ "cp",     docp,     "Copy a file",
      "cp <ufn1> <ufn2>     copy CP/M files <ufn1> to <ufn2>\n"
      "cp u:<path> <ufn>    copy unix <path> to CP/M binary file <ufn>\n"
      "cp t:<path> <ufn>    copy unix <path> to CP/M text file <ufn>\n"
      "cp <ufn> u:<path>    copy binary CP/M file <ufn> to unix <path>\n"
      "cp <ufn> t:<path>    copy text CP/M file <ufn> to unix <path>\n"
      "cp <afn> u:<dir>     copy binary CP/M file(s) <afn> to unix <dir>\n"
      "cp <afn> t:<dir>     copy text CP/M file(s) <afn> to unix <dir>" },
{ "rename", dorename, "Rename a CP/M file",
      "rename <newname>=<oldname>\n"
      "rename <oldname> <newname>" },
{ "type",   dotype,   "Pipe a text file through $PAGER", NULL },
{ "create", docreate, "Create a new disk",
      "create {flags} <file> {size}  creates a unix <file> initialized as a\n"
      "                              CP/M disk of size {size} (default 1MB).\n"
      "       -b <block size>        default 1024 if size < 256K, else 2048\n"
      "       -d <# dir entries - 1> default 1023\n"
      "       -o <track offset>      default 0\n"
      "       -s <sectors per track> default 26" },
{ "!",      doshell,  "Execute a unix command",
      "!          escape to a unix shell\n"
      "!cmd       execute unix cmd" },
{ "quit",   doquit,   "Terminate cdm", NULL },
{ NULL, NULL, NULL, NULL }
};

static int
dohelp(char *tok)
{
    int tlen;
    COMMAND *cp;

    tok = strtok(NULL, white);
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
    if (*cmd == 0 || *cmd == '#')
	return 0;
    add_history(cmd);
    for (tok = cmd; *tok; tok++)
	if (*tok == ' ' || *tok == '\t')
	    break;
    if (*tok == 0 && *--tok == ':') { /* change disk/user */
	int d, u;
	*tok = 0;
	if (!diskuser(cmd, &d, &u))
	    goto bad;
	if (d >= 0) {
	    if (mnttab[d].flags & MNT_ACTIVE)
		curdisk = d;
	    else {
		fprintf(stderr, "disk %c not mounted\n", 'A'+d);
		return 0;
	    }
	}
	if (u >= 0)
	    curuser = u;
	return 0;
    }
    if (*cmd == '!') {
	/* special case */
	doshell(cmd);
	return 0;
    }
    tok = strtok(cmd, white);
    if (tok == NULL || *tok == 0)
	return 0;
    for (tlen = strlen(tok), cp = commands; cp->name; cp++)
	if (strncmp(tok, cp->name, tlen) == 0) {
	    if (func)
		goto bad;		/* ambiguous */
	    else
		func = cp->func;
	}
    if (func)
	return func(tok);
 bad:
    fprintf(stderr, "command not recognized: %s\n", tok);
    return 0;
}

static char *
prompt(void)
{
    static char buf[10];

    if (curdisk < 0 || curdisk > 15)
	return "$>";
    sprintf(buf, "%c%d>", 'A'+curdisk, curuser);
    return buf;
}

int
main(int argc, char **argv)
{
    int d = 0;
    static char *cmd = NULL;

    puts("\nCP/M Disk Manager " VERSION
	 ", Copyright 1995 Frank D. Cringle.\n"
	 "Portions copyright (C) 2004 Carl Mascott.\n"
	 "cdm comes with ABSOLUTELY NO WARRANTY; for details\n"
	 "see the file \"COPYING\" in the distribution directory.\n");

    while (--argc)
	mount(d++, *++argv, 0);
#ifdef USE_GNU_READLINE
    do {
	if (cmd) {
	    free(cmd);
	    cmd = NULL;
	}
	cmd = readline(prompt());
	if (cmd == NULL)
	    putchar('\n');
    } while (!docmd(cmd));
#else
    if (cmd == NULL)
	cmd = xmalloc(BUFSIZ);
    do {
	fputs(prompt(), stdout);
	fflush(stdout);
	if (fgets(cmd, BUFSIZ-1, stdin) == NULL) {
	    putchar('\n');
	    cmd[0] = 0;
	}
    } while (!docmd(cmd));
#endif
    exit(0);
}

#ifndef USE_GNU_READLINE
void *
xmalloc(size_t size)
{
    void *p = malloc(size);

    if (p == NULL) {
	fputs("insufficient memory\n", stderr);
	exit(1);
    }
    return p;
}
#endif
