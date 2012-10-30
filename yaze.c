/* yaze - yet another Z80 emulator.
   Copyright (C) 1995,1998  Frank D. Cringle.

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
#include <sys/types.h>
#include <sys/stat.h>
#include "simz80.h"
#include "yaze.h"

/* 	$Id: yaze.c,v 1.5 2004/04/23 10:05:25 fdc Exp $	 */

#ifndef lint
static char vcid[] = "$Id: yaze.c,v 1.5 2004/04/23 10:05:25 fdc Exp $";
#endif /* lint */


#define VERSION	"1.14"

/* Z80 registers */
WORD af[2];			/* accumulator and flags (2 banks) */
int af_sel;			/* bank select for af */

struct ddregs regs[2];		/* bc,de,hl */
int regs_sel;			/* bank select for ddregs */

WORD ir;			/* other Z80 registers */
WORD ix;
WORD iy;
WORD sp;
WORD pc;
WORD IFF;
WORD IM;

BYTE ram[MEMSIZE*1024];		/* Z80 memory space */
#ifdef MMU
BYTE *pagetable[16];		/* MMU page table */
#endif

#ifndef LIBDIR
#define LIBDIR "/usr/local/lib/"
#endif

static char *bootfile = "yaze.boot";
static char *startup = ".yazerc";
static char *progname;
static long loadadr = -1;
#ifdef BIOS
static long basepage = 0xe4;
static int vflag;
long z3env;

WORD	ccp_base;		/* base address of ccp */
WORD	bdos_base;		/* base address of bdos */
WORD	bios_base;		/* base address of bios */
WORD	bios_top;		/* end of bios, start of global work area */

WORD	dirbuff;		/* common directory buffer for all disks */
WORD	dptable;		/* base of disk parameter table */
BYTE	*global_alv;		/* global allocation vector */
#endif

static void
usage(void)
{
    fprintf(stderr,
            "Usage: %s {flags} {commands}\n"
            "           -b <file>       boot from this file\n"
            "           -l <xxxx>       load bootfile at given (hex) address\n"
#ifdef BIOS
            "           -p <xx>         base page (top 2 hex digits of ccp base)\n"
#endif
            "           -s <file>       startup file\n"
#ifdef BIOS
            "           -v              verbose\n"
            "           -z <xxxx>       (hex) address of z3env (if desired)\n\n"
#endif
            ,progname);
    exit (1);
}

/* This routine loads the bootstrap file into the simulated processor's ram.

   If a load address is given with the -l flag, the boot file is
   loaded to that address, the z80 pc is set to it and hl is set to
   the ccp base address.  The bootfile might be a standalone program
   which ignores the bios vectors or it might contain a
   self-relocating ccp/bdos (clone), in which case it should relocate
   itself and jump to the bios cold boot vector.

   Otherwise, the boot file can contain 1 or 2 copies of cp/m.  If it
   contains 1 copy the specified or default basepage must correspond
   to the run-time location of the cp/m image.  If it contains 2
   copies of the same code assembled to run at different locations,
   the code can and will be relocated to fit the basepage in use. */

static void
load_cpm(const char *bfname)
{
    FILE *cpm;
#ifdef BIOS
    int i, skip = 0;
    WORD offs;
    struct stat	sb;
#endif

    if ((cpm = fopen(bfname, "r")) == NULL) {
	char *p = xmalloc(strlen(bfname) + strlen(LIBDIR) + 1);
	strcpy(p, LIBDIR);
	strcat(p, bfname);
	if ((cpm = fopen(p, "r")) == NULL)
	    free(p);
	else
	    bfname = p;
    }
    if (cpm == NULL) {
	perror(bfname);
	exit(1);
    }
    if (loadadr >= 0) {
	fread(ram+loadadr, 1, 64*1024-loadadr, cpm);
	fclose(cpm);
	pc = loadadr;
#ifdef BIOS
	regs[regs_sel].hl = ccp_base;
#endif
	return;
    }
#ifdef BIOS
    if (fstat(fileno(cpm), &sb) != 0) {
	perror(bfname);
	exit(1);
    }
    if ((sb.st_size >= CPM_LENGTH+0x880 && sb.st_size < 2*CPM_LENGTH) ||
	(sb.st_size >= 2*CPM_LENGTH+0x880)) {
	skip = 0x880;
	fseek(cpm, skip, SEEK_SET);
    }
    fread(ram + ccp_base, CPM_LENGTH, 1, cpm);
    if (sb.st_size < 2*CPM_LENGTH) {
	/* single, non-relocatable copy */
	if (ram[ccp_base] == 0xc3 && (ram[ccp_base+2]&0xfc) != basepage)
	    printf("Warning: %s appears to need -p %2x\n", bfname,
		   ram[ccp_base+2]&0xfc);
    }
    else {
	if ((sb.st_size & 1) || ram[ccp_base] != 0xc3) {
	    fprintf(stderr, "%s does not appear to contain 2 cp/m images\n", bfname);
	    exit(1);
	}
	fseek(cpm, skip + sb.st_size/2, SEEK_SET);
	/* this loses if the initial jump points into the 2nd half of ccp */
	offs = (ccp_base >> 8) - (ram[ccp_base + 2] & 0xfc);
	for (i = 0; i < CPM_LENGTH; i++) {
	    if ((BYTE) getc(cpm) != ram[ccp_base + i])
		ram[ccp_base + i] += offs;
	}
    }
    fclose(cpm);
    printf("Running '%s'\n", bfname);
    ram[3] = 0x95;			/* iobyte */
    ram[4] = 0;				/* disk A: */
    pc = bios_base;
#endif
}

#ifdef BIOS
#define N_VEC	17			/* number of bios entry points */

static void
setvectors(void)
{
    int i;
    long cpm_brk = 64*1024;

    if (z3env == cpm_brk - 1024)	/* z3env at top of memory? */
	cpm_brk = z3env;
    cpm_brk -= 128;			/* space for directory buffer */
    dirbuff = cpm_brk;
    cpm_brk -= 16*(16+15);		/* space for 16 disk parameter headers
					   and disk parameter blocks */
    dptable = cpm_brk;

    if (basepage*256 + CPM_LENGTH + 6*N_VEC >= dptable) {
	fprintf(stderr, "Not enough space in simulated ram to accommodate base at %02lX\n",
		basepage);
	exit(1);
    }

    for (i = 0; i < N_VEC; i++) { /* build bios jump table */
	BYTE *p = ram + bios_base + 3*i;
	p[0] = 0xc3;		/* jump */
	p[1] = 3*(i+N_VEC);
	p[2] = (bios_base >> 8);
	p[3*N_VEC] = 0x76;	/* halt */
	p[3*N_VEC+1] = i;	/* function code */
	p[3*N_VEC+2] = 0xc9;	/* return */
    }
    bios_top = bios_base + 6*N_VEC;
    i = (dptable - bios_top + 7) / 8;
    global_alv = xmalloc(i);
    memclr(global_alv, i);
}
#endif

/* getopt declarations */
extern int getopt();
extern char *optarg;
extern int optind, opterr, optopt;

int
main(int argc, char **argv)
{
    int c, gostart;
#ifdef BIOS
    int goopt;
#endif

    progname = argv[0];

    puts("\nYet Another Z80 Emulator version " VERSION
	 ", Copyright 1995,1998 Frank D. Cringle.\n"
	 "yaze comes with ABSOLUTELY NO WARRANTY; for details\n"
	 "see the file \"COPYING\" in the distribution directory.\n");

    while ((c = getopt(argc, argv, "b:l:s:"
#ifdef BIOS
                                           "p:vz:"
#endif
                                                  )) != EOF)
	switch (c) {
	case 'b':
	    bootfile = optarg;
	    break;
	case 'l':
	    loadadr = strtol(optarg, NULL, 16);
	    break;
	case 's':
	    startup = optarg;
	    break;
#ifdef BIOS
	case 'p':
	    basepage = strtol(optarg, NULL, 16);
	    break;
	case 'v':
	    vflag = 1;
	    break;
	case 'z':
	    z3env = strtol(optarg, NULL, 16);
	    break;
#endif
	case '?':
	    usage();
	}

#ifdef MMU
    for (c=0; c<16; ++c) pagetable[c]=ram+(c<<12);
#endif
#ifdef BIOS
    ccp_base = basepage * 256;
    bdos_base = ccp_base + CCP_LENGTH;
    bios_base = bdos_base + BDOS_LENGTH;
    if (z3env == bios_base)
	bios_base += 1024;
#endif

    load_cpm(bootfile);
#ifdef BIOS
    setvectors();

    if (vflag) {
	printf("bootfile:	%s\n", bootfile);
	if (loadadr >= 0)
	    printf("loadadr:	%4lx\n", loadadr);
	printf("startup file:	%s\n"
	       "basepage:	%2lx\n"
	       "ccp_base:	%4hx\n"
	       "bdos_base:	%4hx\n"
	       "bios_base:	%4hx\n"
	       "bios_top:	%4hx\n",
	       startup, basepage, ccp_base,
	       bdos_base, bios_base, bios_top);
	if (z3env)
	    printf("z3env:		%4lx\n", z3env);
	printf("dptable:	%4hx\n"
	       "dirbuf:		%4hx\n",
	       dptable, dirbuff);
    }
#endif

    gostart = bios_init(startup);

#ifdef BIOS
    for (goopt = 0; !goopt && optind < argc; optind++)
	goopt = docmd(argv[optind]);

    if (!gostart && !goopt)
	monitor(0);
#endif

#ifdef DEBUG
    do {
	FASTWORK adr = simz80(pc);
	if (adr & 0x10000)
	    monitor(adr);
	else
	    bios(ram[adr]);
    } while (1);
#elif !BIOS
    simz80(pc);
    fprintf(stderr,"HALT\n\r");
    fprintf(stderr,"PC   SP   IR   IX   IY   AF   BC   DE   HL   AF'  BC'  DE'  HL'\n\r");
    fprintf(stderr,"%04x %04x %04x %04x %04x %04x %04x %04x %04x %04x %04x %04x %04x\n\r",pc,sp,ir,ix,iy,af[af_sel],regs[regs_sel].bc,regs[regs_sel].de,regs[regs_sel].hl,af[1-af_sel],regs[1-regs_sel].bc,regs[1-regs_sel].de,regs[1-regs_sel].hl);
    for (c=0; c<16; ++c) fprintf(stderr,"F%-4x",c);
    fprintf(stderr,"\r\n");
    for (c=0; c<16; ++c) fprintf(stderr,"%s%04x",c?" ":"",(pagetable[c]-ram)>>12);
    fprintf(stderr,"\r\n");
    exit(0);
#else
    do bios(ram[simz80(pc)]); while (1);
#endif
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
