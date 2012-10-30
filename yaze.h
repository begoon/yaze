/* Header file for the main program.
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

/* 	$Id: yaze.h,v 1.2 2004/01/11 16:11:17 fdc Exp $	 */

#define	CCP_LENGTH	0x800
#define BDOS_LENGTH	0xe00
#define CPM_LENGTH	(CCP_LENGTH + BDOS_LENGTH)

extern WORD	ccp_base;		/* base address of ccp */
extern WORD	bdos_base;		/* base address of bdos */
extern WORD	bios_base;		/* base address of bios */
extern WORD	bios_top;		/* end of bios, start of
					   global work area */

extern WORD	dirbuff;		/* common directory buffer for
					   all disks */
extern WORD	dptable;		/* base of disk parameter
					   headers table */
extern BYTE	*global_alv;		/* global allocation vector */

extern long	z3env;			/* z-system environment (none if z3env==0) */

extern void bios(int func);
extern int  bios_init(const char *initfile);
extern int  docmd(char *cmd);
extern void  monitor(FASTWORK adr);
extern void *xmalloc(size_t size);
extern char *newstr(const char *str);

#ifdef USE_GNU_READLINE
#include <readline/readline.h>
void add_history(char *cmd);
#else
#define add_history(x)
#endif

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
