/* Notes */ /*{{{C}}}*//*{{{*/
/*

(c) 1996-1999 by Michael Haardt (michael@moria.de)
yaze comes with ABSOLUTELY NO WARRANTY; for details see the file "COPYING"
in the distribution directory.

This module implements idealised hardware similar to Udo Munk's Z80-PC
simulator, thus turning yaze into a Z80-PC simulator instead of a Z80
simulator with integrated BIOS emulation.  You can get kits for a CP/M
2.2 compatible OS from http://www.moria.de/yaze-p2dos/ and for CP/M 3.0
from http://www.moria.de/~michael/yaze-cpm3/.

The simulated disk drives' geometry must match the DPB in the BIOS.  A:
and B: are 77 track/26 sector 256KB drives, whereas C: and D: are 1024
track/32 sector 4MB drives.

The rc file supports a similar syntax to yaze BIOS emulator, that is:

mount <CP/M block device> <file> (mount a p2dos)
attach <CP/M character device> <file> (attach lst printout)
attach <CP/M character device> <TCP port> (attach rdr 3000)

If conin/conout (rdr/pun) have the same name, they will share
the file or port.  The following will run yaze with the serial
line connected to a TCP port:

attach rdr 3000
attach pun 3000

When a TCP connection is closed, control-z will be sent to the
simulator, if it still reads characters.

*/

/* 	$Id: io.c,v 1.2 2004/01/11 16:11:17 fdc Exp $	 */

#ifndef lint
static char vcid[] = "$Id: io.c,v 1.2 2004/01/11 16:11:17 fdc Exp $";
#endif /* lint */

/*}}}*/
/* #includes */ /*{{{*/
#include <arpa/telnet.h>
#include <netinet/in.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/socket.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <time.h>
#include <termios.h>
#include <unistd.h>

#include "simz80.h"
#include "yaze.h"
/*}}}*/
/* #defines */ /*{{{*/
/* ports */
#define CONS   0
#define COND   1
#define PRTS   2
#define PRTD   3
#define AUXS   4
#define AUXD   5
#define FDCD  10
#define FDCTL 11
#define FDCTH 12
#define FDCS  13
#define FDCO  14
#define FDCX  15
#define DMAL  16
#define DMAH  17
#define PAGEL 20
#define PAGEH 21
#define FRAME 22
#define CLKC  25
#define CLKD  26
#define HALT  255

/* commands to FDCO port */
#define FDCO_READ      0
#define FDCO_WRITE     1

/* status of FDCX port */
#define FDCX_OK        0
#define FDCX_NODRIVE   1
#define FDCX_NOTRACK   2
#define FDCX_NOSECTOR  3
#define FDCX_SEEKFAIL  4
#define FDCX_READFAIL  5
#define FDCX_WRITEFAIL 6
#define FDCX_NOCMD     7
#define FDCX_DMAFAIL   8

/* number of disk drives */
#define DISKS          4

/* after how many successless syscalls should we sleep? */
#define BENICE        20
/* and how long in microseconds? */
#define NICESLEEP  10000
/*}}}*/

int do_halt=0; /* hack until yaze supports interrupts */

/* variables */ /*{{{*/
/* character devices */ /*{{{*/
struct Device
{
  struct termios raw,cooked;
  char *name;
  const char *cpmname;
  int wr;
  int socket;
  int fd;
  int fd2;
  int connected;
  int binary_ok;
  int mode;
  char ch;
};

static char lst_default[]="/dev/null";
static struct Device lstdev={{0},{0},lst_default,"LST",1};
static struct Device *lst=&lstdev;

static char pun_default[]="pun";
static struct Device pundev={{0},{0},pun_default,"PUN",1};
static struct Device *pun=&pundev;

static char rdr_default[]="rdr";
static struct Device rdrdev={{0},{0},rdr_default,"RDR",0};
static struct Device *rdr=&rdrdev;

static char con_default[]="/dev/tty";
static struct Device conindev={{0},{0},con_default,"CON",0};
static struct Device *conin=&conindev;
static struct Device conoutdev={{0},{0},con_default,"CON",1};
static struct Device *conout=&conoutdev;
/*}}}*/
/* block devices */ /*{{{*/
struct dskdef
{
	char *fn;		/* filename for the disk image */
	int fd;			/* file descriptor to access disk image */
	int secsize;		/* physical sector size */
	int tracks;		/* no. of tracks */
	int sectors;		/* no. of sectors/track */
};

static struct dskdef disk_a = /* disk A: floppy disk 8" IBM SS/SD */
{
	(char*)0,
	0,
	128,
	77,
	26,
};

static struct dskdef disk_b = {	/* disk B: floppy disk 8" IBM SS/SD */
	(char*)0,
	0,
	128,
	77,
	26,
};

static struct dskdef disk_c = {	/* disk C: 4MB hard disk */
	(char*)0,
	0,
	128,
	1024,
	32,
};

static struct dskdef disk_d = {	/* disk D: 4 MB hard disk */
	(char*)0,
	0,
	128,
	1024,
	32,
};

struct dskdef *disks[DISKS] = {
	&disk_a,
	&disk_b,
	&disk_c,
	&disk_d,
};
/*}}}*/

static unsigned int page;
static unsigned int drive;
static unsigned int track;
static unsigned int sector;
static unsigned int status;
static unsigned int dma;
static unsigned int clkcmd;
static int benice=0;
static const char rawmode[]={(char)IAC,(char)WILL,(char)TELOPT_SGA,(char)IAC,(char)WILL,(char)TELOPT_ECHO,(char)IAC,(char)WILL,(char)TELOPT_BINARY};
/*}}}*/

/* telnetinit */ /*{{{*/
static int telnetinit(int fd2)
{
  int binary_ok,mode;
  char c;

  binary_ok=mode=0;
  if (write(fd2,rawmode,sizeof(rawmode))!=sizeof(rawmode)) { close(fd2); return 0; }
  while (!binary_ok)
  {
    if (read(fd2,&c,1)!=1) { close(fd2); return 0; }
    if (mode==0) { if (c==(char)IAC) mode=IAC; }
    else if (mode==IAC && (c>=(char)WILL && c<=(char)DONT)) mode=c&0xff;
    else if (mode==DONT)
    {
      static char buf[]="Sorry, I can't talk to your terminal.\r\n";
  
      write(fd2,buf,sizeof(buf)-1);
      close(fd2);
      return 0;
    } 
    else if (mode==DO && c==TELOPT_BINARY) binary_ok=1;
  }
  return 1;
}
/*}}}*/
/* devopen */ /*{{{*/
static void devopen(struct Device *r)
{
  if (r->name[0]>='0' && r->name[0]<='9') /* open socket */ /*{{{*/
  {
  struct sockaddr_in addr;
  int one=1;

  r->socket=1;
  if ((r->fd=socket(AF_INET,SOCK_STREAM,0))==-1)
  {
    fprintf(stderr,"yaze: opening %s: %s failed: %s\n",r->cpmname,r->name,strerror(errno));
    exit(1);
  }
  setsockopt(r->fd,SOL_SOCKET,SO_REUSEADDR,&one,sizeof(one));
  setsockopt(r->fd,SOL_SOCKET,SO_KEEPALIVE,&one,sizeof(one));
  setsockopt(r->fd,SOL_SOCKET,SO_OOBINLINE,&one,sizeof(one));
  addr.sin_family=AF_INET;
  addr.sin_addr.s_addr=INADDR_ANY;
  addr.sin_port=htons(3000);
  if (bind(r->fd,(struct sockaddr*)&addr,sizeof(addr))==-1 || listen(r->fd,1)==-1)
  {
    fprintf(stderr,"yaze: opening %s: %s failed: %s\n",r->cpmname,r->name,strerror(errno));
    exit(1);
  }
  r->connected=0;
  }
  /*}}}*/
  else /* open file */ /*{{{*/
  {
  if ((r->fd = open(r->name, (r->wr ? O_RDWR : O_RDONLY)|O_CREAT|O_TRUNC, 0666)) == -1) 
  {
    fprintf(stderr,"yaze: opening %s: %s failed: %s\n",r->cpmname,r->name,strerror(errno));
    exit(1);
  }
  tcgetattr(r->fd,&r->cooked);
  r->raw = r->cooked;
  r->raw.c_iflag = 0;
  r->raw.c_oflag = 0;
  r->raw.c_lflag = 0;
  r->raw.c_cc[VTIME]=0;
  r->raw.c_cc[VMIN]=1;
  tcsetattr(r->fd, TCSAFLUSH, &r->raw);
  }
  /*}}}*/
}
/*}}}*/
/* devready */ /*{{{*/
static int devready(struct Device *r)
{
  fd_set s;
  struct timeval t;

  if (r->fd==-1) return 0;
  if (r->socket) /* socket */ /*{{{*/
  {
    struct sockaddr remote;
    size_t remotelen=sizeof(remote);

    if (!r->connected)
    {
      FD_ZERO(&s);
      FD_SET(r->fd,&s);
      t.tv_sec=0;
      t.tv_usec=(benice>=BENICE ? NICESLEEP : 0);
      if (select(r->fd+1,&s,(fd_set*)0,(fd_set*)0,&t)==1)
      {
        if ((r->fd2=accept(r->fd,&remote,&remotelen))==-1)
        {
          if (benice<BENICE) ++benice;
          return 0;
        }
        if (!(r->connected=telnetinit(r->fd2))) return 0;
      }
      else { if (benice<BENICE) ++benice; return 0; }
    }
    FD_ZERO(&s);
    FD_SET(r->fd2,&s);
    t.tv_sec=0;
    t.tv_usec=(benice>=BENICE ? NICESLEEP : 0);
    if (select(r->fd2+1,&s,(fd_set*)0,(fd_set*)0,&t)==1)
    {
      benice=0;
      return 0x3;
    }
    else
    {
      if (benice<BENICE) ++benice;
      return 0x2;
    }
  }
  /*}}}*/
  else /* file */ /*{{{*/
  {
    FD_ZERO(&s);
    FD_SET(r->fd,&s);
    t.tv_sec=0;
    t.tv_usec=(benice>=BENICE ? NICESLEEP : 0);
    if (select(r->fd+1,&s,(fd_set*)0,(fd_set*)0,&t)==1)
    {
      benice=0;
      return 0xff;
    }
    else
    {
      if (benice<BENICE) ++benice;
      return 0;
    }
  }
  /*}}}*/
}
/*}}}*/
/* devread */ /*{{{*/
static int devread(struct Device *r)
{
  char c;

  if (r->socket) /* socket */ /*{{{*/
  {
    struct sockaddr remote;
    size_t remotelen=sizeof(remote);

    while (!r->connected) if ((r->fd2=accept(r->fd,&remote,&remotelen))!=-1) 
    {
      if (!(r->connected=telnetinit(r->fd2))) return 26;
    }
    if (read(r->fd2,&c,1)==1) return c;
    else { r->connected=0; close(r->fd2); return 26; }
  }
  /*}}}*/
  else /* file */ /*{{{*/
  {
    if (read(r->fd,&c,1)==0) return 26;
    else return c;
  }
  /*}}}*/
}
/*}}}*/
/* devwrite */ /*{{{*/
static void devwrite(struct Device *r, char c)
{
  if (r->socket) /* socket */ /*{{{*/
  {
    struct sockaddr remote;
    size_t remotelen=sizeof(remote);
    fd_set s;
    struct timeval t;

    if (!r->connected)
    {
      FD_ZERO(&s);
      FD_SET(r->fd,&s);
      t.tv_sec=0;
      t.tv_usec=(benice>=BENICE ? NICESLEEP : 0);
      if (select(r->fd+1,&s,(fd_set*)0,(fd_set*)0,&t)==1)
      {
        if ((r->fd2=accept(r->fd,&remote,&remotelen))==-1)
        {
          if (benice<BENICE) ++benice;
          return;
        }
        if (!(r->connected=telnetinit(r->fd2))) return;
      }
      else { if (benice<BENICE) ++benice; return; }
    }
    if (r->connected) write(r->fd2,&c,1);
  }
  /*}}}*/
  else /* file */ /*{{{*/
  {
    write(r->fd,&c,1);
  }
  /*}}}*/
  benice=0;
}
/*}}}*/
/* bios_exit */ /*{{{*/
static void bios_exit(void)
{
  int i;

  for (i=0; i<DISKS; i++) if (disks[i]->fd!=-1) close(disks[i]->fd);
  close(lst->fd);
  close(rdr->fd);
  if (rdr!=pun) close(pun->fd);
  tcsetattr(conin->fd, TCSAFLUSH, &conin->cooked);
}
/*}}}*/

/* bios_init */ /*{{{*/
int bios_init(const char *initfile)
{
  /* variables */ /*{{{*/
  int i;
  FILE *fp;
  /*}}}*/

  /* init disk file names */ /*{{{*/
  for (i=0; i<DISKS; ++i) sprintf(disks[i]->fn=xmalloc(11),"drive%c.cpm",i+'a');
  /*}}}*/
  /* process init file */ /*{{{*/
  if ((fp=fopen(initfile,"r")))
  {
    char ln[512];

    while (fgets(ln,sizeof(ln),fp))
    {
      /* variables */ /*{{{*/
      char *argv[3];
      int argc;
      /*}}}*/

      /* split line into argc,argv */ /*{{{*/
      for (argc=0; argc<3 && (argv[argc]=strtok(argc ? (char*)0 : ln," \t\n")); ++argc);
      /*}}}*/
      if (argc==3 && strcmp(argv[0],"mount")==0) /* mount a disk file */ /*{{{*/
      {
        if (argv[1][0]>='a' && argv[1][0]<='p' && argv[1][1]=='\0')
        {
          free(disks[argv[1][0]-'a']->fn);
          strcpy(disks[argv[1][0]-'a']->fn=xmalloc(strlen(argv[2])+1),argv[2]);
        }
        else fprintf(stderr,"YAZE: %s is not a valid disk\n",argv[1]);
      }
      /*}}}*/
      else if (argc==3 && strcmp(argv[0],"attach")==0) /* attach a tty file */ /*{{{*/
      {
        if (strcmp(argv[1],"rdr")==0) /* set RDR: file */ /*{{{*/
        {
          rdrdev.name=xmalloc(strlen(argv[2])+1);
          strcpy(rdrdev.name,argv[2]);
        }
        /*}}}*/
        else if (strcmp(argv[1],"conin")==0) /* set CON: input file */ /*{{{*/
        {
          conindev.name=xmalloc(strlen(argv[2])+1);
          strcpy(conindev.name,argv[2]);
        }
        /*}}}*/
        else if (strcmp(argv[1],"lst")==0) /* set LST: file */ /*{{{*/
        {
          lstdev.name=xmalloc(strlen(argv[2])+1);
          strcpy(lstdev.name,argv[2]);
        }
        /*}}}*/
        else if (strcmp(argv[1],"pun")==0) /* set PUN: file */ /*{{{*/
        {
          pundev.name=xmalloc(strlen(argv[2])+1);
          strcpy(pundev.name,argv[2]);
        }
        /*}}}*/
        else if (strcmp(argv[1],"conout")==0) /* set CON: output file */ /*{{{*/
        {
          conoutdev.name=xmalloc(strlen(argv[2])+1);
          strcpy(conoutdev.name,argv[2]);
        }
        /*}}}*/
      }
      /*}}}*/
      else if (argc) /* complain */ /*{{{*/
      {
        fprintf(stderr,"yaze: invalid command: %s",argv[0]);
        if (argc>=2) fprintf(stderr," %s",argv[1]);
        if (argc>=3) fprintf(stderr," %s",argv[2]);
        putc('\n',stderr);
      }
      /*}}}*/
    }
    fclose(fp);
  }
  /*}}}*/
  if (strcmp(conindev.name,conoutdev.name)==0) conindev.wr=1;
  devopen(conin);
  atexit(bios_exit);
  if (strcmp(conindev.name,conoutdev.name)==0) conout=conin; else devopen(conout);
  devopen(lst);
  if (strcmp(pundev.name,rdrdev.name)==0) rdrdev.wr=1;
  devopen(rdr);
  if (strcmp(pundev.name,rdrdev.name)==0) pun=rdr; else devopen(pun);
  /* open first drive */ /*{{{*/
  if ((disks[0]->fd=open(disks[0]->fn,O_RDWR))==-1) 
  {
    fprintf(stderr,"yaze: opening disk A: %s: %s\n",disks[0]->fn,strerror(errno));
    exit(1);
  }
  /*}}}*/
  /* open other drives, if possible */ /*{{{*/
  for (i=1; i<DISKS; i++) 
  {
    if ((disks[i]->fd=open(disks[i]->fn,O_RDWR))==-1) disks[i]->fd=open(disks[i]->fn,O_RDONLY);
  }
  /*}}}*/
  return 1;
}
/*}}}*/
/* in */ /*{{{*/
int in(unsigned int port)
{
  switch (port) 
  {
    /* CONS */ /*{{{*/
    case CONS: return devready(conin);
    /*}}}*/
    /* COND */ /*{{{*/
    case COND: return devread(conin);
    /*}}}*/
    /* AUXS */ /*{{{*/
    case AUXS: return devready(rdr);
    /*}}}*/
    /* AUXD */ /*{{{*/
    case AUXD: return devread(rdr);
    /*}}}*/
    /* PRTS */ /*{{{*/
    case PRTS: return 0xff;
    /*}}}*/
    /* FDCD */ /*{{{*/
    case FDCD: return drive;
    /*}}}*/
    /* FDCTL */ /*{{{*/
    case FDCTL: return track&0xff;
    /*}}}*/
    /* FDCTH */ /*{{{*/
    case FDCTH: return (track>>8);
    /*}}}*/
    /* FDCS */ /*{{{*/
    case FDCS: return sector;
    /*}}}*/
    /* FDCX */ /*{{{*/
    case FDCX: return status;
    /*}}}*/
    /* DMAL */ /*{{{*/
    case DMAL: return dma&0xff;
    /*}}}*/
    /* DMAH */ /*{{{*/
    case DMAH: return (dma>>8);
    /*}}}*/
    /* CLKD */ /*{{{*/
    case CLKD:
    {
      static struct tm *t;
      static time_t now;

      if (clkcmd==0) { time(&now); t=localtime(&now); }
      switch (clkcmd) 
      {
        /*   0 -- seconds in BCD */ /*{{{*/
        case 0: return (((t->tm_sec/10)<<4)|(t->tm_sec%10));
        /*}}}*/
        /*   1 -- minutes in BCD */ /*{{{*/
        case 1: return (((t->tm_min/10)<<4)|(t->tm_min%10));
        /*}}}*/
        /*   2 -- hours in BCD */ /*{{{*/
        case 2: return (((t->tm_hour/10)<<4)|(t->tm_hour%10));
        /*}}}*/
        /* 3,4 -- low, high byte of number of days since 1.1.1978 */ /*{{{*/
        case 3:
        case 4:
        {
          register int i;
          register int days = 0;

          for (i = 1978; i < 1900 + t->tm_year; ++i) 
          {
            days += 365;
            if (i % 4 == 0)
            ++days;
          }
          days += t->tm_yday + 1;
          return (clkcmd==3 ? days&0xff : days>>8);
        }
        /*}}}*/
        /* default -- 0 */ /*{{{*/
        default: return 0;
        /*}}}*/
      }
    }
    /*}}}*/
    /* default */ /*{{{*/
    default: return 0;
    /*}}}*/
  }
}
/*}}}*/
/* out */ /*{{{*/
void out(unsigned int port, unsigned char byte)
{
  switch (port) 
  {
    /* COND */ /*{{{*/
    case COND: devwrite(conout,byte); break;
    /*}}}*/
    /* AUXD */ /*{{{*/
    case AUXD: devwrite(pun,byte); break;
    /*}}}*/
    /* PRTD */ /*{{{*/
    case PRTD: devwrite(lst,byte); break;
    /*}}}*/
    /* FDCD */ /*{{{*/
    case FDCD: if (byte>15 || disks[byte]==(struct dskdef*)0 || disks[byte]->fd==-1) status=FDCX_NODRIVE; else { drive=byte; status=FDCX_OK; } break;
    /*}}}*/
    /* FDCTL */ /*{{{*/
    case FDCTL: track=(track&0xff00)|byte; break;
    /*}}}*/
    /* FDCTH */ /*{{{*/
    case FDCTH: track=(track&0xff)|(((unsigned int)byte)<<8); break;
    /*}}}*/
    /* FDCS */ /*{{{*/
    case FDCS: sector=byte; break;
    /*}}}*/
    /* FDCO */ /*{{{*/
    case FDCO:
/*
 *      I/O handler for write FDC command:
 *      transfer one sector in the wanted direction,
 *      0 = read, 1 = write
 *
 *      The status byte of the FDC is set as follows:
 *        0 - ok
 *        1 - illegal drive
 *        2 - illegal track
 *        3 - illegal sector
 *        4 - seek error
 *        5 - read error
 *        6 - write error
 *        7 - illegal command to FDC
 *        9 - illegal dma address
    */
    {
      register int size;

      if (disks[drive]->fd==-1) { status=FDCX_NODRIVE; return; }
      if (track>disks[drive]->tracks) fprintf(stderr,"YAZE: track %04x selected\r\n",track);
      if (track>disks[drive]->tracks) { status=FDCX_NOTRACK; return; }
      if (sector>disks[drive]->sectors) { status=FDCX_NOSECTOR; return; }
      size=disks[drive]->secsize;
      if (dma+size>(64*1024)) { status=FDCX_DMAFAIL; return; }
      if (lseek(disks[drive]->fd, (((off_t)track)*((off_t)disks[drive]->sectors)+(off_t)(sector-1))*(off_t)size,SEEK_SET)==(off_t)-1) { status=FDCX_SEEKFAIL; return; }
      switch (byte)
      {
        case FDCO_READ:
        status=(read(disks[drive]->fd,pagetable[((dma)&0xffff)>>12]+((dma)&0x0fff),size)==size ? FDCX_OK : FDCX_READFAIL);
        benice=0;
        break;
        case FDCO_WRITE:
        status=(write(disks[drive]->fd,pagetable[((dma)&0xffff)>>12]+((dma)&0x0fff),size)==size ? FDCX_OK : FDCX_WRITEFAIL);
        benice=0;
        break;
        default:
        status=FDCX_NOCMD;
        break;
      }
      break;
    }
    /*}}}*/
    /* DMAL */ /*{{{*/
    case DMAL: dma=(dma&0xff00)|byte; break;
    /*}}}*/
    /* DMAH */ /*{{{*/
    case DMAH: dma=(dma&0xff)|(((unsigned int)byte)<<8); break;
    /*}}}*/
    /* PAGEL */ /*{{{*/
    case PAGEL: page=(page&0xff00)|byte; break;
    /*}}}*/
    /* PAGEH */ /*{{{*/
    case PAGEH: page=(page&0xff)|(((unsigned int)byte)<<8); break;
    /*}}}*/
    /* FRAME */ /*{{{*/
    /* The page table is updated by first setting the page number port
       and then writing the frame to the frame port which should be
       mapped to the previously given page.
     */
    case FRAME: if ((page<<2)<MEMSIZE) pagetable[((unsigned int)byte)]=&ram[page<<12]; break;
    /*}}}*/
    /* CLKC */ /*{{{*/
    case CLKC: clkcmd=byte; break;
    /*}}}*/
    /* HALT */ /*{{{*/
    case HALT: do_halt=1; break;
    /*}}}*/
  }
}
/*}}}*/
