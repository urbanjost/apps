static char USMID[] = "@(#)cmd/fsplit.c	61.1	11/15/90 00:13:15";
/*---------------------*/
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define PATHSIZE MAX_PATH
#define PATHSIZE 256
/*---------------------*/

/*	COPYRIGHT CRAY RESEARCH, INC.
 *	UNPUBLISHED -- ALL RIGHTS RESERVED UNDER
 *	THE COPYRIGHT LAWS OF THE UNITED STATES.
 */	

/*
 *	Program to split file(s) containing Fortran
 *	procedures into separate files, one per procedure.
 *	Procedure X is put in file X.f.
 *	The -s option causes Fortran procedures to be stripped to 72
 *	or fewer characters, with trailing blanks removed.
 *
 *	Modified to include preceding comments with the file.  This
 *	follows the standard and is critically important for compiler
 *	directives for cft.
 *
 *	Features added:
 *		If no name follows an entry point, "zzzzzz" is used.
 *		If a file of the same name already exists, a new
 *		.f file name will formed by appending a 3 digit
 *		number to it.  This way no routines will be written
 *		over and lost.
 *
 *		Use perror() for all system call failures.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <unistd.h>

#define BIG 1000

#define NO	0
#define YES	1

#define FORTRAN	0
#define FTMP	"fsplit.XXXXXX"

static int blkdatano 	= 0;
static char suffix	= 'f';
static int strip	= NO;
static int name_max;

#define SKIP	while( isspace(*s) ) ++s;


main(argc, argv)
register int argc;
register char **argv;
{
	register FILE *fd;
	register int i;
	register int c;
	register int errflg = 0;
	extern int optind;

	while ((c = getopt(argc,argv,"s")) != EOF)
		switch(c)
		{
		case 's':
			strip = YES;
			break;

		case '?':
		default:
			errflg++;
		}

	if (errflg)  {
		fprintf(stderr,"Usage: fsplit [-s] [files]\n");
		exit(1);
	}

	/*  Get the maximum file size in bytes
	 */
	if ((name_max = pathconf(".",_PC_NAME_MAX)) < 0)
		name_max = 14;

	if(optind == argc)
		splitup(stdin);

	else for(i = optind ; i < argc ; ++i)
	{
		if( (fd = fopen(argv[i], "r")) == NULL)
		{
			fprintf(stderr, "cannot open %s: ", argv[i]);
			perror((char *)0);
			exit(1);
		}
		splitup(fd);
		fclose(fd);
	}

	exit(0);
}

splitup(fin)
register FILE *fin;
{
	register FILE *fout;
	char in[BIG], fname[PATHSIZE], ftmp[PATHSIZE];
	int i;

	(void) strcpy(ftmp, FTMP);
	(void) mktemp(ftmp);
	if( (fout = fopen(ftmp, "w")) == NULL)
	{
		fprintf(stderr, "can't open %s: ", ftmp);
		perror((char *)0);
		exit(1);
	}
	while( fgets(in,BIG,fin) )
	{
		for (i = 0; i<BIG; i++)
		{
			if (in[i] == '\n') {
				break;
			}
			if (in[i] == '\0') in[i] = ' ';
		}
		if( *in=='c' || *in=='C' || *in=='*' ) {
			fputs(in, fout);
			continue;
		}
		for (i = 0; i<72; i++) {
			if (in[i] == '\n') {
				i = 72;
				break;
			}
			if (in[i] != ' ' && in[i] != '\t')
				break;
		}
		if (i == 72) {
			fputs(in, fout);
			continue;
		}
		if(strip)
			shorten(in);
		getname(in, fname);
		printf("%s\n", fname);
		if (unlink(fname),link(ftmp, fname) == -1 || unlink(ftmp) == -1) {
			fprintf(stderr, "cannot move %s to %s: ", ftmp, fname);
			perror((char *)0);
			exit(1);
		}
		fputs(in,fout);
		while( !endcard(in) && fgets(in, BIG, fin) ) {
			if(strip)
				shorten(in);
			fputs(in, fout);
		}
		if( ferror(fout) || fclose(fout)) {
			fprintf(stderr, "error writing output %s: ", fname);
			perror((char *)0);
			exit(1);
		}
		if( (fout = fopen(ftmp, "w")) == NULL) {
			fprintf(stderr, "can't open %s: ", ftmp);
			perror((char *)0);
			exit(1);
		}
	}
	if (unlink(ftmp) == -1) {
		fprintf(stderr, "couldn't remove the temp file %s: ", ftmp);
		perror((char *)0);
		exit(1);
	}
	fclose(fout);
}

getname(s,f)
char *s;
register char *f;
{
	register int i;
	register int j;
	char	*s0 = s;		/*  address of first column of "card"*/
	char justname[PATHSIZE];

	memset (justname, 0, PATHSIZE);
loop:
	if( compar(&s,"subroutine") )	goto bot;
	else if( compar(&s,"function") )	goto bot;
	else if( compar(&s,"procedure") )	goto bot;
	else if( compar(&s,"program") )	goto bot;
	else if( compar(&s,"recursive") )	goto loop;
	else if( compar(&s,"real") )	goto loop;
	else if( compar(&s,"integer") )	goto loop;
	else if( compar(&s,"logical") )	goto loop;
	else if( compar(&s,"double") )	goto loop;
	else if( compar(&s,"precision") )	goto loop;
	else if( compar(&s,"complex") )	goto loop;
	else if( compar(&s,"character") ) goto loop;
	else if( compar(&s,"*") )	/* complex *16  etc */
	{
		for( ++s ; isdigit(*s) || isspace(*s)  ; ++s)
			;
		goto loop;
	}
	else if( compar(&s,"blockdata") )
	{
		SKIP
		    /* no block data name */
		    if ((s-s0) >= 72 || *s == '\0' || *s == '\n') {
			sprintf(f, "BLOCKDATA%03d.%c", ++blkdatano, suffix);
			return;
		}
		goto bot;
	}
	else
		s = "";

bot:
	SKIP

	/* the longest name we are going to allow is 66 characters long
	 * (column 72 - column 6 + 1)
	 */
	for(i=0 ; isalpha(*s) || isdigit(*s) || *s == '_' || *s == '@'; i++) {
		if( i >= name_max || (s-s0) >= 72 )  break;
		justname[i] = *s++;
		SKIP
	}

	/* if no name following entry point, use "zzzzzz"; otherwise use the
	 * the name
	 */
	if (i == 0)
		strcpy(justname,"zzzzzz");

	/* this is file name -- is it OK to use ?
	 */
	sprintf(f,"%.*s.%c", name_max-2, justname, suffix);

	/* if name already exists, create a unique name with 3 digits
	 */
	for (j=1; !access(f,0) && j < 1000; j++)
		sprintf(f,"%.*s%03d.%c", name_max-5, justname, j, suffix);
}

/*	compare two strings for equality.  assume that
 * 	t is all lower case.  ignore blanks and decase s
 * 	during comparison.  s0 points to next character after
 * 	successful comparison.
 */
compar(s0, t)
register char **s0,*t;
{
	register char *s;
	register int s1;
	s = *s0;
	while( *t )
	{
		SKIP
		    s1 = *s++;
		if(isupper(s1))
			s1 = tolower(s1);
		if(s1 != *t++)
			return(NO);
	}
	*s0 = s;
	return(YES);
}

endcard(s)
register char *s;
{
	char	*s0 = s;		/*  column 1 of card image		*/

	if( *s==0 )
		return(YES);

	/*	search for "end" statement somewhere in the card image
	 */
	SKIP
	if (*s != 'e' && *s != 'E' )
		return(NO);
	s++;
	SKIP
	if (*s != 'n' && *s != 'N' )
		return(NO);
	s++;
	SKIP
	if (*s != 'd' && *s != 'D' )
		return(NO);
	s++;
	SKIP

	/*	legitimate ending to "end" card ??
 	 */
	if (*s == '\0' || *s == '!' || *s == '\n' || (s - s0) >= 72)
		return(YES);
	else
		return(NO);
}

shorten(s0)
register char *s0;
{
	register char *s, *s72;
	s72 = s0 + 72;

	for(s=s0 ; s<s72; ++s)
		if(*s=='\n' || *s=='\0')
			break;

	while(s>s0 && s[-1]==' ')
		--s;
	s[0] = '\n';
	s[1] = '\0';
}
