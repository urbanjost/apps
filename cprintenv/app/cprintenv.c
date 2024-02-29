#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <strings.h>
/*
   FUNCTION:
   Find all environment variables listed or beginning with a specified prefix
   and print them in the syntax of a csh(1) setenv command; like the
   printenv(1) command does for sh(1).

   USAGE:
   cprintenv           # print entire environment
   cprintenv -p PREFIX # print environment variables beginning with PREFIX
   cprintenv HOME TMP LOGNAME USER # print selected variables

   PURPOSE:
   env(1) and printenv(1)  print the environment table in Bourne shell syntax; need capability to 
   write variables in csh/tcsh syntax.

   AUTHOR: John S. Urban, May 2009

   DOES YOUR SYSTEM ALLOW?:
   setenv '\!::' 'string'
   setenv '::' 'string'
   
*/

extern char **environ;
extern int  optind;

printquoted(char *p){
/* print variable names and variable values with quotes and 
   special escaping of ' and !
*/
   int i;
   char c;
       putchar('\'');
       for (i = 0; i < strlen(p); i++){
          c = *(p+i);
	  switch (c){
	  case '\'':
	   putchar ('\'');
	   putchar ('\\');
	   putchar ('\'');
	   putchar ('\'');
	   break;
	  case '!':
	   putchar ('\\');
	   putchar (c);
	   break;
	  default:
	   putchar (c);
	   break;
	  }
       }
       putchar('\'');
}
main( int argc, char **argv){

   char **ep = environ;
   int epadd;
   char *p;
   char *e;
   int i;
   long int plen;
   char c;

   int p_opt=0; /* specify prefix */
   char *prefix=NULL;

   int Z_opt=0; /* filter mode */
   int l_opt=0; /* list mode */

   /* getopt */
   static char usage[]="usage: cprintenv [-p PREFIX][ -h ][-v]\n";
   extern char *optarg;
   int s;

/* 
   Crack command line options
*/
   plen=0;
   while ((s = getopt(argc,argv,"Zp:l?uhv")) != EOF){
      switch(s){
      case 'p':
       p_opt=1;
       prefix = optarg;
       plen=strlen(prefix);
       break;
      case 'Z':
       Z_opt=1;
       break;
      case 'l':
       l_opt=1;
       break;
      case 'v':
         fprintf(stderr,"Version 2.0 2009-05-01 John S. Urban\n");
         exit(3);
       break;
      case '?':
      case 'u':
      case 'h':
         fprintf(stderr,"%s\n",usage);
         fprintf(stderr,"   FUNCTION:\n");
         fprintf(stderr,"   Find all environment variables beginning with specified prefix      \n");
         fprintf(stderr,"   and print them in the syntax of a csh(1) setenv command.            \n");
         fprintf(stderr,"   The file is formatted so it may be read in by the source(1) command.\n");
         fprintf(stderr,"\n");
         fprintf(stderr,"   USAGE:\n");
         fprintf(stderr,"   cprintenv           # print entire environment\n");
         fprintf(stderr,"   cprintenv -p PREFIX # print environment variables beginning with PREFIX\n");
         fprintf(stderr,"   cprintenv HOME TMP USER LOGNAME # print specified environment variables\n");
         fprintf(stderr,"\n");
         fprintf(stderr,"   PURPOSE:\n");
         fprintf(stderr,"   env(1) and printenv(1) nearly print the environment table in Bourne shell\n");
         fprintf(stderr,"   syntax; need capability to write variables in csh/tcsh syntax.\n");
         fprintf(stderr,"\n");
         fprintf(stderr,"   AUTHOR: John S. Urban, May 2009\n");
         exit(2);
       break;
       default:
         fprintf(stderr,"%s\n",usage);
         exit(4);
       break;
      }
   }

   /* explicitly named variables */
   for(i = optind ; i < argc ; ++i){
      ep = environ;
      while ((p = *ep++)){
            e=index(p,'=');
            *e='\0';
            if( strcmp(argv[i],p)==0 ){
               printf("setenv ");
               printquoted(p);
               putchar(' ');
               printquoted(e+1);
               putchar('\n');
            }
            *e='=';
      }
   }

   ep = environ;

   /* all or variables matching requested prefix */
   while ((p = *ep++)){
      if( (plen!=0 && strncmp(prefix,p,plen)==0) || (argc-optind <=0 && plen==0) ){
         e=index(p,'=');
         *e='\0';
         printf("setenv ");
         printquoted(p);
         putchar(' ');
         printquoted(e+1);
         putchar('\n');
      }
   }
    return 0;
}
/* ================================================================================================ */
/*
   If system does not support environ variable then in general, the environ
   variable is also passed as the third, optional, parameter to main();
   that is, for example:

#include <stdio.h>
#include <string.h>

int main(int argc, char **argv, char **envp)
{
    char *p;
    char *e;
    while ((p = *envp++))
        if(strncmp("CPE_",p,4)==0){
	   e=index(p,'=');
	   *e='\0';
           printf("setenv %s '%s'\n", p,e+1);
	}
    return 0;
}
*/
