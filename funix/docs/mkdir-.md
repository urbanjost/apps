NAME
====

mkdir-(1f) - \[FUNIX:FILESYSTEM\] call mkdir(3c) to make directories
(LICENSE:PD)

SYNOPSIS
========

mkdir- DIRECTORY \`\`\` \[OPTION\]...

DESCRIPTION
===========

Create the DIRECTORY(ies), if they do not already exist. The file
permission mode by default is "a=rxw-umask".

OPTIONS
=======

**DIRECTORY**

:   directory pathnames. Limited to 4096 characters per pathname.

****--parents****

:   no error if existing, make parent directories as needed

****--verbose****

:   print a message for each created directory

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLE
=======

Samples:

       # silently make specified directory and any needed parent directories
       mkdir- A/B/C -parents

       # show creation of three directories
       mkdir- A B C

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
