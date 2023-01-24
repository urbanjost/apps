### [fpm-tools](https://github.com/search?q="fpm-tools"%20in:topic%20language:fortran):[xpand](https://urbanjost.github.io/xpand/)

# xpand - a filter to expand tabs in a file

The xpand(1) program expands tabs in files.  removes trailing white space,
and optionally removes multiple blank lines.

Tab characters are not standard in Fortran source files, but are often
allowed due to extensions, which vary in how they treat the tabs. It is
therefore best to remove them.

xpand(1) is similar to versions of the ULS command expand(1).  It is
primarily intended for use on systems without access to the Unix command
expand(1).

## BUILDING
To build xpand(1) requires `git`, `fpm`(Fortran Package Manager),
a modern Fortran compiler and WWW access. It was tested with

   + GNU Fortran (GCC) 8.3.1 20191121 
   + ifort (IFORT) 19.1.3.304 20200925

```bash
   # ACCESSING

   # go to where you want to create the `xpand` directory
   mkdir github
   cd github
   # get a clone of the repository
   git clone https://github.com/urbanjost/apps.git
   # enter the repository directory
   cd apps/xpand

   # BUILDING AND INSTALLING

   # install (in the default location)
   fpm install 

   # TRY IT

   # if you placed the program in a directory in your command path you are ready to go!
   ls|xpand 
```
NAME
====

**xpand**(1f) - \[FUNIX\] expand tabs,remove trailing white space and
(optionally) adjacent blank lines

SYNOPSIS
========

xpand \[ **-blanks** NNN\]\[ **--width** \] **FILENAME**(*S*)

DESCRIPTION
===========

Convert tabs in each FILE to spaces, writing to standard output. If no
filename is specified standard input is read. Tab stops are assumed to
be every eight (8) columns. Trailing spaces, carriage returns, and
newlines are removed.

OPTIONS
=======

**FILENAMES**

:   files to expand tab characters in.

****--width**,**-w****

:   line width to produce a warning if exceeded. Defaults to 132. If
    less then or equal to 0 no warnings are

    **produced.**

    :   Warning messages appear on stderr.

****--blanks**,**-b****

:   maximum number of adjacent blank lines to retain. Default is **-1**,
    which is equivalent to unlimited.

STANDARD OPTIONS
================

****--help****

:   display this help and exit

****--version****

:   output version information and exit

****--usage****

:   basic usage information including a list of arguments

****--verbose****

:   verbose mode

EXAMPLES
========

Sample commands:

         xpand < input.txt > output.txt
         xpand input.txt   > output.txt
