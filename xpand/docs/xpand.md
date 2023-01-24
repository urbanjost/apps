NAME
====

xpand(1f) - \[FUNIX\] expand tabs,remove trailing white space and
(optionally) adjacent blank lines

SYNOPSIS
========

xpand \[ **-blanks** NNN\]\[ **--width** \] FILENAME(S)

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
    less then or equal to 0 no warnings are produced. Warning messages
    appear on stderr.

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
