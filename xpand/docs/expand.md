NAME
====

**expand**(1f) - \[FUNIX\] convert tabs to spaces

SYNOPSIS
========

expand **FILENAME**(*S*) \[ **-blanks** NNN\]\[ **--width** \]\|
**--help**\| **--version**

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

****--width****

:   line width to produce a warning if exceeded. Defaults to 132. If
    less then or equal to 0 no warnings are produced.

****--blanks****

:   maximum number of adjacent blank lines to retain. Default is **-1**,
    which is equivalent to unlimited.

****--help****

:   display this help and exit

****--version****

:   output version information and exit

****--usage****

:   basic usage information including a list of arguments

EXAMPLES
========

Sample commands:

         expand < input.txt > output.txt
         expand input.txt   > output.txt
