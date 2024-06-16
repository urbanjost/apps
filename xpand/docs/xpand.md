NAME
====

xpand(1f) - \[FUNIX\] expand tabs,remove trailing white space and
(optionally) adjacent blank lines

SYNOPSIS
========

xpand \[ **-blanks** NNN\]\[ **--width** \] FILENAME(S)

DESCRIPTION
===========

Defaults to converting tabs in each FILE to spaces, writing to standard
output. If no filename is specified standard input is read. Tab stops
are assumed to be every eight (8) columns. Trailing spaces, carriage
returns, and newlines are removed.

OPTIONS
=======

**FILENAMES**

:   files to expand tab characters in.

****--width**,**-w****

:   line width at which to produce a warning if exceeded. If less then
    or equal to 0 no warnings are produced. The default is 0. Warning
    messages appear on stderr.

****--blanks**,**-b****

:   maximum number of adjacent blank lines to retain. Default is **-1**,
    which is equivalent to unlimited.

****--show-nonprinting**,**-c****

:   use \^ and M- notation, except for linefeed

****--show-ends**,**-e****

:   display $ at end of each line

****--number**,**-n****

:   number all output lines **--number-nonblank**,**-N** number nonempty
    output lines, overrides **-n**

****--uppercase**,**-U****

:   convert all lowercase ASCII characters to uppercase

****--lowercase**,**-l****

:   convert all uppercase ASCII characters to lowercase

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
