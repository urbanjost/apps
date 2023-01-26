NAME
====

colrm-(1f) - \[FUNIX\] remove columns from a file (LICENSE:PD)

SYNOPSIS
========

colrm- \[first \[last\]\]

DESCRIPTION
===========

colrm- removes selected character columns from a file. Input is taken
from standard input. Output is sent to standard output.

If called with one parameter the columns of each line will be removed
starting with the specified first column. If called with two parameters
the columns from the first column to the last column will be removed.

Column numbering starts with column 1. Tabs are NOT expanded.

OPTIONS
=======

**first**

:   starting column number to remove

**last**

:   ending column number to remove

****--version****

:   Display version information and exit.

****--help****

:   Display help text and exit.

HISTORY
=======

The colrm(1) command appeared in 3.0BSD.

EXAMPLE
=======

Samples

           # trim file so no line is longer than 72 characters
           cat FILENAME|colrm- 73
           # remove first three characters in each line
           cat FILENAME|colrm- 1 3

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
