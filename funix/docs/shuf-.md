NAME
====

shuf- - \[FUNIX\] generate random permutations of file lines, whole
numbers, or strings (LICENSE:PD)

SYNOPSIS
========

syntax:

       shuf- FILES(s)   [ -n]
       shuf- STRINGS -e [ -n]
       shuf- -i LO-HI   [ -n]

DESCRIPTION
===========

Write a random permutation of the input lines to standard output.

**FILES(s)**

:   files to use as input

****-e****

:   treat each ARG as an input line

****-i** LO HI**

:   treat each number LO through HI as an input line

****-n****

:   output at most COUNT lines (per file)

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLES
========

Sample usage:

       # generate a random number from 0 to 100
       shuf- -i 0 100 -n 1
       # randomly select xterm(1) color
       xterm -bg `shuf- -e green black gray blue -n 1`

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
