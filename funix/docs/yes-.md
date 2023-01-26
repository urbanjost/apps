NAME
====

yes-(1f) - \[FUNIX\] output a string repeatedly until killed or limit is
reached (LICENSE:PD)

SYNOPSIS
========

yes- \[STRING\[ **-repeat** N\]\]\|\[ **--help**\| **--version**\]

DESCRIPTION
===========

yes-(1) prints the command line arguments, separated by spaces and
followed by a newline until the repeat count is reached or endlessly
until it is killed. If no arguments are given, it prints "y" followed by
a newline endlessly until killed. Upon a write error, yes-(1) exits with
status "1".

OPTIONS
=======

****-repeat** N**

:   specify number of times to display string

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLES
========

Sample commands

           # repeat a command 20 times, pausing and clearing:
           yes-  date --repeat 20  |xargs -iXX  sh -c 'XX;sleep 2;clear'

REPORTING BUGS
==============

Report yes- bugs to \<http://www.urbanjost.altervista.org/index.html\>

SEE ALSO
========

yes(1), repeat(1), xargs(1)

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
