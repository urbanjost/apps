NAME
====

tac-(1f) - \[FUNIX:FILESYSTEM\] reverse or transpose lines in a file
(LICENSE:PD)

SYNOPSIS
========

syntax:

       tac- [NAME```] [ --transpose]
         or
       tac- -help|-version

DESCRIPTION
===========

Read file into memory and then write it out with the lines reversed or
transposed.

If no NAME is specified read data from stdin.

OPTIONS
=======

**NAME(S)**

:   filenames

****--transpose**,**-t****

:   transpose the file instead of reversing it

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLES
========

Typical usage:

      tac /usr/bin/sort

SEE ALSO
========

**tac(1),**

:   cat(1)
