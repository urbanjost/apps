NAME
====

basename-(1f) - \[FUNIX:FILESYSTEM\] display last component of file
name(s) (LICENSE:PD)

SYNOPSIS
========

syntax:

       basename- NAME
         or
       basename- [NAME```] [ --zero][--suffix SUFFIX]
         or
       basename- -help|-version

DESCRIPTION
===========

Print NAME with any leading directory components removed. If specified,
also remove a trailing SUFFIX.

If no NAME is specified read names from stdin.

OPTIONS
=======

**NAME(S)**

:   filenames

****-s**, **--suffix**=SUFFIX**

:   remove a trailing SUFFIX.

****-z**, **--zero****

:   end each output line with NUL, not newline

    ****--help****

    :   display this help and exit

    ****--version****

    :   output version information and exit

EXAMPLES
========

Typical usage:

      basename /usr/bin/sort          -> "sort"
      basename -s .h include/stdio.h  -> "stdio"
      basename -s '' include/stdio.h  -> "stdio.h"
      basename any/str1.f90 any/str2   -> "str1" followed by "str2"

or available locally via: info '(coreutils) basename invocation'

SEE ALSO
========

basename(1), basename(1), readlink(1), realpath(1)
