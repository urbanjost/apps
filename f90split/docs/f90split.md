NAME
====

f90split(1f) - \[DEVELOPER\] split Fortran source file into individual
files at module or procedure boundaries. (LICENSE:PD)

SYNOPSIS
========

f90split \[-fcase NAME\] \[-odir DIRECTORY\] largefile(s) \[ \>
list\_file \] \| \[ **--help**\| **--version**\]

DESCRIPTION
===========

f90split(1) is a utility which splits free source form Fortran code into
multiple files, one module or procedure per file. Note that contained
procedures are stored within their parent procedure.

Each output file contains a single program unit named after the unit,
unless that filename exists.

If the initial output file name exists a file will be created named
main0001.f90-main9999.f90, or bdta0001.f90-bdta9999.f90. If a file with
that name already exists, it is put in dupl0001.f90-dupl9999.f90.

f90split(1) also lists on stdout the USE and INCLUDE dependencies

f90split(1) is not aware of preprocessor directives.

OPTIONS
=======

**largfile(s)**

:   list of input files. Defaults to stdin.

****--fcase****

:   case mode for generated filenames

<!-- -->

                       leave  use procedure name case as-is
                       lower  generate all-lowercase filenames
                       upper  generate all-uppercase filenames

****--odir****

:   output directory. Defaults to current directory.

****--help****

:   display this help and exit

****--version****

:   output version information and exit

LICENSE
=======

All rights to this code are waived, so that it may be freely distributed
as public domain software subject to the condition that these 6 lines
are verbatim reproduced.

Originally written by Michel Olagnon, from Ifremer, France, who would be
pleased to receive your comments and corrections.

AUTHOR
======

-   M. Olagnon (Michel.Olagnon@ifremer.fr)

Improved by

-   Phil Garnatz, Cray Research Inc. for makefile generation

-   John S. Urban, added CLI

EXAMPLES
========

Sample commands

           f90split  < myprogram.f90

SEE ALSO
========

fsplit(1)
