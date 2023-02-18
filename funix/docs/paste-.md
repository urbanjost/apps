NAME
====

paste(1f) - \[FILE FILTER\] merge lines of files (LICENSE:PD)

SYNOPSIS
========

paste \[OPTIONS\] \[filenames\] \|**--version**\|**--help** (LICENSE:PD)

DESCRIPTION
===========

paste(1f) writes to standard output lines consisting of sequentially
corresponding lines of each given file, separated by a TAB character.
Standard input is used for a file name of '-' or if no input files are
given.

An exit status of zero indicates success, and a nonzero value indicates
failure.

Unlike the C version the delimiter list cannot be a null set. A
zero-length list will be treated as a blank; and if a blank is a member
of the set it cannot be at the end of the delimiters list.

Standard Fortran does not currently support the same file being opened
multiple times simultaneously so repeating filenames is not supported
but may work, depending on the compiler.

OPTIONS
=======

****-d**, **--delimiters**=LIST**

:   Consecutively use the characters in LIST to separate merged lines.
    When LIST is exhausted, start again at its beginning.

****-s**, **--serial****

:   paste one file at a time instead of in parallel

****-z**, **--zero-terminated****

:   line delimiter is NUL, not newline Delimit items with a zero byte
    rather than a newline (ASCII LF). I.E., treat input as items
    separated by ASCII NUL and terminate output items with ASCII NUL.
    This option can be useful in conjunction with 'perl **-0**' or

same in order to reliably handle arbitrary file names (even those
containing blanks or other special characters).

****--help****

:   Display a help message and exit.

****--version****

:   Display version information and exit.

****--usage****

:   Display table of commandline parameters

EXAMPLES
========

For example, with:

         $ cat two_numbers
         1
         2
         $ cat three_letters
         a
         b
         c

Take lines sequentially from each file:

         $ paste two_numbers three_letters
         1       a
         2       b
                 c

Duplicate lines from a file:

         # Fortran does not allow a file to be open twice
         # so the C program could do this:
         $ paste two_numbers three_letters two_numbers
         1       a      1
         2       b      2
                 c
         # but you have to make a copy of two_numbers to
         # do that with this program

Intermix lines from stdin:

         $ paste - three_letters - < two_numbers
         1       a      2
                 b
                 c

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
