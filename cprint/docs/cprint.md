NAME
====

cprint(1f) - \[FILE FILTER\] filter prints specified columns
(LICENSE:PD)

SYNOPSIS
========

cprint \[ columns \]\[ **-delimiters** delim\] \|**-help**\|**-version**

DESCRIPTION
===========

cprint is a filter that prints the specified columns.

OPTIONS
=======

**columns**

:   numbers indicating which columns to print. A negative value denotes
    the end of a range. A value must be from 1 to 1000.

****-delimiters****

:   input column delimiter character(s) (default: whitespace) Delimiters
    are changed to spaces on outout.

****-help****

:   display command help and exit

****-version****

:   display command metadata and exit

EXAMPLES
========

Sample usage:

          $echo a b c d|cprint 1000 -1 # reverse column order of a table
          d c b a

          $: switch first and second column and skip third column
          $: and print up to column 1000
          $ls -l |cprint 2 1 4 -1000

          $: column numbers may be reused
          $echo d e h l o r w|cprint 3 2 4 4 5 7 5 6 4 1
          h e l l o w o r l d

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
