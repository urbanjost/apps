NAME
====

touch-(1f) - \[FUNIX:FILESYSTEM\] change file access and modify
timestamps to current time, creating file if necessary (LICENSE:PD)

SYNOPSIS
========

touch- \[FILE\`\`\` \[ **-date** DATE\]\]\|\[
**--help**\|**--version**\|**--verbose**\]

DESCRIPTION
===========

Make sure specified filenames exist (by creating them as empty files)
and change file access time to current time or specified time.

OPTIONS
=======

****-date****

:   Change the file timestamps to the specified date instead of the
    current time. Uses guessdate(3f) to read the date.

****--verbose****

:   Display messages showing command progress

****--help****

:   Display help text and exit

****--version****

:   Display version information and exit

EXAMPLES
========

Sample commands

       touch- *.f90
       touch- * -date 2000-01-02 10:20:30

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
