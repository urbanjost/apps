NAME
====

sub(1f) - \[FILE EDIT\] replace fixed strings in files (LICENSE:MIT)

SYNOPSIS
========

sub \[ **--ignorecase**\]\[ **--dryrun**\]\[ **--verbose**\] old new
filename(s)

         or

sub **--help**\| **--version**\| **--usage**

DESCRIPTION
===========

The sub(1) utility changes strings in-place in files.

trailing spaces on OLD and NEW are ignored.

TABS are expanded.

files named on the command are modified in-place, so you may want to
make a copy of the original before converting it. sub(1) prints a
message indicating any of the input files it actually modifies.

Do not change binary files with this program, which uses sequential
access to read and write the files. It can corrupt binary files.

OPTIONS
=======

**old**

:   string to look for

**new**

:   the replacement string

**filenames**

:   list of files to replace strings in

****--ignorecase**,**-i****

:   ignore case of input

****--ask**,**-a****

:   interactively confirm each change

****--dryrun**,**-d****

:   does all file operations except for moving the changed file back to
    the original. Implies **--verbose**.

****--help**,**-h****

:   display a help message and exit.

****--usage**,**-u****

:   display options table

****--version**,**-v****

:   display version information and exit.

****--verbose**,**-V****

:   print information about what the program changes.

AUTHOR
======

John S. Urban

LICENSE
=======

MIT
---
