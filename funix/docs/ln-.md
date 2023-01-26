NAME
====

ln-(1f) - \[FUNIX:FILESYSTEM\] create hard links to a file (LICENSE:PD)

SYNOPSIS
========

Formats:

       ln- TARGET LINK_NAME     #  create a link to TARGET with the name LINK_NAME.
       ln- TARGET               #  create a link to TARGET in the current directory.
       ln- TARGET``` DIRECTORY  #  create links to each TARGET in DIRECTORY.

DESCRIPTION
===========

Create hard links (not symbolic links) each destination (name of new
link) should not already exist. When creating hard links, each TARGET
must exist. Symbolic links can hold arbitrary text; if later resolved, a
relative link is interpreted in relation to its parent directory.

OPTIONS
=======

**TARGET**

:   name of existing file

**LINK\_NAME**

:   if LINK\_NAME follows TARGET create a link called LINK\_NAME that
    points to TARGET

**DIRECTORY**

:   if last option is a directory previous filenames on command are
    linked into DIRECTORY

****--help****

:   display this help and exit

****--version****

:   output version information and exit

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
