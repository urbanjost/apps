NAME
====

**grep-**(1f) - \[FUNIX\] search a file for a pattern (LICENSE:PD)

SYNOPSIS
========

grep- *pattern* \[ **-i**\]\[ **-E**\| **-G**\]\|\[ **--help**\|
**--version**\]

DESCRIPTION
===========

Find lines on stdin that contain the specified regular expression
*pattern*.

OPTIONS
=======

***pattern***

:   regular expression

****-i****

:   Ignore case distinctions in both the PATTERN and the input files.

MATCHER SELECTION
-----------------

****-E****

:   Interpret PATTERN as an **ERE**(extended regular expression).

****-G****

:   Interpret PATTERN as a **BRE**(basic regular expression). This is
    the default. If present, it takes precedence over **-E**.

BASIC VS EXTENDED REGULAR EXPRESSIONS
-------------------------------------

In basic regular expressions the meta-characters ?, +, {, \|, (, and )
lose their special meaning; instead use the backslashed versions \\?,
\\+, \\{, \\\|, \\(, and \\).

INFORMATIVE
-----------

****--help****

:   display this help and exit

****--version****

:   output version information and exit

EXAMPLES
========

Sample commands

           grep-  '^T.*found it' <foundit

REPORTING BUGS
==============

Report grep- bugs to \<http://www.urbanjost.altervista.org/index.html\>

SEE ALSO
========

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
