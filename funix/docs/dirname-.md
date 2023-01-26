NAME
====

dirname-(1f) - \[FUNIX:FILESYSTEM\] strip last component from file name
(LICENSE:PD)

SYNOPSIS
========

dirname- \[NAME\`\`\`\] \[ **-zero**\]\|**-help**\|**-version**\]

DESCRIPTION
===========

Output each NAME with its last non-slash component and trailing slashes
removed. if NAME contains no /'s, output '.' (meaning the current
directory).

If no NAME is specified read names from stdin.

OPTIONS
=======

****-zero****

:   end each output line with NUL, not newline

****-help****

:   display this help and exit

****-version****

:   output version information and exit

EXAMPLES
========

Sample program executions:

         dirname- /usr/bin/          -> "/usr"
         dirname- dir1/str dir2/str  -> "dir1" followed by "dir2"
         dirname- stdio.h            -> "."

SEE ALSO
========

dirname(1), basename(1), readlink(1), realpath(1)
