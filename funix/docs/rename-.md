NAME
====

rename-(1f) - \[FUNIX:FILESYSTEM\] rename files by replacing first
occurrence of a string in a filename with new string (LICENSE:PD)

SYNOPSIS
========

rename- old new file\`\`\` \[ **-verbose**\]\[ **-dryrun**\]\|\[
**-help**\|**-version**\]

DESCRIPTION
===========

rename-(1f) will rename the specified files by replacing the first
occurrence of expression in their name by replacement.

EXAMPLES
========

Given the files foo1, \`\`\`, foo9, foo10, ..., foo278, the commands

          rename- foo foo0 foo?
          rename- foo foo0 foo??

will turn them into foo001, \`\`\`, foo009, foo010, ..., foo278. And

          rename- .htm .html *.htm

will fix the extension of \*.htm files.

OPTIONS
=======

**old**

:   represents a string to change in filenames

**new**

:   the replacement string for EXPRESSION.

****-verbose****

:   Print information about what the program does.

****--help****

:   Display a help message and exit.

****--version****

:   Display version information and exit.

****--dryrun****

:   Does all file operations except for moving the changed file back to
    the original. Implies **--version**.

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
