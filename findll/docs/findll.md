# NAME

**findll(1f)** - \[FILE FILTER\] find long lines **(LICENSE:PD)**

# SYNOPSIS

    findll [ -l LENGTH] [ -wrap] | [ -help| -version] [FILENAMES]

# DESCRIPTION

find lines in files over a specified length and print them; or wrap each
input line to fit in specified width.

Lines ending in a backslash ("\\) are concatenated with the next line.

Non-printable characters are not treated specially (eg. a tab character
is treated as a single character).

# OPTIONS

***FILENAMES***

:   the files to scan for long lines

****-l** NUMBER**

:   maximum line length of lines to ignore. The default is 132.

****--wrap****

:   instead of locating and displaying long lines, fold the lines at the
    specified line length

****--help****

:   display this help and exit

****--version****

:   output version information and exit

# EXAMPLES

Sample commands:

       $ findll <filename

       # show lines over 72 characters in length
       $ findll *.f *.F -l 72
       # show length of all lines on stdin
       $ findll -l -1

# AUTHOR

John S. Urban

# LICENSE

Public Domain
