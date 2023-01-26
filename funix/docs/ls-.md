NAME
====

ls-(1f) - \[FUNIX:FILESYSTEM\] list files in a directory (LICENSE:PD)

SYNOPSIS
========

ls- \[directory\|**--version**\|**--help**\] \[ **-a**\] \[
**-l**\|**-csv**\]

DESCRIPTION
===========

Given a directory name list files in the directory

OPTIONS
=======

**pathname**

:   name of directory or pathname to display contents of. Defaults to
    current directory.

****-a****

:   show hidden files (files beginning with ".").

****-l****

:   long listing

****-fmt****

:   alternate format for date and time. Calls fmtdate(3f).

****-csv****

:   generate output as a CSV file. Filenames should not have ,"'
    characters in them. Very useful for use with sqlite3(1) and making a
    file that can be read into most spreadsheets,

****--help****

:   display command help and exit

****--version****

:   output version information and exit

EXAMPLES
========

Sample command lines \`\`\`

            ls-
            ls- . /tmp -l

            # add Unix Epoch date
            ls- -l -fmt year-month-day hour:minute:second epoch

            # use the phase of the moon for the date
            # ls- -l -fmt %p %P

EXTENDED SQLITE EXAMPLE
=======================

The CSV output can often just be read by spreadsheets. Typically the
file suffix ".csv" is required. Assuming you have bash(1), sqlite3(1)
and column(1) on your platform this is an example script that shows how
SQL statements can be used to generate many kinds of file reports
(number of bytes owned by users, number of files, sorting, \`\`\` It
assumes you or somone who will assist you is familiar with SQL and
sqlite3(1):

       #!/bin/bash
       #@(#) list files accessed today in current directory
       export SCRATCH=/tmp/$(uuidgen).csv          # create scratch file name
       trap "/bin/rm -f $SCRATCH" EXIT             # ensure scratch file is removed
       ls- -csv -- . |tail -n +2>$SCRATCH          # generate CSV file
       (
       # read CSV file into an SQLite file and generate a report as HTML table
       sqlite3 \
        -cmd 'CREATE TABLE directory("Inode_number" INT,
          "Number_of_blocks_allocated" INT,
          "File_mode" TEXT,
          "Number_of_links" INT,
          "Owner" TEXT,
          "Groupname" TEXT,
          "File_size" INT,
          "Last_access" DATE,
          "Last_modification" DATE,
          "Last_status_change" DATE,
          "Pathname" TEXT );' \
          -cmd '.mode csv' \
          -cmd ".import $SCRATCH directory" <<\end_of_file
       -- .schema
       .mode column
       .header on
       SELECT Pathname, File_mode, Owner, Groupname, File_size, strftime('%Y-%m-%d %H:%M:%S', Last_access) as "Last_Access"
          FROM directory
          WHERE DATE('now', 'start of day') < Last_access
          ORDER BY Pathname ASC;
       end_of_file
       )| column -t -s '|'
       exit

AUTHOR
======

John S. Urban

LICENSE
=======

Public Domain
