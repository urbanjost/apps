NAME
====

fcmd(1f) - \[FUNIX:FILESYSTEM\] find the pathname of commands and
optionally perform commands on them. (LICENSE:MIT)

SYNOPSIS
========

fcmd \[commands(s) \[-**-wild**\] \[
**--first**\]\[-**-ignorecase**\]\[-**-test**\] \[ **--cmd**
COMMAND;COMMAND,COMMAND;\`\`\` \]\|\[-**-long**\]\|\[-**-vi**\] \[
**--help**\|**--version**\]

DESCRIPTION
===========

fcmd(1f) takes one or more command names. For each of its arguments by
default it prints to stdout the path of the executables that would have
been executed when this argument had been entered at the shell prompt.
It does this by searching for an executable or script in the directories
listed in the environment variable PATH.

Optionally, commands can be specified to act on the path names found.

OPTIONS
=======

If no options are supplied the current search path is displayed one
directory per line.

        command(s)      names of commands to locate. simple globbing with *
                        and ? is allowed if the names are quoted.
        --ignorecase,i  ignore case of input command(s)
        --first,-f  locate first match of each executable name expression, not all.
        --cmd,-c    invoke the command on the files found. If present with
                    no parameter the desired command is assumed to be
                    the default editor (useful for finding and looking
                    at scripts). The editor command is looked for in the
                    environment variables FCEDIT, EDITOR and then VISUAL.
                    if not found, "vi" is used.

                    Multiple commands delimited by a semi-colon and/or a colon
                    may be used.

                    Abbreviations for common --cmd options:

                    --long,l   abbreviation for "--cmd 'ls -l'"
                    --vi       abbreviation for "--cmd 'vim'"

        --ok        Prompt for a y/n answer before executing the list of
                    commands on each file found.
        --test,-t   print first command found and stop
        --wild,-w   add asterisk as a suffix and prefix to all command names
                    being searched for.
        --version,-v  Print version information on standard output then
                      exit successfully.
        --help,-h   Print usage information on standard output then
                    exit successfully.

EXAMPLE
=======

Sample commands

        fcmd ls           # find path to ls(1) command
        fcmd '*sum*'      # find all commands containing "sum"
        fcmd sum -w       # also find all commands containing "sum"
        fcmd '*'          # list all commands in search path
        fcmd gunzip -c    # edit the script gunzip(1)
        fcmd ls dir       # find both commands

        # find all commands in path and a man-page if they have one
        fcmd '*' -c whereis

        # find a command and use the commands file(1) and stat(1) on the
        # pathnames found.
        fcmd pwd -c 'file;stat'

        Common commands to use are "cat -vet", "ls -l", "strings", "what",
        "sum", "whereis", "stat", "wc", "ldd", and "file".

        #!/bin/bash
        #@(#)  find which command is available and view file
        FILE="$1"
        case "$(fcmd -t w3m lynx links)" in
        w3m) w3m $FILE;;
        lynx) lynx $FILE;;
        links) links $FILE;;
        *) echo 'no browser found';exit;;
        esac

SEE ALSO
========

which(1), xargs(1)
