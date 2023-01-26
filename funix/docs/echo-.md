NAME
====

echo-(1f) - \[FUNIX\] display a line of text (LICENSE:PD)

SYNOPSIS
========

echo- \[OPTION\]\`\`\` \[STRING\]...

DESCRIPTION
===========

Echo the STRING(s) to standard output.

OPTIONS
=======

****-n****

:   do not output the trailing newline

****-E****

:   write message to stderr instead of stdout

****-ne****

:   disable interpretation of escape escapes

****-r** \[n\]**

:   output a string repeatedly until killed if n is blank or n \<= 0,
    else repeat output "n" times. Default value is "1".

****-x****

:   escape character. Default is %

****--help****

:   display this help and exit **--version** output version information
    and exit

Escape sequences

                %%     escape character           %a     alert (BEL)
                %b     backspace                  %c     suppress further output
                %e     escape                     %E     escape
                %f     form feed                  %n     new line
                %r     carriage return            %t     horizontal tab
                %v     vertical tab
                %oNNN  byte with octal value NNN (1 to 3 digits)
                %dNNN  byte with decimal value NNN (1 to 3 digits)
                %xHH   byte with hexadecimal value HH (1 to 2 digits)

EXAMPLES
========

Example invocations:

             echo- Echo this text to stderr -E
             echo- y -r             # repeat "y" until interrupted
             echo- ''"#"'' -r 80 -n # create 80-character break line
             echo- %e[2J   # clear screen on vt102-compatible terminal or emulator
