# NAME

**which-(1f)** - \[FUNIX:FILESYSTEM\] shows the full path of (shell)
commands. **(LICENSE:PD)**

# SYNOPSIS

    which- program_leafname [--all][--verbose]|[--help|--version]

# DESCRIPTION

**which-(1f) takes one or more pathnames. For each of its arguments** it
prints to stdout the full path of the executables that would have been
executed when this argument had been entered at the shell prompt. It
does this by searching for an executable or script in the directories
listed in the environment variable PATH.

# OPTIONS

**program_names**

:   The command name leafs to render. If no path is specified the
    pathname directories are displayed.

****-a,** **--all****

:   Print all matching executables in PATH, not just the first.

****-V,** **--verbose****

:   Print an error message when a command is not found

****-v,** **--version****

:   Print version information on standard output then exit

****-h,** **--help****

:   Print usage information on standard output then exit

# RETURN VALUE

**which-(1) returns the number of failed arguments, or -1 when no**
programname was given.

# EXAMPLE

# AUTHOR

John S. Urban

# LICENSE

Public Domain
