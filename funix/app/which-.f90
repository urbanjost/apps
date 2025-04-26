program demo_which
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdout=>OUTPUT_UNIT
use M_CLI2,    only : set_args, sgets, lget
use M_CLI2,    only : names=>unnamed
use M_strings, only : split
use M_system, only  : system_access, R_OK, W_OK, X_OK, F_OK

implicit none
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
character(len=:),allocatable :: searchpath
character(len=:),allocatable :: directories(:)
character(len=:),allocatable :: pathname
integer                      :: path_line_length
character(len=:),allocatable :: name
integer                      :: i,j
logical                      :: all
logical                      :: found
logical                      :: verbose
   call setup()
   call set_args('-all:a F ',help_text, version_text)                   ! get command-line arguments
   all=lget('all')                                                      ! process --all switch
   verbose=lget('verbose')                                              ! process --verbose switch

   call get_environment_variable(name="PATH", length=path_line_length)  ! get length of $PATH
   allocate(character(len=path_line_length) :: searchpath)              ! make a string variable long enough to hold $PATH
   call get_environment_variable(name="PATH", value=searchpath)         ! get value of environment variable $PATH
   call split(searchpath,directories,':')                               ! create array of directory names in $PATH

   if(size(names).eq.0)then
      write(stdout,'(a)')(trim(directories(i)),i=1,size(directories))
   else
      NAMESLOOP: do j=1,size(names)                                     ! try name appended to each directory name
         name=names(j)
         FOUND=.false.
         DIRLOOP: do i=0,size(directories)
            if(i.eq.0)then                                              ! if a full pathname assume it has / or \ in it
               if(index(name,'/').ne.0)then
                  pathname=trim(name)
               elseif(index(name,'\').ne.0)then
                  pathname=trim(name)
               else
                  cycle
               endif
               if(system_access(pathname,X_OK))then
                  write(stdout,'(a)')pathname
                  FOUND=.true.
               endif
               exit
            else
               pathname=trim(directories(i))//'/'//trim(name)
               if(system_access(pathname,X_OK))then
                  write(stdout,'(a)')pathname
                  FOUND=.true.
                  if(.not.all)exit
               endif
            endif
         enddo DIRLOOP
         if(.not.FOUND)then
            if(verbose)then
               write(stderr,'(*(a))') '<NOT FOUND>',name
            endif
         endif
      enddo NAMESLOOP
   endif
contains

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   which-(1f) - [FUNIX:FILESYSTEM] shows the full path of (shell) commands.',&
'   (LICENSE:PD)                                                            ',&
'                                                                           ',&
'SYNOPSIS                                                                   ',&
'    which- program_leafname [--all][--verbose]|[--help|--version]          ',&
'                                                                           ',&
'DESCRIPTION                                                                ',&
'   which-(1f) takes one or more pathnames. For each of its arguments       ',&
'   it prints to stdout the full path of the executables that would         ',&
'   have been executed when this argument had been entered at the           ',&
'   shell prompt. It does this by searching for an executable or            ',&
'   script in the directories listed in the environment variable PATH.      ',&
'                                                                           ',&
'OPTIONS                                                                    ',&
'   program_names  The command name leafs to render. If no path is specified',&
'                  the pathname directories are displayed.                  ',&
'   -a, --all      Print all matching executables in PATH, not just the first.',&
'   -V, --verbose  Print an error message when a command is not found         ',&
'   -v, --version  Print version information on standard output then exit     ',&
'   -h, --help     Print usage information on standard output then exit       ',&
'                                                                             ',&
'RETURN VALUE                                                                 ',&
'   which-(1) returns the number of failed arguments, or -1 when no           ',&
'   programname was given.                                                    ',&
'                                                                             ',&
'EXAMPLE                                                                      ',&
'AUTHOR                                                                       ',&
'   John S. Urban                                                             ',&
'LICENSE                                                                      ',&
'   Public Domain                                                             ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:      GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:      which-(1f)                                          ',&
'DESCRIPTION:  list pathnames of commands                          ',&
'VERSION:      1.0.0                                               ',&
'DATE:         2017-10-15                                          ',&
'AUTHOR:       John S. Urban                                       ',&
'HOME PAGE:    https://github.com/urbanjost/apps.git               ',&
'LICENSE:      Public Domain. This is free software: you are free to change and',&
'              redistribute it. There is NO WARRANTY, to the extent permitted by',&
'              law.                                                             ',&
'']
end subroutine setup
end program demo_which
