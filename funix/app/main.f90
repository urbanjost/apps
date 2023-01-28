program tac
!LICENSE:PD)
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use M_io,      only : swallow, fileread
use M_strings, only : switch
use M_CLI2   , only : set_args, lget, sget, filenames=>unnamed, specified
implicit none
character(len=:),allocatable     :: help_text(:), version_text(:)
call setup()
!  define command arguments, default values and crack command line
   call set_args(' ',help_text,version_text)
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        funix-(1f)',&
'DESCRIPTION:    fortran-based example programs',&
'VERSION:        1.0.0',&
'DATE:           2023-01-27',&
'AUTHOR:         John S. Urban',&
'HOME PAGE:      https://github.com/urbanjost/apps.git',&
'LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.',&
'                There is NO WARRANTY, to the extent permitted by law.',&
'']
end subroutine setup

end program tac
