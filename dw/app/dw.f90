program demo_split
! find duplicate words
use, intrinsic :: iso_fortran_env, only: stdin => input_unit
use, intrinsic :: iso_fortran_env, only: stderr => error_unit
use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
use M_io, only: read_line
use M_strings, only: split, lower
implicit none
integer                       :: stat, i, ios, icount
character(len=:), allocatable :: line, last
character(len=:), allocatable :: array(:) ! output array of tokens
   last = ' '
   icount = 0
   open (unit=stdin, pad='yes')
   call setup()
   INFINITE: do while (read_line(line, ios=stat) == 0)
      icount = icount + 1
      call split(line, array, ' ,;!.?&')
      do i = 1, size(array)
         if (lower(array(i)) .eq. last) then
            write (*, '(i0.6,1x,a)') icount, trim(last)//'::'//trim(line)
         endif
         last = lower(array(i))
      enddo
   enddo INFINITE
   if (.not. is_iostat_end(stat)) then
      write (stderr, '(*(g0))') &
      & 'error: line ', icount, '==>', trim(line)
   endif
contains
subroutine setup()
!! Put everything to do with command parsing here
use M_CLI2,  only : set_args, set_mode
character(len=:),allocatable :: help_text(:), version_text(:)
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   dw(1f) - [TEXT] find duplicate words',&
'   (LICENSE:PD)',&
'',&
'SYNOPSIS',&
'    dw < infile >outfile',&
'',&
'DESCRIPTION',&
'',&
'   dw(1) filters its standard input, printing on standard output the',&
'   duplicated word and the line it occurs on when a word appears twice',&
'   in succession. Each duplicate word is prefixed by its line number.',&
'',&
'   A word starts with a letter or underscore, and is followed by zero',&
'   or more letters, underscores, or digits. Letter case is ignored.',&
'',&
'   This is a handy tool for finding a common typographical error in',&
'   documentation.',&
'',&
'OPTIONS',&
'   --help            display this help and exit',&
'   --usage           display state of command options and exit',&
'   --version         output version information and exit',&
'',&
'EXAMPLE',&
'  Sample run:',&
'',&
'   dw <<\end_of_data',&
'      In the the house there is a a cat.',&
'      The cat in the house',&
'      house is black.  There is is a dog dog in the',&
'      house that does not like cats.  Cats are feline',&
'      creatures.',&
'   end_of_data',&
'',&
'  Sample output:',&
'',&
'   000001 the::      In the the house there is a a cat.',&
'   000001 a::      In the the house there is a a cat.',&
'   000003 house::      house is black.  There is is a dog dog in the',&
'   000003 is::      house is black.  There is is a dog dog in the',&
'   000003 dog::      house is black.  There is is a dog dog in the',&
'   000004 cats::      house that does not like cats.  Cats are feline',&
'',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PROGRAM:     dw(1)                >',&
'@(#)DESCRIPTION: find duplicate words >',&
'@(#)VERSION:     1.0 2023-02-25       >',&
'@(#)AUTHOR:      John S. Urban        >',&
'@(#)LICENSE:     Public Domain        >',&
'']
call set_mode([character(len=20) :: 'strict','ignorecase'])
! a single call to set_args can define the options and their defaults, set help
! text and version information, and crack command line.
call set_args( ' ',help_text,version_text)
end subroutine setup

end program demo_split
