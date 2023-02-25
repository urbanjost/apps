program xpand
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stderr=>error_unit
use M_CLI2,    only : set_args, iget, lget, files=>unnamed
use M_strings, only : dilate
use M_io,      only : getline
implicit none

! ident_1="@(#) xpand(1f) filter removes tabs trailing white space adjacent blank lines"

character(len=:),allocatable  :: line
character(len=:),allocatable  :: out
integer                       :: ios          ! error flag from read
integer                       :: i,ii
integer                       :: iblanks
integer                       :: blanks
integer                       :: width
character(len=:),allocatable  :: help_text(:)
character(len=:),allocatable  :: version_text(:)
logical                       :: verbose
character(len=*),parameter    :: g='(*(g0))'
character(len=*),parameter    :: g1='(*(g0,1x))'
integer                       :: icount

   call setup()
   call set_args('-blanks:b -1 --width:w 132',help_text,version_text)
   width=iget('width')
   blanks=iget('blanks')
   verbose=lget('verbose')
   icount=0

   if(size(files).eq.0)then                                ! default is to read from stdin, which the filename "-" designates
      files=['-']
   endif

   ALLFILES: do i=1,size(files)                            ! loop through all the filenames

      if(verbose)write(stderr,g)'FILE:',i,':NAME:',trim(files(i))
      if(files(i).eq.'-'.or.files(i).eq.'stdin')then       ! special filename designates stdin
         ii=stdin
      else                                                 ! open a regular file
         ii=20
         open(unit=ii,file=trim(files(i)),iostat=ios,status='old',form='formatted')
         if(ios.ne.0)then
             write(stderr,g) '*xpand* failed to open:',trim(files(i))
            cycle ALLFILES
         endif
      endif
      iblanks=0
      open(unit=ii,pad='yes')
      ALLINES: do while (getline(line,ii)==0)
         icount=icount + 1
         line=dilate(line)
         if(blanks.ge.0)then
            if(line.eq.'')then
               iblanks=iblanks+1
               if(iblanks.gt.blanks)then
                  cycle ALLINES
               endif
            else
               iblanks=0
            endif
         endif
         if(len(line).gt.width)then
            write(stderr,g) trim(files(i)),':L:',icount,':C:',len(line),':',line
         endif
         write(*,"(a)")line
      enddo ALLINES
      close(unit=ii,iostat=ios)
   enddo ALLFILES
contains

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   xpand(1f) - [FUNIX] expand tabs,remove trailing white space and',&
'   (optionally)  adjacent blank lines                             ',&
'                                                                  ',&
'SYNOPSIS                                                          ',&
'    xpand [ -blanks NNN][ --width ] FILENAME(S)                   ',&
'                                                                  ',&
'DESCRIPTION                                                       ',&
'   Convert tabs in each FILE to spaces, writing to standard output.',&
'   If no filename is specified standard input is read. Tab stops are',&
'   assumed to be every eight (8) columns. Trailing spaces, carriage ',&
'   returns, and newlines are removed.                               ',&
'                                                                    ',&
'OPTIONS                                                             ',&
'   FILENAMES    files to expand tab characters in.                  ',&
'   --width,-w   line width to produce a warning if exceeded. Defaults',&
'                to 132. If less then or equal to 0 no warnings are   ',&
'                produced. Warning messages appear on stderr.         ',&
'   --blanks,-b  maximum number of adjacent blank lines to retain.    ',&
'                Default is -1, which is equivalent to unlimited.     ',&
'STANDARD OPTIONS                                                     ',&
'   --help      display this help and exit                            ',&
'   --version   output version information and exit                   ',&
'   --usage     basic usage information including a list of arguments ',&
'   --verbose   verbose mode                                          ',&
'                                                                     ',&
'EXAMPLES                                                             ',&
'   Sample commands:                                                  ',&
'                                                                     ',&
'     xpand < input.txt > output.txt                                  ',&
'     xpand input.txt   > output.txt                                  ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PROGRAM:      xpand(1f)',&
'DESCRIPTION:  convert tabs to spaces',&
'AUTHOR:       John S. Urban         ',&
'VERSION:      1.0.0, 20151220       ',&
'AUTHOR:       John S. Urban         ',&
'VERSION:      1.1.0, 20220626 -- allow filenames, width warning',&
'LICENSE:      MIT                                              ',&
'']
end subroutine setup

end program xpand
