module M_slurp
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>INPUT_UNIT,stdout=>OUTPUT_UNIT
private
public :: readstdin
public :: file_to_byte
public :: getline
public :: writedown
public :: writeup
public :: blame

contains

subroutine readstdin(pageout)
implicit none
character(len=:),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=1),allocatable             :: text(:)    ! array to hold file in memory
   call file_to_byte(text) ! allocate character array and copy file into it
   if(allocated(text))then
      pageout=page(text)
      deallocate(text)     ! release memory
   else
      pageout=[character(len=0):: ]
   endif
contains
function page(array)  result (table)
! ident_5="@(#) page(3fp) function to copy char array to page of text"
character(len=1),intent(in)  :: array(:)
character(len=:),allocatable :: table(:)
integer                      :: i
integer                      :: linelength
integer                      :: length
integer                      :: lines
integer                      :: linecount
integer                      :: position
integer                      :: sz
character(len=1),parameter   :: nl = char(10)
character(len=1),parameter   :: cr = char(13)
   lines = 0
   linelength = 0
   length = 0
   sz=size(array)
   do i = 1,sz
      if( array(i) == nl )then
         linelength = max(linelength,length)
         lines = lines + 1
         length = 0
      else
         length = length + 1
      endif
   enddo
   if( sz > 0 )then
      if( array(sz) /= nl )then
         lines = lines+1
      endif
   endif
   if(allocated(table))deallocate(table)
   allocate(character(len=linelength) :: table(lines))
   table(:) = ' '
   linecount = 1
   position = 1
   do i = 1,sz
      if( array(i) == nl )then
         linecount=linecount+1
         position=1
      elseif( array(i) == cr )then
      elseif( linelength /= 0 )then
         table(linecount)(position:position) = array(i)
         position = position+1
      endif
   enddo
end function page
end subroutine readstdin

subroutine file_to_byte(text)
implicit none
!@(#) file_to_byte(3f) allocate text array and read stdin into it
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer                                  :: nchars=0    ! holds size of file
integer                                  :: igetunit    ! use newunit=igetunit in f08
integer                                  :: iostat=0    ! used for I/O error status
integer                                  :: i
integer                                  :: icount
character(len=256)                       :: message
character(len=4096)                      :: label
character(len=:),allocatable             :: line
   label=''
   message=''
   ! copy stdin to a scratch file
   call copystdin_ascii()
   if(iostat == 0)then  ! if file was successfully opened
      inquire(unit=igetunit, size=nchars)
      if(nchars <= 0)then
         call blame( '*file_to_byte* empty file '//trim(label) )
         return
      endif
      ! read file into text array
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=iostat,iomsg=message) text      ! load input file -> text array
      if(iostat /= 0)then
         call blame( '*file_to_byte* bad read of '//trim(label)//':'//trim(message) )
      endif
   else
      call blame('*file_to_byte* '//message)
      allocate ( text(0) )           ! make enough storage to hold file
   endif
   close(iostat=iostat,unit=igetunit)            ! close if opened successfully or not
contains
subroutine copystdin_ascii()
integer :: iostat
   open(newunit=igetunit, iomsg=message,&
   &form="unformatted", access="stream",status='scratch',iostat=iostat)
   open(unit=stdin,pad='yes')
   INFINITE: do while (getline(line,iostat=iostat)==0)
      if(is_iostat_eor(iostat))then
         ! EOR does not imply NEW_LINE so could add NEW_LINE to end of file
         write(igetunit)line,new_line('a')
      else
         write(igetunit)line
      endif
   enddo INFINITE
   rewind(igetunit,iostat=iostat,iomsg=message)
end subroutine copystdin_ascii
end subroutine file_to_byte

subroutine blame(message)
character(len=*) :: message
   write(stderr,'(a)')trim(message)    ! write message to standard error
end subroutine blame

function getline(line,iostat) result(ier)
implicit none
! ident_11="@(#) M_io getline(3f) read a line from stdin into allocatable string up to line length limit"
character(len=:),allocatable,intent(out) :: line
integer,intent(out)                      :: iostat
character(len=4096)                      :: message
integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=buflen)                    :: buffer
integer                                  :: isize
integer                                  :: ier
   line_local=''
   iostat=huge(0)
   ier=0
   INFINITE: do                                                   ! read characters from line and append to result
      read(stdin,pad='yes',iostat=iostat,fmt='(a)',advance='no', &
      & size=isize,iomsg=message) buffer                          ! read next buffer (might use stream I/O for files
      if(isize > 0)line_local=line_local//buffer(:isize)          ! append what was read to result
      if(is_iostat_eor(iostat))then                               ! if hit EOR reading is complete
         exit INFINITE                                            ! end of reading line
      elseif(iostat /= 0)then                                      ! end of file or error
         line=trim(message)
         ier=iostat
         exit INFINITE
      endif
   enddo INFINITE
   line=line_local                                                ! trim line
end function getline

subroutine writedown(text)
!@(*) write text array to stdout trimmed from top line down
character(len=*),intent(in) :: text(:) 
integer                     :: i 
   write(stdout,'(a)')(trim(text(i)),i=1,size(text))
end subroutine writedown

subroutine writeup(text)
!@(*) write text array to stdout trimmed from bottom line up
character(len=*),intent(in) :: text(:) 
integer                     :: i 
   write(stdout,'(a)')(trim(text(i)),i=size(text),1,-1)
end subroutine writeup

end module M_slurp
