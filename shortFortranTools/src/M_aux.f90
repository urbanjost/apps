module M_aux
use,intrinsic::iso_fortran_env,only:int8
use M_slurp, only : blame
implicit none
private
public :: toupper
public :: tolower
public :: dilate
public :: stretch
public :: argn
public :: cqsort
interface argn
   module procedure argn_char, argn_int
end interface argn
contains

function stretch(line,length) result(strout)
!@(#) M_strings stretch(3f) return string padded to at least specified length
character(len=*),intent(in)  :: line
integer,intent(in)           :: length
character(len=:),allocatable :: strout
   allocate(character(len=max(length,len(line))) :: strout)
   strout(:)=line
end function stretch

function dilate(instr) result(outstr)
!@(#) M_strings::dilate(3f): convert tabs to spaces and trim line, removing CRLF chars
character(len=*),intent(in)  :: instr     ! input line to scan for tab characters
character(len=:),allocatable :: outstr    ! tab-expanded version of INSTR produced
integer,parameter            :: tabsize=8 ! assume a tab stop is set every 8th column
integer                      :: ipos      ! position in OUTSTR to put next character of INSTR
integer                      :: istep     ! counter advances thru string INSTR one char at a time
integer                      :: i, icount
   icount=0 ! number of tab characters in input
   do i=1,len(instr)
      if(iachar(instr(i:i))==9)icount=icount+1
   enddo
   allocate(character(len=(len(instr)+8*icount)) :: outstr)
   outstr(:)=" "                              ! this SHOULD blank-fill string
   ipos=1                                     ! where to put next character in output string OUTSTR
   SCAN_LINE: do istep=1,len_trim(instr)      ! look through input string one character at a time
      EXPAND_TABS : select case (iachar(instr(istep:istep)))! take different actions based on character found
      case(9)      ! if character is a horizontal tab and move pointer out to appropriate column
         ipos = ipos + (tabsize - (mod(ipos-1,tabsize)))
      case(10,13)  ! convert carriage-return and new-line to space ,typically to handle DOS-format files
         ipos=ipos+1
      case default ! character is anything else other than a tab,newline,or return.
         outstr(ipos:ipos)=instr(istep:istep)
         ipos=ipos+1
      end select EXPAND_TABS
   enddo SCAN_LINE
   outstr=trim(outstr)
end function dilate

elemental pure function tolower(str) result (string)
! Changes a string to lowercase
character(*), intent(in) :: str 
character(len(str))      :: string 
integer                  :: i 
integer,parameter        :: diff = iachar('A')-iachar('a') 
   string = str
   do concurrent (i = 1:len_trim(str))  
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = achar(iachar(str(i:i))-diff)   
      case default
      end select
   enddo
end function tolower

recursive function cqsort(d) result(sorted)
!@(#) Compact implementation of the QuickSort algorithm
! This is derived from an example in "Modern Fortran in Practice" by Arjen Markus. As such,
! this work (cqsort) is licensed under the Creative Commons Attribution 3.0 Unported License.
! To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/
character(len=*),intent(in) :: d(:) 
character(len=:),allocatable :: sorted(:) 
   if(allocated(sorted))deallocate(sorted)
   allocate(character(len=len(d)) ::sorted(size(d)))
   if (size(d) > 1) then
      sorted(:) = [character(len=len(d)) :: cqsort(pack(d(2:),d(2:)>d(1))),d(1),cqsort(pack(d(2:),d(2:)<=d(1)))]
   else
      sorted(:) = d
   endif
end function cqsort

recursive function cqsort_v1(d) result(sorted)
!@(#) Compact implementation of the QuickSort algorithm
! This is derived from an example in "Modern Fortran in Practice" by Arjen Markus. As such,
! this work (cqsort_v1) is licensed under the Creative Commons Attribution 3.0 Unported License.
! To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/
character(len=*),intent(in) :: d(:) 
character(len=len(d))       :: sorted(1:size(d)) 
   if (size(d) > 1) then
      sorted(:) = [character(len=len(d)) :: cqsort_v1(pack(d(2:),d(2:)>d(1))),d(1),cqsort_v1(pack(d(2:),d(2:)<=d(1)))]
   else
      sorted(:) = d
   endif
end function cqsort_v1

elemental pure function toupper(str) result (string)
! Changes a string to uppercase 
character(*), intent(in)      :: str        
character(len(str))           :: string    
integer                       :: i        
integer(kind=int8), parameter :: ade_a = iachar('a'), ade_z = iachar('z')
integer(kind=int8), parameter :: diff = iachar('A',kind=int8) - iachar('a',kind=int8)
integer(kind=int8)            :: ade_char
   do concurrent(i=1:len(str))                       
      ade_char = iachar(str(i:i), int8)              
      if (ade_char >= ade_a .and. ade_char <= ade_z) ade_char = ade_char + diff
      string(i:i) = achar(ade_char)
   enddo
   if(len(str).eq.0)string = str
end function toupper

function argn_char(nth,default) result(arg)
! get nth argument from command line
integer                      :: count, nth, argument_length, istat
character(len=:),allocatable :: arg
character(len=*),intent(in)  :: default
   arg=default
   call get_command_argument(number=nth,length=argument_length,status=istat)
   if(istat.eq.0)then
      if(allocated(arg))deallocate(arg)
      allocate(character(len=argument_length) :: arg)
      call get_command_argument(nth, arg,status=istat)
   endif
   if(istat.ne.0)arg=default
   if(arg.eq.'')arg=default
end function argn_char

function argn_int(nth,default) result(iarg)
integer                      :: nth 
integer,intent(in)           :: default 
character(len=:),allocatable :: arg 
character(len=255)           :: iomsg 
integer                      :: iarg 
integer                      :: iostat 
arg=argn_char(nth,'')//repeat(' ',80)
if(arg.eq.'')then
   iarg=default
else
   read(arg,'(i80)',iostat=iostat,iomsg=iomsg)iarg
   if(iostat.ne.0)then
      call blame(trim(iomsg))
      iarg=-huge(iarg)-1
   endif
endif
end function argn_int

end module M_aux
