program lineup
!@(#) line up :: string, mostly for pretty Fortran declarations
use M_slurp, only: readstdin
use M_aux,   only: argn, stretch
implicit none
character(len=:),allocatable :: text(:), left, fence 
integer                      :: i, j, longest_start ,fencelen 
   fence=argn(1,'::') ! the string to line up on
   fencelen=len(fence)
   call readstdin(text)
   longest_start=0
   j=0
   do i=1,size(text) ! find right-most start of fence string
      j=index(text(i),fence)
      if(j.gt.1)then
         j=len_trim(text(i)(:j-1))
      endif
      longest_start=max(longest_start,j)
   enddo
   do i=1,size(text)
      j= index(text(i),fence)
      if( j.gt.0)then ! if found fence stretch left to desired length
         left=trim(text(i)(:j-1))
         write(*,'(*(g0))')stretch(left,longest_start+1),fence,' ',adjustl(trim(text(i)(j+fencelen:)))
      else
         write(*,'(*(g0))')trim(text(i))
      endif
   enddo
end program lineup
