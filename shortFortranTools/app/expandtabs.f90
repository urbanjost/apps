program cleanup
!@(#) print file from last line to first
use M_slurp, only: readstdin
use M_aux,   only: dilate
implicit none
character(len=:),allocatable :: pageout(:)
integer                      :: i
   call readstdin(pageout)
   write(*,'(a)')(dilate(pageout(i)),i=1,size(pageout))
end program cleanup
