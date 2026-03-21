program lowercase
!@(#) print file converted to lowercase
use M_slurp, only: readstdin, writedown
use M_aux,   only: tolower
implicit none
character(len=:),allocatable :: pageout(:) 
integer                      :: i 
   call readstdin(pageout)
   pageout(:)=tolower(pageout)
   call writedown(pageout)
end program lowercase
