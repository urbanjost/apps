program uppercase
!@(#) print file converted to uppercase
use M_slurp, only: readstdin, writedown
use M_aux,   only: toupper
implicit none
character(len=:),allocatable :: pageout(:) 
integer                      :: i 
   call readstdin(pageout)
   pageout(:)=toupper(pageout)
   call writedown(pageout)
contains
end program uppercase
