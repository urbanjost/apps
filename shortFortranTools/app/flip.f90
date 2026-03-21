program bottomup
!@(#) print file from last line to first
use M_slurp, only: readstdin, writeup
implicit none
character(len=:),allocatable :: pageout(:) 
integer                      :: i 
   call readstdin(pageout)
   call writeup(pageout)
end program bottomup
