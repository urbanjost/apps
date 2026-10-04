program demo_print_watch
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
implicit none
integer,parameter            :: dp=kind(0.0d0)
integer                      :: i,j,length,errcode,cmdstat,exitstat
character(len=*),parameter   :: doublequote='"',g0='(*(g0))',g1='(*(g0,1x))'
character(len=256)           :: cmdmsg
character(len=:),allocatable :: arg,arg_requote,cmd
real(kind=dp)                :: start,finish
   cmd=''
   do i=1,command_argument_count()
      call get_command_argument(i,length=length)
      arg=repeat(' ',length)
      call get_command_argument(i,arg)
      arg_requote=''
      ! assume you double a quote on non-GNU/Linux and Unix to escape it
      ! probably needs changed in some PC environments
      do j=1,length
         arg_requote=arg_requote//arg(j:j)
         if(arg(j:j).eq.doublequote) arg_requote=arg_requote//doublequote
      enddo
      arg=arg_requote
      if(i.ne.1)arg=doublequote//arg//doublequote
      cmd=cmd//arg//' '
   enddo

   if(cmd.eq.'')stop

   start=now_as_julian()
   call execute_command_line(cmd,cmdstat=cmdstat,cmdmsg=cmdmsg,exitstat=exitstat)
   finish=now_as_julian()
   write(*,'(a,":",f0.3,"sec")')cmd,(finish-start)*86400
   if(cmdstat.ne.0)then
      write(stderr,g0)trim(cmdmsg)
      stop 1
   endif
   if(exitstat.ne.0)then
      stop exitstat
   endif
contains
function now_as_julian() result(julian)
integer       :: dat(8), A, Y, M, JDN
real(kind=dp) :: julian
   call date_and_time(values=dat)
   associate(yr=>dat(1),mo=>dat(2),day=>dat(3),&
   & hr=>dat(5),min=>dat(6),sec=>dat(7)-(dat(4)*60)+dat(8)/1000.0_dp)
   A=(14-mo)/12
   Y=yr+4800-A
   M=mo+12*A-3
   JDN=day + (153*M+2)/5 + 365*Y + Y/4 - Y/100 + Y/400 - 32045
   julian=JDN + (hr-12)/24.0_dp + min/1440.0_dp + sec/86400.0_dp
end associate
end function now_as_julian
end program demo_print_watch
