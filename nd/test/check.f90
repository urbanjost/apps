program check
   call execute_command_line('env')
!$!----------------------------------------------------------------------------------------------------------------------------------
!$OUTPUT ../tmp/_expect1.txt
!------------------------------------------------------------------------------------------------------------------------------------
!        | *nd* numerical differences
!        | old file=../tmp/_in1
!        | new file=../tmp/_in2
!        | percent threshold =   1.0000000000000001E-005 (%)
!------------------------------------------------------------------------------------------------------------------------------------
!        |maximum difference=   8.1345875138179356E-006 (%)
!------------------------------------------------------------------------------------------------------------------------------------
! Numerical Differences=           0
!------------------------------------------------------------------------------------------------------------------------------------
!$OUTPUT
!$!----------------------------------------------------------------------------------------------------------------------------------
!$OUTPUT ../tmp/_delim1.txt
!10, 20, 30.4, 50505e3;202.303|  10 |
!$OUTPUT ../tmp/_delim2.txt
!10.0000000001 20.0e0 30.400 50505e3 202.303  10
!$OUTPUT
!    If the program finds significant differences it calls STOP 1
!    so the system command should flag an error
!$SYSTEM nd -old ../tmp/_delim1.txt -new ../tmp/_delim2.txt
!$SYSTEM rm -f ../tmp/_delim1.txt ../tmp/_delim2.txt
!$!----------------------------------------------------------------------------------------------------------------------------------
!
!        starting tests of nd
!
!        Initialize database entry for test
!$SYSTEM goodbad nd start
!
!        Run the nd(1) program on the test files
!$SYSTEM nd -old ../tmp/_in1 -new ../tmp/_in2 -percent 0.00001 >../tmp/_test1.txt
!
!        Use diff(1) on expected results and results and update database accordingly
!$SYSTEM diff -b ../tmp/_expect1.txt ../tmp/_test1.txt && goodbad nd good
!
!        Clean up when tests complete successfully
!$SYSTEM rm ../tmp/_expect1.txt ../tmp/_in1 ../tmp/_in2 ../tmp/_test1.txt
!
!------------------------------------------------------------------------------------------------------------------------------------
!         nd UNIT TEST COMPLETE
!------------------------------------------------------------------------------------------------------------------------------------
!#export GITHUB=FALSE
!#export DEMO_OUTDIR=../../example
!#export DEMO_SUBDIR=FALSE
!export UFPP_DOCUMENT_DIR=$(pwd)
!ufpp TESTPRG90 F90 --cstyle doxygen --verbose --allow_links -i nd.[fF][fF] -o ../nd.f90
!txt2man doc/nd.1.man >../../man/man3/nd.1
!man2html ../../man/man3/nd.1 > ../../docs/nd.1.html
!gzip -f ../../man/man3/nd.1
!(
!cd ../..
!)
!exit
!$ENDIF
   call execute_command_line('fpm run -- -old data/_in1 -new data/_in2 -percent 0.0001')
   call execute_command_line('fpm run -- -old data/_in1 -new data/_in2 -percent 0.00000001')
end program check
