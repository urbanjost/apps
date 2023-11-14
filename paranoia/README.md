## The PARANOID routines

These routines test floating-point arithmetic in a Fortran programming
environment.

This is a version of the netlib
[PARANOIA](http://www.netlib.org/paranoia/) Fortran programs converted to
procedures. They can be embedded into programs such that one can verify
the behavior of the options selected to execute a program, or the simple
test program can be used to experiment with various compiler selections.

  * double-precision Fortran procedure
  * single-precision Fortran procedure
  * C program

```fortran
program test_paranoia
implicit none
   write(*,"('*paranoia* single precision test')")
   call sparanoia()
   write(*,"('*paranoia* double precision test')")
   call dparanoia()
end program test_paranoia
```

