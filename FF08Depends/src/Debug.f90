! $Id: Debug.f90 1908 2015-10-13 03:13:06Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Debug module


!*******************************************************************************
!!
!> Global debugging flags.

MODULE Debug
  
  USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Flag to indicate that additional debugging information should be 
  !! displayed.
  LOGICAL, PUBLIC, SAVE :: DbgFlag = .FALSE.
  
  !> Flag to indicate that additional debugging checks should be made.
  !!
  !! These debugging checks have the nature of an assertion - if the 
  !! check indicates that there is a problem then the program will 
  !! terminate with a stop statement that indicates the nature of 
  !! the problem.
  LOGICAL, PUBLIC, PARAMETER :: DbgChecks = .TRUE.
  
  !> Output unit for debug information.  Currently set to the console.
  INTEGER, PUBLIC, SAVE :: DbgUnit = OUTPUT_UNIT
  
  !> Suffix (extension) for log files.
  CHARACTER(*), PARAMETER, PUBLIC :: DbgExtension = '.dbglog'
  
END MODULE Debug
