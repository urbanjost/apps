! $Id: ProgramStmtsUtils.f90 2791 2019-03-20 07:28:15Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ProgramStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the program name from a program statement.
!!
!! This has been split out from the ProgramStmts module to improve source 
!! file granularity.

MODULE ProgramStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: GetProgramName
  
  !> Get the program name.
  INTERFACE GetProgramName
    MODULE PROCEDURE GetProgramName_
  END INTERFACE GetProgramName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the name of the program from the /program-stmt/.
  !!
  !! @param[in]     tlist             The sequence of tokens that makes up the 
  !! /program-stmt/.
  !!
  !! @param[out]    name              The program-name in the statement.  Not 
  !! allocated if the name is not present or cannot be sensibly determined.
  !!
  !! Module statement parsing has a similar procedure, but for program units 
  !! the name of the program is optional.
  
  SUBROUTINE GetProgramName_(tlist, name)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    IF (SIZE(tlist) > 1) THEN
      IF (tlist(2) == ittName) THEN
        name = QueryValue(tlist(2))      ! Kind conversion.
        RETURN
      END IF
    END IF
    ! Return with unallocated name.
    
  END SUBROUTINE GetProgramName_
  
END MODULE ProgramStmtsUtils
