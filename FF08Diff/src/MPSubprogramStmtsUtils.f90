! $Id: MPSubprogramStmtsUtils.f90 2791 2019-03-20 07:28:15Z ian $
! ff08 source code copyright 2014 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the MPSubprogramStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the procedure name from a module procedure 
!! subprogram statement.
!!
!! This duplicates some of the logic in the MPSubprogramStmts module, but 
!! it is here as well to improve source file granularity.

MODULE MPSubprogramStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: GetProcedureName
  
  ! Get the function's name.
  INTERFACE GetProcedureName
    MODULE PROCEDURE GetProcedureName_
  END INTERFACE GetProcedureName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the name of the procedure from the /mpsubprogram-stmt/.
  !!
  !! @param[in]     tlist             The sequence of tokens that makes up the 
  !! /mpsubprogram-stmt/.
  !!
  !! @param[out]    name              The /procedure-name/ in the statement.  
  !! Not allocated if the name cannot be sensibly determined.
  
  SUBROUTINE GetProcedureName_(tlist, name)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    IF (SIZE(tlist) > 2) THEN
      IF (tlist(3) == ittName) THEN
        name = QueryValue(tlist(3))      ! Kind conversion.
      END IF
    END IF
    
  END SUBROUTINE GetProcedureName_
  
END MODULE MPSubprogramStmtsUtils
