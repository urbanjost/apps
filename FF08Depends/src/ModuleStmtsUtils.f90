! $Id: ModuleStmtsUtils.f90 2791 2019-03-20 07:28:15Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ModuleStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the name of a module.
!!
!! This has been split out from the ModuleStmts module to improve source file 
!! granularity.

MODULE ModuleStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: GetModuleName
  
  !> Get the name of the module.
  !!
  !! Lack of the logical argument for the intrinsic nature of the module 
  !! distinguishes this from the similarly named interface in UseStmtsUtils.
  INTERFACE GetModuleName
    MODULE PROCEDURE GetModuleName_
  END INTERFACE GetModuleName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Gets the name of the module.
  !!
  !! @param[in]     tlist             The sequence of tokens that make up the 
  !! /module-stmt/.
  !!
  !! @param[out]    name              The name of the module.  Not allocated 
  !! if the module name could not be sensibly determined.
  
  SUBROUTINE GetModuleName_(tlist, name)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    IF (SIZE(tlist) > 1) THEN
      IF (tlist(2) == ittName) THEN
        name = QueryValue(tlist(2))          ! Kind conversion
        RETURN
      END IF
    END IF
    ! Return with unallocated name.
    
  END SUBROUTINE GetModuleName_
  
END MODULE ModuleStmtsUtils
