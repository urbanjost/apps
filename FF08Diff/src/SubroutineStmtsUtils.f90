! $Id: SubroutineStmtsUtils.f90 2800 2019-03-22 18:13:05Z ian $
! ff08 source code copyright 2014 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SubroutineStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the subroutine name from a subroutine statement.
!!
!! This duplicates some of the logic in the SubroutineStmts module, but 
!! it is here as well to improve source file granularity.

MODULE SubroutineStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: GetSubroutineName
  
  ! Get the function's name.
  INTERFACE GetSubroutineName
    MODULE PROCEDURE GetSubroutineName_
  END INTERFACE GetSubroutineName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the name of the subroutine from the /subroutine-stmt/.
  !!
  !! @param[in]     tlist             The sequence of tokens that makes up the 
  !! /subroutine-stmt/.
  !!
  !! @param[out]    name              The subroutine-name in the statement.  
  !! Not allocated if the name cannot be sensibly determined.
  
  SUBROUTINE GetSubroutineName_(tlist, name)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Token index.
    
    !***************************************************************************
    
    DO i = 1, SIZE(tlist)
      IF (IsToken(tlist(i), ittName, scck_'SUBROUTINE')) THEN
        IF (i < SIZE(tlist)) THEN
          IF (tlist(i+1) == ittName) THEN
            name = QueryValue(tlist(i+1))     ! Kind conversion.
          END IF
        END IF
        RETURN
      END IF
    END DO
    
  END SUBROUTINE GetSubroutineName_
  
END MODULE SubroutineStmtsUtils
