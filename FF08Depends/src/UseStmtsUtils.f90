! $Id: UseStmtsUtils.f90 2800 2019-03-22 18:13:05Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the UseStmts module.


!*******************************************************************************
!!
!> Procedures for extracting the module name from a use statement.
!!
!! This has been split out from the UseStmts module to improve source file 
!! granularity.

MODULE UseStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: GetModuleName
  
  !> Get the name of the module referenced in a /use-stmt/.
  !!
  !! The logical argument for the intrinsic nature of the module distinguishes 
  !! this from the similarly named interface in ModuleStmtsUtils.
  INTERFACE GetModuleName
    MODULE PROCEDURE GetModuleName_
  END INTERFACE GetModuleName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the name of the module (module-name) referenced by a use-stmt.
  !!
  !! @param[in]     tlist             The sequence of tokens that makes up the 
  !! /use-stmt/.
  !!
  !! @param[out]    name              The module-name in the use-stmt.  Not 
  !! allocated if module-name could not be sensibly determined.
  !!
  !! @param[out]    is_intrinsic      .TRUE. if the module is maked as 
  !! being intrinsic, .FALSE. otherwise.
  !!
  !! @todo @a is_intrinsic probably needs to consider NON_INTRINSIC - 
  !! perhaps make this an integer where zero is not specified, 1 is 
  !! non-intrinsic and two is intrinsic.
  
  SUBROUTINE GetModuleName_(tlist, name, is_intrinsic)
    
    USE CompilerKinds, ONLY: scck
    USE Tokens
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:,KIND=scck), INTENT(OUT), ALLOCATABLE :: name
    LOGICAL, INTENT(OUT) :: is_intrinsic
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i             ! Index into tlist.
    
    !***************************************************************************
    
    is_intrinsic = .FALSE.
    
    ! Syntax differs depending on presence or otherwise of '::'.
    i = FindBare(tlist, ittDeclare, scck_'::')
    IF (i /= 0) THEN
      ! module-name immediately follows the '::'.
      IF (SIZE(tlist) > i) THEN
        IF (tlist(i+1) == ittName) THEN
          name = QueryValue(tlist(i+1))
          ! See if the intrinsic attribute is present.
          DO i = 3, i - 1, 2
            IF (IsToken(tlist(i), ittName, scck_'INTRINSIC')) THEN
              is_intrinsic = .TRUE.
              EXIT
            END IF
          END DO
          RETURN
        END IF
      END IF
    ELSE IF (SIZE(tlist) > 1) THEN
      ! module-name is second token.
      IF (tlist(2) == ittName) THEN
        name = QueryValue(tlist(2))
        RETURN
      END IF
    END IF
    ! If we get to here just return with name unallocated.
    
  END SUBROUTINE GetModuleName_
  
END MODULE UseStmtsUtils
