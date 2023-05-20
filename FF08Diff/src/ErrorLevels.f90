! $Id: ErrorLevels.f90 2957 2019-11-24 07:26:13Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ErrorLevels module.


!*******************************************************************************
!!
!> Defines five error levels - in increasing order of severity - None, Info, 
!! Warning, Error and Fatal.  See the documentation for the relevant 
!! errLevel* parameter for more information.
!!
!! This module has no dependencies.
!!
!! There may be a call for another level in between Error and Fatal (or 
!! perhaps in between Warning and Error), which indicates that while parsing 
!! or some similar operation failed, it is still worth trying to parse (or 
!! whatever) other nearby stuff.

MODULE ErrorLevels
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: OPERATOR(>)
  PUBLIC :: OPERATOR(>=)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: OPERATOR(<=)
  PUBLIC :: OPERATOR(<)
  PUBLIC :: OPERATOR(.Describe.)
  
  !-----------------------------------------------------------------------------
  ! Error level type
  
  !> Represents an error level.
  TYPE, PUBLIC :: ErrorLevel
    PRIVATE
    !> The integer for the particular level.
    INTEGER :: level
  END TYPE ErrorLevel
  
  !-----------------------------------------------------------------------------
  ! The levels themselves
  
  !> The operation completed successfully.
  TYPE(ErrorLevel), PARAMETER, PUBLIC :: errLevelNone = ErrorLevel(0)
  
  !> The operation completed successfully.  Message for information only.
  TYPE(ErrorLevel), PARAMETER, PUBLIC :: errLevelInfo = ErrorLevel(1)
  
  !> The operation completed succesfully, but some problems were detected.
  TYPE(ErrorLevel), PARAMETER, PUBLIC :: errLevelWarning = ErrorLevel(2)
  
  !> Errors occurred that prevented the operation from completing.  Retrying
  !! the operation with different parameters may result in success.
  TYPE(ErrorLevel), PARAMETER, PUBLIC :: errLevelError = ErrorLevel(3)
  
  !> Errors occurred that should terminate the program.
  TYPE(ErrorLevel), PARAMETER, PUBLIC :: errLevelFatal = ErrorLevel(4)
  
  !-----------------------------------------------------------------------------
  ! Defined operations
  
  !> Greater than comparison of two error levels.
  INTERFACE OPERATOR(>)
    MODULE PROCEDURE gt_level
  END INTERFACE OPERATOR(>)
  
  !> Greater than or equal to comparison of two error levels.
  INTERFACE OPERATOR(>=)
    MODULE PROCEDURE ge_level
  END INTERFACE OPERATOR(>=)
  
  !> Equality comparison of two error levels.
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE eq_level
  END INTERFACE OPERATOR(==)
  
  !> Inequality comparison of two error levels.
  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE ne_level
  END INTERFACE OPERATOR(/=)
  
  !> Less than or equal to comparison of two error levels.
  INTERFACE OPERATOR(<=)
    MODULE PROCEDURE le_level
  END INTERFACE OPERATOR(<=)
  
  !> Less than comparison of two error levels.
  INTERFACE OPERATOR(<)
    MODULE PROCEDURE lt_level
  END INTERFACE OPERATOR(<)
  
  !> Provide a text description of an error level.
  INTERFACE OPERATOR(.Describe.)
    MODULE PROCEDURE describe_level
  END INTERFACE OPERATOR(.Describe.)
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Carries out .GT. (>) comparison of two ErrorLevels.
  !!
  !! @param[in]     lhs               The left hand side of the comparison.
  !!
  !! @param[in]     rhs               The left hand side of the comparison.
  !!
  !! @returns .TRUE. if @a lhs is more severe than @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION gt_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(ErrorLevel), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level > rhs%level
    
  END FUNCTION gt_level
  
  
  !*****************************************************************************
  !!
  !> Carries out .GE. (>=) comparison of two ErrorLevels.
  !!
  !! @param[in]     lhs               The left hand side of the comparison.
  !!
  !! @param[in]     rhs               The left hand side of the comparison.
  !!
  !! @returns .TRUE. if @a lhs is the same or more severe than @a rhs, .FALSE. 
  !! otherwise.
  
  ELEMENTAL FUNCTION ge_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(ErrorLevel), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level >= rhs%level
    
  END FUNCTION ge_level
  
  
  !*****************************************************************************
  !!
  !> Carries out .EQ. (==) comparison of two ErrorLevels.
  !!
  !! @param[in]     lhs               The left hand side of the comparison.
  !!
  !! @param[in]     rhs               The left hand side of the comparison.
  !!
  !! @returns .TRUE. if @a lhs is the same severity as @a rhs, .FALSE. 
  !! otherwise.
  
  ELEMENTAL FUNCTION eq_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(ErrorLevel), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level == rhs%level
    
  END FUNCTION eq_level
  
  
  !*****************************************************************************
  !!
  !> Carries out .NE. (/=) comparison of two ErrorLevels.
  !!
  !! @param[in]     lhs               The left hand side of the comparison.
  !!
  !! @param[in]     rhs               The left hand side of the comparison.
  !!
  !! @returns .TRUE. if @a lhs is not the same severity as @a rhs, .FALSE. 
  !! otherwise.
  
  ELEMENTAL FUNCTION ne_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(ErrorLevel), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level /= rhs%level
    
  END FUNCTION ne_level
  
  
  !*****************************************************************************
  !!
  !> Carries out .LE. (<=) comparison of two ErrorLevels.
  !!
  !! @param[in]     lhs               The left hand side of the comparison.
  !!
  !! @param[in]     rhs               The left hand side of the comparison.
  !!
  !! @returns .TRUE. if @a lhs is the same or not as severe as @a rhs, .FALSE. 
  !! otherwise.
  
  ELEMENTAL FUNCTION le_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(ErrorLevel), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level <= rhs%level
    
  END FUNCTION le_level
  
  
  !*****************************************************************************
  !!
  !> Carries out .LT. (<) comparison of two ErrorLevels.
  !!
  !! @param[in]     lhs               The left hand side of the comparison.
  !!
  !! @param[in]     rhs               The left hand side of the comparison.
  !!
  !! @returns .TRUE. if @a lhs is not as severe as @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION lt_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(ErrorLevel), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level < rhs%level
    
  END FUNCTION lt_level
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! String stuff
  
  
  !*****************************************************************************
  !!
  !> Implementation of operator(.Describe.) - provides a text description of 
  !! the level.
  !!
  !! @param[in]     level             The level to describe.
  !!
  !! @returns A text representation of @a level.
  !!
  !! This setup makes gfortran barf unless it has been patched appropriately.
  
  PURE FUNCTION describe_level(level) RESULT(s)
    
    !> Strings to describe each level
    !!
    !! @todo Move to be module level once a bug with IVF around mangling of 
    !! the exported name associated with the parameter is fixed.
    CHARACTER(*), PARAMETER :: level_descriptions(0:4) = [  &
        '       ',  &
        'Info   ',  &
        'Warning',  &
        'Error  ',  &
        'Fatal  ' ]
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(ErrorLevel), INTENT(IN) :: level
    
    ! Function result
    CHARACTER(LEN=LEN_TRIM(level_descriptions(level%level))) :: s
    
    !***************************************************************************
    
    s = level_descriptions(level%level)
    
  END FUNCTION describe_level
  
END MODULE ErrorLevels
