! $Id: ProcedureStmtsUtils.f90 2791 2019-03-20 07:28:15Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ProcedureStmtsUtils module.


!*******************************************************************************
!!
!> Procedure to extract the procedure names from a /procedure-stmt/.
!!
!! A procedure statement is the form of statement that appears inside an 
!! interface block.

MODULE ProcedureStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: GetProcedureNames
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Gets the names of the procedures mentioned in a /procedure-stmt/.
  !!
  !! @param[out]    names             The names in the statement.  Not 
  !! allocated if no names could be determined.  There is no guarantee that 
  !! all names in the statement will be returned, particularly if the 
  !! statement has syntax errors.  If allocated will have size greater 
  !! or equal to one.
  
  SUBROUTINE GetProcedureNames(tlist, names)
    
    USE CompilerKinds
    USE MatchUtils
    USE Strings
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: names(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it             ! Token index.
    
    !***************************************************************************
    
    ! MODULE is optional.
    IF (Match(tlist, 1, ittName, scck_'MODULE')) THEN
      it = 3
    ELSE
      it = 2
    END IF
    
    ! :: is optional.
    IF (Match(tlist, it, ittDeclare, scck_'::')) it = it + 1
    
    ! Walk the list of name [, name]...
    DO WHILE (it <= SIZE(tlist))
      ! Bail if we have any inconsistency.
      IF (tlist(it) /= ittName) RETURN
      CALL Append(names, QueryValue(tlist(it)))
      it = it + 2
    END DO
    
  END SUBROUTINE GetProcedureNames
  
END MODULE ProcedureStmtsUtils
