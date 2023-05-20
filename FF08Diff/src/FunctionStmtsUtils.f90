! $Id: FunctionStmtsUtils.f90 2791 2019-03-20 07:28:15Z ian $
! ff08 source code copyright 2014 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the FunctionStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the function name from a function statement.
!!
!! This duplicates some of the logic in the FunctionStmts module, but 
!! it is here as well to improve source file granularity.

MODULE FunctionStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: GetFunctionName
  
  ! Get the function's name.
  INTERFACE GetFunctionName
    MODULE PROCEDURE GetFunctionName_
  END INTERFACE GetFunctionName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Get the name of the function from the /function-stmt/.
  !!
  !! @param[in]     tlist             The sequence of tokens that makes up the 
  !! /function-stmt/.
  !!
  !! @param[out]    name              The function-name in the statement.  Not 
  !! allocated if the name cannot be sensibly determined.
  
  SUBROUTINE GetFunctionName_(tlist, name)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Token index.
    INTEGER :: nest           ! Parentheses nesting.
    
    !***************************************************************************
    
    nest = 0
    DO i = 1, SIZE(tlist)
      ASSOCIATE (tok => tlist(i))
        SELECT CASE (QueryType(tok))
        CASE (ittDelimiter)
          IF (tok == scck_'(') THEN
            nest = nest + 1
          ELSE IF (tok == scck_')') THEN
            nest = nest - 1
          END IF
          
        CASE (ittName, ittKeyword)
          IF ( (nest == 0)  &
              .AND. (tok == scck_'FUNCTION') ) THEN
            IF (i < SIZE(tlist)) THEN
              IF (tlist(i+1) == ittName) THEN
                name = QueryValue(tlist(i+1))      ! Kind conversion.
              END IF
            END IF
            RETURN
          END IF
          
        END SELECT
      END ASSOCIATE
    END DO
    
  END SUBROUTINE GetFunctionName_
  
END MODULE FunctionStmtsUtils
