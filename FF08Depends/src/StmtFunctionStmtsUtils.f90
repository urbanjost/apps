! $Id: StmtFunctionStmtsUtils.f90 2800 2019-03-22 18:13:05Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the StmtFunctionStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for testing whether a sequence of tokens is a statement function.
!!
!! This module is used by the Classifier (to try and split assignment 
!! statements from statement function statements), so it must not depend on 
!! the classifier either directly or indirectly.
!!
!! This has been split out from StmtFunctionStmts module to improve source 
!! file granularity.

MODULE StmtFunctionStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: IsMaybeStmtFunction
  
  !> Test whether a sequence of tokens could be a statement function.
  INTERFACE IsMaybeStmtFunction
    MODULE PROCEDURE IsMaybeStmtFunction_
  END INTERFACE IsMaybeStmtFunction
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Test a sequence of tokens to see if it might possibly be a statement 
  !! function.
  !!
  !! @param[in]     tlist             The sequence of tokens that makes up the 
  !! statement function.
  !!
  !! @returns .TRUE. if it is possible that the list of tokens is a statement 
  !! function, .FALSE. otherwise.  Note that a result of .TRUE. does not mean 
  !! that the sequence of tokens is definitely a statement function - it 
  !! is possible that the primary name (the function name) is actually an 
  !! array and that all the dummy arguments to the statement function are 
  !! actually parameters.
  !!
  !! This test is required (at least...) to determine whether the executable 
  !! part of a program unit has (definitely) commenced.
  
  FUNCTION IsMaybeStmtFunction_(tlist) RESULT(is_match)
    
    USE CompilerKinds
    USE MatchUtils
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    ! Function result
    LOGICAL :: is_match
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: closing        ! Index of closing delimiter ).
    INTEGER :: it             ! Token index.
    
    !***************************************************************************
    
    ! Assume failure
    is_match = .FALSE.
    
    ! Require a minimum of 5 tokens - name ( ) = value
    IF (SIZE(tlist) < 5) RETURN
    
    ! First token must be a name
    IF (tlist(1) /= ittName) RETURN
    
    ! Second token must be a (.
    IF (.NOT. IsToken(tlist(2), ittDelimiter, scck_'(')) RETURN
    
    ! Find the closing ).
    closing = FindClosingParens(tlist, 2)
    IF (closing == 0) RETURN
    
    IF (SIZE(tlist) < closing + 2) RETURN
    
    ! Token after the closing ) must be an =.
    IF (.NOT. IsToken(tlist(closing+1), ittAssign, scck_'=')) RETURN
    
    ! Work through the contents of the parens.
    DO it = 3, closing - 2, 2
      IF (tlist(it) /= ittName) RETURN
      IF (.NOT. IsToken(tlist(it+1), ittComma, scck_',')) RETURN
    END DO
    
    IF (closing > 3) THEN
      IF (tlist(closing - 1) /= ittName) RETURN
    END IF
    
    ! Passed all tests.
    is_match = .TRUE.
    
  END FUNCTION IsMaybeStmtFunction_
  
END MODULE StmtFunctionStmtsUtils
