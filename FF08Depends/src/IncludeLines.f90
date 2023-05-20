! $Id: IncludeLines.f90 2799 2019-03-22 17:36:34Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @files
!! Defines the IncludeLines module


!*******************************************************************************
!!
!> Defines the TestInclude procedure that tests whether a statement's 
!! worth of tokens looks like an INCLUDE line.

MODULE IncludeLines
  
  IMPLICIT NONE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: CheckInclude
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Test a proto statement to see if it looks like an INCLUDE line.
  !!
  !! @param[in]     tlist             The sequence of tokens in the 
  !! proto-statement.
  !!
  !! @returns .TRUE. if the statements look like an include line 
  !! (there may still be syntactic errors - CheckInclude can be used 
  !! for that), .FALSE. otherwise.
  !!
  !! As a minimum (i.e. client code can depend on these things being true) 
  !! we require two tokens, the first must be INCLUDE and the second must 
  !! be a character literal.
  
  FUNCTION TestInclude(tlist) RESULT(is_include)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    ! Function result
    LOGICAL :: is_include
    
    !***************************************************************************
    
    is_include = .FALSE.
    
    IF (SIZE(tlist) /= 2) RETURN
    IF (tlist(1) /= ittName) RETURN
    
    IF (QueryValue(tlist(1)) /= scck_'INCLUDE') RETURN
    
    IF (tlist(2) /= ittCharacterLiteral) RETURN
    
    is_include = .TRUE.
    
  END FUNCTION TestInclude
  
  
  !*****************************************************************************
  !!
  !> Check a proto-statement to see if it looks like an INCLUDE line and, 
  !! if it is considered an include line, check its syntax.
  !!
  !! @param[in]     tlist             The sequence of tokens in the 
  !! proto-statement.
  !!
  !! @param[in]     statement_label   The "statement" label, not allocated 
  !! if there was no label.
  !!
  !! @param[out]    is_include        .TRUE. if the statements look like an 
  !! include line (there may still be syntactic errors), .FALSE. otherwise.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! We consider the sequence of tokens to be consistent with an include line 
  !! if there are two tokens, the first token is INCLUDE and the second token 
  !! is a character literal.  Otherwise we assume that it is some type of 
  !! statement (perhaps it is a malformed include line, but it is almost 
  !! certain that this will generate an error message in later processing).
  !!
  !! An error is generated if the character literal has a kind parameter or 
  !! if the relevant tokens are not the only tokens on the line.
  !!
  !! We also generate an error if label is not zero.  That aspect, and 
  !! perhaps other aspects of INCLUDE lines, might be better addressed by 
  !! the source forms.
  
  SUBROUTINE CheckInclude(tlist, statement_label, is_include, err_list)
    
    USE CharUtils
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE LabelsUtils
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    TYPE(Label), INTENT(IN), ALLOCATABLE :: statement_label
    LOGICAL, INTENT(OUT) :: is_include
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    is_include = TestInclude(tlist)
    IF (.NOT. is_include) RETURN
    
    !---------------------------------------------------------------------------
    ! The syntax checks.
    
    ! An include line is not a statement and hence must not have a label.
    IF (ALLOCATED(statement_label)) THEN
      CALL Add( err_list,  &
          CODE=errBadIncludeLine,  &
          LOCATION=QueryLocation(tlist(2)),  &
          MSG='A statement label is not permitted on an INCLUDE line.',  &
          COMPONENT='3.3.2.6/3.3.3.5' )
    END IF
    
    ! An include line must be the only thing on the line.
    IF ( .NOT. IsFirst(tlist(1))  &
        .OR. IsLast(tlist(1))  &
        .OR. IsFirst(tlist(2))  &
        .OR. .NOT. IsLast(tlist(2)) ) THEN
      CALL Add( err_list,  &
          CODE=errBadIncludeLine,  &
          LOCATION=QueryLocation(tlist(2)),  &
          MSG='An INCLUDE line was either continued or contained other &
            &statements.',  &
          COMPONENT='3.4p5' )
    END IF
    
    IF (HasKind(tlist(2))) THEN
      ! A named kind type parameter is not legal, but the numeric constant 
      ! sort is.  Scan the kind text to see if it has any non-digit characters.
      IF (.NOT. is_digit_string(QueryKind(tlist(2)))) THEN
        CALL Add( err_list,  &
            CODE=errBadIncludeCharLiteral,  &
            LOCATION=QueryLocation(tlist(2)),  &
            MSG=QueryKind(tlist(2)) // ' is not a valid kind_param for the &
              &char-literal-constant in an INCLUDE line.',  &
            COMPONENT='3.4p3' )
      END IF
    END IF
    
  END SUBROUTINE CheckInclude
  
  
  !*****************************************************************************
  !!
  !> Tests whether a string is a valid digit string.
  !!
  !! @param[in]     str               The string to test.
  !!
  !! @returns True if @a str only has digits in it (as tested by IsDigitChar), 
  !! false otherwise.
  
  PURE FUNCTION is_digit_string(str) RESULT(b)
    
    USE CharacterTypes
    USE CompilerKinds
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*,KIND=scck), INTENT(IN) :: str
    
    ! Function result.
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Index into str.
    
    !***************************************************************************
    
    DO i = 1, LEN(str)
      IF (.NOT. IsDigitChar(str(i:i))) THEN
        b = .FALSE.
        RETURN
      END IF
    END DO
    b = .TRUE.
    
  END FUNCTION is_digit_string
  
END MODULE IncludeLines
