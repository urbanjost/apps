! $Id: MatchUtils.f90 2800 2019-03-22 18:13:05Z ian $
! ff08 source code copyright 2013 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the MatchUtils module.


!*******************************************************************************
!!
!> Helper routines for common token matching activities.
!!
!! These procedures were originally part of ParseUtils, but were split out 
!! for source granularity reasons.

MODULE MatchUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: FindBare
  PUBLIC :: FindBareReverse
  PUBLIC :: FindClosingParens
  PUBLIC :: GetNextParensRange
  PUBLIC :: HasBare
  
  PUBLIC :: Match
  PUBLIC :: MatchName
  PUBLIC :: MatchEOS
  
  !-----------------------------------------------------------------------------
  
  !> Find a "bare" (not in parentheses) token of some description in a 
  !! sequence of tokens.
  INTERFACE FindBare
    MODULE PROCEDURE FindBare_
    MODULE PROCEDURE FindBare_start
    MODULE PROCEDURE FindBare_type
  END INTERFACE FindBare
  
  !> Find a "bare" (not in parentheses) token of some description, starting 
  !! the search at the end of the token sequence and working towards the 
  !! front.
  INTERFACE FindBareReverse
    MODULE PROCEDURE FindBareReverse_
    MODULE PROCEDURE FindBareReverse_type
  END INTERFACE FindBareReverse
  
  !> Find the mathing closing delimiter (parenthesis, typically) for 
  !! an opening delimiter in a token sequence.
  INTERFACE FindClosingParens
    MODULE PROCEDURE FindClosingParens_
    MODULE PROCEDURE FindClosingParens_start
  END INTERFACE FindClosingParens
  
  !> Test whether a sequence of tokens contains a particular bare token.
  INTERFACE HasBare
    MODULE PROCEDURE HasBare_
  END INTERFACE HasBare
  
  !> Test whether a token sequence matches a certain pattern of tokens.
  INTERFACE Match
    MODULE PROCEDURE Match_type
    MODULE PROCEDURE Match_tt
  END INTERFACE Match
  
CONTAINS
  
  
  !*****************************************************************************
  !!
  !> Determines whether a particular token appears in a sequence of tokens 
  !! outside of paired '(' or ')' or a paired '[' or ']' or a paired 
  !! '(/' or '/)'.
  !!
  !! @param[in]     tlist             The sequence of tokens to search.  The 
  !! search starts from the very first token in this list.
  !!
  !! @param[in]     itt               The type of token to search for.
  !!
  !! @param[in]     text              The text of the token to search for.
  !!
  !! @returns The location of the first bare token in @a tlist if such 
  !! a bare token exists, or zero otherwise.  Both @a type and @a text must 
  !! match.  The text is compared case sensitively.
  !!
  !! Looking for a '(', '[',']', or ')' is not likely to work.
  
  FUNCTION FindBare_(tlist, itt, text) RESULT(i)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: itt
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    
    ! Function result
    INTEGER :: i
    
    !***************************************************************************
    
    i = FindBare(tlist, itt, text, 1)
    
  END FUNCTION FindBare_
  
  
  !*****************************************************************************
  !!
  !> Determines whether a particular token appears in a sequence of tokens 
  !! outside of paired '(' or ')' or a paired '[' or ']' or a paired 
  !! '(/' or '/)'.
  !!
  !! @param[in]     tlist             The sequence of tokens to search.
  !!
  !! @param[in]     itt               The type of token to search for.
  !!
  !! @param[in]     text              The text of the token to search for.
  !!
  !! @param[in]     start             The starting point of the search.  This 
  !! must be greater than zero.
  !!
  !! @returns The location of the first bare token in @a tlist if such 
  !! a bare token exists, or zero otherwise.  Both @a type and @a text must 
  !! match.  The text is compared case sensitively.
  !!
  !! Looking for a '(', '[',']', ')' ,'(/' or '/)' is not likely to work.
  
  FUNCTION FindBare_start(tlist, itt, text, start) RESULT(i)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: itt
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    INTEGER :: i
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: round_nest     ! Nest depth for the '(' and ')' pair.
    INTEGER :: square_nest    ! Nest depth for the '[' and ']' pair.
    INTEGER :: array_nest     ! Nest depth for the '(/' and '/)' pair.
    
    !***************************************************************************
    
    round_nest = 0
    square_nest = 0
    array_nest = 0
    
    DO i = start, SIZE(tlist)
      IF (tlist(i) == ittDelimiter) THEN
        SELECT CASE (QueryValue(tlist(i)))
        CASE (scck_'(')
          round_nest = round_nest + 1
        CASE (scck_')')
          IF (round_nest > 0) round_nest = round_nest - 1
        CASE (scck_'[')
          square_nest = square_nest + 1
        CASE (scck_']')
          IF (square_nest > 0) square_nest = square_nest - 1
        CASE (scck_'(/')
          array_nest = array_nest + 1
        CASE (scck_'/)')
          IF (array_nest > 0) array_nest = array_nest - 1
        END SELECT
      END IF
      
      IF ( (round_nest == 0)  &
          .AND. (square_nest == 0)  &
          .AND. (array_nest == 0) ) THEN
        IF (tlist(i) == itt) THEN
          IF (tlist(i) == text) THEN
            RETURN
          END IF
        END IF
      END IF
    END DO
    
    i = 0
    
  END FUNCTION FindBare_start
  
  
  !*****************************************************************************
  !!
  !> Determines whether a particular type of token appears in a sequence of 
  !! tokens outside of paired '(' or ')' or a paired '[' or ']' or a paired 
  !! '(/' or '/)'.
  !!
  !! @param[in]     tlist             The sequence of tokens to search.
  !!
  !! @param[in]     itt               The type of token to search for.
  !!
  !! @param[in]     start             The starting point of the search.  This 
  !! must be greater than zero.  It may be greater than SIZE(@a tlist), in 
  !! which case the sort after token will not be found.
  !!
  !! @returns The location of the bare token in @a tlist if such a bare 
  !! token exists from position @a start on, or zero otherwise.
  !!
  !! Looking for an ittDelimiter is not likely to work.
  
  FUNCTION FindBare_type(tlist, itt, start) RESULT(i)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: itt
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    INTEGER :: i
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: round_nest     ! Nest depth for the '(' and ')' pair.
    INTEGER :: square_nest    ! Nest depth for the '[' and ']' pair.
    INTEGER :: array_nest     ! Nest depth for the '(/' and '/)' pair.
    
    !***************************************************************************
    
    round_nest = 0
    square_nest = 0
    array_nest = 0
    
    DO i = start, SIZE(tlist)
      IF (tlist(i) == ittDelimiter) THEN
        SELECT CASE (QueryValue(tlist(i)))
        CASE (scck_'(')
          round_nest = round_nest + 1
        CASE (scck_')')
          IF (round_nest > 0) round_nest = round_nest - 1
        CASE (scck_'[')
          square_nest = square_nest + 1
        CASE (scck_']')
          IF (square_nest > 0) square_nest = square_nest - 1
        CASE (scck_'(/')
          array_nest = array_nest + 1
        CASE (scck_'/)')
          IF (array_nest > 0) array_nest = array_nest - 1
        END SELECT
      END IF
      
      IF ( (round_nest == 0)  &
          .AND. (square_nest == 0)  &
          .AND. (array_nest == 0) ) THEN
        IF (tlist(i) == itt) RETURN
      END IF
    END DO
    
    i = 0
    
  END FUNCTION FindBare_type
  
  
  !*****************************************************************************
  !!
  !> Determines whether a particular token appears in a sequence of tokens 
  !! outside of paired '(' or ')' or a paired '[' or ']' or a paired 
  !! '(/' or '/)', searching from the end backwards.
  !!
  !! @param[in]     tlist             The sequence of tokens to search.
  !!
  !! @param[in]     itt               The type of token to search for.
  !!
  !! @param[in]     text              The text of the token to search for.
  !!
  !! @param[in]     start             The starting point of the search.  This 
  !! must be greater than zero and less than or equal to SIZE(tlist).
  !!
  !! @returns The location of the bare token in @a tlist if such a bare 
  !! token exists, or zero otherwise.  Both @a type and @a text must match.  
  !! The text is compared case sensitively.
  !!
  !! Looking for an ittDelimiter is not likely to work.
  
  FUNCTION FindBareReverse_(tlist, itt, text, start) RESULT(i)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: itt
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    INTEGER :: i
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: round_nest     ! Nest depth for the '(' and ')' pair.
    INTEGER :: square_nest    ! Nest depth for the '[' and ']' pair.
    INTEGER :: array_nest     ! Nest depth for the '(/' and '/)' pair.
    
    !***************************************************************************
    
    round_nest = 0
    square_nest = 0
    array_nest = 0
    
    DO i = start, 1, -1
      IF (tlist(i) == ittDelimiter) THEN
        SELECT CASE (QueryValue(tlist(i)))
        CASE (scck_')')
          round_nest = round_nest + 1
        CASE (scck_'(')
          IF (round_nest > 0) round_nest = round_nest - 1
        CASE (scck_']')
          square_nest = square_nest + 1
        CASE (scck_'[')
          IF (square_nest > 0) square_nest = square_nest - 1
        CASE (scck_'/)')
          array_nest = array_nest + 1
        CASE (scck_'(/')
          IF (array_nest > 0) array_nest = array_nest - 1
        END SELECT
      END IF
      
      IF ( (round_nest == 0)  &
          .AND. (square_nest == 0)  &
          .AND. (array_nest == 0) ) THEN
        IF (tlist(i) == itt) THEN
          IF (tlist(i) == text) THEN
            RETURN
          END IF
        END IF
      END IF
    END DO
    
    i = 0
    
  END FUNCTION FindBareReverse_
  
  
  !*****************************************************************************
  !!
  !> Determines whether a particular token type appears in a sequence of 
  !! tokens outside of paired '(' or ')' or a paired '[' or ']' or a paired 
  !! '(/' or '/)', searching from the end backwards.
  !!
  !! @param[in]     tlist             The sequence of tokens to search.
  !!
  !! @param[in]     itt               The type of token to search for.
  !!
  !! @param[in]     start             The starting point of the search.  This 
  !! must be greater than zero and less than or equal to SIZE(tlist).
  !!
  !! @returns The location of the bare token in @a tlist if such a bare 
  !! token exists, or zero otherwise.  Both @a type and @a text must match.  
  !! The text is compared case sensitively.
  !!
  !! Looking for an ittDelimiter is not likely to work as this includes the 
  !! parenthesis related tokens.
  
  FUNCTION FindBareReverse_type(tlist, itt, start) RESULT(i)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: itt
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    INTEGER :: i
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: round_nest     ! Nest depth for the '(' and ')' pair.
    INTEGER :: square_nest    ! Nest depth for the '[' and ']' pair.
    INTEGER :: array_nest     ! Nest depth for the '(/' and '/)' pair.
    
    !***************************************************************************
    
    round_nest = 0
    square_nest = 0
    
    DO i = start, 1, -1
      IF (tlist(i) == ittDelimiter) THEN
        SELECT CASE (QueryValue(tlist(i)))
        CASE (scck_')')
          round_nest = round_nest + 1
        CASE (scck_'(')
          IF (round_nest > 0) round_nest = round_nest - 1
        CASE (scck_']')
          square_nest = square_nest + 1
        CASE (scck_'[')
          IF (square_nest > 0) square_nest = square_nest - 1
        CASE (scck_'/)')
          array_nest = array_nest + 1
        CASE (scck_'(/')
          IF (array_nest > 0) array_nest = array_nest - 1
        END SELECT
      END IF
      
      IF ( (round_nest == 0)  &
          .AND. (square_nest == 0)  &
          .AND. (array_nest == 0) ) THEN
        IF (tlist(i) == itt) RETURN
      END IF
    END DO
    
    i = 0
    
  END FUNCTION FindBareReverse_type
  
  
  !*****************************************************************************
  !!
  !> Locate the position of the closing parentheses that matches the type 
  !! of the parentheses at the start of the token list.
  !!
  !! @param[in]     tlist             The list of tokens.  The first token 
  !! in this must be the opening parentheses.
  !!
  !! @returns The position in @a tlist of the matching closing 
  !! parentheses, zero if no matching closing parentheses was found.  Note 
  !! that this can never be 1 (as the first token must be the opening 
  !! parenthesis.
  !!
  !! Usage: say you have a tlist 'a', '(', 'b' ,')', 'c' and you call 
  !! this function:
  !! @code
  !! start = 2
  !! finish = FindClosingParens(tlist(start:))
  !! @endcode
  !! which will return 3 (the third token in the passed list), then the 
  !! index of the closing parenthesis is start + finish - 1, and the 
  !! index of the token after the closing parenthesis is start + finish.
  
  FUNCTION FindClosingParens_(tlist) RESULT(i)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    ! Function result
    INTEGER :: i
    
    !***************************************************************************
    
    i = FindClosingParens(tlist, 1)
    
  END FUNCTION FindClosingParens_
  
  
  !*****************************************************************************
  !!
  !> Find the position of a matching closing parenthesis/brace/bracket.
  !!
  !! @param[in]     tlist             The list of tokens.  The first token 
  !! in this must be the opening parentheses.
  !!
  !! @param[in]     start             The position in tlist that contains 
  !! the opening parentheses.  This must be greater than zero.  This is 
  !! also the point after which the search starts.
  !!
  !! @returns The position in @a tlist of the matching closing 
  !! parentheses, zero if no matching closing parentheses was found or if 
  !! the token at @a tlist (@a start) is not a n opening parenthesis.
  !!
  !! Valid parentheses pairs are '(/' and '/)', '[' and ']', '(' and ')'.
  
  FUNCTION FindClosingParens_start(tlist, start) RESULT(i)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    INTEGER :: i
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    CHARACTER(*,KIND=scck), PARAMETER :: open(3) = ['( ', '[ ', '(/']
    CHARACTER(*,KIND=scck), PARAMETER :: close(3) = [') ', '] ', '/)']
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: nest       ! Depth of parenthesis nesting.
    
    ! Parenthesis type.
    ! - 1: Look for ( and )
    ! = 2: Look for [ and ]
    ! - 3: Look for (/ and /)
    INTEGER :: ptype
    
    !***************************************************************************
    
    nest = 0
    
    DO ptype = 1, SIZE(open)
      IF (tlist(start) == open(ptype)) EXIT
    END DO
    IF (ptype > SIZE(open)) THEN
      i = 0
      RETURN
    END IF
    
    ! Start the search.
    DO i = start + 1, SIZE(tlist)
      IF (tlist(i) == ittDelimiter) THEN
        IF (tlist(i) == open(ptype)) THEN 
          nest = nest + 1
        ELSE IF (tlist(i) == close(ptype)) THEN
          IF (nest == 0) THEN
            RETURN
          ELSE
            nest = nest - 1
          END IF
        END IF
      END IF
    END DO
    
    i = 0
  
  END FUNCTION FindClosingParens_start
  
  
  !*****************************************************************************
  !!
  !> Simple yes/no version of FindBare.
  !!
  !! @param[in]     tlist             The sequence of tokens to search.  The 
  !! search starts from the very first token in this list.
  !!
  !! @param[in]     itt               The type of token to search for.
  !!
  !! @param[in]     start             The starting point of the search.  This 
  !! must be greater than zero.  It may be greater than SIZE(@a tlist), in 
  !! which case the sort after token will not be found.
  
  FUNCTION HasBare_(tlist, itt, text) RESULT(b)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: itt
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = FindBare(tlist, itt, text) /= 0
    
  END FUNCTION HasBare_
  
  
  !*****************************************************************************
  !!
  !> Get the indices that delimit the next sub-sequence of tokens that are 
  !! enclosed by parentheses.
  !!
  !! @param[in]      tlist             The sequence of tokens.  The search 
  !! for the parenthesised subsequence begins at the start of this array.
  !!
  !! @param[out]    start             The position of the token that is 
  !! immediately after the opening parenthesis.
  !!
  !! @param[out]    finish            The position of the token that is 
  !! immediately before the closing parenthesis.
  !!
  !! If @a start and @finish are non-zero, but @a start is greater than 
  !! @a finish, then there is nothing inside the parentheses.
  
  SUBROUTINE GetNextParensRange(tlist, start, finish)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: start
    INTEGER, INTENT(OUT) :: finish
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i
    
    !***************************************************************************
    
    DO i = 1, SIZE(tlist)
      IF (IsToken(tlist(i), ittDelimiter, scck_'(')) THEN
        finish = FindClosingParens(tlist(i:))
        IF (finish == 0) EXIT
        start = i + 1
        finish =finish + i - 2
        RETURN
      END IF
    END DO
    
    start = 0
    finish = 0
    
  END SUBROUTINE GetNextParensRange
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  
  
  !*****************************************************************************
  !!
  !> Tests whether a token at a particular position in a token sequence 
  !! matches a certain type.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               The position in tlist to test.  This 
  !! may be larger than the size of tlist.
  !!
  !! @param[in]     expected_type     The type of token to match.
  !!
  !! @returns True if @a pos is less than or equal to the size of @a tlist, 
  !! and the token at @a pos matches the given type, false otherwise.
  
  FUNCTION Match_type(tlist, pos, expected_type) RESULT(match)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    INTEGER, INTENT(IN) :: expected_type
    
    ! Function result
    LOGICAL :: match
    
    !***************************************************************************
    
    IF (pos > SIZE(tlist)) THEN
      match = .FALSE.
    ELSE
      match = tlist(pos) == expected_type
    END IF
    
  END FUNCTION Match_type
  
  
  !*****************************************************************************
  !!
  !> Tests whether a token at a particular position in a token sequence 
  !! matches in both type and text.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               The position in tlist to test.  This 
  !! may be larger than the size of tlist.
  !!
  !! @param[in]     expected_type     The type of token to match.
  !!
  !! @param[in]     expected_text     The text of the token to match.
  !!
  !! @returns True if @a pos is less than or equal to the size of @a tlist, 
  !! and the token at @a pos matches both type and text, false otherwise.
  
  FUNCTION Match_tt(tlist, pos, expected_type, expected_text) RESULT(match)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    INTEGER, INTENT(IN) :: expected_type
    CHARACTER(*,KIND=scck), INTENT(IN) :: expected_text
    
    ! Function result
    LOGICAL :: match
    
    !***************************************************************************
    
    IF (pos > SIZE(tlist)) THEN
      match = .FALSE.
    ELSE IF (tlist(pos) == expected_type) THEN
      match = tlist(pos) == expected_text
    ELSE
      match = .FALSE.
    END IF
    
  END FUNCTION Match_tt
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Routines that match the ParseUtils::MatchIntf abstract interface, that 
  ! can be used as an argument to Expect with a procedure list (and hence 
  ! these are not generic!).
  
  
  !*****************************************************************************
  !!
  !> Tests whether a given token is a name token.
  !!
  !! @param[in]     tlist             The list of tokens.
  !!
  !! @param[in]     start             The position of the token to match.
  !!
  !! @returns .TRUE. if @a start is within @a tlist and the element at
  !! @a start in @a tlist is a name token, otherwise .FALSE..
  
  PURE FUNCTION MatchName(tlist, start) RESULT(b)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (SIZE(tlist) >= start) THEN
      IF (tlist(start) == ittName) THEN
        b = .TRUE.
        RETURN
      END IF
    END IF
    
    b = .FALSE.
    
  END FUNCTION MatchName
  
  
  !*****************************************************************************
  !!
  !> Tests whether a given token position is beyond the end of the token 
  !! sequence.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     start             The position to test.
  !!
  !! @returns .TRUE. if @a start is off the end of @a tlist.
  
  FUNCTION MatchEOS(tlist, start) RESULT(b)
    
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = start > SIZE(tlist)
    
  END FUNCTION MatchEOS
  
END MODULE MatchUtils
