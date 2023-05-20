! $Id: ParseUtils.f90 2813 2019-03-24 02:16:15Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ParseUtils module.


!*******************************************************************************
!!
!> Helper routines for common token parsing activities.
!!
!! The Expect subroutines create an error if a token that matches certain 
!! criteria is not found in the token list at a particular position, while 
!! the Match functions simply return a LOGICAL.  This is used as a template 
!! elsewhere for more specific Expect and Match functions defined elsewhere 
!! that might be looking for sequences or combinations of tokens.
!!
!! We expect that the use of the Expect procedures will provide a more 
!! consistent form of error reporting, possibly at a cost of less context 
!! in the error message.
!!
!! Test converage of this module is mostly implicit in tests of the client 
!! code of this module.  In some cases this is pretty woeful.  There is a 
!! skeleton ParseUtilsTests in the ParsingTests project, but it only deals 
!! with some isolated cases that were previously identified as suspect.
!!
!! This module was written very early on in the piece.  Some conventions with 
!! respect to argument names and position are not consistent with what is 
!! used elsewhere.  Unfortunately, as this module was written very early on 
!! in the piece, it is used bloody everywhere.  One day we will fix it up.
!!
!! You could probably replace all the parsing routines with some sort of 
!! table driven thing, but that would be boring.
!!
!! The argument for a sequence of tokens is typically called tlist, because 
!! in years gone by it used to be a "list of tokens", but for some reason 
!! that offended us.

MODULE ParseUtils
  
  USE MatchUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: Articalise
  
  PUBLIC :: Expect
  PUBLIC :: ExpectEnd
  PUBLIC :: ExpectEOS
  PUBLIC :: ExpectName
  PUBLIC :: ExpectKeyword
  PUBLIC :: ExpectSplittableKeyword
  PUBLIC :: ExpectClosing
  
  PUBLIC :: ParseNameList
  PUBLIC :: ListItemCount
  PUBLIC :: GetNextListItem
  
  PUBLIC :: SafeLocation
  
  !-----------------------------------------------------------------------------
  
  !> Articalise a word for error reporting.
  INTERFACE Articalise
    MODULE PROCEDURE Articalise_
  END INTERFACE Articalise
  
  !> Expect a pattern of tokens in a token sequence, issuing an error if 
  !! our expectations are not met.
  !!
  !! @todo Having this many overloads was a mistake.  Move some of these 
  !! out to their own generic - ExpectSplittable, ExpectName, for example.
  INTERFACE Expect
    MODULE PROCEDURE Expect_type
    MODULE PROCEDURE Expect_tt
    MODULE PROCEDURE ExpectSplittableKeyword
    MODULE PROCEDURE Expect_array_tt
    MODULE PROCEDURE Expect_array_msg
    MODULE PROCEDURE Expect_array
    MODULE PROCEDURE Expect_proc
    MODULE PROCEDURE ExpectName
  END INTERFACE Expect
  
  !> Expect a keyword.
  INTERFACE ExpectKeyword
    MODULE PROCEDURE ExpectKeyword_list
    MODULE PROCEDURE ExpectKeyword_specific
  END INTERFACE ExpectKeyword
  
  !> Get the range of tokens in a token sequence that are the next item 
  !! in a comma separated list.
  INTERFACE GetNextListItem
    MODULE PROCEDURE GetNextListItem_sf
    MODULE PROCEDURE GetNextListItem_
  END INTERFACE
  
  !> Interfaces for components of the MatchProcList type.
  ABSTRACT INTERFACE
    !> Interface for the "Match" function used by one of the Expect variants 
    !! - tests a sequence of tokens for a particular syntax element.
    !!
    !! @param[in]     tlist             The sequence of tokens.
    !!
    !! @param[in]     start             Index of the first token in the list.
    !!
    !! @returns .TRUE. if the particular syntax element being tested for was 
    !! matched, otherwise .FALSE..
    !!
    !! @todo Consider making the result of this function be the position of 
    !! the token that is the end of the syntax element being tested for 
    !! if that syntax element was found and zero otherwise.  This makes the 
    !! function more generally useful (the end position is handy when chopping 
    !! up tlist) for minimal extra effort.
    FUNCTION MatchIntf(tlist, start) RESULT(b)
      USE Tokens
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      TYPE(Token), INTENT(IN) :: tlist(:)
      INTEGER, INTENT(IN) :: start
      ! Function result
      LOGICAL :: b
    END FUNCTION MatchIntf
  END INTERFACE
  
  !> An element for an array of procedures that have the MatchIntf interface.
  TYPE, PUBLIC :: MatchProcList
    PROCEDURE(MatchIntf), NOPASS, POINTER :: item
  END TYPE MatchProcList
  
  INTERFACE ParseNameList
    MODULE PROCEDURE ParseNameList_
  END INTERFACE ParseNameList
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! The Expect* procedures
  
  
  !*****************************************************************************
  !!
  !> Checks that the type of the token is consistent with a keyword 
  !! and that the text of the token matches given text, and returns an error 
  !! if it does not.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               Starting position in @a tlist to 
  !! test.
  !!
  !! @param[in]     keyword_text      The text of the keyword to expect.
  !!
  !! @param[out]    err               Error object.
  !!
  !! Use this specific procedure when a specific keyword is expected.
  
  SUBROUTINE ExpectKeyword_specific(tlist, pos, keyword_text, err)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    CHARACTER(*,KIND=scck), INTENT(IN) :: keyword_text
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    ! Check tlist length
    CALL expect_test_size(tlist, pos, '"' // keyword_text // '"', err)
    IF (Failure(err)) RETURN
    
    ! pos is <= SIZE(tlist) from here on.
    
    IF (.NOT. IsToken(tlist(pos), ittName, keyword_text)) THEN
      IF (pos > 1) THEN
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='"' // keyword_text // '" expected after "'  &
              // QueryRaw(tlist(pos - 1)) // '" but "'  &
              // QueryRaw(tlist(pos)) // '" was encountered instead.' )
      ELSE
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='"' // keyword_text // '" expected but "'  &
              // QueryRaw(tlist(pos)) // '" was encountered instead.' )
      END IF
      RETURN
    END IF
    
    err = NoError()
    
  END SUBROUTINE ExpectKeyword_specific
  
  
  !*****************************************************************************
  !!
  !> Checks that the type of the token is consistent with a keyword 
  !! from a list of keywords and returns an error if it does not.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               Starting position in @a tlist to 
  !! test.
  !!
  !! @param[in]     keyword_texts     The texts of the keywords to expect.
  !!
  !! @param[out]    opt               The one based index into 
  !! @a keyword_texts that matched, or zero if there was no match.
  !!
  !! @param[in]     syntax_rule       Syntax rule that describes the 
  !! list of keywords, including the delimiting slashes if applicable.
  !!
  !! @param[out]    err               Error object.
  !!
  !! The intended use of this procedure is in specifier lists such as those 
  !! in open statements, etc, where there are a number of options for 
  !! KEYWORD=xxx specifiers.
  
  SUBROUTINE ExpectKeyword_list( tlist, pos, keyword_texts, syntax_rule,  &
      opt, err )
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    CHARACTER(*,KIND=scck), INTENT(IN) :: keyword_texts(:)
    CHARACTER(*), INTENT(IN) :: syntax_rule
    INTEGER, INTENT(OUT) :: opt
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    CALL Expect_array_msg(  &
        tlist, pos,  &
        SPREAD(ittName, 1, SIZE(keyword_texts)),  &
        keyword_texts,  &
        syntax_rule,  &
        opt, err )
    
  END SUBROUTINE ExpectKeyword_list
  
  
  !*****************************************************************************
  !!  
  !> Checks to see whether a particular token in a list matches a certain 
  !! token type, and returns an error if it does not.
  !!
  !! @param[in]     tlist             The sequence of tokens.  May be empty.
  !!
  !! @param[in]     pos               Position to check.  Must be greater 
  !! than zero.
  !!
  !! @param[in]     expected_type     Token type (itt*) expected to be 
  !! at tlist(pos).  This may be ittEndStatement.
  !!
  !! @param[out]    err               Error object.  A syntax error with 
  !! appropriate text is returned if the expected token was not found, 
  !! otherwise set to NoError().
  
  SUBROUTINE Expect_type(tlist, pos, expected_type, err)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    INTEGER, INTENT(IN) :: expected_type
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    IF (expected_type == ittEndStatement) THEN
      IF (pos > SIZE(tlist)) RETURN
      IF (pos > 1) THEN
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='Expected end-of-statement after "'  &
              // QueryRaw(tlist(pos-1)) // '" but '  &
              // articalise(QueryTypeText(tlist(pos)))  &
              // ' was encountered instead.' )
      ELSE
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='Expected end-of-statement but '  &
              // articalise(QueryTypeText(tlist(pos)))  &
              // ' was encountered instead.' )
      END IF
      RETURN
    END IF
    
    ! Check tlist length
    CALL expect_test_size( tlist, pos,  &
        TRIM(TokenTypeNames(expected_type)), err )
    IF (Failure(err)) RETURN
    
    ! pos is <= SIZE(tlist) from here on.
    
    IF (tlist(pos) /= expected_type) THEN
      IF (pos > 1) THEN
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='Expected ' // TRIM(TokenTypeNames(expected_type))  &
              // ' after "' // QueryRaw(tlist(pos - 1)) // '" but '  &
              // articalise(QueryTypeText(tlist(pos)))  &
              // ' was encountered instead.' )
      ELSE
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='Expected ' // TRIM(TokenTypeNames(expected_type))  &
              // ' but '  &
              // articalise(QueryTypeText(tlist(pos)))  &
              // ' was encountered instead.' )
      END IF
      RETURN
    END IF
    
    err = NoError()
    
  END SUBROUTINE Expect_type
  
  
  !*****************************************************************************
  !!
  !> Checks the type and text.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               Starting position in @a tlist to test.
  !!
  !! @param[in]     expected_type     The type of token to expect.  This 
  !! may not be ittEndStatement.
  !!
  !! @param[in]     expected_text     The text of the token to expect.
  !!
  !! @param[out]    err               Error object.
  
  SUBROUTINE Expect_tt(tlist, pos, expected_type, expected_text, err)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    INTEGER, INTENT(IN) :: expected_type
    CHARACTER(*,KIND=scck), INTENT(IN) :: expected_text
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    ! Check tlist length
    CALL expect_test_size(tlist, pos, '"' // expected_text // '"', err)
    IF (Failure(err)) RETURN
    
    ! pos is <= SIZE(tlist) from here on.
    
    IF (.NOT. IsToken(tlist(pos), expected_type, expected_text)) THEN
      IF (pos > 1) THEN
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='"' // expected_text // '" expected after "'  &
              // QueryRaw(tlist(pos - 1)) // '" but "'  &
              // QueryRaw(tlist(pos)) // '" was encountered instead.' )
      ELSE
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG='"' // expected_text // '" expected but "'  &
              // QueryRaw(tlist(pos)) // '" was encountered instead.' )
      END IF
      RETURN
    END IF
    
    err = NoError()
    
  END SUBROUTINE Expect_tt
  
  
  !*****************************************************************************
  !!
  !> Issues an error if the next token or next two tokens matches either 
  !! two name tokens - text1 followed by text2, or a single name token that 
  !! consists of text1 with text2 concatenated.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               Starting position in @a tlist to test.
  !!
  !! @param[in]     text1             Text for the first token, or the first 
  !! part of the token.
  !!
  !! @param[in]     text2             Text for the second token, or the second 
  !! part of the token.
  !!
  !! @param[out]    after             Index of the token after the tokens that 
  !! match, if tokens match.
  !!
  !! @param[out]    err               Error object.
  
  SUBROUTINE ExpectSplittableKeyword(tlist, pos, text1, text2, after, err)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    CHARACTER(*,KIND=scck), INTENT(IN) :: text1
    CHARACTER(*,KIND=scck), INTENT(IN) :: text2
    INTEGER, INTENT(OUT) :: after
    TYPE(Error), INTENT(OUT) :: err
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! The combined form of the two keywords.
    CHARACTER(:), ALLOCATABLE :: combined
    
    ! String describing what was expected for error reporting.
    CHARACTER(:), ALLOCATABLE :: expected
    
    !***************************************************************************
    
    combined = text1 // text2
    expected = '"' // combined // '" or "' // text1 // ' ' // text2 // '"'
    ! Check tlist length
    CALL expect_test_size(tlist, pos, expected, err)
    IF (Failure(err)) RETURN
    
    ! pos is <= SIZE(tlist) from here on.
    after = pos + 1
    IF (IsToken(tlist(pos), ittName, combined)) RETURN
    
    ! Not dealing with the combined form.
    
    IF (.NOT. IsToken(tlist(pos), ittName, text1)) THEN
      IF (pos > 1) THEN
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG=expected // ' expected after "'  &
              // QueryRaw(tlist(pos - 1)) // '" but "'  &
              // QueryRaw(tlist(pos)) // '" was encountered instead.' )
      ELSE
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG=expected // ' expected but "'  &
              // QueryRaw(tlist(pos)) // '" was encountered instead.' )
      END IF
      RETURN
    END IF
    
    after = pos + 2
    CALL Expect(tlist, pos + 1, ittName, text2, err)
    
  END SUBROUTINE ExpectSplittableKeyword
  
  
  !*****************************************************************************
  !!
  !> Issues an error if the next token doesn't match an option from 
  !! an array of tokens types and texts.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               Starting position in @a tlist to test.
  !!
  !! @param[in]     types             The token types that are expected.  
  !! One of these can be ittEndStatement, in which case the test is that 
  !! pos is out of range of @a tlist.
  !!
  !! @param[in]     texts             Array of token texts to expect.  An 
  !! element that is all blanks means that the text of the token is not 
  !! significant (the corresponding type still will be).  Must be the 
  !! same size as @a types.
  !!
  !! @param[out]    opt               The one based index into @a types and 
  !! @ texts that matched, or zero if there was no match.
  !!
  !! @param[out]    err               Error object.
  !!
  !! A suitable description of the types and token texts being expected is 
  !! is built from @a types and @a texts if an error does need to be 
  !! reported.  For control over this description use the Expect_array_msg 
  !! specific procedure.
  
  SUBROUTINE Expect_array_tt(tlist, pos, types, texts, opt, err)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    INTEGER, INTENT(IN) :: types(:)
    CHARACTER(*,KIND=scck), INTENT(IN) :: texts(:)
    INTEGER, INTENT(OUT) :: opt
    TYPE(Error), INTENT(OUT) :: err
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Description of the expected token types and texts for error reporting.
    CHARACTER(:), ALLOCATABLE :: msg
    
    INTEGER :: i              ! type/text index.
    
    !***************************************************************************
    
    ! Build the message
    msg = ''
    DO i = 1, SIZE(texts) - 2
      msg = msg // expect_text(types(i), texts(i)) // ', '
    END DO
    IF (SIZE(texts) > 1) THEN
      i = SIZE(texts) - 1
      msg = msg // expect_text(types(i), texts(i)) // ' or '
    END IF
    IF (SIZE(texts) /= 0) THEN
      i = SIZE(texts)
      msg = msg // expect_text(types(i), texts(i))
    END IF
    
    ! Use the message taking specific procedure.
    CALL Expect(tlist, pos, types, texts, msg, opt, err)
    
  END SUBROUTINE Expect_array_tt
  
  
  !*****************************************************************************
  !!
  !> Expect a token that has a particular text chosen from an array.  The 
  !! client code needs to provide the clause that appears in the error message 
  !! if one of the expected tokens is not found.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               Index in @a tlist to start testing.
  !!
  !! @param[in]     types             The token types that are expected.  
  !! One of these can be ittEndStatement, in which case the test is that 
  !! pos is out of range of @a tlist.
  !!
  !! @param[in]     texts             Array of token texts to expect.  An 
  !! element that is all blanks means that the text of the token is not 
  !! significant (the corresponding type still will be).  Must be the 
  !! same size as @a types.
  !!
  !! @param[in]     msg               Expected clause part of the message 
  !! to use for error reporting.
  !!
  !! @param[out]    opt               The one based index into @a types and 
  !! @ texts that matched, or zero if there was no match.
  !!
  !! @param[out]    err               Error object.
  
  SUBROUTINE Expect_array_msg(tlist, pos, types, texts, msg, opt, err)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    INTEGER, INTENT(IN) :: types(:)
    CHARACTER(*,KIND=scck), INTENT(IN) :: texts(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER, INTENT(OUT) :: opt
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    ! Check end of statement.
    eos_check: DO opt = 1, SIZE(types)
      IF (types(opt) == ittEndStatement) THEN
        IF (pos > SIZE(tlist)) RETURN
        EXIT eos_check
      END IF
    END DO eos_check
    
    ! Check tlist length
    CALL expect_test_size(tlist, pos, msg, err)
    IF (Failure(err)) RETURN
    
    ! pos is <= SIZE(tlist) from here on.
    
    DO opt = 1, SIZE(texts)
      IF (tlist(pos) == types(opt)) THEN
        IF (texts(opt) == '') RETURN
        IF (tlist(pos) == texts(opt)) RETURN
      END IF
    END DO
    
    ! No match.
    IF (pos > 1) THEN
      err = ConstructError(  &
          CODE=errSyntax,  &
          LOCATION=QueryLocation(tlist(pos)),  &
          MSG=msg // ' expected after "'  &
            // QueryRaw(tlist(pos - 1)) // '" but "'  &
            // QueryRaw(tlist(pos)) // '" was encountered instead.' )
    ELSE
      err = ConstructError(  &
          CODE=errSyntax,  &
          LOCATION=QueryLocation(tlist(pos)),  &
          MSG=msg // ' expected but "'  &
            // QueryRaw(tlist(pos)) // '" was encountered instead.' )
    END IF
    
  END SUBROUTINE Expect_array_msg
  
  
  !*****************************************************************************
  !!
  !> Expect a series of tokens that matches one out of an array of test 
  !! functions.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     start             The starting position for the match.
  !!
  !! @param[in]     procs             Functions for each test.
  !!
  !! @param[in]     msg               Text fragment (what was expected) to 
  !! use in the error message.
  !!
  !! @param[out]    opt               Index of the matching function if 
  !! there was no error (no match is an error).
  !!
  !! @param[out]    err               Error object.
  
  SUBROUTINE Expect_proc(tlist, start, procs, msg, opt, err)
    
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: start
    TYPE(MatchProcList), INTENT(IN) :: procs(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER, INTENT(OUT) :: opt
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    DO opt = 1, SIZE(procs)
      IF (procs(opt)%item(tlist, start)) RETURN
    END DO
    
    ! No match.
    IF (start > 1) THEN
      err = ConstructError(  &
          CODE=errSyntax,  &
          LOCATION=QueryLocation(tlist(start)),  &
          MSG=msg // ' expected after "' // QueryRaw(tlist(start-1)) // '".' )
    ELSE
      err = ConstructError(  &
          CODE=errSyntax,  &
          LOCATION=QueryLocation(tlist(start)),  &
          MSG=msg // ' expected.' )
    END IF
    
  END SUBROUTINE Expect_proc
  
  
  !*****************************************************************************
  !!
  !> Expects the primary text of a token to match a element in an array of
  !! characters.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     start             The starting position for the match.
  !!
  !! @param[in]     array             The array of token texts to match.
  !!
  !! @param[in]     msg               Text fragment (what was expected) to 
  !! use in the error message.
  !!
  !! @param[out]    opt               Index of the matching function if 
  !! there was no error (no match is an error).
  !!
  !! @param[out]    err               Error object.
  !!
  !! Don't use this for matching literal constant tokens.
  
  SUBROUTINE Expect_array(tlist, start, array, msg, opt, err)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: start
    CHARACTER(*,KIND=scck), INTENT(IN) :: array(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER, INTENT(OUT) :: opt
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    ! Check tlist length
    CALL expect_test_size(tlist, start, msg, err)
    IF (Failure(err)) RETURN
    
    !---------------------------------------------------------------------------
    
    IF ( (tlist(start) == ittName)  &
        .OR. (tlist(start) == ittOperator)  &
        .OR. (tlist(start) == ittDelimiter)  &
        .OR. (tlist(start) == ittComma)  &
        .OR. (tlist(start) == ittAssign)  &
        .OR. (tlist(start) == ittAssociate)  &
        .OR. (tlist(start) == ittColon)  &
        .OR. (tlist(start) == ittDeclare)  &
        .OR. (tlist(start) == ittPercent) ) THEN
      DO opt = 1, SIZE(array)
        IF (tlist(start) == array(opt)) RETURN
      END DO
    END IF
    
    !---------------------------------------------------------------------------
    ! No match.
    
    IF (start > 1) THEN
      err = ConstructError(  &
          CODE=errSyntax,  &
          LOCATION=QueryLocation(tlist(start)),  &
          MSG=msg // ' expected after "' // QueryRaw(tlist(start-1)) // '".' )
    ELSE
      err = ConstructError(  &
          CODE=errSyntax,  &
          LOCATION=QueryLocation(tlist(start)),  &
          MSG=msg // ' expected.' )
    END IF
    
  END SUBROUTINE Expect_array
  
  
  !*****************************************************************************
  !!
  !> Expect a name and allow the client code to specify what the class of name 
  !! is expected.
  !!
  !! This allows error messages to say things like "Expected /function-name/." 
  !! which is a bit more specific and user-friendly than just "Expected 
  !! /name/."
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               The position within @a tlist where the 
  !! name is expected.
  !!
  !! @param[in]     msg               The text to use to describe the name. 
  !! Something like '/use-name/' as an example.
  !!
  !! @param[out]    err               Error object - set if there is no token 
  !! at @a pos (i.e. @ pos is out of bounds) or if the token at @a pos is not 
  !! a ittName.
  
  SUBROUTINE ExpectName(tlist, pos, msg, err)
    
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    CHARACTER(*), INTENT(IN) :: msg
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    ! Check tlist length
    CALL expect_test_size(tlist, pos, msg, err)
    IF (Failure(err)) RETURN
    
    !---------------------------------------------------------------------------
    
    IF (tlist(pos) /= ittName) THEN
      IF (pos > 1) THEN
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG=msg // ' expected after "'  &
              // QueryRaw(tlist(pos - 1)) // '" but '  &
              // articalise(QueryTypeText(tlist(pos)))  &
              // ' was encountered instead.' )
      ELSE
        err = ConstructError(  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(pos)),  &
            MSG=msg // ' expected but '  &
              // articalise(QueryTypeText(tlist(pos)))  &
              // ' was encountered instead.' )
      END IF
    END IF
    
  END SUBROUTINE ExpectName
  
  
  !*****************************************************************************
  !!
  !> Worker routine for Expect - creates an error if tlist is not long enough 
  !! to include pos.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               The position that must be valid within 
  !! @a tlist.
  !!
  !! @param[in]     text              Text that was otherwise expected at 
  !! @a tlist ( @a pos ) if @a tlist was big enough.  Used in the error 
  !! message.
  !!
  !! @param[out]    err               Error object, set to an appopriate 
  !! error if tlist is not big enough, NoError() otherwise.
  
  SUBROUTINE expect_test_size(tlist, pos, text, err)
    
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    CHARACTER(*), INTENT(IN) :: text
    TYPE(Error), INTENT(OUT) :: err
    
    !---------------------------------------------------------------------------
    
    IF (pos <= SIZE(tlist)) RETURN ! Rely on default initialisation of err
    
    ! Our position is off the end of the list.
    IF (SIZE(tlist) /= 0) THEN
      ! Use the last token in the sequence for location information.
      err = ConstructError(  &
            CODE=errSyntax,  &
          LOCATION=QueryLocation(tlist(SIZE(tlist))),  &
          MSG='Expected ' // text // ' after "'  &
            // QueryRaw(tlist(SIZE(tlist))) // '".' )
    ELSE
      ! No token location information available.
      err = ConstructError(  &
          CODE=errSyntax,  &
          MSG='Expected ' // text // '.' )
    END IF
    
  END SUBROUTINE expect_test_size
  
  
  !*****************************************************************************
  !!
  !! Work routine for Expect - return the descriptive text that corresponds
  !! to the given token type and text.
  !!
  !! @param[in]     type              The token type.
  !!
  !! @param[in]     text              The token text.
  !!
  !! @returns A string that can be used to describe the token to the user.  
  !! In most cases this is just the token text, but if the token has no 
  !! text then the type of token is substituted instead.  If token text is 
  !! used it is placed in double quotes..
  
  FUNCTION expect_text(type, text) RESULT(str)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: type
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: str
    
    !***************************************************************************
    
    SELECT CASE (type)
    CASE (ittEndStatement)
      str = 'end-of-statement'
    CASE DEFAULT
      IF (text == '') THEN
        str = TRIM(TokenTypeNames(type))
      ELSE
        str = '"' // TRIM(text) // '"'
      END IF
    END SELECT
    
  END FUNCTION expect_text
  
  
  !*****************************************************************************
  !!
  !> Utility routine to test for the end of a token sequence.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     it                Position expected to be beyond the end.
  !!
  !! @param[in]     comp              Component for error reporting.
  !!
  !! @param[in,out] err_list          Error list to add the error to.  Note 
  !! that this is INTENT(INOUT).  This means that the calling procedure cannot 
  !! find out easily whether we got what we expected, but because this is 
  !! typically the last thing called in a parse that shouldn't matter.
  
  SUBROUTINE ExpectEnd(tlist, it, comp, err_list)
    
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: it
    CHARACTER(*), INTENT(IN) :: comp
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    IF (it <= SIZE(tlist)) THEN
      IF (it > 1) THEN
        CALL Add( err_list,  &
            CODE=errUnexpectedExtraTokens,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(tlist(it)),  &
            MSG='Unexpected extra tokens following "'  &
              // QueryRaw(tlist(it-1)) // '".' )
      ELSE
        CALL Add( err_list,  &
            CODE=errUnexpectedExtraTokens,  &
            COMPONENT=comp,  &
            LOCATION=QueryLocation(tlist(it)),  &
            MSG='Unexpected extra tokens.' )
      END IF
    END IF
    
  END SUBROUTINE ExpectEnd
  
  
  !*****************************************************************************
  !!
  !> Utility routine to test for the end of a token sequence that represents 
  !! an entire statement.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     it                Position expected to be beyond the end 
  !! of the sequence (beyond the end of the statement).
  !!
  !! @param[in]     comp              Component for error reporting.
  !!
  !! @param[in,out] err_list          Error list to add the error to.  Note 
  !! that this is INTENT(INOUT).  This means that the calling procedure cannot 
  !! find out easily whether we got what we expected, but because this is 
  !! typically the last thing called in a parse that shouldn't matter.
  
  SUBROUTINE ExpectEOS(tlist, it, comp, err_list)
    
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: it
    CHARACTER(*), INTENT(IN) :: comp
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    IF (it <= SIZE(tlist)) THEN
      IF (tlist(it) /= ittEndStatement) THEN
        IF (it > 1) THEN
          CALL Add( err_list,  &
              CODE=errUnexpectedExtraTokens,  &
              COMPONENT=comp,  &
              LOCATION=QueryLocation(tlist(it)),  &
              MSG='Expected end-of-statement after "'  &
                // QueryRaw(tlist(it-1)) // '" but '  &
                // articalise(QueryTypeText(tlist(it)))  &
                // ' was encountered instead.' )
        ELSE
          CALL Add( err_list,  &
              CODE=errUnexpectedExtraTokens,  &
              COMPONENT=comp,  &
              LOCATION=QueryLocation(tlist(it)),  &
              MSG='Expected end-of-statement but '  &
                // articalise(QueryTypeText(tlist(it)))  &
                // ' was encountered instead.' )
        END IF
      END IF
    END IF
    
  END SUBROUTINE ExpectEOS
  
  
  !*****************************************************************************
  !!
  !> Utility routine to determine the location of a closing parentheses 
  !! or bracket, and report and error if one cannot be found.
  !!
  !! @param[in]     tlist             List of tokens.
  !!
  !! @param[in]     it                Position in @a tlist of the opening 
  !! parenthesis.
  !!
  !! @param[out]    closing           Position of the closing parenthesis.
  !!
  !! @param[out]    err               Error object.
  
  SUBROUTINE ExpectClosing(tlist, it, closing, err)
    
    USE Errors
    USE ErrorCodes
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: it
    INTEGER, INTENT(OUT) :: closing
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    closing = FindClosingParens(tlist, it)
    IF (closing == 0) THEN
      IF (IsToken(tlist(it), ittDelimiter, scck_'[')) THEN
        err = ConstructError(  &
            CODE=errMissingCloseParens,  &
            LOCATION=QueryLocation(tlist(it)),  &
            MSG='Missing matching closing bracket.' )
      ELSE
        err = ConstructError(  &
            CODE=errMissingCloseParens,  &
            LOCATION=QueryLocation(tlist(it)),  &
            MSG='Missing matching closing parentheses.' )
      END IF
    END IF
  
  END SUBROUTINE ExpectClosing
  
  
  !-----------------------------------------------------------------------------  
  !-----------------------------------------------------------------------------  
  
  
  !*****************************************************************************
  !!
  !> Prepends an indefinite article to a noun.
  !!
  !! @param[in]     noun              The nound to follow the article.
  !!
  !! @returns A string with a leading article ('a' or 'an') for the following 
  !! noun.
  !!
  !! If the first character of @a noun is not a letter, then the second 
  !! character is checked.
  !!
  !! Leading vowels and 'h' result in the article being 'an', otherwise 'a' is 
  !! used.
  
  FUNCTION Articalise_(noun) RESULT(str)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: noun
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Character index.
    
    !***************************************************************************
    
    ! Empty string comes back if an empty string given.
    IF (LEN(noun) == 0) THEN
      str = ''
      RETURN
    END IF
    
    i = 1
    DO
      ! If we find a vowel then prepend article and return.  We'll consider 
      ! h to be a vowel too - which might be a bit dubious.
      IF (SCAN(noun(i:i), 'aAeEiIoOuUhH') /= 0) THEN
        str = 'an ' // noun
        RETURN
      ELSE IF (SCAN( noun(i:i),  &
          'bBcCdDfFgGjJkKlLmMnNpPqQrRsStTuUvVwWxXyYzZ' ) /= 0) THEN
        str = 'a ' // noun
        RETURN
      END IF
      
      ! Only check one character in
      IF (i /= 1) EXIT
      
      ! Don't check characters that aren't there.
      IF (LEN(noun) == 1) EXIT
      
      ! Check the next character
      i = i + 1
    END DO
    
    ! If we make to here then we just use 'a'
    str = 'a ' // noun
    
  END FUNCTION Articalise_
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! List utilities, where list is as defined by R101.
  
  
  !*****************************************************************************
  !!
  !> Parse a sequence of tokens that is a comma separated list of names.
  !!
  !!This is obsolete, as parsing into String is being replaced with 
  !! Identifier.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     list_start        Start index of the list in @a tlist.
  !!
  !! @param[in]     list_finish       End index of the list in @a tlist.
  !!
  !! @param[in]     syntax_rule       The syntax term for each name, used for 
  !! error reporting.  This is used as the @a msg parameter in a child call 
  !! to Expect_name.  Include the trailing and leading slashes.
  !!
  !! @param[out]    name_list         List of names extracted from @a tlist.
  !!
  !! @param[out]    err_list          List of errors.  No specific component 
  !! is assigned.
  !!
  !! The syntax of a comma separated list is defined by syntax rule R101 
  !! for a /list/, but we don't explicitly refer to that in any 
  !! generated error messages.
  
  SUBROUTINE ParseNameList_( tlist, list_start, list_finish, syntax_rule,  &
      name_list, err_list )
    
    USE Strings
    USE Errors
    USE ErrorCodes
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: list_start
    INTEGER, INTENT(IN) :: list_finish
    CHARACTER(*), INTENT(IN) :: syntax_rule
    TYPE(String), INTENT(OUT), ALLOCATABLE :: name_list(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: item_count     ! Number of items in the list.
    INTEGER :: iname          ! List item index.
    INTEGER :: item_start     ! Start index in tlist of a particular item.
    INTEGER :: item_finish    ! Finish index in tlist of a particular item.
    TYPE(Error) :: err        ! Error code for procedure calls.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    item_count = ListItemCount(tlist(list_start:list_finish))
    ALLOCATE(name_list(item_count))
    item_finish = 0
    DO iname = 1, item_count
      CALL GetNextListItem(  &
          tlist,  &
          list_start, list_finish,  &
          item_start,  &
          item_finish )
      CALL ExpectName(tlist, item_start, syntax_rule, err)
      CALL Add(err_list, err)
      IF (Failure(err)) RETURN
      ! This assignment may also do kind conversion from scck to default char.
      name_list(iname)%item = QueryValue(tlist(item_start))
      IF (item_finish > item_start) THEN
        CALL Add( err_list,  &
            CODE=errSyntax,  &
            LOCATION=QueryLocation(tlist(list_start+1)),  &
            MSG='Unexpected token "' // QueryRaw(tlist(list_start+1)) //'".' )
        RETURN
      END IF
    END DO
    
  END SUBROUTINE ParseNameList_
  
  
  !*****************************************************************************
  !!
  !> Count the number of items in an /xyz-list/ (R101).
  !!
  !! @params[in]      tlist           The list of tokens.
  !!
  !! @returns One more than the number of bare commas in @a tlist.
  !!
  !! If @a tlist ends in a ',' then that counts as an additional item, i.e. 
  !! 'name,' will result in a count of two.
  
  FUNCTION ListItemCount(tlist) RESULT(number)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    
    ! Function result
    INTEGER :: number
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it
    INTEGER :: comma
    
    !***************************************************************************
    
    number = 1
    it = 1
    DO
      comma = FindBare(tlist, ittComma, scck_',', it)
      IF (comma == 0) EXIT
      it = comma + 1
      number = number + 1
    END DO
    
  END FUNCTION ListItemCount
  
  
  !*****************************************************************************
  !!
  !> Get the range of tokens occupied by an item in a list.
  !!
  !! @param[in]     tlist             The list of tokens that make up the 
  !! list.
  !!
  !! @param[out]    item_start        The index of the start of the next list 
  !! item.
  !!
  !! @param[in,out] item_finish       On input - the end of the previous item 
  !! in the list, on output - the end of the next item in the list.  To 
  !! retrieve the range of the first item set this to zero.
  !!
  !! The typical usage is:
  !! @code
  !! list_count = ListItemCount(tlist)
  !! ALLOCATE(list_items(list_count))
  !! item_finish = 0
  !! DO i = 1, list_count
  !!   CALL GetNextListItem(tlist, item_start, item_finish)
  !!   CALL parse_item( tlist(item_start, item_finish), list_items(i),  &
  !!       sub_err_list )
  !!   CALL Add(err_list, sub_err_list, COMPONENT=comp)
  !!   IF (Failure(sub_err_list)) RETURN
  !! END DO
  !! @endcode
  !!
  !! Alternatively, if the number of items isn't required:
  !!
  !! @code
  !! item_finish = 0
  !! DO
  !!   CALL GetNextListItem(tlist, item_start, item_finish)
  !!   IF (start == 0) EXIT
  !!   CALL parse_item(tlist(item_start, item_finish), item, sub_err_list)
  !!   CALL Add(err_list, sub_err_list, COMPONENT=comp)
  !!   IF (Failure(sub_err_list)) RETURN
  !! END DO
  !! @endcode
  
  SUBROUTINE GetNextListItem_(tlist, item_start, item_finish)
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: item_start
    INTEGER, INTENT(INOUT) :: item_finish
    
    !***************************************************************************
    
    IF (item_finish == 0) THEN
      item_start = 1
    ELSE IF (item_finish == SIZE(tlist)) THEN
      ! The list is finished
      item_start = 0
      item_finish = 0
    ELSE
      ! Skip the comma
      item_start = item_finish + 2
    END IF
    
    item_finish = FindBare(tlist, ittComma, scck_',', item_start)
    IF (item_finish == 0) item_finish = SIZE(tlist)
    
  END SUBROUTINE GetNextListItem_
  
  
  !*****************************************************************************
  !!
  !> Get the range of tokens occupied by a list item, where the list is 
  !! specified by start and end tokens.
  !!
  !! Typical usage:
  !1
  !! @code
  !! list_count = ListItemCount(tlist(list_start:list_finish)
  !! ALLOCATE(list_items(list_count))
  !! item_finish = 0
  !! DO i = 1, list_count
  !!   CALL GetNextListItem( tlist, list_start, list_finish, item_start,  &
  !!       item_finish )
  !!   CALL parse_item( tlist(item_start, item_finish), list_items(i),  &
  !!       sub_err_list )
  !!   CALL Add(err_list, sub_err_list, COMPONENT=comp)
  !!   IF (Failure(sub_err_list)) RETURN
  !! END DO
  !! @endcode
  !!
  !! Alternatively, if the number of items isn't required:
  !!
  !! @code
  !! item_finish = 0
  !! DO
  !!   CALL GetNextListItem( tlist, list_start, list_finish, item_start,  &
  !!       item_finish )
  !!   IF (start == 0) EXIT
  !!   CALL parse_item(tlist(item_start, item_finish), item, sub_err_list)
  !!   CALL Add(err_list, sub_err_list, COMPONENT=comp)
  !!   IF (Failure(sub_err_list)) RETURN
  !! END DO
  !! @endcode
  
  SUBROUTINE GetNextListItem_sf( tlist, list_start, list_finish, item_start,  &
      item_finish )
    
    USE CompilerKinds
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: list_start
    INTEGER, INTENT(IN) :: list_finish
    INTEGER, INTENT(OUT) :: item_start
    INTEGER, INTENT(INOUT) :: item_finish
    
    !***************************************************************************
    
    IF (item_finish < list_start) THEN
      item_start = list_start
    ELSE IF (item_finish >= list_finish) THEN
      ! The list is finished
      item_start = 0
      item_finish = 0
      RETURN
    ELSE
      ! Skip the comma
      item_start = item_finish + 2
    END IF
    
    item_finish = FindBare(tlist, ittComma, scck_',', item_start)
    IF (item_finish == 0) THEN
      item_finish = list_finish
    ELSE IF (item_finish > list_finish) THEN
      item_finish = list_finish
    ELSE
      ! The item finishes before the comma.
      item_finish = item_finish - 1
    END IF
    
  END SUBROUTINE GetNextListItem_sf
  
  
  !*****************************************************************************
  !!
  !> Get the location of a token in a sequence of tokens, or no location if 
  !! the given location is out of range.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     pos               The token of interest.  This may be 
  !! out of range of @a tlist.
  !!
  !! @returns The location of the given token in tlist, or a location that 
  !! indicates no location.
  
  FUNCTION SafeLocation(tlist, pos) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: pos
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    IF (pos < 1) THEN
      IF (SIZE(tlist) > 0) THEN
        loc = QueryLocation(tlist(1))
      ELSE
        loc = SourceLocation()
      END IF
    ELSE IF (pos > SIZE(tlist)) THEN
      IF (SIZE(tlist) > 0) THEN
        loc = QueryLocation(tlist(SIZE(tlist)))
      ELSE
        loc = SourceLocation()
      END IF
    ELSE
      loc = QueryLocation(tlist(pos))
    END IF
    
  END FUNCTION SafeLocation
  
END MODULE ParseUtils
