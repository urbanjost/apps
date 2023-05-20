! $Id: ScanFreeForm.f90 2798 2019-03-22 17:18:41Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ScanFreeForm module.


!*******************************************************************************
!!
!> Defines the FreeFormSource type - an extension of the SourceForm type that 
!! carries out scanning (lexing) of free form source fortran files into 
!! lexical tokens.
!!
!! Free form source was what was used to originally design the Scanner, so 
!! most things below are sane.

MODULE ScanFreeForm
  
  USE CompilerKinds
  USE Scanner
  USE Sources
  USE Tokens
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: FreeFormSource
  
  !-----------------------------------------------------------------------------
  
  !> Type that provides an interface between a free form source file and 
  !! other parsing routines.
  TYPE, EXTENDS(SourceFormBase) :: FreeFormSource
    !> The source file that is being translated by the particular instance 
    !! of the source form.
    CLASS(Source), POINTER :: source_file
    !> The contents of the last line read (the current line).
    CHARACTER(:,KIND=scck), ALLOCATABLE :: line
    !> The position in @a line of the next character to be returned by GetChar.
    INTEGER :: pos
    !> .TRUE. immediately after a new line, which isn't a continuation line, 
    !! has been read in and then set to false when a non-blank character is 
    !! retrieved or skipped.
    LOGICAL :: start_of_line
    !> Number of continuation lines that have been read since the last non-
    !! continuation line.
    INTEGER :: continuations
  CONTAINS
    ! Bindings inherited from SourceForm.
    PROCEDURE :: Clone => freeform_Clone
    PROCEDURE :: GetSource => freeform_GetSource
    PROCEDURE :: GetChar => freeform_GetChar
    PROCEDURE :: GetStatement => freeform_GetStatement
    PROCEDURE :: SplitTokenError => freeform_SplitTokenError
    PROCEDURE :: SplitCharLiteralError => freeform_SplitCharLiteralError
    PROCEDURE :: CheckTokenSeparation => freeform_CheckTokenSeparation
  END TYPE FreeFormSource
  
  !-----------------------------------------------------------------------------
  ! Characters with special meaning in free form source.
  
  !> Maximum line length of a free form source line.
  INTEGER, PARAMETER :: line_len = 132
  
  !> Line continuation character.
  CHARACTER(*, KIND=scck), PARAMETER :: continue_char = scck_'&'
  
  !> Comment character.
  CHARACTER(*, KIND=scck), PARAMETER :: comment_char = scck_'!'
  
  !> In-line statement separator.
  CHARACTER(*, KIND=scck), PARAMETER :: eos_char = scck_';'
  
  !-----------------------------------------------------------------------------
  
  !> Construct a FreeFormSource object.
  INTERFACE FreeFormSource
    MODULE PROCEDURE FreeFormSrc_
  END INTERFACE FreeFormSource
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Construct a FreeFormSource object given a reference to a source file.
  !!
  !! @param[in]     source_file       Pointer to the source file object that 
  !! the FreeFormSource object will read from.
  !!
  !! @returns A FreeFormSource object constructed to read from the given 
  !! source file.
  
  FUNCTION FreeFormSrc_(source_file) RESULT(form)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(Source), INTENT(IN), POINTER :: source_file
    
    ! Function result
    TYPE(FreeFormSource) :: form
    
    !***************************************************************************
    
    form%source_file => source_file
    form%pos = 0
    
  END FUNCTION FreeFormSrc_
  
  
  !*****************************************************************************
  !!
  !> Implementation of FreeFormSource%Clone - create a copy of the source form 
  !! attached to a different source file.
  !!
  !! @param[in]     form              The object to clone.
  !!
  !! @param[in]     source_form       The source to attach the new source
  !! form object to.
  !!
  !! @param[out]    new_form          A FreeFormSource object attached to
  !! the @a source_file source.
  
  SUBROUTINE freeform_Clone(form, source_file, new_form)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(IN) :: form
    CLASS(Source), INTENT(IN), POINTER :: source_file
    CLASS(SourceForm), INTENT(OUT), ALLOCATABLE :: new_form
    
    !***************************************************************************
    
    ALLOCATE(new_form, SOURCE=FreeFormSource(source_file))
    
  END SUBROUTINE freeform_Clone
  
  
  !*****************************************************************************
  !!
  !> Implementation of FreeFormSource%GetSource - return the source object that 
  !! the source form is attached to.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[out]    source_file       A pointer to the source file that 
  !! @a form is attached to.
  
  SUBROUTINE freeform_GetSource(form, source_file)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(IN) :: form
    CLASS(Source), INTENT(OUT), POINTER :: source_file
    
    !***************************************************************************
    
    source_file => form%source_file
    
  END SUBROUTINE freeform_GetSource
  
  
  !*****************************************************************************
  !!
  !> Implementation of FreeFormSource%GetStatement - get a statement's worth 
  !! of tokens.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[in]     part_stack        The syntax part stack prior to the 
  !! statement to be retrieved.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    statement_tokens  The sequence of tokens for the 
  !! next statement.
  !!
  !! @param[out]    statement_label   The label for the next statement, 
  !! or zero if the statement had no label.
  !!
  !! We don't require context information in order to resolve tokens.
  !!
  !! A bare semi-colon or end of line without a continuation character 
  !! terminates collection of the statement.
  !!
  !! A leading integer literal is interpreted as being a statement label, 
  !! always.  Depending on how the interpretation for F08/0075 goes, this 
  !! might need rethinking.  One possible outcome of F08/0075 is to ban 
  !! any leading digits in a statement - technically we would let that 
  !! through without warning if we decided that the initial digits were 
  !! part of a real literal or the kind parameter of a character literal.
  !!
  !! @todo Review when j3 work out what they want to do.
  
  SUBROUTINE freeform_GetStatement( form, part_stack,  &
      err_list, statement_tokens, statement_label )
    
    USE CharacterTypes
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE LabelsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(INOUT) :: form
    INTEGER, INTENT(IN) :: part_stack(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT), ALLOCATABLE :: statement_tokens(:)
    TYPE(Label), INTENT(OUT), ALLOCATABLE :: statement_label
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! For efficiency reasons, we use a larger buffer for our tokens that 
    ! is necessarily required.  This is the initial size of that buffer.  
    ! Ideally this would be set such that 90% (thumb-suck) of statements 
    ! had total token counts less than or equal to this value.
    INTEGER, PARAMETER :: initial_token_size = 10
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Current token.
    TYPE(Token) :: the_token
    
    ! Error messages from procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    ! Buffer of tokens used to assemble the statements worth.
    TYPE(Token), ALLOCATABLE ::tmp_tokens(:)
    
    ! Number of elements in tmp_tokens that are in use.
    INTEGER :: tmp_token_size
    
    ! Flag to indicate whether we are dealing with the first token in a 
    ! statement.  Used for statement label logic.
    LOGICAL :: first_token
    
    ! The token for the statement label.  We save this after processing the 
    ! token into an integer in case we need to do later error reporting.  
    ! Use the value of @a statement_label to discern whether a statement 
    ! label has been encountered.
    TYPE(Token) :: label_token
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ALLOCATE(tmp_tokens(initial_token_size))
    tmp_token_size = 0
    first_token = .TRUE.    ! The first token is ... the first token.
    
    DO
      ! Free form scanning never has to worry about context.
      CALL GetToken(form, .TRUE., sub_err_list, the_token)
      CALL Add(err_list, sub_err_list)
      ! On error parsing a token we abandon the entire statement.
      IF (Failure(sub_err_list)) RETURN
      
      ! First token might actually be a statement label.  A valid statement 
      ! label looks like an integer literal.
      IF (first_token) THEN
        first_token = .FALSE.   ! Tokens after the first are not the first.
        IF (the_token == ittIntegerLiteral) THEN
          ! To be a valid label it the thing that looks like an integer 
          ! literal must have no kind.
          IF (.NOT. HasKind(the_token)) THEN
            ! Stash the statement label token away for later error reporting.
            label_token = the_token
            ! Convert the label into an integer.
            ALLOCATE(statement_label)
            CALL ScanLabel(  &
                QueryRaw(the_token),  &
                QueryLocation(the_token),  &
                statement_label,  &
                sub_err_list )
            CALL Add(err_list, sub_err_list)
            ! Keep going on error?
            CYCLE
          END IF
        END IF
        ! @todo To implement one possible outcome of F08/0075 we should 
        ! probably check that the first token here doesn't start with a 
        ! digit in its raw text.
      END IF
      
      IF (the_token == ittEndStatement) THEN
        ! Chop out relevant part of temporary buffer.
        statement_tokens = tmp_tokens(1:tmp_token_size)
        IF ((tmp_token_size == 0) .AND. ALLOCATED(statement_label)) THEN
          CALL Add( err_list,  &
              CODE=errLabelWithNoStatement,  &
              COMPONENT='3.2.5p2',  &
              LOCATION=QueryLocation(label_token),  &
              MSG='A label appears without an associated statement.' )
        END IF
        RETURN
      END IF
      
      ! If the token list is getting too big for its boots, then grow its 
      ! boots.
      IF (tmp_token_size == SIZE(tmp_tokens)) THEN
        ! Double allocation strategy.
        ALLOCATE(statement_tokens(SIZE(tmp_tokens) * 2))
        statement_tokens(1:SIZE(tmp_tokens)) = tmp_tokens
        CALL MOVE_ALLOC(statement_tokens, tmp_tokens)
      END IF
      
      tmp_token_size = tmp_token_size + 1
      tmp_tokens(tmp_token_size) = the_token
      
    END DO
    
  END SUBROUTINE freeform_GetStatement
  
  
  !*****************************************************************************
  !!
  !> Implementation of FreeFormSource%SplitTokenError - called when the 
  !! Scanner has a token that is split across a line without an ampersand.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[in]     initial_chars     The token character sequence as it 
  !! existed prior to the split.
  !!
  !! @param[in]     next_ch           The first character on the next 
  !! line.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! Free form source has requirements around split tokens.  Note that the 
  !! Scanner may not detect all cases.
  
  SUBROUTINE freeform_SplitTokenError(form, initial_chars, next_ch, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(IN) :: form
    TYPE(SourceChar), INTENT(IN) :: initial_chars(:)
    TYPE(SourceChar), INTENT(IN) :: next_ch
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Text for the characters on the intial line.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: initial_text
    
    !***************************************************************************
    
    ! Jump out early if there is a chance that we have an keyword sequence 
    ! with optional blank.  Ideally we'd be a bit more precise with the 
    ! following, but requires more characters on the next line or even 
    ! statement classification.
    IF ((SIZE(initial_chars) >= 2) .AND. (SIZE(initial_chars) <= 6)) THEN
      initial_text = CharsToText(initial_chars)
      CALL UpperCase(initial_text)
      IF (initial_text == scck_'GO') THEN
        IF (upper_is_in_set(next_ch%char, scck_'T')) RETURN
      ELSE IF (initial_text == scck_'IN') THEN
        IF (upper_is_in_set(next_ch%char, scck_'O')) RETURN
      ELSE IF (initial_text == scck_'END') THEN
        IF (upper_is_in_set(next_ch%char, scck_'ABCDEFIMPSTW')) RETURN
      ELSE IF (initial_text == scck_'ELSE') THEN
        IF (upper_is_in_set(next_ch%char, scck_'IW')) RETURN
      ELSE IF (initial_text == scck_'BLOCK') THEN
        IF (upper_is_in_set(next_ch%char, scck_'D')) RETURN
      ELSE IF (initial_text == scck_'SELECT') THEN
        IF (upper_is_in_set(next_ch%char, scck_'CT')) RETURN
      ELSE IF (initial_text == scck_'DOUBLE') THEN
        IF (upper_is_in_set(next_ch%char, scck_'P')) RETURN
      END IF
    END IF
    
    ! Continued token must start with an ampersand.
    CALL Add( err_list,  &
        CODE=errSplitTokenNoAmpersand,  &
        LEVEL=errLevelWarning,  &
        COMPONENT='3.3.2.4p3',  &
        LOCATION=next_ch%pos,  &
        MSG='Split lexical token without a & as the first non-blank &
          &character on the next line after the continuation &.' )
    
  END SUBROUTINE freeform_SplitTokenError
  
  
  !*****************************************************************************
  !!
  !> Utility routine for freeform_SplitTokenError that does an upper case 
  !! character comparison of a character with a set of characters.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @param[in]     set               The set of characters to test against.  
  !! These should be upper case.
  
  FUNCTION upper_is_in_set(ch, set)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    CHARACTER(*,KIND=scck), INTENT(IN) :: set
    
    ! Function result.
    LOGICAL :: upper_is_in_set
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Upper case form of next_ch.
    CHARACTER(KIND=scck) :: upper_ch
    
    !***************************************************************************
    
    upper_ch = ch
    CALL UpperCase(upper_ch)
    
    upper_is_in_set = SCAN(upper_ch, set) /= 0
    
  END FUNCTION upper_is_in_set
  
  
  !*****************************************************************************
  !!
  !> Implementation of FreeFormSource%SplitCharLiteralError - called if an 
  !! improper continuation  (no ampersand on the continued line) in a 
  !! character literal constant is detected by the Scanner.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[in]     initial_chars     The token character sequence as it 
  !! existed prior to the split.
  !!
  !! @param[in]     next_ch           The first character on the next 
  !! line.
  !!
  !! @param[out]    err_list          List of errors.  Note that this is 
  !! INTENT(INOUT).
  !!
  !! Free form source has requirements around split character literals 
  !! (similar requirements to the general case of a split lexical token).
  
  SUBROUTINE freeform_SplitCharLiteralError( form, initial_chars,  &
      next_ch, err_list )
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(IN) :: form
    TYPE(SourceChar), INTENT(IN) :: initial_chars(:)
    TYPE(SourceChar), INTENT(IN) :: next_ch
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! Continued char literal must start with an ampersand.
    CALL Add( err_list,  &
        CODE=errNoAmpersand,  &
        LOCATION=next_ch%pos,  &
        LEVEL=errLevelWarning,  &
        COMPONENT='3.3.2.4p4',  &
        MSG='Continued character constant does not have a & as the &
          &first non-blank character on the next line after the &
          &continuation &.' )
    
  END SUBROUTINE freeform_SplitCharLiteralError
  
  
  !*****************************************************************************
  !!
  !> Implementation of FreeFormSource%CheckTokenSeparation - gives an 
  !! error if there isn't a blank between tokens that require it.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[in]     the_token         The token that has just finished.
  !!
  !! @param[in]     next_ch           The character immediately following the 
  !! token.
  !!
  !! @param[in,out] err_list          List of errors.  Note that this is 
  !! INTENT(INOUT).
  
  SUBROUTINE freeform_CheckTokenSeparation(form, the_token, next_ch, err_list)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(IN) :: form
    TYPE(Token), INTENT(IN) :: the_token
    TYPE(SourceChar), INTENT(IN) :: next_ch
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! If end-of-statement or end-of-file then there is no adjoining token.
    IF (next_ch%eos .OR. next_ch%eof) RETURN
    
    IF ( (the_token == ittName)  &
        .OR. (the_token == ittIntegerLiteral)  &
        .OR. (the_token == ittRealLiteral)  &
        .OR. (the_token == ittLogicalLiteral)  &
        .OR. (the_token == ittCharacterLiteral)  &
        .OR. (the_token == ittBOZLiteral)  &
        .OR. (the_token == ittStatementLabel) ) THEN
      ! logical-literal-constant's start with a dot, but that could also 
      ! be an operator.  It's too hard to check without looking ahead.
      IF ( IsLetterChar(next_ch%char)  &
            .OR. IsDigitChar(next_ch%char)  &
            .OR. IsQuoteChar(next_ch%char) ) THEN
        ! We have a name or constant that runs into another name or constant.
        CALL Add( err_list,  &
            CODE=errMissingBlankBetweenTokens,  &
            COMPONENT='3.3.2.2',  &
            LOCATION=next_ch%pos,  &
            MSG='Missing blank between adjacent constant or name tokens.' )
      END IF
    END IF
    
  END SUBROUTINE freeform_CheckTokenSeparation
  
  
  !*****************************************************************************
  !!
  !> Implementation of FreeFormSource%GetChar - get a character from a free 
  !! form source file.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[in]     in_character_context    Flag to indicate if a character 
  !! context (inside the quotes of a character literal or a character string 
  !! edit descriptor.
  !! 
  !! @param[in]     context_chars     Defined only if in_character_context 
  !! is .TRUE., in which case if it is non-zero length then it contains 
  !! the characters that are considered to continue the character context, 
  !! other characters are not considered to be part of the character context.  
  !! If zero length then all characters will be considered part of the 
  !! character context.
  !!
  !! @param[out]    ch                The next character read from the source 
  !! form.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE freeform_GetChar( form, in_character_context, context_chars,  &
      ch, err_list )
    
    USE Errors
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    
    CLASS(FreeFormSource), INTENT(INOUT) :: form
    LOGICAL, INTENT(IN) :: in_character_context
    CHARACTER(*), INTENT(IN) :: context_chars
    TYPE(SourceChar), INTENT(OUT) :: ch
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Error list for procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    LOGICAL :: local_char_context     ! Are we really in a char context?
    LOGICAL :: local_end_of_line      ! Are we at end of line?
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! Check if this is the first time that we've been called for this instance.
    IF (form%pos == 0) THEN
      CALL get_normal_line(form, sub_err_list)
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
      
      IF (.NOT. ALLOCATED(form%line)) THEN
        ! Empty file?
        ch%eos = .TRUE.
        ch%char = ''
        ch%eof = .TRUE.
        ch%start_of_line = .TRUE.
        RETURN
      END IF
    END IF
    
    ! The position that we return is the position of the character (or end 
    ! of statement) that we return.
    ch%pos = SourceLocationIfortBug(form%source_file, form%pos)
    ch%start_of_line = form%start_of_line
    form%start_of_line = .FALSE.
    
    ! End of statement here can be because we ran out of characters on the 
    ! line or because we hit a comment.
    IF (form%pos > LEN(form%line)) THEN
      local_end_of_line = .TRUE.
    ELSE IF (TestCharacterContext(  &
        in_character_context,  &
        context_chars,  &
        ch%char )) THEN
      local_end_of_line = .FALSE.
      local_char_context = .TRUE.
    ELSE IF (form%line(form%pos:form%pos) == comment_char) THEN
      local_end_of_line = .TRUE.
    ELSE
      local_end_of_line = .FALSE.
      local_char_context = .FALSE.
    END IF
    
    IF (local_end_of_line) THEN
      ! End of statement, in which case we also need to get a new line.
      ch%eos = .TRUE.
      ch%char = ''
      ch%got_amp = .TRUE.
      ch%eof = .FALSE.
      ch%end_of_line = .TRUE.
      
      CALL get_normal_line(form, sub_err_list)
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
      
      ch%eof = .NOT. ALLOCATED(form%line)
    ELSE
      IF (test_continuation(form, local_char_context)) THEN
        CALL get_continuation_line(form, sub_err_list, ch%got_amp)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        ! pos will be valid here.  Without an ampersand it will be 1.  
        ! Note that no following continuation line is an error anyway, so 
        ! we don't need to worry about checking for EOF.
      ELSE
        ch%got_amp = .TRUE.
      END IF
      ch%end_of_line = .FALSE.
      ch%eof = .FALSE.
      ch%char = form%line(form%pos:form%pos)
      IF (local_char_context) THEN
        ch%eos = .FALSE.
        form%pos = form%pos + 1
      ELSE
        IF (ch%char == eos_char) THEN
          ch%eos = .TRUE.
          CALL skip_semicolons(form%line, form%pos)
          
          ! If we are here then we are not in a character context.
          IF (form%pos > LEN(form%line)) THEN
            local_end_of_line = .TRUE.
          ELSE IF (form%line(form%pos:form%pos) == comment_char) THEN
            local_end_of_line = .TRUE.
          ELSE
            local_end_of_line = .FALSE.
          END IF
          
          IF (local_end_of_line) THEN
            CALL get_normal_line(form, sub_err_list)
            CALL Add(err_list, sub_err_list)
            IF (Failure(sub_err_list)) RETURN
            ch%eof = .NOT. ALLOCATED(form%line)
          END IF
        ELSE
          ch%eos = .FALSE.
          form%pos = form%pos + 1
        END IF
      END IF
    END IF
    
  END SUBROUTINE freeform_GetChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether the current character is a continuation ampersand.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[in]     in_character_context    Flag to indicate if a character 
  !! context (inside the quotes of a character literal or a character string 
  !! edit descriptor.  This is not the same as the argument of the same 
  !! name passed to freeform_GetChar - for this procedure we need the cases 
  !! where the context_char argument is non-zero length and the character 
  !! is not in context_char filtered out as NOT being in a character context.
  !! 
  !! A continuation ampersand is an ampersand that is the last non-blank 
  !! and non-comment character on the line.  If we are in a character 
  !! context then drop the non-comment requirement too (or you'd never 
  !! be able to embed a & inside a character literal).
  
  FUNCTION test_continuation(form, in_character_context) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(FreeFormSource), INTENT(IN) :: form
    LOGICAL, INTENT(IN) :: in_character_context
    
    ! Function result
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: tmp_pos        ! Temporary line position.
    
    !***************************************************************************
    
    IF (form%line(form%pos:form%pos) == continue_char) THEN
      ! It's an ampersand - but is it the last one on the line?
      tmp_pos = form%pos + 1
      CALL skip_ws(form%line, tmp_pos)
      IF (tmp_pos > LEN(form%line)) THEN
        b = .TRUE.
        RETURN
      END IF
      IF ( .NOT. in_character_context  &
          .AND. (form%line(tmp_pos:tmp_pos) == comment_char) ) THEN
        b = .TRUE.
        RETURN
      END IF
    END IF
    
    b = .FALSE.
    
  END FUNCTION test_continuation
  
  
  !*****************************************************************************
  !!
  !> Gets the next line when that line is a continuation line.
  !!
  !! @param[in,out] form              The source form object.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    got_amp           Flag to indicate that an ampersand 
  !! was present at the start of the continued line.
  !!
  !! On success (no failure in @a err_list), always returns with the @a pos 
  !! component of @ form being a valid position in the line (it is illegal to 
  !! have a continuation line without some sort of statement character in it).
  !!
  !! @a pos will be advanced to the first character after the & (if present) 
  !! on the continued line, or just the first character (if no & present).
  
  SUBROUTINE get_continuation_line(form, err_list, got_amp)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    LOGICAL, INTENT(OUT) :: got_amp
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(Error) :: err      ! Error object for procedure calls.
      
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    DO
      ! Get another line of source.
      CALL form%source_file%ReadLine(form%line, err)
      CALL Add(err_list, err)
      IF (Failure(err)) RETURN
      
      ! Check for end of file.  This is an error.
      IF (.NOT. ALLOCATED(form%line)) THEN
        ! @todo Review location.
        CALL Add( err_list,  &
            CODE=errNoFollowingLine,  &
            COMPONENT='3.3.2.4',  &
            LOCATION=SourceLocationIfortBug(form%source_file, 1),  &
            MSG='Continuation without following line.' )
        RETURN
      END IF
      
      ! Check line length.
      CALL check_line_length(form, err)
      CALL Add(err_list, err)
      
      ! Skip whitespace
      form%pos = 1
      CALL skip_ws(form%line, form%pos)
      
      ! Check for comment line, which is a line with all blanks or no 
      ! characters, or a line where the first non-blank is ! (3.3.2.3)
      IF (form%pos > LEN(form%line)) CYCLE
      IF (form%line(form%pos:form%pos) == comment_char) CYCLE
      
      ! Track number of continuation lines.
      form%continuations = form%continuations + 1
      IF (form%continuations == 256) THEN
        ! The error is reported against the first non blank character of the 
        ! continued line.  @todo Review.
        CALL Add( err_list,  &
            CODE=errTooManyContinuationLines,  &
            LEVEL=errLevelWarning,  &
            COMPONENT='3.3.2.6p2',  & 
            LOCATION=SourceLocationIfortBug(form%source_file, form%pos),  &
            MSG='A statement shall not have more than 255 continuation &
              &lines.' )
      END IF
      
      IF (test_continuation(form, .FALSE.)) THEN
        ! Requirement that no line shall contain & as the only 
        ! non-blank character.  @todo Review location.
        CALL Add( err_list,  &
            CODE=errBareAmpersand,  &
            COMPONENT='3.3.2.4p1',  &
            LOCATION=SourceLocationIfortBug(form%source_file, form%pos),  &
            MSG='Only nonblank character on line (excluding comments) &
              &is "&".' )
        RETURN
      END IF
      
      IF (form%line(form%pos:form%pos) == continue_char) THEN
        ! If we make it through the test_continue block above, then the
        ! ampersand is not the only character on the line.
        !
        ! Statement continues after the initial ampersand.
        form%pos = form%pos + 1
        got_amp = .TRUE.
      ELSE
        ! Continued statement without ampersand on the continuation line 
        ! starts from column one.
        form%pos = 1
        got_amp = .FALSE.
      END IF
      
      ! Got something
      EXIT
    END DO
    
  END SUBROUTINE get_continuation_line
  
  
  !*****************************************************************************
  !!
  !> Gets the next line when that line is not a continuation line.
  !!
  !! @param[in,out] form              The source form object.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! End of file is indicated by the line component not being allocated.
  
  SUBROUTINE get_normal_line(form, err_list)
    
    USE ErrorCodes
    USE Errors
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(Error) :: err    ! Error object for Source%ReadLine and other calls.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! Reset the continuation line counter.
    form%continuations = 0
    
    DO
      ! Get a line of source.
      CALL form%source_file%ReadLine(form%line, err)
      CALL Add(err_list, err)
      IF (Failure(err)) RETURN
      
      ! Check for end of file.
      IF (.NOT. ALLOCATED(form%line)) RETURN
      
      ! Check line length.
      CALL check_line_length(form, err)
      CALL Add(err_list, err)
      
      ! Skip leading blanks
      form%pos = 1
      form%start_of_line = .TRUE.
      CALL skip_ws(form%line, form%pos)
      
      ! Check for blank line (which is a form of comment line).
      IF (form%pos > LEN(form%line)) CYCLE
      
      IF (form%line(form%pos:form%pos) == eos_char) THEN
        ! Skip leading semicolons and blanks
        CALL skip_semicolons(form%line, form%pos)
        IF (form%pos > LEN(form%line)) CYCLE
        form%start_of_line = .FALSE.
      ELSE IF (test_continuation(form, .FALSE.)) THEN
        ! Check the requirement that no line shall contain & as the only 
        ! non-blank character.  @todo Review location.
        CALL Add( err_list,  &
            CODE=errBareAmpersand,  &
            COMPONENT='3.3.2.4p1',  &
            LOCATION=SourceLocationIfortBug(form%source_file, form%pos),  &
            MSG='Only non-blank character on line (excluding comments) &
              &is &.' )
        RETURN
      END IF
      
      ! Check for comment line.
      IF (form%line(form%pos:form%pos) == comment_char) CYCLE
      
      ! Got something
      EXIT
    END DO
    
  END SUBROUTINE get_normal_line
  
  
  !*****************************************************************************
  !!
  !> Advances over whitespace to the next non-blank character or one past the 
  !! end of the string.
  !!
  !! @param[in]     line              The string.
  !!
  !! @param[in,out] pos               Current position in the string - updated 
  !! to point to the next non-blank character or otherwise one beyond the end 
  !! of the string.
  
  SUBROUTINE skip_ws(line, pos)
    
    USE CharacterTypes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*, KIND=scck), INTENT(IN) :: line
    INTEGER, INTENT(INOUT) :: pos
    
    !***************************************************************************
    
    DO WHILE (pos <= LEN(line))
      IF (.NOT. IsBlankChar(line(pos:pos))) EXIT
      pos = pos + 1
    END DO
    
  END SUBROUTINE skip_ws
  
  
  !*****************************************************************************
  !!
  !> Advance over a sequence of "zero or more blanks and one or more ';' 
  !! terminators, in any order", until a different character or the end 
  !! of line is reached.
  !!
  !! @param[in]     line              The string.
  !!
  !! @param[in,out] pos               Current position in the string - updated 
  !! to point to the next character that is not a blank or semicolon 
  !! or otherwise one beyond the end of the string.
  
  SUBROUTINE skip_semicolons(line, pos)
    
    USE CharacterTypes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*, KIND=scck), INTENT(IN) :: line
    INTEGER, INTENT(INOUT) :: pos
    
    !***************************************************************************
    
    DO WHILE (pos <= LEN(line))
      ! Check not blank or semicolon.
      IF (.NOT. ( IsBlankChar( line(pos:pos) )  &
          .OR. (line(pos:pos) == eos_char) )) EXIT
      pos = pos + 1
    END DO
    
  END SUBROUTINE skip_semicolons
  
  
  !*****************************************************************************
  !!
  !> Check that the length of a line fits within the free form source limits.
  !!
  !! @param[in]     form              The source form object that has the 
  !! line (in the line component) to check.
  !!
  !! @param[out]    err               Error object.
  !!
  !! There are two possible warnings returned in @a err.  errLineTooLong 
  !! is returned if there are non-blank characters past position 132, 
  !! otherwise errLineWithBlanksTooLong is returned if there are blank 
  !! characters past position 132.
  !!
  !! If line is less than 132 characters in length then no error is returned.
  !!
  !! ("132" is actually whatever the value of the line_len module parameter.)
  
  SUBROUTINE check_line_length(form, err)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FreeFormSource), INTENT(IN) :: form
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
      
    ! Check line length - first the non-blank length, then the length with
    ! blanks.
    IF (LEN_TRIM(form%line) > line_len) THEN
      err = ConstructError(  &
          CODE=errLineTooLong,  &
          LEVEL=errLevelWarning,  &
          COMPONENT='3.3.2.1p1',  &
          LOCATION=SourceLocationIfortBug(form%source_file, line_len),  &
          MSG='The last non-blank character on the &
            &line exceeds position ' // ToString(line_len) // '.' )
    ELSE IF (LEN(form%line) > line_len) THEN
      err = ConstructError(  &
          CODE=errLineWithBlanksTooLong,  &
          LEVEL=errLevelWarning,  &
          COMPONENT='3.3.2.1p1',  &
          LOCATION=SourceLocationIfortBug(form%source_file, line_len),  &
          MSG='Blank characters on the line exceed position '  &
            // ToString(line_len) // '.' )
    END IF
    
  END SUBROUTINE check_line_length
  
END MODULE ScanFreeForm
