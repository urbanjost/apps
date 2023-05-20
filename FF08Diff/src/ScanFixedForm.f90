! $Id: ScanFixedForm.f90 2802 2019-03-22 18:39:26Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ScanFixedForm module.


!*******************************************************************************
!!
!> Provides an extension of the SourceForm type named FixedFormSource, that 
!! can process fixed form source.
!!
!! Unfortunately (because it makes things very complicated) token boundaries 
!! in fixed form source are context dependent.  consider DOI=1,2 - which 
!! has a 'DO' token followed by 'I', '=', '1', ',' and '2'; 
!! then compare with DOI=1, which has a DOI, then '=' and '1'.
!!
!! Scanning of fixed for into tokens is a horrible exercise because of the 
!! lack of a definitive token separation charater (blanks are not 
!! signficant) and context dependencies that are almost ubiquitous.
!!
!! Consequently, we "import" much of the syntax contents into this module, 
!! primarily as lists of keywords, but there's also some other nonsense 
!! for specific statement forms that require support from the form-
!! independent procedures of the Scanner.
!!
!! This means that fixed form scanning is not particular efficient, and 
!! statement classification, once you have a sequence of tokens, is 
!! essentially a repeat of what was required to build the sequence of tokens.  
!! But it's fixed form source, so who cares.

MODULE ScanFixedForm
  
  USE CompilerKinds
  USE Scanner
  USE Sources
  USE Tokens
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: FixedFormSource
  
  !-----------------------------------------------------------------------------
  ! Characteristics of fixed form source.
  
  !> Column number for indicating comment lines.
  INTEGER, PARAMETER :: comment_col = 1
  
  !> Column numbers occupied by statement labels - start to finish.
  INTEGER, PARAMETER :: label_cols(2) = [ 1, 5 ]
  
  !> Column number used for the continuation marker.
  INTEGER, PARAMETER :: continue_col = 6
  
  !> Maximum line length.
  !!
  !! Lines longer than this generate a warning, even if only blank characters 
  !! are in the columns further to the right.
  INTEGER, PARAMETER :: line_len = 72
  
  !> Character that can be used to terminate statements in the middle of a 
  !! line.
  CHARACTER(*, KIND=scck), PARAMETER :: eos_char = scck_';'  
  
  !> Characters that can be used to denote a comment line if present 
  !! in column 1.
  CHARACTER(*, KIND=scck), PARAMETER :: line_comment_chars = scck_'!Cc*'
  
  !> Character that can be used to denote a comment at any position bar 
  !! position six.
  CHARACTER(*, KIND=scck), PARAMETER :: comment_char = scck_'!'
  
  !-----------------------------------------------------------------------------
  
  !> Type that represents a fixed form source file.
  TYPE, EXTENDS(SourceFormBase) :: FixedFormSource
    !> The source file that is being translated by the particular instance 
    !! of the source form.
    CLASS(Source), POINTER :: source_file
    
    !> The contents of the last line read (the current line).
    CHARACTER(line_len,KIND=scck) :: line
    
    !> The position in @a line of the last character returned by GetChar.
    !!
    !! This is set to zero during construction and is then used to indicate 
    !! that no lines have yet been read by the source form.
    INTEGER :: pos
    
    !> Number of continuation lines that have been read since the last non-
    !! continuation line.
    INTEGER :: continuations
  CONTAINS
    ! These are all inherited bindings from SourceForm.
    PROCEDURE :: Clone => fixed_Clone
    PROCEDURE :: GetSource => fixed_GetSource
    PROCEDURE :: GetChar => fixed_GetChar
    PROCEDURE :: GetStatement => fixed_GetStatement
    PROCEDURE :: SplitTokenError => fixed_SplitTokenError
    PROCEDURE :: SplitCharLiteralError => fixed_SplitTokenError
    PROCEDURE :: CheckTokenSeparation => fixed_CheckTokenSeparation
  END TYPE FixedFormSource
  
  
  !> A structure constructor overload for the FixedFormSource type.
  INTERFACE FixedFormSource
    MODULE PROCEDURE FixedFormSource_
  END INTERFACE FixedFormSource
  
  !-----------------------------------------------------------------------------
  ! Keyword splittage stuff
  
  !> Maximum length of a single keyword.
  !!
  !! Our longest keyword is DOUBLEPRECISION.
  INTEGER, PARAMETER :: keyword_length = 15
  
  !> The intrinsic-type-spec keywords
  CHARACTER(keyword_length), PARAMETER :: intrinsic_typespec_keywords(6) = [  &
      !123456789012345    123456789012345    123456789012345
      'REAL           ', 'INTEGER        ', 'CHARACTER      ',  &
      'LOGICAL        ', 'COMPLEX        ', 'DOUBLEPRECISION' ]
  
  !> Keywords that might need to be split from statements that 
  !! can start with a construct name.
  CHARACTER(keyword_length), PARAMETER :: construct_keywords(2) = [  &
      !123456789012345    123456789012345
      'DO             ', 'SELECT         ' ]
  
  !> Keywords that could go at the start of action-stmt's (only those 
  !! that could be the sub-statement in an if-stmt) that might require 
  !! splittage.
  CHARACTER(keyword_length), PARAMETER :: action_keywords(12) = [  &
      !123456789012345    123456789012345    123456789012345
      'BACKSPACE      ', 'CALL           ', 'CYCLE          ',  &
      'ENDFILE        ', 'ERROR          ', 'EXIT           ',  &
      'PRINT          ', 'READ           ', 'REWIND         ',  &
      'SYNC           ', 'STOP           ', 'GOTO           ' ]
  
  !> Keywords that could be hidden in a bigger token.
  !!
  !! This is limited to statement constructs that run keywords or keywords 
  !! and names or keywords and numbers together.  We don't include keywords 
  !! such as IF, because it should always be followed by something which 
  !! can't be part of a keyword token (the opening parentheses).  However, 
  !! ELSEIF and ENDIF can be followed by a construct name, so they are in 
  !! the list.
  !!
  !! Some times you can get three tokens in series.  In that case the 
  !! second_keywords list is used for the second keyword, and whatever 
  !! is left over goes into the third keyword.
  !!
  !! Where free form source allows it we combine keywords together without 
  !! an intervening space to reduce the size of the second_keywords list.
  !!
  !! This list is searched in order, so if there is a a chance that 
  !! part of a keyword could be accidentally matched by a shorter 
  !! keyword then the shorter keyword needs to come later in the list. 
  !! The END keyword is particularly prone to this.
  !!
  !! The type-guard-stmt and case-stmt keywords are handled separately. 
  !! (eg: "CASE DEFAULT construct_name" or "CLASS DEFAULT construct_name")
  CHARACTER(keyword_length), PARAMETER :: other_keywords(56) = [  &
      intrinsic_typespec_keywords,  &     ! six of these.
      !123456789012345    123456789012345    123456789012345
      ! Program unit type statements
      'PROGRAM        ', 'ENDPROGRAM     ', 'ENDFUNCTION    ',  &
      'ENDSUBROUTINE  ', 'ENDMODULE      ', 'BLOCKDATA      ',  &
      'ENDBLOCKDATA   ', 'ENDSUBMODULE   ',  &
      ! attributes
      'PUBLIC         ', 'PRIVATE        ', 'PROTECTED      ',  &
      'DIMENSION      ', 'CODIMENSION    ', 'ASYNCHRONOUS   ',  &
      'CONTIGUOUS     ', 'DATA           ', 'OPTIONAL       ',  &
      'POINTER        ', 'SAVE           ', 'TARGET         ',  &
      'VALUE          ', 'VOLATILE       ', 'EXTERNAL       ',  &
      'INTRINSIC      ',  &
      ! misc specification
      'PROCEDURE      ', 'INTERFACE      ', 'ENDINTERFACE   ',  &
      'USE            ', 'ABSTRACT       ',  &
      'IMPLICIT       ', 'COMMON         ', 'ENUMERATOR     ',  &
      'FINAL          ', 'IMPORT         ', 'TYPE           ',  &
      ! Executable constructs.  Most of these are here because they 
      ! can be followed with a construct-name.
      'ELSE           ', 'ELSEIF         ', 'ELSEWHERE      ',  &
      'ENDASSOCIATE   ', 'ENDBLOCK       ', 'ENDCRITICAL    ',  &
      'ENDDO          ', 'ENDENUM        ', 'ENDFORALL      ',  &
      'ENDIF          ', 'ENDPROCEDURE   ', 'ENDSELECT      ',  &
      'ENDTYPE        ', 'ENDWHERE       ',  &
      ! Appears pretty much everywhere.
      'END            ' ]
  
  !> For function-stmt's, a type-spec can also be a prefix keyword.
  CHARACTER(keyword_length), PARAMETER :: other_prefix_keywords(5) = [  &
      !123456789012345    123456789012345    123456789012345
      'PURE           ', 'ELEMENTAL      ', 'RECURSIVE      ',  &
      'IMPURE         ', 'MODULE         ' ]
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Construct a fixed form source object given a source of source.
  !!
  !! @param[in]     source_file       The source of source.
  !!
  !! @returns A correctly constructed FixedFormSource object.
  
  FUNCTION FixedFormSource_(source_file) RESULT(form)
    
    CLASS(Source), INTENT(IN), POINTER :: source_file
    
    ! Function result
    TYPE(FixedFormSource) :: form
    
    !***************************************************************************
    
    form%source_file => source_file
    form%pos = 0
    
  END FUNCTION FixedFormSource_
  
  
  !*****************************************************************************
  !!
  !> Implementation of FixedFormSource%Clone - create a copy of the source 
  !! form attached to a different source file.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[in]     source_file       An alternative source of source that 
  !! the clone (which isn't then a clone, but nevermind...) should be 
  !! connected to.
  !!
  !! @param[out]    new_form          The clone of @a form, but connected 
  !! to @a source_file instead of whatever @a form was connected to before.
  
  SUBROUTINE fixed_Clone(form, source_file, new_form)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FixedFormSource), INTENT(IN) :: form
    CLASS(Source), INTENT(IN), POINTER :: source_file
    CLASS(SourceForm), INTENT(OUT), ALLOCATABLE :: new_form
    
    !***************************************************************************
    
    ALLOCATE(new_form, SOURCE=FixedFormSource(source_file))
    
  END SUBROUTINE fixed_Clone
  
  
  !*****************************************************************************
  !!
  !> Implementation of FixedFormSource%GetSource - return the source object 
  !! that the source form is attached to.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[out]    source_file       The source of source that the source 
  !! form sources its source from.
  
  SUBROUTINE fixed_GetSource(form, source_file)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FixedFormSource), INTENT(IN) :: form
    CLASS(Source), INTENT(OUT), POINTER :: source_file
    
    !***************************************************************************
    
    source_file => form%source_file
    
  END SUBROUTINE fixed_GetSource
  
  
  !*****************************************************************************
  !!
  !> Get a statement from a fixed form file.
  !!
  !! @param[in,out] form              The source form.
  !!
  !! @param[in]     part_stack        Current syntax part stack prior to 
  !! the statement being retrieved.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    statement_tokens  The sequence of tokens for the next 
  !! statement in the file.
  !!
  !! @param[out]    statement_label   The statement label associated with 
  !! the next statement, or zero if the next statement had no label.
  !!
  !! In order to determine token boundaries we need to have some idea of 
  !! what possible statements we could be expecting - hence we rely upon 
  !! the @a part_stack argument.  This means that processing of fixed form has 
  !! some additional overhead (we almost classify each statement twice), 
  !! but if you are using fixed form source you deserve it.
  
  SUBROUTINE fixed_GetStatement( form, part_stack,  &
      err_list, statement_tokens, statement_label )
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE LabelsUtils
    USE MatchUtils
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FixedFormSource), INTENT(INOUT) :: form
    INTEGER, INTENT(IN) :: part_stack(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT), ALLOCATABLE :: statement_tokens(:)
    TYPE(Label), INTENT(OUT), ALLOCATABLE :: statement_label
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Size of the buffer initially allocated for a statement's worth of 
    ! tokens.  The buffer is grown as needed on an exponential basis from this 
    ! starting point.  It would be sane for this to be of the order of the 
    ! ninth decile of statement token lengths, or similar.
    INTEGER, PARAMETER :: initial_token_size = 10
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    LOGICAL :: eof            ! End of file flag.
    LOGICAL :: continued      ! Statement continuation flag.
    
    ! Label pulled from the start of a line of source.
    CHARACTER(5,KIND=scck) :: label_text
    
    INTEGER :: i1             ! Utility character index.
    INTEGER :: i2             ! Utility character index
    
    ! Temporary buffer used to assemble the list of tokens.
    TYPE(Token), ALLOCATABLE ::tmp_tokens(:)
    
    ! Number of elements in tmp_tokens that have been defined.
    INTEGER :: tmp_token_size
    
    ! Error list for child procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    INTEGER :: round_nest     ! Depth of nest of () pairs.
    
    LOGICAL :: seen_assign    ! Have we seen a bare '='?
    LOGICAL :: seen_comma     ! Have we seen a bare ','?
    LOGICAL :: seen_declare   ! Have we seen '::'?
    
    ! Is a real-literal-constant possible for the next token?
    LOGICAL :: real_possible
    
    TYPE(Token) :: the_token  ! The current token being retrieved.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! First call for the file - make sure we've got a line loaded.
    IF (form%pos == 0) THEN
      CALL get_next_line(form, eof, continued, sub_err_list)
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
      
      ! Test for empty file.
      IF (eof) RETURN
    END IF
    
    !---------------------------------------------------------------------------
    
    ! To cut down on realloc/move's a bit we use a "buffer" of tokens, 
    ! expanding the buffer substantially when its full (as opposed to 
    ! growing a vector of tokens that we grow incrementally each token).
    !
    ! In hindsight, this was probably pointless complexity.
    ALLOCATE(tmp_tokens(initial_token_size))
    tmp_token_size = 0        ! Initally the buffer is empty.
    
    IF (form%pos == 1) THEN
      ! Check for statement label
      i2 = 0
      DO i1 = 1, 5
        IF (.NOT. IsBlankChar(form%line(i1:i1))) THEN
          i2 = i2 + 1
          label_text(i2:i2) = form%line(i1:i1)
        END IF
      END DO
      IF (i2 /= 0) THEN
        ALLOCATE(statement_label)
        CALL ScanLabel(  &
            label_text(:i2),  &
            SourceLocationIfortBug(form%source_file, 1),  &
            statement_label,  &
            sub_err_list )
        CALL Add(err_list, sub_err_list)
      END IF
      form%pos = 7
    END IF
    
    ! Declarations of character variables with the deprecated form of the 
    ! length selector can confuse the parser: CHARACTER*80D0 could come 
    ! back as CHARACTER * 80D0 when we really want CHARACTER * 80 D0.  
    ! This also applies to function prefixes - CHARACTER*80ELEMENTAL could 
    ! be regarded as CHARACTER * 80E LEMENTAL, with the real literal missing 
    ! its exponent digits.
    !
    ! In other cases, the type-spec is always followed by some sort of 
    ! delimiter.
    !
    ! So we suppress the possibility of a real-literal until we are sure that 
    ! we are not in the relevant part of a function-stmt or 
    ! type-declaration-stmt
    
    round_nest = 0
    seen_declare = .FALSE.
    seen_assign = .FALSE.
    seen_comma = .FALSE.
    
    DO
      real_possible = (round_nest > 0) .OR. seen_declare .OR. seen_assign  &
          .OR. seen_comma
      CALL GetToken(form, real_possible, sub_err_list, the_token)
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
      
      IF (the_token == ittEndStatement) THEN
        statement_tokens = tmp_tokens(1:tmp_token_size)
        IF (tmp_token_size == 0) THEN
          IF (ALLOCATED(statement_label)) THEN
            ! Label without statement.  We assign the location of the error 
            ! to the first column of the statement label field.
            CALL Add( err_list, CODE=errLabelWithNoStatement,  &
                COMPONENT='3.2.5p2',  &
                LOCATION=SourceLocationIfortBug(form%source_file, label_cols(1)),  &
                MSG='A label appears without an associated statement.' )
          END IF
          ! End of file ends up here too.
          RETURN
        END IF
        ! Exit the token aquisition part to do token splitting.
        EXIT
      END IF
      
      ! real_possible logic.
      IF (IsToken(the_token, ittDelimiter, scck_'(')) THEN
        round_nest = round_nest + 1
      ELSE IF (IsToken(the_token, ittDelimiter, scck_')')) THEN
        round_nest = round_nest - 1
      ELSE IF (the_token == ittDeclare) THEN
        seen_declare = .TRUE.
      ELSE IF (round_nest == 0) THEN
        IF (the_token == ittComma) THEN
          seen_comma = .TRUE.
        ELSE IF (the_token == ittAssign) THEN
          seen_assign = .TRUE.
        END IF
      END IF
      
      ! Put the token in the list.
      IF (tmp_token_size == SIZE(tmp_tokens)) THEN
        ALLOCATE(statement_tokens(SIZE(tmp_tokens) * 2))
        statement_tokens(1:SIZE(tmp_tokens)) = tmp_tokens
        CALL MOVE_ALLOC(statement_tokens, tmp_tokens)
      END IF
      
      tmp_token_size = tmp_token_size + 1
      tmp_tokens(tmp_token_size) = the_token
      
    END DO
    
    !---------------------------------------------------------------------------
    ! One of the truely delightful things about fixed-form source is that 
    ! token "boundaries" depend on context and possibly also on syntax that 
    ! appears much later in the statement.  Oh joy.
    
    CALL do_keyword_splitting(statement_tokens, part_stack)
    
    !---------------------------------------------------------------------------
    ! Check for continued end statement.
    !
    ! Checks for a continued statement that appears to be a program unit 
    ! end statement may catch some of the cases of continued end statement, 
    ! but not all.
    !
    ! In particular the cases that will be missed are the ones where the 
    ! keywords are split.
    
    ! @todo Not s
    
  END SUBROUTINE fixed_GetStatement
  
  
  !*****************************************************************************
  !!
  !> Conduct keyword token splitting.
  !!
  !! @param[in,out] statement_tokens  The sequence of tokens that needs to 
  !! be checked and split if necessary.
  !!
  !! @param[in]     part_stack        Current syntax part stack prior to 
  !! the statement being retrieved.
  !!
  !! This has been split out from fixed_GetStatement because it has various 
  !! return points and there is common processing that we want to do after 
  !! splitting out keywords.
  
  SUBROUTINE do_keyword_splitting(statement_tokens, part_stack)
    
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: statement_tokens(:)
    INTEGER, INTENT(IN) :: part_stack(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: start          ! Starting token for keyword extraction.
    INTEGER :: pos            ! Character position in a token.
    
    !***************************************************************************
    
    start = 1
    
    ! Do we have a construct-name?  If so, then the statement must be a 
    ! construct type statement, unless the user is telling fibs.  If the user 
    ! is using fixed source form and they are also telling fibs, then we 
    ! shall have no mercy...
    IF (SIZE(statement_tokens) > 2) THEN
      IF ( (statement_tokens(1) == ittName)  &
          .AND. IsToken(statement_tokens(2), ittColon, scck_':') ) THEN
        ! Looks like we do!  For token splitting we skip it.
        CALL check_construct_keywords(statement_tokens, 3)
        RETURN
      END IF
    END IF
    
    ! The if-stmt could have bits that require splitting later in the token 
    ! sequence.  To qualify, must at least have IF ( condition ) blah
    !
    ! This test isn't required for FORALL and WHERE single statements, as 
    ! the child statement in both those cases must be an assignment statement.
    IF (SIZE(statement_tokens) > 4) THEN
      IF (IsKeyword(statement_tokens(1), scck_'IF')) THEN
        IF (IsToken(statement_tokens(2), ittDelimiter, scck_'(')) THEN
          start = FindClosingParens(statement_tokens(2:))
          IF (start /= 0) THEN
            start = start + 2   ! token immediately after ')'
            IF (SIZE(statement_tokens) >= start) THEN
              CALL check_action_keywords(statement_tokens, start)
            END IF
          END IF
        END IF
        RETURN
      ELSE IF (IsKeyword(statement_tokens(1), scck_'ELSEIF')) THEN
        IF (IsToken(statement_tokens(2), ittDelimiter, scck_'(')) THEN
          start = FindClosingParens(statement_tokens(2:))
          IF (start /= 0) THEN
            start = start + 2   ! token immediately after ')'
            IF (SIZE(statement_tokens) >= start) THEN
              ! To be fussy, we require that the THEN keyword be after 
              ! the closing parenthesis - if the user hasn't supplied that 
              ! then they will get a syntax error at some stage regardless 
              ! of the splitting that we do.  Here we are potentially 
              ! splitting off a construct name.
              pos = compare_keyword(statement_tokens(start), scck_'THEN') 
              IF (pos /= 0) THEN
                CALL split_token(statement_tokens, start, pos)
              END IF
            END IF
          END IF
        END IF
        RETURN
      END IF
    END IF
    
    ! We fell out of the IF test above - see if we are dealing with some 
    ! other sort of statement that requires keywordising of the starting 
    ! token.
    CALL check_all_keywords(statement_tokens, start, part_stack)
    
  END SUBROUTINE do_keyword_splitting
  
  
  !*****************************************************************************
  !!
  !> Conduct construct keyword splittage.
  !!
  !! @param[in,out] tlist             The sequence of tokens (thus far) 
  !! that we need to check.
  !!
  !! @param[in]     start             The index in @a tlist from which point 
  !! the checks shall begin (token after the construct ":", for example).
  !!
  !! Call for statements that start with a construct - this rules out a large 
  !! number of the otherwise fiddly keyword checks.
  
  SUBROUTINE check_construct_keywords(tlist, start)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    !***************************************************************************
    
    ! No need to check assignment statements and friends.
    IF (no_need_to_check(tlist, start)) RETURN
    
    ! Do the hard work.
    CALL do_check_construct_keywords(tlist, start)
    
  END SUBROUTINE check_construct_keywords
  
  
  !*****************************************************************************
  !!
  !> Worker routine for construct keyword splittage,  factored out so that it 
  !! can be part of all keyword checking and the construct keyword checking.
  !!
  !! @param[in,out] tlist             The sequence of tokens to check.
  !!
  !! @param[in]     start             The index in @a tlist from which point 
  !! the checks shall begin (token after the construct ":", for example).
  
  SUBROUTINE do_check_construct_keywords(tlist, start)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: pos            ! Character position in a token.
    INTEGER :: ik             ! Keyword array index.
    
    !***************************************************************************
    
    ! Single keyword choppage
    DO ik = 1, SIZE(construct_keywords)
      pos = compare_keyword(tlist(start), construct_keywords(ik))
      IF (pos /= 0) THEN
        CALL split_token(tlist, start, pos)
      END IF
    END DO
    
  END SUBROUTINE do_check_construct_keywords
  
  
  !*****************************************************************************
  !!
  !> Conduct action keyword splittage.
  !!
  !! @param[in,out] tlist             The sequence of tokens to check.
  !!
  !! @param[in]     start             Index in @a tlist at which to start the 
  !! checks (should the the index of the token following the closing 
  !! parenthesis of the IF (...) bit, for example).
  
  SUBROUTINE check_action_keywords(tlist, start)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    !***************************************************************************
    
    ! No need to check assignment statements and friends.
    IF (no_need_to_check(tlist ,start)) RETURN
    
    ! Do the hard work.
    CALL do_check_action_keywords(tlist, start)
    
  END SUBROUTINE check_action_keywords
  
  
  !*****************************************************************************
  !!
  !> Worker routine for action keyword splittage, factored out so that it 
  !! can be part of all keyword checking and the /if-stmt/'s /action-stmt/ 
  !! keyword checking.
  !!
  !! @param[in,out] tlist             The sequence of tokens to check.
  !!
  !! @param[in]     start             Index in @a tlist at which to start the 
  !! checks (should the the index of the token following the closing 
  !! parenthesis of the IF (...) bit).
  
  SUBROUTINE do_check_action_keywords(tlist, start)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: pos            ! Character position in a token.
    INTEGER :: ik             ! Keyword array index.
    
    !***************************************************************************
    
    ! Single keyword choppage
    DO ik = 1, SIZE(action_keywords)
      pos = compare_keyword(tlist(start), action_keywords(ik))
      IF (pos /= 0) THEN
        CALL split_token(tlist, start, pos)
      END IF
    END DO
    
  END SUBROUTINE do_check_action_keywords
  
  
  !*****************************************************************************
  !!
  !> Conduct all possible keyword splitting checks.
  !!
  !! @param[in,out] tlist             The sequence of tokens to check.
  !!
  !! @param[in]     start             Index in @a tlist at which to start the 
  !! checks (allows construct names to be skipped).
  !!
  !! @param[in]     part_stack        Current syntax part stack.
  
  SUBROUTINE check_all_keywords(tlist, start, part_stack)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: tlist(:)
    INTEGER, INTENT(IN) :: start
    INTEGER, INTENT(IN) :: part_stack(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it             ! Token index (into tlist).
    INTEGER :: ik             ! Keyword index (in one of our lists.
    INTEGER :: pos            ! Character position in a token.
    
    ! Flag to indicate that we are dealing with the first token in the 
    ! sequence (starting with start).
    INTEGER :: first
    
    !***************************************************************************
    
    ! Test for assignment statement, pointer statement or other sort of 
    ! statement that cannot contain splittable keywords.
    IF (no_need_to_check(tlist, start)) RETURN
    
    ! If we make it to here then the statement does not look like an 
    ! assignment-stmt or a pointer-assignment-stmt.  The first token 
    ! (at least) in the statement is therefore NOT a /name/ - it must be a 
    ! /keyword/.  Whether that keyword is followed by more keywords depends 
    ! on the particular type of statement.
    !
    ! In one case we need to worry about the context as well - it is 
    ! ambiguous whether the sequence of characters:
    !   MODULEPROCEDURENAME
    ! is the module-stmt at the start of a module, or whether it is a 
    ! procedure-stmt in an interface block or whether it is a 
    ! mp-subprogram-stmt in the module-subprogram-part of a module (the 
    ! bit after the CONTAINS).
    
    ! There are some special cases for the first keyword - we deal with them 
    ! here.
    
    pos = compare_keyword(tlist(start), scck_'MODULE')
    IF ((pos /= 0) .AND. (pos <= LEN(QueryValue(tlist(start))))) THEN
      CALL split_token(tlist, 1, pos)
      IF (stack_top_match(part_stack, iptInterfaceBlock)) THEN 
        pos = compare_keyword(tlist(2), scck_'PROCEDURE')
        IF (pos /= 0) THEN
          CALL split_token(tlist, 2, pos)
        END IF
        ! If it wasn't MODULE PROCEDURE, then it is a syntax error, but 
        ! we're done here for now.
        RETURN
      ELSE IF (stack_top_match(part_stack, iptModuleSubprogramPart)) THEN
        pos = compare_keyword(tlist(2), scck_'PROCEDURE')
        IF (pos /= 0) THEN
          CALL split_token(tlist, 2, pos)
          RETURN
        END IF
        ! Don't return here - it might be a MODULE FUNCTION etc - handled 
        ! below.
      ELSE 
        ! That's it - we just want the MODULE split out.
        RETURN
      END IF
    END IF
    
    ! Function and subroutine prefixen.
    !
    ! INTEGERFUNCTIONNAME(10) appears to be a function-stmt, but it is 
    ! actually a type-declaration-stmt.  To distinguish, if we see an 
    ! a declaration-type-spec at the start of the first token then we need to 
    ! see that there is a pair of parentheses later in the statement 
    ! (parentheses are required for function-stmt's) AND that the contents 
    ! of the parentheses is an identifier (not a integer-constant), BUT 
    ! we need to be mindful that the parentheses that enclose the function 
    ! argument list may not be the only parentheses on the line (earlier 
    ! sets could be type-parameter lists, later sets could be part of the 
    ! BIND suffix).
    !
    ! Source such as TYPE(xxx) PURE RECURSIVE FUNCTION requires splitting 
    ! after the parentheses.  This could also be INTEGER(kind) PURE FUNCTION, 
    ! etc.
    !
    ! Limit our search to the places that you can find function-stmt's and 
    ! subroutine-stmt's and mp-subprogram-stmt's (the statements that have 
    ! prefixes).  This is just an optimisation.
    IF ( stack_top_match(part_stack, iptProgram)  &
        .OR. stack_top_match(part_stack, iptInterfaceBlock)  &
        .OR. stack_top_match(part_stack, iptModuleSubprogramPart)  &
        .OR. stack_top_match(part_stack, iptInternalSubprogramPart) ) THEN
      it = start - 1
      first = 2
      token_loop: DO
        it = it + 1
        IF (first > 0) first = first - 1
        IF (it > SIZE(tlist)) EXIT token_loop
        IF (tlist(it) /= ittName) EXIT token_loop
        
        DO ik = 1, SIZE(intrinsic_typespec_keywords)
          pos = compare_keyword(tlist(it), intrinsic_typespec_keywords(ik))
          IF (pos /= 0) THEN
            ! We'd get here with INTEGER(xx) or INTEGERPURExxx
            IF (pos <= LEN(QueryValue(tlist(it)))) THEN
              ! Still "in" text - no parens.
              CALL split_token(tlist, it, pos)
            ELSE
              ! Does nothing if there are no parens.  If the text was 
              ! DOUBLEPRECISION and the user accidentally provided parens, 
              ! then this will skip them, but later parsing will error.
              CALL advance_over_parens(tlist, it)
            END IF
            
            IF (first > 0) THEN
              ! We need to decide now whether this is a function-stmt or a 
              ! type-declaration-stmt.  If it is a function-stmt - then cycle 
              ! the loop to consume more prefixes.  If it isn't, then return 
              ! to avoid incorrect splitting.
              IF (.NOT. stack_top_match(part_stack, iptProgram))  &
                  CYCLE token_loop
              IF (is_function_stmt(tlist, it)) CYCLE token_loop
              ! Looks like a type-declaration - bail.
              RETURN
            END IF
          END IF
        END DO
        
        pos = compare_keyword(tlist(it), scck_'TYPE')
        IF (pos == 0) pos = compare_keyword(tlist(it), scck_'CLASS')
        IF (pos /= 0) THEN
          ! To be the keyword in a type-specifier of some sort, TYPE or CLASS 
          ! must be the end of the text and be followed by parens.  
          IF (pos /= LEN(QueryValue(tlist(it)))) EXIT token_loop
              
          ! If there's a problem with the parens, then it isn't changed.  
          ! After the CYCLE it will be incremented to point to the troublesome 
          ! token and the lop will exit.
          CALL advance_over_parens(tlist, it)
          ! type-declaration-stmt vs function-stmt check.  See comments for 
          ! intrinsic-type-spec's above.
          IF (first > 0) THEN
            IF (.NOT. stack_top_match(part_stack, iptProgram))  &
                CYCLE token_loop
            IF (is_function_stmt(tlist, it)) CYCLE token_loop
            ! Looks like a type-declaration - bail.
            RETURN
          END IF
        END IF
        
        DO ik = 1, SIZE(other_prefix_keywords)
          pos = compare_keyword(tlist(it), other_prefix_keywords(ik))
          IF (pos /= 0) THEN
            IF (pos <= LEN(QueryValue(tlist(it)))) THEN
              CALL split_token(tlist, it, pos)
              CYCLE token_loop
            ELSE
              ! Something like PURE(..., which will be a syntax error.
              RETURN
            END IF
          END IF
        END DO
        
        ! Any text after the next three options is the name of the procedure.
        
        pos = compare_keyword(tlist(it), scck_'FUNCTION')
        IF (pos /= 0) THEN
          CALL split_token(tlist, it, pos)
          RETURN    ! We're done.
        END IF
        
        pos = compare_keyword(tlist(it), scck_'SUBROUTINE')
        IF (pos /= 0) THEN
          CALL split_token(tlist, it, pos)
          RETURN    ! We're done.
        END IF
        
        pos = compare_keyword(tlist(it), scck_'PROCEDURE')
        IF (pos /= 0) THEN
          CALL split_token(tlist, it, pos)
          RETURN    ! We're done.
        END IF
        
        ! IF we made it to here we didn't get to a function/subroutine/ 
        ! procedure keyword and we've got some sort of unknown "keyword".
        
        ! In the context of "this might be the first statement of the 
        ! program", this could be any old statement, so fall through to 
        ! the more general processing below.
        !
        ! Other possibilities include an interface specification (inside 
        ! an interface block) that is not an interface body or that is 
        ! the END INTERFACE statement.
        !
        ! Or we might be confused.  Whatever - jump out to general 
        ! processing.
        EXIT
        
      END DO token_loop
      
    END IF    ! contexts for function-stmt's, etal.
      
    ! If we make it to here, then we are not dealing with a function-stmt, 
    ! subroutine-stmt's or mp-subprogram-stmt (and the associated keywords).  
    ! The list of keywords in other_keywords reflects that.
    
    ! Some other special cases where we might have a construct name 
    ! following two keywords in a row.
    IF (compare_keyword(tlist(start), scck_'CASEDEFAULT') /= 0) THEN
      CALL split_token(tlist, start,     5)   ! Chop out CASE
      CALL split_token(tlist, start + 1, 8)   ! Chop out DEFAULT
      RETURN
    END IF
    
    IF (compare_keyword(tlist(start), scck_'CLASSDEFAULT') /= 0) THEN
      CALL split_token(tlist, start,     6)   ! Chop out CLASS
      CALL split_token(tlist, start + 1, 8)   ! Chop out DEFAULT
      RETURN
    END IF
    
    IF (tlist(start) == scck_'CLASSIS') THEN
      CALL split_token(tlist, start,     6)   ! Chop out CLASS
      RETURN
    ELSE IF (tlist(start) == scck_'TYPEIS') THEN
      CALL split_token(tlist, start,     5)   ! Chop out TYPE
      RETURN
    END IF
    
    CALL do_check_action_keywords(tlist, start)
    CALL do_check_construct_keywords(tlist, start)
    
    ! Single keyword choppage
    DO ik = 1, SIZE(other_keywords)
      pos = compare_keyword(tlist(start), other_keywords(ik))
      IF (pos /= 0) THEN
        CALL split_token(tlist, start, pos)
      END IF
    END DO
    
  END SUBROUTINE check_all_keywords
  
  
  !*****************************************************************************
  !!
  !> Test a sequence of tokens to see whether it looks like a statement that 
  !! cannot (legally) contain keywords that need splitting.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in]     start             The index in tlist at which to start 
  !! checking.
  !!
  !! @returns .TRUE. if the token sequence looks like one of the following 
  !! types of statement (.FALSE. otherwise):
  !! - an /assignment-stmt/ or /pointer-assignment-stmt/
  !!
  !! Be aware of the double negative - a return value of .TRUE. means that 
  !! there is no need to check for keywords that need splitting.
  !!
  !! The list used to be longer, but /procedure-stmt/ caught us out :(.
  
  FUNCTION no_need_to_check(tlist, start) RESULT(b)
    
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(IN) :: start
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    !---------------------------------------------------------------------------
    ! Some sanity checks first
    
    b = .TRUE.
    
    ! Probable syntax error that will be processed elsewhere.
    IF (tlist(start) /= ittName) RETURN
    
    ! A zero length statement doesn't require token splitting.
    IF (SIZE(tlist(start:)) == 0) RETURN
    
    !---------------------------------------------------------------------------
    
    ! Previously we used to check for the presence of a bare :: and eliminated 
    ! a whole heap of other statements that don't need splitting.  But 
    ! a /procedure-stmt/ can start with MODULEPROCEDURE, so we've had to 
    ! remove that shortcut.  Now we just use the presence of a declare to 
    ! shortcut the other way.
    b = .NOT. HasBare(tlist(start:), ittDeclare, scck_'::')
    IF (.NOT. b) RETURN  ! If it has a bare "::", return .FALSE.
    
    ! We want to filter out assignment-like statements, but we need to 
    ! filter out the loop-control of a DO statement first.  This also 
    ! triggers on type-declaration and attribute statements with more than 
    ! one variable being declared/attributed that haven't already be detected.
    b = .NOT. HasBare(tlist(start:), ittComma, scck_',')
    IF (.NOT. b) RETURN  ! If it has a bare ",", return .FALSE.
    
    ! "=" or "=>" means that it looks like an /assignment-stmt/ or a 
    ! /pointer-assignment-stmt/, in which case there's no need for special 
    ! processing of tokens.
    b = ( HasBare(tlist(start:), ittAssign, scck_'=')  &
        .OR. HasBare(tlist(start:), ittAssociate, scck_'=>') )
    IF (b) RETURN
    
    ! Anything else requires token split checks.
    b = .FALSE.
    
  END FUNCTION no_need_to_check
  
  
  !*****************************************************************************
  !!
  !> Tests to see whether the token list could represent a function-stmt 
  !! or a type-declaration-stmt.
  !!
  !! Call after identifying an intrinsic type name or type statement at the 
  !! start of the statement AND when the context is ambiguous (iptProgram).
  !!
  !! @param[in]     tlist             The sequence of tokens, so far.
  !!
  !! @param[in]     it                Index in tlist of the token that 
  !! could possibly contain the FUNCTION keyword.  This is the token after the 
  !! closing parentheses on any type parameters for the intrinsic type spec or 
  !! derived type spec.
  
  FUNCTION is_function_stmt(tlist, it) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(INOUT) :: it
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    ! Assume type-declaration-stmt
    b = .FALSE.
    
    ! function-stmt's must have parentheses.
    IF (it + 1 > SIZE(tlist)) RETURN
    IF (.NOT. IsToken(tlist(it+1), ittDelimiter, scck_'(')) RETURN
    ! function-stmt must have a name as keyword.
    IF (tlist(it+1) /= ittName) RETURN 
    
    b = .TRUE.
    
  END FUNCTION is_function_stmt
  
  
  !*****************************************************************************
  !!
  !> Advance a token sequence index over a set of opening and closing 
  !! parentheses.
  !!
  !! @param[in]     tlist             The sequence of tokens.
  !!
  !! @param[in,out] it                The index into the sequence.
  !!
  !! On input, @a it should be the index prior to any opening parens.  If 
  !! the next token is not an opening parens, then this call does nothing.  
  !! If there is no closing parens, this call does nothing.  Otherwise 
  !! @a it becomes the index of the closing parens.
  
  SUBROUTINE advance_over_parens(tlist, it)
    
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(INOUT) :: it
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: finish         ! Index in sub-sequence of closing parenthesis.
    
    !***************************************************************************
    
    IF (it + 1 <= SIZE(tlist)) THEN
      IF (IsToken(tlist(it+1), ittDelimiter, scck_'(')) THEN
        finish = FindClosingParens(tlist(it+1:))
        IF (finish == 0) RETURN
        it = it + finish
      END IF
    END IF
    
  END SUBROUTINE advance_over_parens
  
  
  !*****************************************************************************
  !!
  !> Split a token in the token list into two tokens.
  !!
  !! @param[in,out] tlist             The token list.  This will come back 
  !! one token bigger.
  !!
  !! @param[in]     idx               The index of the token to split.
  !!
  !! @param[in]     pos               The character position to be the start 
  !! of the new (second) token.
  !!
  !! If pos is already off the end of the token, then no action is taken.
  
  SUBROUTINE split_token(tlist, idx, pos)
    
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT), ALLOCATABLE :: tlist(:)
    INTEGER, INTENT(IN) :: idx
    INTEGER, INTENT(IN) :: pos
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the list of tokens.
    TYPE(Token), ALLOCATABLE :: tmp(:)
    
    !***************************************************************************
    
    ! Make sure we are actually splitting.
    IF (pos > LEN(QueryValue(tlist(idx)))) RETURN
    
    ! Make space for the new token.
    ALLOCATE(tmp(SIZE(tlist)+1))
    
    ! Copy tokens up to and including the one to be split over.
    tmp(:idx) = tlist(:idx)
    
    ! Split the token
    CALL Split(tmp(idx), pos, tmp(idx+1))
    
    ! Copy the remaining tokens over.
    tmp(idx+2:) = tlist(idx+1:)
    
    ! Transfer the temporary allocation across.
    CALL MOVE_ALLOC(tmp, tlist)
    
  END SUBROUTINE split_token
  
  
  !*****************************************************************************
  !!
  !> Compare the characters at the start of the value of a token with a 
  !! keyword and if there's a match, return the character position one after 
  !! the end of the match.
  !!
  !! @param[in]     tkn               The token whose value should be tested.  
  !! This should be an ittName token, or much strangeness will happen.
  !!
  !! @param[in]     keyword           The keyword to test @a tkn against.  
  !! Trailing blanks are not significant.  This should be upper case (the 
  !! value of a name token is already upper case).
  !!
  !! @returns The position of the character immediately after the keyword 
  !! if a match is found, or zero if no match is found.
  
  FUNCTION compare_keyword(tkn, keyword) RESULT(it)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: tkn
    CHARACTER(*, KIND=scck), INTENT(IN) :: keyword
    
    ! Function result
    INTEGER :: it
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: l              ! Significant length of the keyword.
    
    ! Value of the token.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: value
    
    !***************************************************************************
    
    value = QueryValue(tkn)
    l = LEN_TRIM(keyword)
    IF (l > LEN_TRIM(value)) THEN
      it = 0
    ELSE IF (value(:l) == keyword(:l)) THEN
      it = l + 1
    ELSE
      it = 0
    END IF
    
  END FUNCTION compare_keyword
  
  
  !*****************************************************************************
  !!
  !> Implementation of FixedFormSource%GetChar - get the next character from 
  !! a fixed form source file.
  !!
  !! @param[in,out] form              The source form for the source file 
  !! being read.
  !!
  !! @param[in]     in_character_context  Flag to indicate whether token 
  !! processing is currently inside a character literal (.TRUE.) or 
  !! in ordinary code (.FALSE.).  Source forms can use this flag 
  !! to change the meaning of some characters.
  !!
  !! @param[in]     context_chars     Defined only if @a in_char_literal is 
  !! .TRUE., in which case if this is non-zero length it contains the only 
  !! characters that would still be considered part of the character 
  !! context.
  !!
  !! @param[out]    ch                The next source character in the 
  !! file.  This includes information about end of line and end of 
  !! statement status.  See comments for the SourceChar type for more 
  !! information.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! Typically, outside of a character context this procedure never returns 
  !! a blank.  An exception is made when the blank immediately follows a 
  !! character context.
  
  SUBROUTINE fixed_GetChar( form, in_character_context, context_chars, ch,  &
      err_list )
    
    USE CharacterTypes
    USE Errors
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FixedFormSource), INTENT(INOUT) :: form
    LOGICAL, INTENT(IN) :: in_character_context
    CHARACTER(*), INTENT(IN) :: context_chars
    TYPE(SourceChar), INTENT(OUT) :: ch
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Flag to indicate that the new line is a continuation line.
    LOGICAL :: continued
    
    ! Indicates whether we are in a character context, updated given the 
    ! current character.
    LOGICAL :: local_in_character_context
    
    ! Flag for whether we have hit the end of a line.
    LOGICAL :: local_end_of_line
    
    ! Error list for child calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    ! Used to save the old (existing) line of text
    CHARACTER(line_len) :: old_line
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
      
    ch%got_amp = .TRUE.   ! Always for fixed form source.
    
    ! Check if this is the first time we've been called for this instance.
    !
    ! Ordinarily this will have been covered by the same code in the 
    ! opening part of the GetStatement binding.  It is here as well because 
    ! it was here first and in case the GetChar binding is called outside of 
    ! GetStatement.
    IF (form%pos == 0) THEN
      CALL get_next_line(form, ch%eof, continued, sub_err_list)
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
      
      ! Test for empty file.
      IF (ch%eof) THEN
        ch%eof = .TRUE.
        ch%eos = .TRUE.
        ch%char = ''
        RETURN
      END IF
    END IF
    
    !---------------------------------------------------------------------------
    
    ! Depending on the first character we look at, we might override the 
    ! in_character_context flag.
    local_in_character_context = in_character_context
    
    ! Loop until we get a character or we decide that there are no more 
    ! characters to get.
    DO
      
      ! Determine whether we are at the end of the line, considering 
      ! comments as line terminators and/or in a character context.
      !
      ! Set local_end_of_line to indicate that we need to get a new 
      ! non-comment line before we examine characters.
      
      ! Are we at the physical end of line?
      IF (form%pos > LEN(form%line)) THEN
        local_end_of_line = .TRUE.
        ! we leave local_character_context untouched.
        
      ! Are we in a character context?  If we are in a character context, 
      ! then we cannot be looking at the start of a comment.  Note that 
      ! if local_character_context is false then the test always returns 
      ! false.
      ELSE IF (TestCharacterContext( local_in_character_context,  &
          context_chars, form%line(form%pos:form%pos) )) THEN
        local_end_of_line = .FALSE.
        ! local_in_character_context will be true.  We will definitely 
        ! get a character this pass through.
        
      ! Are we looking at a comment character?
      ELSE IF (form%line(form%pos:form%pos) == comment_char) THEN
        ! There are no more statement characters available on the current line.
        local_end_of_line = .TRUE.
        ! We can only be encountering comment characters if we are not 
        ! in a character context.
        local_in_character_context = .FALSE.
        
      ELSE
        local_end_of_line = .FALSE.
        local_in_character_context = .FALSE.
      END IF
      
      ! Check whether we need to get another line.
      IF (local_end_of_line) THEN
        ! Save the position and the previous line.  If the next line is a 
        ! continuation line then we check the previous line to make sure that 
        ! the partial statement at the end didn't look like a program unit 
        ! end statement.
        ch%pos = SourceLocationIfortBug(form%source_file, form%pos)
        old_line = form%line
        
        ! Get the next line (into form%line) and also see if it is a 
        ! continuation line.
        CALL get_next_line(form, ch%eof, continued, sub_err_list)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        ! On eof continued is also set to false.
        IF (.NOT. continued) THEN
          ch%eos = .TRUE.
          RETURN
        END IF
        
        ! If the previous line was an initial line, then it may be worthwhile
        ! examining whether the partial statement on the line "appear" to be 
        ! a program unit end statement (which is probibited).  The partial 
        ! statement will definitely not look like a program unit end 
        ! statement if we have ended up in a character context.
        IF ( .NOT. in_character_context  &
            .AND. (.NOT. is_continuation_line(old_line)) ) THEN
          ! Did the previous line end with something that "appears" like a 
          ! program unit end statement?
          CALL check_appears_to_be_program_unit_end( old_line, ch%pos,  &
              sub_err_list )
          CALL Add(err_list, sub_err_list)
          ! The above call should just return a warning.  If we ever change 
          ! it to return an error then we need to return here, otherwise 
          ! we end up consuming characters from the next line.
          IF (Failure(err_list)) RETURN
        END IF
        
        ! Retest the character context.  If we already decided above that 
        ! we were not in a character context because we hit a comment, 
        ! then this test will return false because in_local_character_context 
        ! is already false, in which case the test returns false.
        !
        ! If we definitely are in a character context, then context_chars 
        ! will be zero length and local_inc_character_context will be 
        ! true, in which case the test returns true.
        !
        ! This test is really for... the last call to GetChar returned what 
        ! might be the closing quote of a literal, we needed to look at 
        ! the next character to see if the literal really had finished but 
        ! we hit the very end of the line, so we have had to load a new line, 
        ! make sure it was a continuation line and then check its first 
        ! character.
        local_in_character_context = TestCharacterContext(  &
            local_in_character_context, context_chars,  &
            form%line(form%pos:form%pos) )
      END IF
      
      ! If we make it through the test_eol test and the subsequent branch, 
      ! then form%pos will be in the range of form%line (it may have been 
      ! moved forward to the start of a continuation line).
      
      ! If we are in a character literal, return what we have
      IF (local_in_character_context) THEN
        ch%pos = SourceLocationIfortBug(form%source_file, form%pos)
        ch%char = form%line(form%pos:form%pos)
        ch%eos = .FALSE.
        form%pos = form%pos + 1
        RETURN
        
      ! If we were potentially in a character context, but didn't get a 
      ! character that continued the context (context_chars was not zero 
      ! length and our character wasn't in context_chars) then we return 
      ! whatever in-statement character we have, even if it is a blank.  This 
      ! stops silliness such as 'Hello' 'world' getting parsed as 
      ! 'Hello''world'.
      ELSE IF ( in_character_context .OR.  &
          .NOT. IsBlankChar(form%line(form%pos:form%pos)) ) THEN
        ! If we are not in a character literal, return any character that is 
        ! not a semicolon.  A semicolon gets transformed to end of statement.
        ch%pos = SourceLocationIfortBug(form%source_file, form%pos)
        ch%char = form%line(form%pos:form%pos)
        ! The ; character means end of statement.
        ch%eos = ch%char == eos_char
        IF (ch%eos) THEN
          ! If we did hit ';', then we want to skip forward to the next 
          ! in-statement character.  This call skips both ';' and blanks.
          CALL skip_semicolons(form%line, form%pos)
          ! If the above has skipped through to the end of the line or comment 
          ! then we will be "tricked" into returning another EOS indicator 
          ! when next called.  To avoid that, if we have hit the end of the 
          ! line then reposition ourselves at the start of the next line.
          !
          ! We don;t have to check that there aren't ';' characters starting 
          ! the next line because that is illegal and checked as part of 
          ! get_next_line.
          !
          ! We don't have to worry about the character context stuff here, 
          ! because we have tested that we were not in a character context.
          IF (form%pos > LEN(form%line)) THEN
            local_end_of_line = .TRUE.
          ELSE IF (form%line(form%pos:form%pos) == comment_char) THEN
            local_end_of_line = .TRUE.
          ELSE
            local_end_of_line = .FALSE.
          END IF
          IF (local_end_of_line) THEN
            CALL get_next_line(form, ch%eof, continued, sub_err_list)
            CALL Add(err_list, sub_err_list)
            ! We don't need to check for continuation, as the ; means we are 
            ! definitely at an end of statement.
          END IF
        ELSE
          form%pos = form%pos + 1
        END IF
        RETURN
      END IF
      
      ! Got a blank.  Skip it, try again.  Note that we never get here 
      ! if we are in a character context - local_in_character_context 
      ! is false so the character context related tests at the top of the 
      ! loop will all just say "not in a character context".
      form%pos = form%pos + 1
    END DO
    
  END SUBROUTINE fixed_GetChar
  
  
  !*****************************************************************************
  !!
  !> Test whether characters at the end of the previous line appear like 
  !! a program unit end statment.
  !!
  !! @param[in]     line              The line to be tested.  This must be 
  !! an initial line that had a continuation line following.
  !!
  !! @param[in]     pos               Source location used for error reporting.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! Appearing like a program unit end statement is a superset of IS the 
  !! program unit end statement.  In this way we are also testing that 
  !! *the* program unit end statement is not being continued, not that we 
  !! have any idea of what that program unit end statement might be at this 
  !! point.
  
  SUBROUTINE check_appears_to_be_program_unit_end(line, pos, err_list)
    
    USE CharUtils
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*,KIND=scck), INTENT(IN) :: line
    TYPE(SourceLocation), INTENT(IN) :: pos
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! A buffer to store the characters from the statement at the end of the 
    ! previous line without intervening blanks.
    CHARACTER(line_len - continue_col,KIND=scck) :: buffer
    
    ! Index of the last character placed into buffer.  We start at the end 
    ! and work towards index one.
    INTEGER :: ib
    
    ! Index of the character in line being considered.
    INTEGER :: il
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Build the buffer of the initial line from the previous statement.
    
    ! No really necessary as we don't reference undefined characters in 
    ! buffer, but this makes debugging easier.
    buffer = ''
    ib = LEN(buffer) + 1
    
    ! If we come across anything that tells use this is not a program unit 
    ! end statement (any non-keyword or name characters) then we just return.
    DO il = LEN(line), continue_col + 1, -1
      ! Skip over blanks.
      IF (IsBlankChar(line(il:il))) CYCLE
      ! End-of-the-previous statement?  We are done populating the buffer.
      IF (line(il:il) == eos_char) EXIT
      ! Not a name character?  Then not an end statement.
      IF (.NOT. IsNameChar(line(il:il))) RETURN
      ! Store this character.
      ib = ib - 1
      buffer(ib:ib) = line(il:il)
    END DO
    
    !---------------------------------------------------------------------------
    ! Examine the buffer from ib on to see if it looks like a program unit 
    ! end statement.
    
    ! Need to have at least three characters.
    IF (LEN(buffer) - ib + 1 < 3) RETURN
    
    ! Upper case the buffer to make comparisons easier.
    CALL UpperCase(buffer(ib:))
    
    ! Needs to start with END
    IF (INDEX(buffer(ib:), scck_'END') /= 1) RETURN
    
    ! Needs to either be END or start with one of the following.
    IF ( (LEN(buffer) - ib + 1 == 3)  &
        .OR. (INDEX(buffer(ib:), 'ENDBLOCKDATA') == 1)  &
        .OR. (INDEX(buffer(ib:), 'ENDFUNCTION') == 1)  &
        .OR. (INDEX(buffer(ib:), 'ENDMODULE') == 1)  &
        .OR. (INDEX(buffer(ib:), 'ENDPROGRAM') == 1)  &
        .OR. (INDEX(buffer(ib:), 'ENDSUBMODULE') == 1)  &
        .OR. (INDEX(buffer(ib:), 'ENDSUBROUTINE') == 1) ) THEN
      CALL Add( err_list, CODE=errContinuedAppearsLikeEnd,  &
          COMPONENT='3.3.3.5', LOCATION=pos, LEVEL=errLevelWarning,  &
          MSG='The initial line of a continued statement appears to be a &
            &program unit end statement.' )
    END IF
    
  END SUBROUTINE check_appears_to_be_program_unit_end
  
  
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
  !> Implementation of FixedFormSource%SplitCharLiteralError and 
  !! FixedFormSource%SplitTokenError - issue an error message when a 
  !! split token is detected.
  !!
  !! @param[in]     form              The fixed source form object.
  !!
  !! @param[in]     initial_chars     The token character sequence as it 
  !! existed prior to the split.
  !!
  !! @param[in]     next_ch           The first character on the next 
  !! line.
  !!
  !! @param[out]    err_list          List of errors, presumably including 
  !! one about a split token.  Note that this is declared INTENT(INOUT).
  !!
  !! Fixed form source doesn't have restrictions about token splitting.
  !!
  !! Because every character that comes back from FixedFormSource%GetChar 
  !! has the got_amp component set, the Scanner should never ask us to 
  !! check or generate an error.
  
  SUBROUTINE fixed_SplitTokenError(form, initial_chars, next_ch, err_list)
    
    USE Errors
    USE ErrorCodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FixedFormSource), INTENT(IN) :: form
    TYPE(SourceChar), INTENT(IN) :: initial_chars(:)
    TYPE(SourceChar), INTENT(IN) :: next_ch
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! Fixed form source doesn't care about split tokens.
    STOP 'Internal error in ScanFixedForm%%fixed_SplitTokenError, &
        &this procedure should never be called!'
    
  END SUBROUTINE fixed_SplitTokenError
  
  
  !*****************************************************************************
  !!
  !> Implementation of FixedFormSource%CheckTokenSeparation - check that 
  !! the necessary separators exist between tokens.  In the case 
  !! of fixed form there are no requirements - so this procedure does nothing.
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
  
  SUBROUTINE fixed_CheckTokenSeparation(form, the_token, next_ch, err_list)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FixedFormSource), INTENT(IN) :: form
    TYPE(Token), INTENT(IN) :: the_token
    TYPE(SourceChar), INTENT(IN) :: next_ch
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! Fixed form doesn't care about token separation.
    
  END SUBROUTINE fixed_CheckTokenSeparation
  
  
  !*****************************************************************************
  !!
  !> Gets the next non-comment line into the source form's line buffer.
  !!
  !! @param[in,out] form              The source file to read from.
  !!
  !! @param[out]    eof               End of file flag.  Set if there was 
  !! no next non-comment line in the file.
  !!
  !! @param[out]    continued         Continuation flag - set if the next 
  !! line was a continuation line.  Set to false on end of file or if the 
  !! next line was an initial line.
  !!
  !! @param[out]    err_list          List of errors.  End of file is not 
  !! an error.
  !!
  !! Call when the line position gets to 72 or end of line, whichever comes 
  !! first.
  !!
  !! @a pos == 7 - the statement was continued.  @a pos == 1, it wasn't!.
  
  SUBROUTINE get_next_line(form, eof, continued, err_list)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FixedFormSource), INTENT(INOUT) :: form
    LOGICAL, INTENT(OUT) :: eof
    LOGICAL, INTENT(OUT) :: continued
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Error object for procedure calls.
    TYPE(Error) :: err
    
    ! Temporary copy of the line, as supplied by the source object.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: tmp_line
    
    ! Local line position.  Don't confuse this with form%pos.
    INTEGER :: pos
    
    !***************************************************************************    
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    DO
      ! End of line reached with no tokens.  Get another line.
      CALL form%source_file%ReadLine(tmp_line, err)
      CALL Add(err_list, err)
      IF (Failure(err)) RETURN
      ! End of file indication.
      IF (.NOT. ALLOCATED(tmp_line)) THEN
        continued = .FALSE.
        eof = .TRUE.
        RETURN
      END IF
      
      CALL check_line_length(form%source_file, tmp_line, err)
      CALL Add(err_list, err)
      
      form%line = tmp_line
      
      pos = VERIFY(form%line, ' ')
      
      ! Check for comment lines
      IF (pos == 0) CYCLE     ! all blanks
      ! !, * or C in column 1.
      IF (SCAN(form%line(comment_col:comment_col), line_comment_chars) /= 0)  &
          CYCLE
      ! First character not in column six is a !
      IF ((form%line(pos:pos) == comment_char) .AND. (pos /= continue_col))  &
          CYCLE
      
      ! Got something.
      EXIT
      
    END DO
    
    eof = .FALSE.
    IF (is_continuation_line(form%line)) THEN
      IF (form%pos == 0) THEN
        ! First time that a line has been retrieved by this source form - and 
        ! it is a continuation line!  We set the error location to be the 
        ! continuation character column.
        CALL Add( err_list,  &
            LEVEL=errLevelWarning,  &
            CODE=errFirstLineContinued,  &
            COMPONENT='3.3.3.3/3.4 (7)',  &
            LOCATION=SourceLocationIfortBug(form%source_file, continue_col),  &
            MSG='The first line of the source file was marked as a &
              &continuation line.' )
        form%continuations = 0
      ELSE
        form%continuations = form%continuations + 1
        ! Check the number of continuations - just for standard compliance.
        IF (form%continuations == 256) THEN
          ! We set the error location to be the continuation character 
          ! column.
          CALL Add( err_list,  &
              CODE=errTooManyContinuationLines,  &
              LEVEL=errLevelWarning,  &
              COMPONENT='3.3.3.5p1',  & 
              LOCATION=SourceLocationIfortBug(form%source_file, continue_col),  &
              MSG='A statement shall not have more than 255 continuation &
                &lines.' )
        END IF
      END IF
      
      form%pos = 7
      continued = .TRUE.
      ! Check that the label field on continued lines doesn't contain 
      ! garbage.  We set the error location to be the first column of 
      ! the statement label field, though that may not be where the 
      ! garbage was found (it will be close).
      IF (VERIFY(form%line(label_cols(1):label_cols(2)), scck_' ') /= 0) THEN 
        CALL Add( err_list,  &
            CODE=errSyntax,  &
            COMPONENT='3.3.3.5p1',  &
            LOCATION=SourceLocationIfortBug(form%source_file, label_cols(1)),  &
            MSG='Non-blank characters in the statement label field of a &
              &continued line.' )
        RETURN
      END IF
    ELSE
      form%continuations = 0
      form%pos = 1
      continued = .FALSE.
      ! First non-blank character must not be ;
      pos = 7
      DO pos = 7, LEN(form%line)
        IF (form%line(pos:pos) == eos_char) THEN  
          CALL Add( err_list,  &
              CODE=errEmptyStatement,  &
              COMPONENT='3.3.3.4p2',  &
              LOCATION=SourceLocationIfortBug(form%source_file, pos),  &
              MSG='A ";" shall not appear as the first nonblank character &
                &on an initial line.' )
          RETURN
        ELSE IF (.NOT. IsBlankChar(form%line(pos:pos))) THEN
          EXIT
        END IF
      END DO
    END IF
    
  END SUBROUTINE get_next_line
  
  
  !*****************************************************************************
  !!
  !> Test whether the top of the syntax part stack matches a particular syntax 
  !! part.
  !!
  !! @param[in]     stack             The syntax part stack.
  !!
  !! @param[in]     ipt               The part to check.
  !!
  !! @returns .TRUE. if the top of the part stack is @a ipt, .FALSE. otherwise 
  !! (including if the part stack is empty).
  
  FUNCTION stack_top_match(stack, ipt) RESULT(b)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: stack(:)
    INTEGER, INTENT(IN) :: ipt
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (SIZE(stack) > 1) THEN
      b = stack(SIZE(stack)) == ipt
    ELSE
      ! iptProgram implicit in an empty stack.
      b = ipt == iptProgram
    END IF
    
  END FUNCTION stack_top_match
  
  
  !*****************************************************************************
  !!
  !> Test a line of source for a continuation character.
  !!
  !! @param[in]     line              The line of source.
  !!
  !! @returns true of the line is a continuation line, false otherwise.
  
  FUNCTION is_continuation_line(line)
    
    USE CharacterTypes
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*,KIND=scck), INTENT(IN) :: line
    
    ! Function result.
    LOGICAL :: is_continuation_line
    
    !***************************************************************************
    
    is_continuation_line = .NOT. (  &
        IsBlankChar(line(continue_col:continue_col))  &
          .OR. (line(continue_col:continue_col) == scck_'0') ) 
        
  END FUNCTION is_continuation_line
  
  
  !*****************************************************************************
  !!
  !> Check that the length of a line fits within the fixed form source limits.
  !!
  !! @param[in]     line              The line to check.
  !!
  !! @param[out]    err               Error object.
  !!
  !! There are two possible warnings returned in @a err.  errLineTooLong 
  !! is returned if there are non-blank characters past position 72, 
  !! otherwise errLineWithBlanksTooLong is returned if there are blank 
  !! characters past position 72.
  !!
  !! If line is less than 72 characters in length then no error is returned.
  !!
  !! ("72" is actually whatever is set in the line_len module parameter.)
  
  SUBROUTINE check_line_length(file, line, err)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(Source), INTENT(IN), POINTER :: file
    CHARACTER(*,KIND=scck), INTENT(IN) :: line
    TYPE(Error), INTENT(OUT) :: err
    
    !***************************************************************************
    
    ! Check line length - first the non-blank length, then the length with
    ! blanks.
    IF (LEN_TRIM(line) > line_len) THEN
      err = ConstructError(  &
          CODE=errLineTooLong,  &
          LEVEL=errLevelWarning,  &
          COMPONENT='3.3.3.1p1',  &
          LOCATION=SourceLocationIfortBug(file, line_len),  &
          MSG='The last non-blank character on the &
            &line exceeds position ' // ToString(line_len) // '.' )
    ELSE IF (LEN(line) > line_len) THEN
      err = ConstructError(  &
          CODE=errLineWithBlanksTooLong,  &
          LEVEL=errLevelWarning,  &
          COMPONENT='3.3.3.1p1',  &
          LOCATION=SourceLocationIfortBug(file, line_len),  &
          MSG='Blank characters on the line exceed position '  &
            // ToString(line_len) // '.' )
    END IF
    
  END SUBROUTINE check_line_length
  
END MODULE ScanFixedForm
