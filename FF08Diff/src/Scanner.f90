! $Id: Scanner.f90 2802 2019-03-22 18:39:26Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Scanner module.


!*******************************************************************************
!!
!> The SourceForm parent type, the GetToken procedure and associated support 
!! procedures and types for converting lines of text into a list of tokens.
!!
!! Client code would typically interact with the Scanner via a SourceForm.  
!! The GetTokens interface is also provided for test programs that wish to 
!! get a relatively raw and unprocessed token stream from a file.
!!
!! <h2>Philosophy</h2>
!! It started with a free form source scanner, which was straight forward 
!! enough.  Then we thought we'd extend things to fixed form.  Fixed form 
!! source comes with a plethora of potentially ambiguous scans in the 
!! absence of context - so this complicated things somewhat.  In the end 
!! we would probably have been better off keeping the free form scanner and 
!! the fixed form scanner separate, with the SourceForm object being purely 
!! abstract.  This would simplify error checking because we could take 
!! advantage of the better token delineation that free form source offers.
!!
!! Anyway, the way that it currently works is that SourceForms have two 
!! roles:
!! - to external code:
!!   - They provide a statement's worth of tokens (and possibly an associated 
!!     statement label) via the GetStatement binding.
!!   - They provide information about the underlying source object via the 
!!     GetSource binding.
!! - to scanner based code (extensions of SourceFormBase only):
!!   - They provide a means of getting a character of source, sans 
!!     continuation and comments, via the GetChar binding.
!!   - They provide a means of checking syntactic correctness via the 
!!     SplitTokenError, SplitCharLiteralError and CheckTokenSeparation 
!!     bindings.
!!
!! Of course all of this assumes that it is worth doing the lexical analysis 
!! ahead of parsing, which is a bit debatable.
!!
!! The dual role above implies that there should be two separate types, 
!! perhaps linked by composition.  The arrangement is complicated futher 
!! by "testing" type entry points used for early development.  These entry 
!! points are probably no longer required (where did the testing code go?), 
!! so that complication may no longer be there... perhaps we should have 
!! another look at this set-up one day.
!!
!! Extensions of the source form type currently do not handle include lines, 
!! even though the notion of including source into an existing stream of 
!! source feels like it should be a source form related concept.  The current 
!! design is to allow client code to decide how to handle the include line, 
!! for example the client code may decide that the included source is to be 
!! scanned using a different source form to the parent source based on the 
!! extension in the character literal in the include line.  Source forms 
!! at the moment don't really know anuthing about the file they are attached 
!! to, but including source requires file related activity at some stage.
!!
!! Similar capability could have been provided by some sort of callback 
!! processing arrangement, however this would have required at least one 
!! more argument to be passed into the scanning routines (and that argument 
!! would probably have to be propagated down to reasonable depth inside 
!! this module).  That seems messy.  This sort of thing may be required in 
!! future to support extensions - for example fixed form source parsing needs 
!! to know what the significant leading keywords are for many statement 
!! forms, so if the ability to dynamically customise the scanner is added 
!! then it will require a similar callback mechanism.
!!
!! (Ideally the callback mechanism would probably be implemented by passing 
!! an object of type Target along with the GetStatement or whatever "entry
!! point", but that may cause issues in terms of circular module 
!! dependencies - source forms would need to know about targets and targets 
!! would need to know about source forms.  Some sort of intermediate proxy 
!! object may be required.)
!!
!! An intermediate appropach would be to have the source forms perform the 
!! error checking for the include line, so that the odd looking first and 
!! last fields in the token type are no longer required.  At this stage this 
!! seems like an appropriate direction to head in future.
!!
!! <h2> Low level routines</h2>
!! The low level procedures (procedure names end in _seq) collect text 
!! sequences rather than tokens.  They all have the same argument style:
!!
!!   get_xxx_seq(form, err_list, text)
!!
!! where:
!!
!! @param[in,out] form      The source form to use to read the text.
!!
!! @param[out]    err_list  The list of errors associated with the scan.
!!
!! @param[out]    text      The text read by the scan.  This typically does 
!! not include any delimiter type characters.
!!
!! @todo Tather than returning a character variable these low level routines 
!! could return an array of SourceChar.  This would preserve position 
!! information in the event that a higher level routine wanted to be more 
!! specific about where an error occurred.  Once the higher level routine 
!! had verified that the returned SourceChar sequence was appropriate it 
!! could collapse the array of SourceChar to a normal CHARACTER variable, 
!! and discard the position information.  (This transition is in progress.)
!!
!! There may also be some benefit to remembering the end of a token (the 
!! location of the last character in the token) in addition to the location 
!! of the first character that is currently stored.
!!
!! It may be appropriate to further extend the storing of source characters 
!! into the actual token type (i.e. take it all the way to the top level).  
!! This would be an alternatively way of passing meta-data from the source 
!! form and scanner through parsing to client code at a later stage - the 
!! meta-data could be stored in the characters of the token itself.  This 
!! implies that we may want to be able to extend Token, or perhaps the 
!! SourceChar type is extensible or similar.  Ideally that customisation 
!! would be specified by the source form and the scanner would be ignorant 
!! of it.  Had an initial look at this but decided more thought was required.
!!
!! These low level routines are implicitly operating on a statements worth 
!! of source characters stored in the source form object.  Characters are 
!! "read" from the buffer by advancing an index into the buffer.  Characters 
!! that previously read can then be put back in the buffer by simply 
!! decrementing the index - see the peek_char, get_char, next_char and 
!! advance_char procedures.
!!
!! This source character buffer approach was a later modification - not all 
!! of which might be properly bedded in yet.
!!
!! <h2>Token aquisition routines</h2>
!! The token acquisition routines build on top of the text sequence routines 
!! to return tokens as defined by section three of the standard.  These are 
!! essentially worker routines for the GetToken procedure.
!!
!! <h2>Scanning ambiguities</h2>
!!
!! One unfortunate idiosyncracy of Fortran source is that it is ambiguous 
!! how to tokenize some character sequences without having in-statement 
!! context.  Identified examples:
!!
!! - The two colons in an array subscript will be parsed by a greedy 
!!   scanner as a single double colon "declare" token (unless in free 
!!   form there are blanks between the colons).  At the moment, this is 
!!   managed by placing a requirement on downstream code that it recognises 
!!   a double colon token in place of two consecutive colons where that is 
!!   appropriate.
!!
!!   This may also have implications for token splitting:
!! @code
!!   array(:&
!!     :2)
!! @endcode
!!
!!   could result in an error (if we bothered to check), when it shouldn't.
!!
!!   (Do we need to check?  I don't think so, but that depends on whether 
!!   you define a source form that accepts some split tokens as a new 
!!   "source form".)
!!
!!   A potential, though difficult solution, would be to flag that we were 
!!   in an array subscript context.  This would require much finer context 
!!   detail to be passed to GetToken than is currently passed.
!!
!!   In terms of client code - this is handled in 
!!   SubscriptLists%%parse_section_subscript.
!!
!! - The sequences (//), (/=) and (/), which are valid in a /generic-spec/ 
!!   that designates an operator, will be parsed by a greedy scanner into 
!!   (/ /), (/ = ) and (/ ), but we want ( // ), ( /= ) and ( / ).
!!
!!   To work around this the scanner will look ahead and see if one of the 
!!   original sequences exist before deciding on the final tokenisation.
!!
!! - The sequence // in a /common-stmt/ designates blank common and should be 
!!   tokenized as / /.  In an expression or other situations where an operator 
!!   is accepted it is the concatenation operator and should be parsed as a 
!!   single token.
!!
!!   This is managed at the moment by placing a requirement on downstream code 
!!   that it recognises the concatenation operator as a substitute for two 
!!   double slashes in sequence.
!!
!!   In terms of client code - this is handled in CommonStmts%%ParseCommonStmt 
!!   and CommonStmt%%parse_common_block_set.
!!
!! - The obsolescent form of the character intrinsic type specifier in a type 
!!   declaration statement, when combined with suitably named variables, could 
!!   result in an incorrect parse as a real literal constant in fixed form.  
!!   For example, CHARACTER*80D0 should be parsed as CHARACTER * 80 D0, but a 
!!   greedy scanner would combine the 80 and the D0.   The absence of a 
!!   separating blank means that this sequence is an error in free form.
!!
!!   To work around this the GetToken procedure takes a real_possible argument 
!!   as a flag to indicate that real literal tokens are not valid at the 
!!   current point in the source.
!!
!!   Because this flag isn't passed to the GetTokens procedure (which is 
!!   called once per statement, and not progressively through the statement 
!!   like GetToken) it cannot resolve the ambiguity.  There is no work around 
!!   for this.

MODULE Scanner
  
  USE CompilerKinds
  USE Sources
  USE SourceLocations
  USE Tokens
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: GetToken
  PUBLIC :: GetTokens
  PUBLIC :: TestCharacterContext
  PUBLIC :: CharsToText
  
  !-----------------------------------------------------------------------------
  
  !> Represents a character read from a source file.
  !!
  !! Note that the char component is not defined if eos and eof are not true.
  TYPE, PUBLIC :: SourceChar
    !> Flag for end of statement.
    !!
    !! If .TRUE. @a got_amp is not defined, but @a pos should still 
    !! indicate the location of the end of the statement. @a char will be 
    !! defined, but not necessarily usefully.
    LOGICAL :: eos
    
    !> The character read, only valid if @a eof is .FALSE.
    CHARACTER(KIND=scck) :: char
    
    !> Flag for proper continuation ampersand present before @a char.
    LOGICAL :: got_amp
    
    !> Position for @a char.
    TYPE(SourceLocation) :: pos
    
    !> Flag for normal end of file.
    !!
    !! Unlike @a eos, this is set to indicate that the file ends after 
    !! the last character read.  Typically @a eos and @a eof would 
    !! both be .TRUE., unless there is some sort of perverse 
    !! "the file ends but the statement doesn't" convention going on, 
    !! which should be an error anyway.
    LOGICAL :: eof
    
    !> Flag for start of line position.
    !!
    !! If set then the character was the first non-blank character on the 
    !! line.  This is required for correctness checking of INCLUDE lines.
    LOGICAL :: start_of_line
    
    !> Flag for end of line position.
    !!
    !! If set then the character was the last non-blank character on the 
    !! line.  This is required for correctness checking of INCLUDE lines.
    !!
    !! Current source forms will have eos and end_of_line always set the same.
    LOGICAL :: end_of_line
  END TYPE SourceChar
  
  
  !> Parent type to describe the interfaces for the various source forms.
  !!
  !! This type has no facility to actually nominate/store the source file 
  !! that is associated with a particular entity of an extension of this 
  !! type.  That facility and a means to connect the source form object 
  !! with a source file needs to be provided by the extension types.
  TYPE, PUBLIC, ABSTRACT :: SourceForm
  CONTAINS
    !> Called to get another instance of the source form, perhaps to process 
    !! an included file.
    PROCEDURE(sf_Clone), DEFERRED :: Clone
    
    !> Called to get the underlying Source object that the source form is 
    !! connected to.
    PROCEDURE(sf_GetSource), DEFERRED :: GetSource
    
    !> Called to get the next statement from the source file.
    !!
    !! This is called by client code using a particular source form.
    !!
    !! This may also return include lines, which aren't real statements.  
    !! Use the TestInclude procedure to check for those.
    !!
    !! @todo Rename to make the line possibility clear.
    PROCEDURE(sf_GetStatement), DEFERRED :: GetStatement
  END TYPE SourceForm
  
  !> Interfaces for deferred bindings in SourceForm.
  ABSTRACT INTERFACE
    !> Interface for SourceForm%Clone - create a copy of the same source form 
    !! but for a different file.
    !!
    !! @param[in]     form              The source form to be cloned.
    !!
    !! @param[in]     source_file       The source of source to attach the 
    !! new source form object to.
    !!
    !! @param[out]    new_form          A new source form object attached 
    !! to @a source_file.
    !!
    !! The clone is not a complete clone - as the source may vary.  This is 
    !! more or less the same as ALLOCATE(new_form, MOLD=form), followed by 
    !! construction of arg based on @a source_file.
    SUBROUTINE sf_Clone(form, source_file, new_form)
      USE Sources
      IMPORT :: SourceForm
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(SourceForm), INTENT(IN) :: form
      CLASS(Source), INTENT(IN), POINTER :: source_file
      CLASS(SourceForm), INTENT(OUT), ALLOCATABLE :: new_form
    END SUBROUTINE sf_Clone
    
    !> Interface for SourceForm%GetSource - return a pointer to the source 
    !! object that the source form object is attached to.
    !!
    !! @param[in]     form              The source form to query.
    !!
    !! @param[out]    source_file       The source object that the source form 
    !! is attached to.
    !!
    !! Anticipate that this might be required for error reporting - when 
    !! client code wants to be a little more specific about which file is 
    !! being read.
    SUBROUTINE sf_GetSource(form, source_file)
      USE Sources
      IMPORT :: SourceForm
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(SourceForm), INTENT(IN) :: form
      CLASS(Source), INTENT(OUT), POINTER :: source_file
    END SUBROUTINE sf_GetSource
    
    !> Interface for SourceForm%GetStatement - get the series of tokens 
    !! associated with the next statement in the source file associated with 
    !! the source form.
    !!
    !! @param[in,out] form              The source form.
    !!
    !! @param[in]     part_stack        Current syntax part stack prior to 
    !! the statement being retrieved.
    !!
    !! @param[out]    err_list          List of errors.
    !!
    !! @param[out]    statement_tokens  The sequence of tokens for the next 
    !! statement in the file.  If there were no errors and this is zero 
    !! length, then the end of file was encountered.
    !!
    !! @param[out]    statement_label   The statement label associated with 
    !! the next statement, or zero if the next statement had no label.
    !!
    !! @todo Using a default integer to store the statement label implies 
    !! that the default integer on the host processor is big enough to store 
    !! all possible statement labels (i.e. up to 99999 by R312).  This may 
    !! need to be reviewed.  Because statement labels don't really have the 
    !! characteristics of integers (well, any more - given that computed goto 
    !! crap got ditched) this is probably not appropriate - may be better to 
    !! use a character(5) variable.
    !!
    !! Any tokens associated with the statement label are not present in 
    !! @a statement_tokens.
    !!
    !! If @a statement_tokens has zero size and label is zero then there was 
    !! no next statement in the file (end of file was reached).
    !!
    !! The implementation of this binding in extensions must check:
    !! - that if a statement is labeled that it contains a nonblank character 
    !!   (3.2.5p2).
    !! - other things?
    SUBROUTINE sf_GetStatement( form, part_stack,  &
        err_list, statement_tokens, statement_label )
      USE Errors
      USE LabelsUtils
      IMPORT :: SourceForm
      IMPORT :: Token
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(SourceForm), INTENT(INOUT) :: form
      INTEGER, INTENT(IN) :: part_stack(:)
      TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
      TYPE(Token), INTENT(OUT), ALLOCATABLE :: statement_tokens(:)
      TYPE(Label), INTENT(OUT), ALLOCATABLE :: statement_label
    END SUBROUTINE sf_GetStatement
  END INTERFACE
  
  !> Extension of SourceForm with additional bindings that permit it to 
  !! operate with the basic tokenisation procedures in this module.
  TYPE, EXTENDS(SourceForm), ABSTRACT, PUBLIC :: SourceFormBase
    !> Buffer for storing the source characters for the current statement.
    TYPE(SourceChar), ALLOCATABLE :: stmt_chars(:)
    !> Highest in-use index in stmt_chars.
    INTEGER :: stmt_len = 0
    !> Current position in stmt_chars
    INTEGER :: stmt_pos = 0
    !> Flag to indicate that we have a format statement and need to switch 
    !! token parsing modes after the sixth character.
    LOGICAL :: is_format_stmt
  CONTAINS
    !> Called to get the next character from the source file.
    !!
    !! This is called by GetToken and related procedures.  It should not be 
    !! called by client code.
    PROCEDURE(sf_GetChar), DEFERRED :: GetChar
    
    !> Called by GetToken and related procedures when a split token is
    !! potentially encountered.
    !!
    !! A split token is one that spans a continuation line.  In free form 
    !! source a split token without a leading ampersand on the continued 
    !! line is an error.  In free form source a split token is legal.
    PROCEDURE(sf_SplitTokenError), DEFERRED :: SplitTokenError
    
    !> Called by GetToken and related procedures when a split char-literal 
    !! is potentially encountered.
    !!
    !! A split char-literal is one that spans a continuation line.  In 
    !! free form source a char-literal that is continued must have a leading 
    !! ampersand on the continuation line.  In free form source a split 
    !! char-literal is legal.
    PROCEDURE(sf_SplitTokenError), DEFERRED :: SplitCharLiteralError
    
    !> Called by GetToken and related procedures when a token ends.
    PROCEDURE(sf_CheckTokenSeparation), DEFERRED :: CheckTokenSeparation
  END TYPE SourceFormBase
  
  ! Interfaces for deferred bindings in SourceFormBase.
  ABSTRACT INTERFACE
    !> Interface for SourceFormBase%GetChar - get a character from the source 
    !! file.
    !!
    !! @param[in,out] form              The source form for the source 
    !! file being read.
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
    SUBROUTINE sf_GetChar( form, in_character_context, context_chars,  &
        ch, err_list )
      USE CompilerKinds
      USE Errors
      USE Sources
      IMPORT :: SourceFormBase
      IMPORT :: SourceChar
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(SourceFormBase), INTENT(INOUT) :: form
      LOGICAL, INTENT(IN) :: in_character_context
      CHARACTER(*), INTENT(IN) :: context_chars 
      TYPE(SourceChar), INTENT(OUT) :: ch
      TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    END SUBROUTINE sf_GetChar
    
    !> Interface for SourceFormBase%SplitTokenError - test/handle a potential 
    !! split token error.
    !!
    !! @param[in]     form              The source form.
    !!
    !! @param[in]     initial_chars     The token character sequence as it 
    !! existed prior to the split.
    !!
    !! @param[in]     next_ch           The first character on the next 
    !! line.
    !!
    !! @param[out]    err_list          List of errors.  If the split token 
    !! or split literal is an error - then the extensions of SourceForm 
    !! can add an appropriate message to the list.
    !!
    !! The two different bindings allow a customised message for split tokens 
    !! versus split char-literals to be created.
    SUBROUTINE sf_SplitTokenError(form, initial_chars, next_ch, err_list)
      USE Errors
      USE SourceLocations
      USE Tokens
      IMPORT :: SourceChar
      IMPORT :: SourceFormBase
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(SourceFormBase), INTENT(IN) :: form
      TYPE(SourceChar), INTENT(IN) :: initial_chars(:)
      TYPE(SourceChar), INTENT(IN) :: next_ch
      TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    END SUBROUTINE sf_SplitTokenError
    
    !> Interface for SourceFormBase%CheckTokenSeparation.
    !!
    !! @param[in]     form              The source form object.
    !!
    !! @param[in]     the_token         The token that has just finished.
    !!
    !! @param[in]     next_ch           The character immediately following 
    !! the token.
    !!
    !! @param[in,out] err_list          List of errors.  Note that this 
    !! is INOUT.
    SUBROUTINE sf_CheckTokenSeparation(form, the_token, next_ch, err_list)
      USE Errors
      IMPORT :: SourceFormBase
      IMPORT :: SourceChar
      IMPORT :: Token
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(SourceFormBase), INTENT(IN) :: form
      TYPE(Token), INTENT(IN) :: the_token
      TYPE(SourceChar), INTENT(IN) :: next_ch
      TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    END SUBROUTINE sf_CheckTokenSeparation
  END INTERFACE
  
  !-----------------------------------------------------------------------------
  ! Other interfaces
  
  !> Get a token from a file/source form.
  !!
  !! This is the primary entry point for the parsing.
  INTERFACE GetToken
    MODULE PROCEDURE GetToken_
  END INTERFACE GetToken
  
  !> Get a statement worth of tokens from a file/source form.
  !!
  !! This maybe useful for testing, but note that it can't handle 
  !! label processing or token splitting.
  INTERFACE GetTokens
    MODULE PROCEDURE GetTokens_
  END INTERFACE GetTokens
  
  !> Test whether a character context is current.
  INTERFACE TestCharacterContext
    MODULE PROCEDURE TestCharacterContext_
  END INTERFACE TestCharacterContext
  
  !> Convert an array of source characters to a character string.
  INTERFACE CharsToText
    MODULE PROCEDURE CharsToText_
  END INTERFACE CharsToText
  
  !> Add an additional Token constructor that takes a source char.
  INTERFACE Tkn
    MODULE PROCEDURE Token_ch
  END INTERFACE Tkn
  
  !-----------------------------------------------------------------------------
  
  !> Initial size of the queue inside the SourceForm class.
  !!
  !! I think the largest this will ever need to be is 2.  The reasons for 
  !! it being 10 are lost in antiquity.
  INTEGER, PARAMETER :: initial_size = 10
  
  !> Initial size of the statement character buffer inside the SourceForm 
  !! class.  This is the expected maximum statement buffer size - 
  !! statements larger than this are handled, but with a (once off, so 
  !! who cares) reallocation will be required.
  INTEGER, PARAMETER :: initial_chars = 10 * 132
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Gets all tokens from the file until an end of statement or end of file 
  !! character is reached.
  !!
  !! @param[in]     form              The source form to read the file.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    token_list        A sequence of a statement's worth of 
  !! tokens.
  !!
  !! This is a backdoor interface into the tokenisation routines that is 
  !! useful for testing.  It is really only useful for free form source as 
  !! otherwise contiguous runs of keywords and names are not split.
  
  SUBROUTINE GetTokens_(form, err_list, token_list)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT), ALLOCATABLE :: token_list(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Size of the buffer initially allocated for a statement's worth of 
    ! tokens.  The buffer is grown as needed on an exponential basis from this 
    ! starting point.
    INTEGER, PARAMETER :: initial_token_size = 10
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! The current token being retrieved.
    TYPE(Token) :: the_token
    
    ! Error messages from procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    ! Temporary copy of token_list to allow for array expansion.
    TYPE(Token), ALLOCATABLE ::tmp_tokens(:)
    
    ! In-use size (number of defined elements) of tmp_tokens.
    INTEGER :: tmp_token_size
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ALLOCATE(tmp_tokens(initial_token_size))
    tmp_token_size = 0
    
    DO
      ! Fixed form bites, so we'll assume we're testing under free form.
      CALL GetToken(form, .TRUE., sub_err_list, the_token)
      CALL Add(err_list, sub_err_list)
      ! On error parsing a token we abandon everything.
      IF (Failure(sub_err_list)) RETURN
      
      IF (the_token == ittEndStatement) THEN
        token_list = tmp_tokens(1:tmp_token_size)
        RETURN
      END IF
      
      IF (tmp_token_size == SIZE(tmp_tokens)) THEN
        ALLOCATE(token_list(SIZE(tmp_tokens) * 2))
        token_list(1:SIZE(tmp_tokens)) = tmp_tokens
        CALL MOVE_ALLOC(token_list, tmp_tokens)
      END IF
      
      tmp_token_size = tmp_token_size + 1
      tmp_tokens(tmp_token_size) = the_token
    END DO
    
  END SUBROUTINE GetTokens_
  
  
  !*****************************************************************************
  !!
  !> Get a statement worth of characters from a source form and store in 
  !! the line component of the source form object.
  !!
  !! Along the way we track whether we are in a character context or not, 
  !! so the source form can respond appropriately.
  !!
  !! @param[in,out] form              The source form object.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! The source form is responsible for stripping out comments, handling 
  !! continuation, etc.
  !!
  !! Here we do some statement level processing - such as working out 
  !! whether we are dealing with a FORMAT statement.
  
  SUBROUTINE get_statement_chars(form, err_list)
    
    USE CharacterTypes
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Character sequence that has caused us much philosophical difficulty 
    ! over the last few years.
    CHARACTER(*), PARAMETER :: format_string = scck_'FORMAT('
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Flag for whether we are currently in a character literal context.
    LOGICAL :: in_char_literal
    
    ! Quote character that started the current character literal context, 
    ! if we happen to be in one.
    CHARACTER(KIND=scck) :: quote
    
    ! Error list for child procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    TYPE(SourceChar) :: ch    ! Current source character.
    
    ! Flag that indicates last char was a quote relevant to the current 
    ! character literal context (irrelevant if we are not in a char 
    ! literal).
    LOGICAL :: got_quote
    
    ! Temporary for growing our list of chars.
    TYPE(SourceChar), ALLOCATABLE :: tmp(:)
    
    ! Progressive track of FORMAT statement match.  See the check_format_state 
    ! internal for information about the individual values.
    INTEGER :: format_state
    
    ! Parentheses nesting count.  This is required to stop silliness like 
    ! FORMAT(I0) = 10 being parsed as a FORMAT statement.
    INTEGER :: nest
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    IF (.NOT. ALLOCATED(form%stmt_chars))  &
        ALLOCATE(form%stmt_chars(initial_chars))
    
    !---------------------------------------------------------------------------
    
    ! Initially, we are not in a character literal, the statement is quite 
    ! short, we didn't historically cop a quote, we have no idea whether 
    ! we are in a FORMAT statement or not and we haven't seen any parentheses.
    in_char_literal = .FALSE.
    form%stmt_len = 0
    form%stmt_pos = 1
    got_quote = .FALSE.
    format_state = 0
    nest = 0
    
    ! Go get some chars.
    DO
      ! If we are in a char literal and we've seen a quote we're not 
      ! really sure if we've left the char literal until we see the 
      ! next char.
      IF (got_quote) THEN
        CALL form%GetChar(in_char_literal, quote, ch, sub_err_list)
      ELSE
        CALL form%GetChar(in_char_literal, scck_'', ch, sub_err_list)
      END IF
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
      
      ! How's that FORMAT statement looking?
      CALL check_format_state
      
      ! Statement length must have grown.
      form%stmt_len = form%stmt_len + 1
      
      ! Expand buffer if we need to.
      IF (form%stmt_len > SIZE(form%stmt_chars)) THEN
        ALLOCATE(tmp(SIZE(form%stmt_chars) * 2))
        tmp(:SIZE(form%stmt_chars)) = form%stmt_chars
        CALL MOVE_ALLOC(tmp, form%stmt_chars)
      END IF
      
      ! Store character
      form%stmt_chars(form%stmt_len) = ch
      
      IF (ch%eos) EXIT
      
      IF (got_quote) THEN
        got_quote = .FALSE.
        IF (ch%char /= quote) THEN
          ! 'abc'x
          !      ^
          in_char_literal = .FALSE.
          CALL check_nest
        END IF
      ELSE IF (in_char_literal) THEN
        ! Two quotes in a row might be one quote in the literal, etc.
        IF (ch%char == quote) got_quote = .TRUE.
      ELSE IF (IsQuoteChar(ch%char)) THEN
        quote = ch%char
        in_char_literal = .TRUE.
      ELSE
        CALL check_nest
      END IF
    END DO
    
    ! The tests are structured such that FORMAT(I0) i_am_a_zucchini ends 
    ! up classified as a format statement.
    form%is_format_stmt = (format_state == 7)
    
  CONTAINS
    !***************************************************************************
    ! Update the format_state variable based on the current character.
    SUBROUTINE check_format_state
      USE CharacterTypes
      !*************************************************************************
      ! Once we have conclusively decided that we have a format statement or 
      ! that we do not have a format statement, then there's no need for 
      ! further checks
      IF ((format_state == 7) .OR. (format_state == -1)) RETURN
      IF (ch%eos) RETURN
      IF (IsBlankChar(ch%char)) THEN
        IF (format_state == 0) RETURN   ! Can have blanks before FORMAT.
        IF (format_state == 6) RETURN   ! Can have blanks after FORMAT.
        format_state = -1               ! Can not have blanks in FORMAT.
        RETURN
      END IF
      ! Ideally we would know whether the statement characters could include 
      ! a statement label, and if that was not possible (fixed form source) 
      ! then the presence of digits would cause us to decide that this 
      ! was not a format statement.  But in fixed form source, digits 
      ! before FORMAT will result in an unclassified statement anyway, 
      ! so we can tolerate this for now.
      !
      ! We could also get fancy for free form source for source constructs 
      ! like `1 2 3 FORMAT(xyz)`, but again that is going to result in 
      ! a classification error.
      IF (IsDigitChar(ch%char)) THEN
        IF (format_state == 0) RETURN   ! Can have digits before FORMAT.
        format_state = -1               ! Can not have digits in FORMAT.
        RETURN
      END IF
      IF (ch%char /= format_string(format_state+1:format_state+1)) THEN
        format_state = -1
        RETURN
      END IF
      ! Match if we make it to here.
      format_state = format_state + 1
    END SUBROUTINE check_format_state
    
    !***************************************************************************
    ! Update the nest state based on the current character.
    SUBROUTINE check_nest
      IF (format_state == 7) THEN
        IF (ch%char == scck_'(') THEN
          nest = nest + 1
        ELSE IF (ch%char == scck_')') THEN
          nest = nest - 1
        ELSE IF (nest == 0) THEN
          IF (ch%char == scck_'=')  &
              format_state = 0
        END IF
      END IF
    END SUBROUTINE check_nest
  END SUBROUTINE get_statement_chars
  
  
  !*****************************************************************************
  !!
  !> Retrieve the next character from the source form's internal statement 
  !! buffer, without updating the character position.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[out]    ch                The next character in the statement 
  !! buffer.
  
  SUBROUTINE peek_char(form, ch)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(IN) :: form
    TYPE(SourceChar), INTENT(OUT) :: ch
    
    !***************************************************************************
    
    ch = form%stmt_chars(form%stmt_pos)
    
  END SUBROUTINE peek_char
  
  
  !*****************************************************************************
  !!
  !> Retrieve the next character from the source form's internal statement 
  !! buffer, without updating the character position.
  !!
  !! @param[in]     form              The source form object.
  !!
  !! @param[out]    ch                The next character in the statement 
  !! buffer.
  
  SUBROUTINE peek_ahead_char(form, offset, ch)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(IN) :: form
    INTEGER, INTENT(IN) :: offset
    TYPE(SourceChar), INTENT(OUT) :: ch
    
    !***************************************************************************
    
    ch = form%stmt_chars(form%stmt_pos + offset)
    
  END SUBROUTINE peek_ahead_char
  
  
  !*****************************************************************************
  !!
  !> Retrieve the current character from the source form's internal statement 
  !! buffer and advances the character position.
  !!
  !! @param[in,out] form              The source form object.
  !!
  !! @param[out]    ch                The current character in the statement 
  !! buffer.
  
  SUBROUTINE get_char(form, ch)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(SourceChar), INTENT(OUT) :: ch
    
    !***************************************************************************
    
    ch = form%stmt_chars(form%stmt_pos)
    form%stmt_pos = form%stmt_pos + 1
    
  END SUBROUTINE get_char
  
  
  !*****************************************************************************
  !!
  !> Advance the character position and retrieve the next character from the 
  !! source form's internal statement buffer.
  !!
  !! @param[in,out] form              The source form object.
  !!
  !! @param[out]    ch                The next character in the statement 
  !! buffer.
  
  SUBROUTINE next_char(form, ch)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(SourceChar), INTENT(OUT) :: ch
    
    !***************************************************************************
    
    form%stmt_pos = form%stmt_pos + 1
    ch = form%stmt_chars(form%stmt_pos)
    
  END SUBROUTINE next_char
  
  
  !*****************************************************************************
  !!
  !> Retrieve the next character from the source form's internal statement 
  !! buffer, advancing the character position.
  !!
  !! @param[in,out] form              The source form object.
  !!
  !! @param[out]    ch                The next character in the statement 
  !! buffer.
  !!
  !! We ask for the character that is to be ungot even though we already 
  !! know it - just as a logic check.
  
  SUBROUTINE unget_char(form, ch)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(SourceChar), INTENT(IN) :: ch
    
    !***************************************************************************
    
    form%stmt_pos = form%stmt_pos - 1
    IF (ch%char /= form%stmt_chars(form%stmt_pos)%char) THEN
      STOP 'Internal error in Scanner%%unget_char, &
          &bad unget'
    END IF
    
  END SUBROUTINE unget_char
  
  
  !*****************************************************************************
  !!
  !> Advance the statement buffer index without retrieving a character,
  !!
  !! @param[in,out] form              The source form object.
  
  SUBROUTINE advance_char(form)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    
    !***************************************************************************
    
    form%stmt_pos = form%stmt_pos + 1
    
  END SUBROUTINE advance_char
  
  
  !*****************************************************************************
  !!
  !> Gets the next token from the file.
  !! 
  !! @param[in]     form              The source form of the file.
  !!
  !! @param[in]     real_possible     Flag to indicate whether 
  !! real-literal-constant's are possible at this stage of the scan.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    next_token        The token.
  !!
  !! This is a thin wrapper around get_token_worker that ensures that 
  !! we dump the statement buffer if a token assembly error is encountered.
  
  SUBROUTINE GetToken_(form, real_possible, err_list, next_token)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    LOGICAL, INTENT(IN) :: real_possible
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT) :: next_token
    
    !***************************************************************************
    
    CALL get_token_worker(form, real_possible, err_list, next_token)
    
    IF (Failure(err_list)) THEN
      ! Dump the current statement.  The next time that get_token_worker 
      ! is called it will reload the statement character buffer.  As an 
      ! exception, if the current character indicates end of file, then 
      ! we leave that end of file marker as the only character in the 
      ! buffer
      IF (form%stmt_chars(form%stmt_len)%eof) THEN
        form%stmt_chars(1) = form%stmt_chars(form%stmt_len)
        form%stmt_len = 1
        form%stmt_pos = 1
      ELSE
        form%stmt_len = 0
      END IF
    END IF
    
  END SUBROUTINE GetToken_
  
  
  !*****************************************************************************
  !!
  !> Worker routine for GetToken.
  !!
  !! The arguments have the same meaning as for GetToken_.
  !!
  !! If @a line is unallocated on entry, then a fresh line is retrieved from 
  !! @a source_file.  If @a line is unallocated on exit then the end of 
  !! @a source_file was encountered.  @a next_token may still be valid 
  !! in that case (it should only be an end-of-statement token).
  !!
  !! The basic strategy of this procedure (specifically) is to get a 
  !! single char, and then hand off to a series of worker procedures 
  !! based on that char (e.g. get a digit - hand off to a procedure that 
  !! scans things that start with a digit).
  
  SUBROUTINE get_token_worker( form, real_possible,  &
      err_list, next_token )
    
    USE CharUtils
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    LOGICAL, INTENT(IN) :: real_possible
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT) :: next_token
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Error list for procedure calls
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    ! Buffer to build the text of some types of token.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: text
    
    ! Current source file character.
    TYPE(SourceChar) :: ch
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Fill the statement buffer
    
    IF (form%stmt_len == 0) THEN
      CALL get_statement_chars(form, sub_err_list)
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
    END IF
    
    !---------------------------------------------------------------------------
    
    next_token = Tkn()      ! Sets type to ittNFI.
    
    DO
      CALL peek_char(form, ch)
      IF (ch%eos) THEN
        ! End of statement.
        next_token = Tkn(ittEndStatement, ch)
        ! If the file ended too then leave the file termination "character" 
        ! in the buffer, so that if we are called again we'll give it back 
        ! again.  Eventually the calling routine will get the hint that 
        ! there's no more tokens.
        IF (.NOT. ch%eof) form%stmt_len = 0
        RETURN
      END IF
      
      IF (ch%eof) THEN
        ! End of file without end of statement.  Shouldn't happen, but to be 
        ! sure...
        CALL Add( err_list,  &
            CODE=errFileEndedButStatementDidnt,  &
            MSG='File ended before statement ended.',  &
            LOCATION=ch%pos )
        RETURN
      END IF
      
      IF (IsBlankChar(ch%char)) THEN
        ! Ignore
        CALL advance_char(form)
        CYCLE
      
      !-------------------------------------------------------------------------
      ! Single character tokens.
      
      ELSE IF (ch%char == scck_';') THEN
        ! We used to have ; statement termination in the parser, but moved it 
        ! out to the source forms as it is defined for each source form.
        STOP 'Internal error in Scanner%%get_token_worker, &
            &encountered ";".'
      ELSE IF (ch%char == scck_',') THEN
        next_token = Tkn(ittComma, ch)
        CALL advance_char(form)
        RETURN
      ELSE IF ((ch%char == scck_'[')  &
          .OR. (ch%char == scck_']')  &
          .OR. (ch%char == scck_')') ) THEN
        next_token = Tkn(ittDelimiter, ch)
        CALL advance_char(form)
        RETURN
      ELSE IF (ch%char == scck_'%') THEN
        next_token = Tkn(ittPercent, ch)
        CALL advance_char(form)
        RETURN
      ELSE IF ( (ch%char == scck_'+')  &
          .OR. (ch%char == scck_'-') ) THEN
        next_token = Tkn(ittOperator, ch)
        CALL advance_char(form)
        RETURN
      
      !-------------------------------------------------------------------------
      ! Multiple character tokens
      
      ELSE IF (IsQuoteChar(ch%char)) THEN
        ! Something that starts with a quote.  This can only be a 
        ! character-literal.
        CALL get_quote_token(form, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
      ELSE IF (IsLetterChar(ch%char)) THEN
        ! Something that starts with a letter.  This could be a statement 
        ! keyword, a variable, procedure or argument name or the kind 
        ! parameter of a character literal.
        CALL get_name_token(form, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
      ELSE IF (IsDigitChar(ch%char)) THEN
        ! Something that starts with a number.  This could be a statement 
        ! label (that depends on context), an integer-literal, a real-literal 
        ! or the kind of a character literal
        CALL get_digit_token(form, real_possible, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
      ELSE IF (ch%char == scck_'.') THEN
        ! Something that starts with a dot.  This could be a built-in operator 
        ! or a user defined operator, or a logical-literal or a real-literal.
        CALL get_dot_token(form, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
      ELSE IF (ch%char == scck_'*') THEN
        ! Something that starts with a *.  This could be a single * (for 
        ! multiply, "assumed", the default unit or the list-directed format) 
        ! or a double start (for exponentiation).
        next_token = Tkn( ittOperator, ch%pos, '', ch%start_of_line,  &
            ch%end_of_line )
        CALL get_symbol_seq(form, scck_'*', sub_err_list, text)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        CALL AppendRawValue(next_token, text)
        RETURN
      ELSE IF (ch%char == scck_'/') THEN
        ! Something that starts with a /.  This could be a single / (for 
        ! divide) , // for concatenate, /= for not equals or /) to close an 
        ! array constructor.  Assume that it is an operator for now.
        next_token = Tkn( ittOperator, ch%pos, '', ch%start_of_line,  &
            ch%end_of_line )
        CALL get_symbol_seq(form, scck_'/=)', sub_err_list, text)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        IF (text == scck_'/)') THEN
          CALL check_close_operator_symbol(form, text)
        END IF
        CALL AppendRawValue(next_token, text)
        ! If necessary, adjust the token type.
        IF (text == scck_'/)') next_token = ittDelimiter
        RETURN
      ELSE IF (ch%char == scck_'(') THEN
        ! Something that starts with a (.  This could be a single ( or a (/.  
        ! It could also be our local concept (outside the standard) of a 
        ! format specification token.
        !
        ! We consider any token sequence starting with a parenthesis to 
        ! be the start of the format spec token if the statement has 
        ! been identified as a format statement.  This means that if 
        ! the source is `100 FORMAT(xyz)(abc)` we will return two format 
        ! specification tokens.  Parsing code later on should get quite 
        ! upset with that.  Similarly for additional tokens following 
        ! the closing parenthesis.
        IF (form%is_format_stmt) THEN
          CALL get_format_spec_token(form, sub_err_list, next_token)
          CALL Add(err_list, sub_err_list)
          RETURN
        END IF
        
        next_token = Tkn( ittDelimiter, ch%pos, '', ch%start_of_line,  &
            ch%end_of_line )
        CALL get_symbol_seq(form, scck_'/', sub_err_list, text)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        IF (text == scck_'(/') THEN
          ! There is a potential ambiguity with the otherwise valid 
          ! sequences (/), (/=) and (//).  A greedy parse will combine the 
          ! opening parenthesis with the slash to make the delimiter used in 
          ! F90 style array constructors.  However, we always want the 
          ! sequence where the opening parenthesis is on its own - consider 
          ! the possible syntax in the OPERATOR clause of a /generic-spec/.  
          ! This means we need to fetch additional character from the source 
          ! to check.
          CALL check_open_operator_symbol(form, text)
        END IF
        CALL AppendRawValue(next_token, text)
        RETURN
      ELSE IF (ch%char == scck_'=') THEN
        ! Something that starts with a =.  This could be a single =, ==,
        ! or =>.  We'll assume '=' only for now
        next_token = Tkn( ittAssign, ch%pos, '', ch%start_of_line,  &
            ch%end_of_line )
        CALL get_symbol_seq(form, scck_'=>', sub_err_list, text)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        CALL AppendRawValue(next_token, text)
        ! If necessary, update the type of token.
        IF (text == scck_'=>') next_token = ittAssociate
        IF (text == scck_'==') next_token = ittOperator
        RETURN
      ELSE IF (ch%char == scck_'<') THEN
        ! Something that starts with a <.  This could be a single < or <=.  
        ! Both of these are operators.
        next_token = Tkn( ittOperator, ch%pos, '', ch%start_of_line,  &
            ch%end_of_line )
        CALL get_symbol_seq(form, scck_'=', sub_err_list, text) 
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        CALL AppendRawValue(next_token, text)
        RETURN
      ELSE IF (ch%char == scck_'>') THEN
        ! Something that starts with a >.  This could be a single > or >=.
        ! Both of these are operators.
        next_token = Tkn( ittOperator, ch%pos, '', ch%start_of_line,  &
            ch%end_of_line )
        CALL get_symbol_seq(form, scck_'=', sub_err_list, text)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        CALL AppendRawValue(next_token, text)
        RETURN
      ELSE IF (ch%char == scck_':') THEN
        ! Something that starts with a :.  This could be a single : or ::.  
        ! We'll assume the single token for now.
        next_token = Tkn( ittColon, ch%pos, '', ch%start_of_line,  &
            ch%end_of_line )
        CALL get_symbol_seq(form, scck_':', sub_err_list, text)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        CALL AppendRawValue(next_token, text)
        ! If necessary, update the type.
        IF (text == scck_'::') next_token = ittDeclare
        RETURN
        
      ELSE
        !-----------------------------------------------------------------------
        ! Anything else?
        
        CALL Add( err_list,  &
            CODE=errStrangeCharacter,  &
            LOCATION=ch%pos,  &
            MSG='The character ''' // ch%char // ''' was unexpected.' )
        CALL advance_char(form)
        RETURN
      END IF
      
      ! Let the source form know ahead of time what's made us decide that 
      ! next_token is done.
      CALL peek_char(form, ch)
      CALL form%CheckTokenSeparation(next_token, ch, err_list)
      RETURN
      
    END DO
    
  END SUBROUTINE get_token_worker
  
  
  !*****************************************************************************
  !!
  !> Gets a format specification "token".  See comments in the Tokens module 
  !! for the ittFormatSpecification constant for more information about this 
  !! token.
  !!
  !! The next non-blank character in the statement should be the opening 
  !! parentheses.  The format specification token is considered to run up to 
  !! and including the matching closing parenthesis, or the end of statement 
  !! if that comes first.
  !!
  !! @param[in,out] form              The source form.  The statement 
  !! character position will be changed by this procedure.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    next_token        In the absence of errors set to the 
  !! format specification token.
  
  SUBROUTINE get_format_spec_token(form, err_list, next_token)
    
    USE CharacterTypes
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT) :: next_token
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: nest
    LOGICAL :: got_quote
    LOGICAL :: in_char_literal
    TYPE(SourceChar) :: ch
    CHARACTER(KIND=scck) :: quote
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! Skip leading blanks prior to the opening '('.
    DO
      CALL peek_char(form, ch)
      IF (ch%eos) THEN
        STOP 'Internal error in Scanner%%get_format_spec_token, &
            &end of statement when expecting ''(''.'
      END IF
      IF (ch%char == scck_'(') EXIT 
      IF (.NOT. IsBlankChar(ch%char)) THEN
        STOP 'Internal error in Scanner%%get_format_spec_token, &
            &character other than ''('' or '' '' encountered.'
      END IF
      CALL advance_char(form)
    END DO
    
    nest = 0
    got_quote = .FALSE.
    in_char_literal = .FALSE.
    CALL peek_char(form, ch)
    next_token = Tkn(  &
        ittFormatSpecification,  &
        ch%pos,  &
        ch%start_of_line,  &
        ch%end_of_line )
    DO
      ! If we exit then do so leaving the eos as the current character.
      IF (ch%eos) EXIT
      CALL advance_char(form)
      
      CALL AppendRaw(next_token, ch%char)
      
      IF (got_quote) THEN
        IF (ch%char /= quote) THEN
          in_char_literal = .FALSE.
          CALL check_nest
          IF (nest == 0) EXIT
        END IF
      ELSE IF (in_char_literal) THEN
        IF (ch%char == quote) got_quote = .TRUE.
      ELSE IF (IsQuoteChar(ch%char)) THEN
        quote = ch%char
        in_char_literal = .FALSE.
      ELSE
        CALL check_nest
        IF (nest == 0) EXIT
      END IF
      IF (form%stmt_pos > form%stmt_len) EXIT
      CALL peek_char(form, ch)
    END DO
    
    CALL SetValue(next_token, QueryRaw(next_token))
    
  CONTAINS
    
    SUBROUTINE check_nest
      IF (ch%char == scck_'(') THEN 
        nest = nest + 1
      ELSE IF (ch%char == scck_')') THEN 
        nest = nest - 1
      END IF
    END SUBROUTINE check_nest
    
  END SUBROUTINE get_format_spec_token
  
  
  !*****************************************************************************
  !!
  !> Gets a character sequence that starts with whatever the current symbol 
  !! is and and continues with one character from a given list.
  !!
  !! If the second character doesn't match, then it is left in @a form.  
  !! This is different behaviour to other _seq procedures.
  !!
  !! @param[in,out] form              The source form to read the file.
  !!
  !! @param[in,out] ch                Current source character.
  !!
  !! @param[in]     symbols           Valid characters for the second 
  !! charcter in the token.  Any character that isn't in this variable 
  !! is considered part of the next token.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    text              Text of the token.
  
  SUBROUTINE get_symbol_seq(form, symbols, err_list, text)
    
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    CHARACTER(*,KIND=scck), INTENT(IN) :: symbols
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    CHARACTER(:,KIND=scck), INTENT(OUT), ALLOCATABLE :: text
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! First character of the sequence.
    TYPE(SourceChar) :: initial
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! Store the current character.
    CALL get_char(form, initial)
    text = initial%char
    
    CALL peek_char(form, ch)
    IF (.NOT. ch%eos) THEN
      IF (SCAN(symbols, ch%char) /= 0) THEN
        ! Looks like we have a two character token.
        CALL advance_char(form)
        IF (.NOT. ch%got_amp) THEN
          CALL form%SplitTokenError([initial], ch, err_list)
        ELSE
          text = text // ch%char
        END IF
        RETURN
      END IF
    END IF
    
  END SUBROUTINE get_symbol_seq
  
  
  !*****************************************************************************
  !!
  !> Call if you have the character sequence '(/' (only) and you want to make 
  !! sure that you don't have '(/=)', (//) or (/).  If anything else follows 
  !! the '(/' sequence we assume that we have the opening delimiter of a F90 
  !! style array constructor.
  !!
  !! @param[in,out] form              Source form to read the file.
  !!
  !! @param[in,out] text              On input should be the text string 
  !! '(/'.  On output is set to the appropriate token given subsequent 
  !! characters (may be just a '(' or unchanged).
  !!
  !! The current character before calling should be the character after '/'.
  
  SUBROUTINE check_open_operator_symbol(form, text)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    CHARACTER(:,KIND=scck), INTENT(INOUT), ALLOCATABLE :: text
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! The next char.
    
    !***************************************************************************
    
    CALL peek_char(form, ch)
    
    IF (.NOT. ch%eos) THEN
      IF ( (ch%char == scck_'=')  &
          .OR. (ch%char == scck_'/')  &
          .OR. (ch%char == scck_')') ) THEN
        text = scck_'('
        ! Back up, such that the current character is now the '/'.
        form%stmt_pos = form%stmt_pos - 1
        RETURN
      END IF
    END IF
    
  END SUBROUTINE check_open_operator_symbol
  
  
  !*****************************************************************************
  !!
  !> Call if you have the character sequence '/)' (only) and you want to make 
  !! sure that combining the two characters into the one token is the right 
  !! thing to do.
  !!
  !! @param[in,out] form              Source form to read the file.
  !!
  !! @param[in,out] text              On input should be the text string 
  !! '/)'.  On output is set to the appropriate token given subsequent 
  !! characters (may be just a '/' or unchanged).
  !!
  !! We make the decision by looking at the previous character.  If it is 
  !! an opening parenthesis then we are probably in the symbol bit of an 
  !! OPERATOR clause, and should keep the characters in separate tokens.
  
  SUBROUTINE check_close_operator_symbol(form, text)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    CHARACTER(:,KIND=scck), INTENT(INOUT), ALLOCATABLE :: text
    
    !***************************************************************************
    
    IF (form%stmt_pos > 3) THEN
      IF (form%stmt_chars(form%stmt_pos-3)%char == scck_'(') THEN
        text = scck_'/'
        form%stmt_pos = form%stmt_pos - 1
      END IF
    END IF
    
  END SUBROUTINE check_close_operator_symbol
  
  
  !*****************************************************************************
  !!
  !> Process a token that starts with a digit.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[in]     real_possible     If .TRUE. then the scanner will consider 
  !! the possibility that the digit string followed by an exponent character 
  !! is a real literal.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    next_token        The scanned token. This could be a 
  !! integer literal, a real literal or a character literal (with leading 
  !! numeric kind).
  !!
  !! @a real_possible plays a role in disambiguating fixed source form 
  !! where a type-declaration-stmt with the old form of the length-selector:
  !!
  !! CHARACTER*80 D0
  !!
  !! could be interpreted as the following token sequence:
  !!
  !! CHARACTER * 80D0  
  !!
  !! Similarly a function-stmt, again using the old form of the 
  !! length-selector:
  !!
  !! CHARACTER*80ELEMENTALFUNCTIONNAME
  !!
  !! could be interpreted as the following token sequence, where the 
  !! real-literal is missing its exponent digits:
  !!
  !! CHARACTER * 80E LEMENTALFUNCTIONNAME
  !!
  !! (We could protect against the function-stmt clash by checking whether 
  !! there are acceptable exponent characters (digits or a sign) after the 
  !! E (or D - but no valid function prefixes start with D), but in the 
  !! general case a real-literal with a missing exponent digit is a reasonably 
  !! likely typographical error, so the greedy matching is preferred.)
  !!
  !! Before calling the current character should be the digit character that 
  !! starts the token.  After calling the current character will be whatever 
  !! terminates the scan of the current token.
  
  SUBROUTINE get_digit_token(form, real_possible, err_list, next_token)
    
    USE CharacterTypes
    USE CharUtils
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    LOGICAL, INTENT(IN) :: real_possible
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT) :: next_token
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Error list for procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    TYPE(SourceChar) :: ch    ! Current char.
    
    ! Look ahead chars.
    TYPE(SourceChar) :: maybe_exp_char
    TYPE(SourceChar) :: maybe_sig_char
    
    ! Text assembly buffers
    TYPE(SourceChar), ALLOCATABLE :: raw_text(:)
    CHARACTER(:,KIND=scck), ALLOCATABLE :: value_text
    TYPE(SourceChar), ALLOCATABLE :: value_chars(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Create the initial token to remember the position only.  At this stage 
    ! it is an integer literal (a single digit).
    
    CALL peek_char(form, ch)
    
    ! Sanity check.
    IF (.NOT. IsDigitChar(ch%char)) THEN
      STOP 'Internal error in Scanner%%get_digit_token, &
          &first character of a integer literal scan is not a digit.'
    END IF
    
    next_token = Tkn(  &
        ittIntegerLiteral,  &
        ch%pos,  &
        ch%start_of_line,  &
        ch%end_of_line )
    
    !---------------------------------------------------------------------------
    ! Get the digit sequence.
    
    CALL get_digit_seq(form, sub_err_list, value_chars)
    CALL Add(err_list, sub_err_list)
    IF (Failure(sub_err_list)) RETURN
    CALL AppendRawValue(next_token, CharsToText(value_chars))
    
    ! Examine what follows the digit sequence
    CALL peek_char(form, ch)
    
    ! Digit sequence terminated by end of line.
    IF (ch%eos) RETURN
    
    IF (IsUnderscoreChar(ch%char)) THEN
      CALL AppendRaw(next_token, ch%char)
      
      ! dddd_
      ! This could be the underbar that separates an integer literal 
      ! from its kind, or maybe we just scanned the kind of a character 
      ! literal - check the next character to find out.
      CALL next_char(form, ch)
      
      IF (ch%eos) THEN
        ! dddd_
        CALL Add( err_list,  &
            CODE=errBadLiteralKind,  &
            LOCATION=ch%pos,  &
            COMPONENT='R408',  &   ! Syntax rule for /kind-param/.
            MSG='Invalid integer literal kind.' )
        RETURN
      END IF
      
      IF (.NOT. ch%got_amp) THEN
        STOP 'Internal error in Scanner%%get_digit_token, &
            &the programmer stopped writing code.'
        ! @todo Resolve.
        ! CALL form%SplitTokenError(next_token%chars, ch, err_list)
        ! CALL form%SplitTokenError(ch, err_list)
        RETURN
      END IF
      
      IF (IsQuoteChar(ch%char)) THEN
        ! dddd_' or dddd_" - must be a character literal
        next_token = ittCharacterLiteral
        CALL ValueToKind(next_token)
        CALL get_string_seq(form, sub_err_list, raw_text, value_text)
        CALL Add(err_list, sub_err_list)
        ! raw_text and value-text still have something in case of error.
        CALL AppendRaw(next_token, CharsToText(raw_text))
        CALL SetValue(next_token, value_text)
        IF (raw_text(SIZE(raw_text))%end_of_line) CALL SetLast(next_token)
        RETURN
      ELSE IF (IsDigitChar(ch%char) .OR. IsLetterChar(ch%char)) THEN
        ! dddd_d or dddd_a - must be an integer literal with kind.
        CALL get_numeric_kind_seq(form, sub_err_list, raw_text)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        CALL AppendRawKind(next_token, CharsToText(raw_text))
        RETURN
      ELSE
        ! dddd_?  We assume it was an integer literal with dodgy kind.
        CALL Add( err_list,  &
            CODE=errBadLiteralKind,  &
            LOCATION=ch%pos,  &
            COMPONENT='R408',  &   ! Syntax rule for /kind-param/.
            MSG='Invalid integer literal kind.' )
        RETURN
      END IF
    ELSE IF (IsExponentChar(ch%char)) THEN
      ! See comments in the procedure pre-amble.
      IF (real_possible) THEN
        ! ddddE or ddddD - must be a real constant.
        CALL get_real_literal(form, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        RETURN
      ELSE
        ! What looks like an exponent char isn't by context - just 
        ! return the integer literal that we have built up so far.
        RETURN
      END IF
    ELSE IF (ch%char == scck_'.') THEN
      ! dddd. 
      ! Might be a real constant 123.4 or might be something like 123.and..  
      ! To find out we need to inspect up to the next two characters.
      CALL peek_ahead_char(form, 1, maybe_exp_char)
      
      IF (is_end_token(maybe_exp_char)) THEN
        ! It was a real literal where the dot was at the end of the statement.
        CALL get_real_literal(form, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        RETURN
      END IF
      
      IF (IsDigitChar(maybe_exp_char%char)) THEN
        ! dddd.d - must be a real literal.
        CALL get_real_literal(form, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        RETURN
      END IF
      
      IF (IsExponentChar(maybe_exp_char%char)) THEN
        ! Need to check one more character as dddd.EQ. or similar is possible.
        CALL peek_ahead_char(form, 2, maybe_sig_char)
        IF (maybe_sig_char%eos) THEN
          ! dddd.E - this is an error but we let get_real_literal handle it.
          CALL get_real_literal(form, sub_err_list, next_token)
          CALL Add(err_list, sub_err_list)
          RETURN
        END IF
        
        IF ( IsSignChar(maybe_sig_char%char)  &
              .OR. IsDigitChar(maybe_sig_char%char) ) THEN
          ! dddd.E+ or dddd.Ed - must be a real literal.
          CALL get_real_literal(form, sub_err_list, next_token)
          CALL Add(err_list, sub_err_list)
          RETURN
        END IF
        
        ! Doesn't look like a real-literal.  Return our already built 
        ! integer-literal below.
      ELSE IF (IsLetterChar(maybe_exp_char%char)) THEN
        ! It was an integer literal with what looks like a dot operator 
        ! following.  ch is the dot.  Return our constructed integer 
        ! literal token below.
      ELSE
        ! Real literal terminating with a dot - ddd.?
        CALL get_real_literal(form, sub_err_list, next_token)
        CALL Add(err_list, sub_err_list)
        RETURN
      END IF
      RETURN
      
    ELSE IF (IsDigitChar(ch%char)) THEN
      ! The digit sequence scanner should never end on a digit.  In the 
      ! future, one exception might be that we treat a bad split token (no 
      ! following ampersand) as two separate tokens.
      STOP 'Internal error in Scannner%%get_digit_token, &
          &the digit sequence did not end when expected.'
    ELSE
      ! Digit sequence terminated by something that starts the next token 
      ! or acts as a token delimiter (a blank).
      RETURN
    END IF
    
  END SUBROUTINE get_digit_token
  
  
  !*****************************************************************************
  !!
  !> Scan something starting with a quote.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    next_token        The scanned token. This can only be a 
  !! character literal that doesn't have a kind-param.
  
  SUBROUTINE get_quote_token(form, err_list, next_token)
    
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT) :: next_token
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Text assembly buffers
    TYPE(SourceChar), ALLOCATABLE :: raw_text(:)
    CHARACTER(:,KIND=scck), ALLOCATABLE :: value_text
    
    !***************************************************************************
    
    ! err_list gets allocated by the call to get_string_seq.
    
    ! Save the starting position in the token to be built.
    CALL peek_char(form, ch)
    next_token = Tkn(  &
        ittCharacterLiteral,  &
        ch%pos,  &
        '',  &
        ch%start_of_line,  &
        ch%end_of_line )
    
    ! String sequences are always part of a character literal.  Character 
    ! literals may also contain a preceding kind, but that would initially 
    ! be detected as either an integer literal or an identifier.
    
    ! If ch isn't a quote then the following will through an internal error 
    ! for us anyway.
    CALL get_string_seq(form, err_list, raw_text, value_text)
    ! raw_text and vslue_text still allocated even if errors occurred.
    CALL AppendRaw(next_token, CharsToText(raw_text))
    CALL SetValue(next_token, value_text)
    IF (raw_text(SIZE(raw_text))%end_of_line) CALL SetLast(next_token)
    
  END SUBROUTINE get_quote_token
  
  
  !*****************************************************************************
  !!
  !> Scan something that starts with a letter that, at this stage, looks like 
  !! a bit like a name.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    next_token        The scanned token. This could be a 
  !! name, a character literal (with leading parameter kind) or a BOZ 
  !! constant.
  !!
  !! Before calling the current character should be the character that 
  !! starts the token that looks like a name.  After calling it will be 
  !! whatever follows (terminates) the token in next_token.
  
  SUBROUTINE get_name_token(form, err_list, next_token)
    
    USE CharacterTypes
    USE CharUtils
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT) :: next_token
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Raw text of the sequence.
    TYPE(SourceChar), ALLOCATABLE :: raw_seq(:)
    
    ! Value text of the sequence.
    CHARACTER(:,scck), ALLOCATABLE :: value_text
    
    ! Error list for child procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    CHARACTER :: boz_radix    ! Radix character for a BOZ literal.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Save the position in the token to be built.
    
    CALL peek_char(form, ch)
    
    ! Sanity check
    IF (.NOT. IsLetterChar(ch%char)) THEN
      STOP 'Internal error in Scanner%%get_name_token, &
          &first character of name scan is not a letter.'
    END IF
    
    next_token = Tkn(ittName, ch%pos, '', ch%start_of_line, ch%end_of_line)
    
    !---------------------------------------------------------------------------
    
    CALL get_name_seq(form, err_list, raw_seq)
    value_text = CharsToText(raw_seq)
    ! Set the token up as if it was a name token.  No further processing 
    ! is then required when if decide that it is a name token.
    CALL AppendRawValue(next_token, value_text)
    
    ! See what terminated the name.
    CALL peek_char(form, ch)
    
    IF (ch%eos) RETURN
    
    IF (IsQuoteChar(ch%char)) THEN
      ! Three options:
      ! - the name was the kind of a character literal
      ! - the name was the identifying character out the front of a 
      !   /boz-literal-constant/
      ! - the name was a keyword like STOP and that's followed by a 
      !   separate character literal token.
      
      ! If it is a char literal kind, then the last character of 
      ! the "name" retrieved above will be "_".
      IF (IsUnderscoreChar(raw_seq(SIZE(raw_seq))%char)) THEN
        ! Probably a kind.  Check that token splitting rules were followed.
        IF (.NOT. ch%got_amp) THEN
          CALL form%SplitTokenError(raw_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        next_token = ittCharacterLiteral
        ! Chop the last character (the underbar) out of the value.  Setting 
        ! the value to then set the kind and clear the value is a bit odd, 
        ! but that is the only way we currently have of setting the kind.
        value_text = QueryValue(next_token)
        CALL SetValue(next_token, value_text(:LEN(value_text)-1))
        CALL ValueToKind(next_token)
        CALL get_string_seq(form, sub_err_list, raw_seq, value_text)
        CALL Add(err_list, sub_err_list)
        ! raw_seq and value_text still have something in case of error.
        CALL AppendRaw(next_token, CharsToText(raw_seq))
        CALL SetValue(next_token, value_text)
        IF (raw_seq(SIZE(raw_seq))%end_of_line) CALL SetLast(next_token)
        IF (Failure(sub_err_list)) RETURN
      ELSE IF (LEN(value_text) == 1) THEN
        ! Probably a /boz-literal-constant/. Check token splitting rules.
        IF (.NOT. ch%got_amp) THEN
          CALL form%SplitTokenError(raw_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        SELECT CASE (value_text)
        CASE (scck_'b', scck_'B')
          boz_radix = 'B'
          CALL get_binary_seq(form, sub_err_list, raw_seq, value_text)
        CASE (scck_'o', scck_'O')
          boz_radix = 'O'
          CALL get_octal_seq(form, sub_err_list, raw_seq, value_text)
        CASE (scck_'z', scck_'Z')
          boz_radix = 'Z'
          CALL get_hex_seq(form, sub_err_list, raw_seq, value_text)
        CASE DEFAULT
          ! Reported against the start of the token.
          CALL Add( err_list,  &
              CODE=errSyntax,  &
              COMPONENT='R464/R465/R466',  &
              LOCATION=QueryLocation(next_token),  &
              MSG='''' // value_text // ''' is an invalid &
                &/boz-literal-constant/ type character.' )
          RETURN
        END SELECT
        CALL Add(err_list, sub_err_list)
        ! Set the token characteristics before we bail.
        CALL AppendRaw(next_token, CharsToText(raw_seq))
        CALL SetValue(next_token, boz_radix)
        CALL ValueToKind(next_token)
        CALL SetValue(next_token, value_text)
        next_token = ittBOZLiteral
        IF (Failure(sub_err_list)) RETURN
      ELSE
        ! Leave this as a separate name token followed perhaps by a 
        ! char literal, and see what happens.
      END IF
    END IF
    
  END SUBROUTINE get_name_token
  
  
  !*****************************************************************************
  !!
  !> Scan a real literal, starting from a relatively arbitrary position in 
  !! the literal.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[in,out] next_token        On input - the text part of this 
  !! token must have been initialised to the text that has been scanned up to 
  !! (but not including - that goes in @a ch) the point that it was apparent 
  !! we were dealing with a real literal.  This can be a zero length string.  
  !! On output contains the real literal.
  !!
  !! Only ever returns a real literal or an error.
  !!
  !! Before calling the current character should be whatever convinced us 
  !! that we are dealing with a real literal.  After calling it will be 
  !! the character that terminated the real literal.
  
  SUBROUTINE get_real_literal(form, err_list, next_token)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(INOUT) :: next_token
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Note the following should really be named "done_xxx" or similar - they 
    ! indicate the state of the parser, rather than what we have actually 
    ! seen.
    LOGICAL :: got_dot        ! Have we seen the decimal separator?
    LOGICAL :: got_fraction   ! Have we seen the fraction part?
    LOGICAL :: got_exponent   ! Have we seen the exponent part?
    LOGICAL :: got_kind       ! Have we seen a kind?
    
    ! Source characters retrieved in total for the literal so far.  This is 
    ! currently incomplete, pending a little redesign of the higher level 
    ! tokenizing routines to work with SourceChars rather than text.
    TYPE(SourceChar), ALLOCATABLE :: all_chars(:)
    
    ! Source characters retrieved for a part of the real literal.
    TYPE(SourceChar), ALLOCATABLE :: sub_chars(:)
    
    ! Exponent source character - saved for error reporting.
    TYPE(SourceChar) :: exp_char
    
    LOGICAL :: got_amp
    LOGICAL :: end_of_statement
    
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************    
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Sanity check
    
    CALL peek_char(form, ch)
    IF ( ch%char /= scck_'.'  &
        .AND. (.NOT. IsExponentChar(ch%char))  &
        .AND. (.NOT. IsSignChar(ch%char))  &
        .AND. (.NOT. IsDigitChar(ch%char)) ) THEN
      STOP 'Internal error in Scanner%%get_real_literal, &
          &first character of real literal constant scan is not a valid &
          &part of a real-literal-constant.'
    END IF
    
    !---------------------------------------------------------------------------
    
    ! Might have already been set, but if not...
    next_token = ittRealLiteral
    
    got_dot = SCAN(scck_'.', QueryValue(next_token)) /= 0
    got_fraction = .FALSE.
    got_exponent = .FALSE.
    got_kind = .FALSE.
    
    end_of_statement = .FALSE.
    got_amp = .TRUE.
    
    ALLOCATE(all_chars(0))
    
    DO
      
      ! Test if literal ends at the end of line.
      ! dddd.      or dddd.Esdd         or dddd.Esdd_kind
      !      ^                 ^                         ^
      IF (ch%eos) THEN
        ! For completeness - in case we are called without this definitively 
        ! being a real literal.
        IF ((.NOT. got_dot) .AND. (.NOT. got_exponent)) THEN
          STOP 'Internal error in Scanner%%get_real_literal, &
              &a real literal scan tried to scan something that wasn''t &
              &a real literal'
        END IF
      END IF
      
      IF (.NOT. ch%got_amp) THEN
        ! @todo The sub_text argument is not right.
        CALL form%SplitTokenError(all_chars, ch, err_list)
        IF (Failure(err_list)) RETURN
      END IF
      
      ! Test:
      ! dddd.  or dddd
      !     ^        ^
      IF (.NOT. got_dot) THEN
        IF (IsDigitChar(ch%char)) THEN
          ! We wouldn't expect this to be activated, because other scanning 
          ! routines would find it hard to identify that they were conclusively 
          ! dealing with a real literal without having read the leading digit 
          ! sequence.  So this is just to be complete.
          CALL get_digit_seq(form, sub_err_list, sub_chars)
          CALL Add(err_list, sub_err_list)
          IF (Failure(sub_err_list)) RETURN
          
          all_chars = [all_chars, sub_chars]
          CALL AppendRawValue(next_token, CharsToText(sub_chars))
          ! Loop to test the new character after the initial digit sequence.
          CALL peek_char(form, ch)
          CYCLE
        END IF
        
        ! We expect that the dot is the first character, or it is not present 
        ! in the literal.
        got_dot = .TRUE.
        IF (ch%char == scck_'.') THEN
          CALL append_char(all_chars, ch)
          CALL AppendRawValue(next_token, ch%char)
          CALL next_char(form, ch)
          ! Loop to test the new character after the dot.
          CYCLE
        END IF
      END IF
      
      ! Test:
      ! dddd.dddd
      !      ^
      IF (.NOT. got_fraction .AND. IsDigitChar(ch%char)) THEN
        CALL get_digit_seq(form, sub_err_list, sub_chars)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        all_chars = [all_chars, sub_chars]
        CALL AppendRawValue(next_token, CharsToText(sub_chars))
        got_fraction = .TRUE.
        ! Loop to test the new character after the fraction.
        CALL peek_char(form, ch)
        CYCLE
      END IF
      
      ! Test:
      ! dddd.ddddE
      !          ^
      IF (.NOT. got_exponent .AND. IsExponentChar(ch%char)) THEN
        ! Save for D/E kind checking and error reporting.
        exp_char = ch
        CALL get_real_exponent_seq(form, sub_err_list, sub_chars)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        all_chars = [all_chars, sub_chars]
        CALL AppendRawValue(next_token, CharsToText(sub_chars))
        got_exponent = .TRUE.
        got_fraction = .TRUE.
        ! Loop to test the new character after the exponent.
        CALL peek_char(form, ch)
        CYCLE
      END IF
      
      IF (.NOT. got_kind .AND. IsUnderscoreChar(ch%char)) THEN
        CALL append_char(all_chars, ch)
        CALL AppendRaw(next_token, ch%char)
        
        CALL next_char(form, ch)
        
        IF (ch%eos) THEN
          CALL Add( err_list,  &
              CODE=errBadLiteralKind,  &
              COMPONENT='R408',  &
              LOCATION=ch%pos,  &
              MSG='Invalid real literal kind.' )
          RETURN
        END IF
        
        IF (.NOT. ch%got_amp) THEN
          ! @todo The sub_text argument is not correct.
          CALL form%SplitTokenError(all_chars, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        IF (IsDigitChar(ch%char) .OR. IsLetterChar(ch%char)) THEN
          CALL get_numeric_kind_seq(form, sub_err_list, sub_chars)
          CALL Add(err_list, sub_err_list)
          IF (Failure(sub_err_list)) RETURN
          all_chars = [all_chars, sub_chars]
          CALL AppendRawKind(next_token, CharsToText(sub_chars))
          
          IF (got_exponent) THEN
            IF ( (exp_char%char /= scck_'E')  &
                .AND. (exp_char%char /= scck_'e') ) THEN
              CALL Add( err_list,  &
                  CODE=errWrongExponentLetter,  &
                  COMPONENT='C412 on R413',  &
                  LOCATION=exp_char%pos,  &
                  MSG='An exponent letter of E must be used if the literal &
                    &constant includes a kind-param.' )
              ! No immediate return.
            END IF
          END IF
          
          ! Update state.
          got_exponent = .TRUE.
          got_fraction = .TRUE.
          got_kind = .TRUE.
          
          ! Loop to test the new character after the kind.  This cannot be 
          ! part of this literal, but we do this cycle anyway, for reasons 
          ! that escape me.
          CALL peek_char(form, ch)
          CYCLE
        ELSE
          CALL Add( err_list,  &
              CODE=errBadLiteralKind,  &
              COMPONENT='R408',  &
              LOCATION=ch%pos,  &
              MSG='Invalid real literal kind.' )
          RETURN
        END IF
        
      END IF
      
      ! Any other character terminates the token.
      RETURN
    END DO
    
  END SUBROUTINE get_real_literal
  
  
  !*****************************************************************************
  !!
  !> Gets a token that starts with a dot.
  !!
  !! @param[in,out] form              The source form object.
  !!
  !! @param[in,out] ch                On input - the dot that starts this 
  !! scan.  On output the character that terminates the scan.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    next_token        The token that started with a dot.  This 
  !! could be a real literal, a logical literal (.TRUE. or .FALSE.) or an 
  !! operator of the form .lll..
  !!
  !! In the cases of the logical literal and user defined operator, the token 
  !! text includes the delimiting dots.
  !!
  !! Before calling the current character should be the character that starts 
  !! the scan.  On output it will be the character that terminates the scan.
  
  SUBROUTINE get_dot_token(form, err_list, next_token)
    
    USE CharacterTypes
    USE CharUtils
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(Token), INTENT(OUT) :: next_token
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Dot character that starts the token.
    TYPE(SourceChar) :: dot_ch
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Child sequence characters.
    TYPE(SourceChar), ALLOCATABLE :: char_seq(:)
    
    ! Token value or child sequence text.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: text
    
    ! Error list for child procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Create an arbitrary token 
    
    CALL get_char(form, dot_ch)
    
    ! Sanity check.
    IF (dot_ch%char /= scck_'.') THEN
      STOP 'Internal error in Scanner%%get_dot_token, &
          &First character of dot token scan is not a ''.'''
    END IF
    
    next_token = Tkn(ittNFI, dot_ch)
    
    !---------------------------------------------------------------------------
    
    CALL peek_char(form, ch)
    
    ! Test if dot at the end of line.  If it is then we have no idea what 
    ! sort of token it is.
    IF (ch%eos) THEN
      CALL Add( err_list,  &
          CODE=errSyntax,  &
          LOCATION=ch%pos,  &
          COMPONENT='3.2.1',  &     ! Make 'em read the whole chapter.
          MSG="'" // ch%char //  ''' on its own is not a valid source token.' )
      RETURN
    END IF
    
    IF (.NOT. ch%got_amp) THEN
      CALL form%SplitTokenError([dot_ch], ch, err_list)
      IF (Failure(err_list)) RETURN
    END IF
    
    ! Note a real literal that looks like .E4 is not valid.
    IF (IsDigitChar(ch%char)) THEN
      ! .d
      CALL get_real_literal(form, sub_err_list, next_token)
      CALL Add(err_list, sub_err_list)
      RETURN
    ELSE IF (IsLetterChar(ch%char)) THEN
      ! .a
      CALL get_dot_seq(form, sub_err_list, char_seq)
      CALL Add(err_list, sub_err_list)
      IF (Failure(sub_err_list)) RETURN
      CALL AppendRawValue(next_token, CharsToText(char_seq))
      
      text = QueryValue(next_token)
      IF ((text == '.FALSE.') .OR. (text == '.TRUE.')) THEN
        next_token = ittLogicalLiteral
        ! Might have a kind.
        CALL peek_char(form, ch)
        IF (ch%eos) RETURN
        
        IF (IsUnderscoreChar(ch%char)) THEN
          
          IF (.NOT. ch%got_amp) THEN
            CALL form%SplitTokenError(char_seq, ch, err_list)
            RETURN
          END IF
          
          ! We probably have a kind.
          CALL AppendRaw(next_token, ch%char)
          CALL next_char(form, ch)
          ! Filter the initial character passed to get_numeric_kind_seq or an 
          ! internal error (STOP) will result.
          IF (.NOT. is_valid_kind_char(ch)) THEN
            ! .xxx._
            CALL Add( err_list,  &
                CODE=errBadLiteralKind,  &
                LOCATION=ch%pos,  &
                COMPONENT='R408',  &      ! syntax rule for /kind-param/.
                MSG='Invalid logical literal kind.' )
            RETURN
          END IF
          
          CALL get_numeric_kind_seq(form, sub_err_list, char_seq)
          CALL Add(err_list, sub_err_list)
          IF (Failure(sub_err_list)) RETURN
          CALL AppendRawKind(next_token, CharsToText(char_seq))
          
          RETURN
        END IF
      ELSE
        next_token = ittOperator
      END IF
      RETURN
    ELSE
      ! Something that can't follow a dot followed a dot.  This is not 
      ! the most helpful error message.  @todo Fix.
      CALL Add( err_list,  &
          CODE=errSyntax,  &
          LOCATION=ch%pos,  &
          COMPONENT='3.2',  &
          MSG='The character ''' // ch%char // ''' was unexpected.' )
      RETURN
    END IF
    
  END SUBROUTINE get_dot_token
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Low level sequence collection procedures.
  
  
  !*****************************************************************************
  !!
  !> Gets a sequence .llll., where l is a letter, and the next character to 
  !! be read from the source form is the first l.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    char_seq          The character sequence read from 
  !! @a form, without the surrounding dots.
  !!
  !! If the sequence doesn't end in a dot an error occurs.
  !!
  !! This is useful for reading in tokens that represent user defined 
  !! operators.
  !!
  !! Before calling the current character should be the letter that follows 
  !! the dot.  After calling it will be the character that terminated the 
  !! scan.  In the absence of error this will be the character that followed 
  !! the dot.
  
  SUBROUTINE get_dot_seq(form, err_list, char_seq)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    !***************************************************************************
    
    CALL peek_char(form, ch)
    
    IF (.NOT. IsLetterChar(ch%char)) THEN
      STOP 'Internal error in Scanner%%get_dot_seq, &
          &first character of dot sequence scan is not a letter.'
    END IF
    
    CALL get_letter_seq(form, err_list, char_seq)
    IF (Failure(err_list)) RETURN
    
    CALL peek_char(form, ch)
    IF (ch%eos) THEN
      CALL Add( err_list,  &
          CODE=errSyntax,  &
          LOCATION=ch%pos,  &
          COMPONENT='3.2',  &
          MSG='Encountered end-of-statement when expecting ''.''.' )
      RETURN
    END IF
    
    CALL advance_char(form)
    
    IF (ch%char /= scck_'.') THEN
      CALL Add( err_list,  &
          CODE=errSyntax,  &
          COMPONENT='3.2',  &
          LOCATION=ch%pos,  &
          MSG='Encountered ''' // ch%char // '''when expecting ''.''.' )
      RETURN
    END IF
    
    CALL append_char(char_seq, ch)
    
  END SUBROUTINE get_dot_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a sub-token that could be a numeric kind.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    char_seq          The character sequence read from @a form.
  !!
  !! Things that could be numeric kinds could also be character kinds, but 
  !! numeric kinds come after the number, while character kinds come before.
  !!
  !! Before calling the current character should be the character that 
  !! starts the kind specifier - a digit or a letter.  This should be the 
  !! character after the underscore.  After calling this will be the 
  !! character that terminated the scan.  In the absence of error this will 
  !! be the character that follows the kind specifier.
  
  SUBROUTINE get_numeric_kind_seq(form, err_list, char_seq)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Error list for procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    CALL peek_char(form, ch)
    
    IF (IsDigitChar(ch%char)) THEN
      CALL get_digit_seq(form, sub_err_list, char_seq)
    ELSE IF (IsLetterChar(ch%char)) THEN
      CALL get_name_seq(form, sub_err_list, char_seq)
    ELSE
      STOP 'Internal error in Scanner%%get_numeric_kind_seq, &
          &first character of numeric kind scan is not valid for a &
          &numeric kind.'
    END IF
    CALL Add(err_list, sub_err_list)
    
  END SUBROUTINE get_numeric_kind_seq
  
  
  !*****************************************************************************
  !!
  !> Get a sequence of characters that make up the exponent-letter and 
  !! exponent part of a real-literal-constant.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    char_seq          The character sequence read from 
  !! @a form.  This includes the exponent-letter given in @a ch on input.
  !!
  !! Before calling the current character should be the D or E character 
  !! that starts the exponent.  After calling this will be the character 
  !! that terminated the scan.  In the absence of error this will be the 
  !! character that follows the numeric part of the exponent.
  
  SUBROUTINE get_real_exponent_seq(form, err_list, char_seq)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Flag to indicate whether we've seen the sign part of the 
    ! signed-digit-string that makes up the exponent part of the exponent.
    LOGICAL :: got_sign
    
    ! The character sequence of the digits that make up the exponent.
    TYPE(SourceChar), ALLOCATABLE :: digit_seq(:)
    
    ! Error list for procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    CALL get_char(form, ch)
    IF (.NOT. IsExponentChar(ch%char)) THEN
      STOP 'Internal error in Scanner%%get_real_exponent_seq, &
          &First character of exponent sequence scan is not an &
          &exponent-letter.'
    END IF
    
    !---------------------------------------------------------------------------
    
    char_seq = [ch]
    got_sign = .FALSE.
    
    DO
      
      CALL peek_char(form, ch)
      
      IF (ch%eos) THEN
        CALL Add( err_list,  &
            CODE=errMissingExponent,  &
            COMPONENT='R416',  &
            LOCATION=ch%pos,  &
            MSG='Real literal is missing its exponent digits.' )
        RETURN
      END IF
      
      IF (.NOT. got_sign .AND. IsSignChar(ch%char)) THEN
        CALL advance_char(form)
        
        IF (.NOT. ch%got_amp) THEN
          CALL form%SplitTokenError(char_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        CALL append_char(char_seq, ch)
        got_sign = .TRUE.
        CYCLE
      END IF
      
      IF (IsDigitChar(ch%char)) THEN
        IF (.NOT. ch%got_amp) THEN
          CALL form%SplitTokenError(char_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        CALL get_digit_seq(form, sub_err_list, digit_seq)
        CALL Add(err_list, sub_err_list)
        IF (Failure(sub_err_list)) RETURN
        
        char_seq = [char_seq, digit_seq]
        RETURN
      END IF
      
      ! Something else
      CALL Add( err_list,  &
          CODE=errMissingExponent,  &
          COMPONENT='R416',  &
          LOCATION=ch%pos,  &
          MSG='Real literal is missing its exponent digits.' )
      RETURN
      
    END DO
    
  END SUBROUTINE get_real_exponent_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a sequence of digits that finishes with anything that isn't a 
  !! digit.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    char_seq          The sequence of characters read from 
  !! @a form, including the input value of @a ch.
  !!
  !! Before calling the current character should be the first digit in the 
  !! sequence.  After calling the current character will be the character 
  !! that terminated the scan.  In the absence of error this will not be 
  !! a digit.
  
  SUBROUTINE get_digit_seq(form, err_list, char_seq)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    CALL get_char(form, ch)
    IF (.NOT. IsDigitChar(ch%char)) THEN
      STOP 'Internal error in Scanner%%get_digit_seq, &
          &first character of digit sequence scan is not a digit.'
    END IF
    
    !---------------------------------------------------------------------------
    
    char_seq = [ch]
    
    DO
      CALL peek_char(form, ch)
      
      ! End of line is end of sequence.
      IF (ch%eos) RETURN
      
      IF (.NOT. IsDigitChar(ch%char)) THEN
        RETURN
      ELSE
        CALL advance_char(form)
        
        IF (.NOT. ch%got_amp) THEN
          CALL form%SplitTokenError(char_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        CALL append_char(char_seq, ch)
      END IF
    END DO
    
  END SUBROUTINE get_digit_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a sequence of characters that starts with a quote and finishes with 
  !! the same sort of quote.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.  Errors can include 
  !! end of statement before end of string, plus line continuation issues.  
  !! @a char_seq and @a text still contain some useful information regardless.
  !!
  !! @param[out     char_seq          The raw character sequence, including 
  !! the delimiting quotes and without any transformation of doubled quote 
  !! sequences.  Because this includes the delimiting quotes, and this 
  !! procedure MUST only be called when the first delimiting quote is the 
  !! current character, this will always be allocated and always with a 
  !! length greater than one on return.
  !!
  !! @param[out]    text              The text read from @a form, without 
  !! the delimiting quotes.  Two quotes in a row get transformed into a 
  !! single quote, as per the Fortran quote escaping rules.  Always allocated, 
  !! but may be zero length.
  !!
  !! A string is only ever the last part of a lexical token (it may also be 
  !! the only part) - so we don't have to worry about the no_amp argument.
  !!
  !! Before calling the current character should be the quote character 
  !! (double or single) that starts the string sequence.  After calling 
  !! the current character will be the character that terminated the 
  !! scan.  In the absence of error this will be the character following 
  !! the closing quote.
  
  SUBROUTINE get_string_seq(form, err_list, char_seq, text)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    CHARACTER(:,KIND=scck), INTENT(OUT), ALLOCATABLE :: text
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! The delimiting quote character.
    CHARACTER(KIND=scck) :: quote
    
    ! Set to .TRUE. if we encounter a quote inside the literal, which might 
    ! be the end of literal or a doubled character to represent a single quote 
    ! character.
    LOGICAL :: got_quote
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ! save the opening quote.
    CALL get_char(form, ch)
    quote = ch%char
    IF (.NOT. IsQuoteChar(quote)) THEN
      STOP 'Internal error in Scanner%%get_string_seq, &
          &first character of character string scan is not a quote.'
    END IF
    
    char_seq = [ch]
    text = ''
    got_quote = .FALSE.
    
    !---------------------------------------------------------------------------
    
    DO
      CALL peek_char(form, ch)
      
      IF (got_quote) THEN
        ! If that's it - then that's it.
        IF (ch%eos) THEN
          ! If the end of statement marker also indicated end of line, 
          ! then transfer that end of statement marker to the previous 
          ! character.  This is a hack to try and get the end of line 
          ! marker working for the char literal that appears in an 
          ! include line.
          char_seq(SIZE(char_seq))%end_of_line = ch%end_of_line
          RETURN
        END IF
        
        ! If not a quote, then that's it.
        IF (ch%char /= quote) RETURN
        
        CALL advance_char(form)
        
        ! If not continued properly - error.
        IF (.NOT. ch%got_amp) THEN
          ! Continued character constants must start with an ampersand.
          CALL form%SplitCharLiteralError(char_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        ! Add this character (the quote) to the literal below.
        got_quote = .FALSE.
      ELSE
        
        IF (ch%eos) THEN
          ! End of line reached unexpectedly
          CALL Add( err_list,  &
              CODE=errUnterminatedCharLiteral,  &
              COMPONENT='R423',  &
              LOCATION=ch%pos,  &
              MSG='Unterminated character literal.' )
          RETURN
        END IF
        
        CALL advance_char(form)
        
        IF (.NOT. ch%got_amp) THEN
          ! Continued character constants must start with an ampersand.
          CALL form%SplitCharLiteralError(char_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        IF (ch%char == quote) THEN
          got_quote = .TRUE.
          CALL append_char(char_seq, ch)
          CYCLE
        END IF
        
        ! Add this character to the literal below.
      END IF
      
      ! Just add the character
      CALL append_char(char_seq, ch)
      text = text // ch%char
    END DO
    
  END SUBROUTINE get_string_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a sequence of name characters that finishes with anything that isn't 
  !! a name character.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    char_seq          The character sequence read from 
  !! @a form, including the initial character in @a ch.
  !!
  !! Before calling the current character must be the first character of 
  !! the name sequence.  After calling the current character will be the 
  !! character that terminated the scan.  In the absence of error this will 
  !! not be a letter, a digit or underscore.
  
  SUBROUTINE get_name_seq(form, err_list, char_seq)
    
    USE CharacterTypes
    USE Errors
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Sanity check
    
    CALL get_char(form, ch)
    IF (.NOT. IsLetterChar(ch%char)) THEN
      STOP 'Internal error in Scanner%%get_name_seq, &
          &first character of name sequence scan is not a letter.'
    END IF
    
    char_seq = [ch]
    
    DO
      CALL peek_char(form, ch)
      
      ! End of line is end of sequence.
      IF (ch%eos) RETURN
      
      IF (.NOT. IsNameChar(ch%char)) THEN
        RETURN
      ELSE
        CALL advance_char(form)
        
        ! If not continued properly - error.
        IF (.NOT. ch%got_amp) THEN
          ! Continued character constants must start with an ampersand.
          CALL form%SplitTokenError(char_seq, ch, err_list)
          IF (Failure(err_list)) RETURN
        END IF
        
        CALL append_char(char_seq, ch)
      END IF
    END DO
    
  END SUBROUTINE get_name_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a sequence of letters that finishes with anything that isn't 
  !! a letter.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[out]    char_seq          The character sequence read from 
  !! @a form, including the initial character in @a ch.
  !!
  !! This is useful for reading the internals of a user defined (or .text. 
  !! based intrinsic) operator.
  !!
  !! Before calling the current character must be the first character of 
  !! the letter sequence.  After calling the current character will be 
  !! the letter that terminated the scan.  In the absence of error this 
  !! will not be a letter.
  
  SUBROUTINE get_letter_seq(form, err_list, char_seq)
    
    USE CharacterTypes
    USE Errors  
    USE ErrorCodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(INOUT), ALLOCATABLE :: char_seq(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    CALL get_char(form, ch)
    IF (.NOT. IsLetterChar(ch%char)) THEN
      STOP 'Internal error in Scanner%%get_letter_seq, &
          &first character of letter sequence scan is not a letter.'
    END IF
    
    char_seq = [ch]
    
    DO
      CALL peek_char(form, ch)
      
      ! End of line is end of sequence.
      IF (ch%eos) RETURN
      
      IF (.NOT. IsLetterChar(ch%char)) THEN
        RETURN
      ELSE
        CALL advance_char(form)
        
        ! If not continued properly - error.
        IF (.NOT. ch%got_amp) THEN
          ! Continued character constants must start with an ampersand.
          CALL form%SplitCharLiteralError(char_seq, ch, err_list)
          RETURN
        END IF
        
        CALL append_char(char_seq, ch)
      END IF
    END DO
    
  END SUBROUTINE get_letter_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a character string that could represent a binary literal-constant.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[in,out] char_seq          The "raw" character sequence of the 
  !! binary constant - case may vary, includes delimiters.
  !!
  !! @param[out]    value             The value text read from @a form, 
  !! without the delimiting quotes.
  !!
  !! Before calling the current character must be the quote that delimits 
  !! the start of the binary literal constant.  After calling the current 
  !! character will be the character that terminated the literal scan.  
  !! In the absence of error this will be the character following the 
  !! terminating quote.
  
  SUBROUTINE get_binary_seq(form, err_list, char_seq, value)
    
    USE Errors
    USE ErrorCodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    CHARACTER(:,KIND=scck), INTENT(OUT), ALLOCATABLE :: value
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Position of the starting quote of the binary literal.
    TYPE(SourceLocation) :: initial_pos
    
    ! Index of the first invalid character.
    INTEGER :: bad
    
    !***************************************************************************
    
    ! err_list allocated by call to get_string_seq.
    
    ! Save initial position for error reporting.
    CALL peek_char(form, ch)
    initial_pos = ch%pos
    
    CALL get_string_seq(form, err_list, char_seq, value)
    IF (Failure(err_list)) RETURN
    
    bad = VERIFY(value, scck_'01')
    IF (bad /= 0) THEN
      ! We report the error against the starting quote of the binary literal,
      ! rather than the actual character that caused the error.
      CALL Add( err_list,  &
          CODE=errBadBinaryLiteral,  &
          COMPONENT='R464',  &
          LOCATION=initial_pos,  &
          MSG='The character ''' // value(bad:bad) // ''' is not valid in &
            &a binary constant.' )
      RETURN
    END IF
    
  END SUBROUTINE get_binary_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a character string that could represent an octal literal-constant.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[in,out] char_seq          The "raw" character sequence of the 
  !! octal constant - case may vary, includes delimiters.
  !!
  !! @param[out]    value             The value text read from @a form, 
  !! without the delimiting quotes.
  !!
  !! Before calling the current character must be the quote that delimits 
  !! the start of the octal literal constant.  After calling the current 
  !! character will be the character that terminated the literal scan.  
  !! In the absence of error this will be the character following the 
  !! terminating quote.
  
  SUBROUTINE get_octal_seq(form, err_list, char_seq, value)
    
    USE Errors
    USE ErrorCodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    CHARACTER(:,KIND=scck), INTENT(INOUT), ALLOCATABLE :: value
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Position of the starting quote of the octal literal.
    TYPE(SourceLocation) :: initial_pos
    
    ! Index of the first invalid character
    INTEGER :: bad
    
    !***************************************************************************
    
    ! err_list allocated by call to get_string_seq.
    
    ! Save initial position for error reporting.
    CALL peek_char(form, ch)
    initial_pos = ch%pos
    
    CALL get_string_seq(form, err_list, char_seq, value)
    IF (Failure(err_list)) RETURN
    
    bad = VERIFY(value, scck_'01234567')
    IF (bad /= 0) THEN
      ! We report the error against the starting quote of the octal literal,
      ! rather than the actual character that caused the error.
      CALL Add( err_list,  &
          CODE=errBadOctalLiteral,  &
          COMPONENT='R465',  &
          LOCATION=initial_pos,  &
          MSG='The character ''' // value(bad:bad) // ''' is not valid in &
            &an octal constant.' )
      RETURN
    END IF
    
  END SUBROUTINE get_octal_seq
  
  
  !*****************************************************************************
  !!
  !> Gets a character string that could represent a hexadecimal 
  !! literal-constant.
  !!
  !! @param[in,out] form              The source form to read from.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @param[in,out] char_seq          The "raw" character sequence of the 
  !! hexadecimal constant - case may vary, includes delimiters.
  !!
  !! @param[out]    value             The value text read from @a form, 
  !! without the delimiting quotes.
  !!
  !! Before calling the current character must be the quote that delimits 
  !! the start of the hecadecimal literal constant.  After calling the 
  !! current character will be the character that terminated the literal 
  !! scan.  In the absence of error this will be the character following the 
  !! terminating quote.
  
  SUBROUTINE get_hex_seq(form, err_list, char_seq, value)
    
    USE Errors
    USE ErrorCodes
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SourceFormBase), INTENT(INOUT) :: form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    TYPE(SourceChar), INTENT(OUT), ALLOCATABLE :: char_seq(:)
    CHARACTER(:,KIND=scck), INTENT(INOUT), ALLOCATABLE :: value
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(SourceChar) :: ch    ! Current character.
    
    ! Position of the starting quote of the octal literal.
    TYPE(SourceLocation) :: initial_pos
    
    ! Index of the first invalid character
    INTEGER :: bad
    
    !***************************************************************************
    
    ! err_list allocated by call to get_string_seq.
    
    ! Save initial position for error reporting.
    CALL peek_char(form, ch)
    initial_pos = ch%pos
    
    CALL get_string_seq(form, err_list, char_seq, value)
    IF (Failure(err_list)) RETURN
    
    bad = VERIFY(value, scck_'0123456789abcdefABCDEF')
    IF (bad /= 0) THEN
      ! We report the error against the starting quote of the hex literal, 
      ! rather than the actual character that caused the error.
      CALL Add( err_list,  &
          CODE=errBadHexLiteral,  &
          COMPONENT='R466',  &
          LOCATION=initial_pos,  &
          MSG='The character ''' // value(bad:bad) // ''' is not valid in &
            &a hexadecimal constant.' )
      RETURN
    END IF
    
  END SUBROUTINE get_hex_seq
  
  
  !*****************************************************************************
  !!
  !> Test to see if the source character is something that terminates a 
  !! token.
  !!
  !! @param[in]     ch                The source char to test.
  !!
  !! @returns .TRUE. if the char represents the end of statement or 
  !! a blank.
  
  ELEMENTAL FUNCTION is_end_token(ch) RESULT(b)
    
    USE CharacterTypes
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(SourceChar), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = ch%eos
    IF (b) RETURN
    b = ch%char == scck_';'
    b = IsBlankChar(ch%char)
    
  END FUNCTION is_end_token
  
  
  !*****************************************************************************
  !!
  !> Test to see if the source character is something that could be part of 
  !! a numeric kind.
  !!
  !! @param[in]     ch                The source character to test.
  !!
  !! This procedure is robust to ch being end-of-statement etc.  The intent is 
  !! that this can be used as a filter on the initial character prior to 
  !! calling get_numeric_kind_seq.
  !!
  !! Valid kind characters are letters or digits.
  
  ELEMENTAL FUNCTION is_valid_kind_char(ch) RESULT(b)
    
    USE CharacterTypes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(SourceChar), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = .NOT. ch%eos
    IF (.NOT. b) RETURN
    
    b = IsDigitChar(ch%char)
    IF (b) RETURN
    
    b = IsLetterChar(ch%char)
    IF (b) RETURN
    
  END FUNCTION is_valid_kind_char
  
  
  !*****************************************************************************
  !!
  !> Test whether we are in a character context.
  !!
  !! @param[in]     in_char_literal   The character literal scanning status
  !! as reported by the Scanner in the call to the GetChar binding.
  !!
  !! @param[in]     context_chars     The characters that are still considered
  !! part of the character literal.  If zero length then all characters are
  !! considered part of the literal.
  !!
  !! @param[in]     ch                The character being considered.
  !!
  !! @returns .TRUE. if a character context is still current, .FALSE. 
  !! otherwise.
  !!
  !! In some circumstances (after the quote character that opened the 
  !! character literal) whether we are in a character literal depends on the
  !! particular character we are scanning.
  
  ELEMENTAL FUNCTION TestCharacterContext_( in_character_context,  &
      context_chars, ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    LOGICAL, INTENT(IN) :: in_character_context
    CHARACTER(*,KIND=scck), INTENT(IN) :: context_chars
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = in_character_context
    IF (.NOT. b) RETURN
    
    b = (LEN(context_chars) == 0)
    IF (b) RETURN
    
    b = SCAN(ch, context_chars) /= 0
    
  END FUNCTION TestCharacterContext_
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! (More) Token constructors
  
  
  !*****************************************************************************
  !!
  !> Create a token using a source char.
  !!
  !! The character is added to the main text of the token.  The kind text
  !! is initialise to be empty.
  !!
  !! @param[in]     type              The type of token to create.
  !!
  !! @param[in]     ch                The initial raw and value text of 
  !! the token.
  !!
  !! @returns An appropriately constructed Token.
  
  FUNCTION Token_ch(type, ch) RESULT(t)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: type
    TYPE(SourceChar), INTENT(IN) :: ch
    
    ! Function result
    TYPE(Token) :: t
    
    !***************************************************************************
    
    ! Tkn constructors cannot be pure due to SourceLocation component.
    t = Tkn(type, ch%pos, ch%char, ch%start_of_line, ch%end_of_line)
    
  END FUNCTION Token_ch
  
  
  !*****************************************************************************
  !!
  !> Convert an array of source characters into a character string.
  !!
  !! @param[in]     array             The array of source characters.
  !!
  !! @returns The text in @a array.
  
  FUNCTION CharsToText_(array)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(SourceChar), INTENT(IN) :: array(:)
    
    ! Function result.
    CHARACTER(SIZE(array),KIND=scck) :: CharsToText_
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Character index.
    
    !***************************************************************************
    
    FORALL (i=1:SIZE(array)) CharsToText_(i:i) = array(i)%char
    
  END FUNCTION CharsToText_
  
  
  !*****************************************************************************
  !!
  !> Append a source character to a vector of source characters.
  !!
  !! @param[in,out] vector            The vector of characters to append 
  !! to.
  !!
  !! @param[in]     ch                The character to append.
  !!
  !! This is equivalent to `vector = [vector, ch]`.  We split it out to 
  !! accomodate any compiler or performance issues with that particular 
  !! form.
  
  SUBROUTINE append_char(vector, ch)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(SourceChar), INTENT(INOUT), ALLOCATABLE :: vector(:)
    TYPE(SourceChar), INTENT(IN) :: ch
    
    !***************************************************************************
    
    vector = [vector, ch]
    
  END SUBROUTINE append_char
  
END MODULE Scanner
