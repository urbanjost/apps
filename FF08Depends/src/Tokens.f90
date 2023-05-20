! $Id: Tokens.f90 2802 2019-03-22 18:39:26Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Tokens module.


!*******************************************************************************
!!
!> Defines the Token type and associated procedures to store the fundamental 
!! lexical tokens of the language, as described in Clause 3.2.
!!
!! To test the type of a token, simply use the == or /= operators to compare 
!! it to an integer.
!!
!! To test the value text of a token, simply use the == or /= operators 
!! to compare it to a character string.
!!
!! This is a relatively fundamental module - so limit its dependencies.
!!
!! Some tokens (symbols type tokens, mostly) have multiple types in the 
!! standard.  For simplicitly's sake we always choose a particular type 
!! for a particular sequence of characters - for example '/' is always an 
!! operator.  Client code needs to be aware of this.
!!
!! Similarly we cannot tell keywords apart from names.
!!
!! We have also created a new token type for format specifications, as the 
!! language's parsing rules for those appear to be different from normal 
!! source.
!!
!! Comparison routines expect that case insensitive strings are passed 
!! in in "value" case (i.e. upper case).
!!
!! Delusions exist that by varying the token comparison routines we might 
!! be able to switch to a case sensitive compile.  This might be useful 
!! from the point of view of checking source style consistency.  The flag 
!! for that would have to be stored in a global variable or similar, 
!! otherwise the procedure interface for comparison and value retrieval 
!! would need to change, and that would be a nightmare.
!!
!! In future we may want to bundle additional "meta-data" with particular 
!! tokens, such as in-source documentation a-la doxygen.  Initial thinking 
!! is that data would typically be associated with name tokens, but to 
!! avoid the need for a polymorphic token type (and the associated hassle 
!! of requiring a container for token sequences) we would probably have to 
!! pop the components for that meta data into all tokens.  An alternative 
!! would be to pop the meta data string into its own token type.
!!
!! The first and last components of the token type are are little odd - they 
!! are required for error checking of include lines - which are required to 
!! have nothing else on the line (so INCLUDE must be `first` and the character 
!! literal must be `last`.  Really include lines are an aspect of the source 
!! form, but we have lifted them up to a position equivalent with statements 
!! to allow client code of the scanner to interpret their meaning.  See 
!! comments above the Scanner module for more information.
!!
!! Sooner than later, we should investigate hashing keyword/identifier tokens 
!! too.

MODULE Tokens
  
  USE CompilerKinds
  USE Sources
  USE SourceLocations
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: Tkn               ! Structure constructor - rename when supported.
  PUBLIC :: QueryType
  PUBLIC :: QueryValue
  PUBLIC :: QueryKind
  PUBLIC :: QueryRaw
  PUBLIC :: QueryErrorText
  PUBLIC :: QueryLocation
  PUBLIC :: QueryTypeText
  PUBLIC :: HasKind
  PUBLIC :: IsFirst
  PUBLIC :: IsLast
  PUBLIC :: IsToken
  PUBLIC :: IsKeyword
  PUBLIC :: AppendRawValue
  PUBLIC :: AppendRawKind
  PUBLIC :: AppendRaw
  PUBLIC :: SetValue
  PUBLIC :: SetKind
  PUBLIC :: SetLast
  PUBLIC :: ValueToKind
  PUBLIC :: Split
  
  !-----------------------------------------------------------------------------
  ! Token types
  
  !> No idea what the token is.  This indicates an error or end-of-file 
  !! condition.
  INTEGER, PARAMETER, PUBLIC :: ittNFI = 0
  
  !> Token is a keyword.
  !!
  !! Not currently used - as we can't tell keywords from names without lots 
  !! of context.  But we have plans for ExpectKeyword, that would modify 
  !! the token sequence to flag keywords appropriately.
  !!
  !! Other components have the same interpretation as for ittName.
  !!
  !! @todo Use.
  INTEGER, PARAMETER, PUBLIC :: ittKeyword = 1
  
  !> Token is a name (or a keyword).
  !!
  !! The value component contains the name.  The kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittName = 2
  
  !> Token is an /integer-literal-constant/.
  !!
  !! dddd[_kind]
  !!
  !! The value component contains the digit sequence, while the kind 
  !! component contains the kind parameter if it is present, otherwise 
  !! it is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittIntegerLiteral = 3
  
  !> Token is a /real-literal-constant/.
  !!
  !! ddd[.[ddd]][E[s]ddd][_kind] or .ddd[E[s]ddd][_kind]
  !!
  !! The value component contains the number bit, while the kind 
  !! component contains the kind parameter if it is present, otherwise it is 
  !! zero length.
  INTEGER, PARAMETER, PUBLIC :: ittRealLiteral = 4
  
  !> Something inside single or double quotes.
  !!
  !! The string less the quotes and with doubled delimitnig quote characters 
  !! collapsed to a delimiting single quote character goes in the value 
  !! component, the kind parameter goes in the kind component if it is present 
  !! otherwise the kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittCharacterLiteral = 5
  
  !> Token is a /logical-literal-constant/.
  !!
  !! .TRUE. or .FALSE., possibly with a trailing kind specifier.
  !!
  !! The constant (including the dots) goes in the value component, the kind 
  !! component gets the kind parameter if present, otherwise it is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittLogicalLiteral = 6
  
  !> B'10', O'12345670', Z'123456789ABCDEF0'
  !!
  !! The value component is the content of the string, without the 
  !! delimiters.
  !!
  !! The kind component is B, O or Z, as appropriate for the original 
  !! radix of the constant.
  INTEGER, PARAMETER, PUBLIC :: ittBOZLiteral = 7
  
  !> Built-in operators comprised of symbols, built-in operators .OP. and 
  !! user defined operators.
  !!
  !! Note this includes '*', which is used in approximately fourteen 
  !! thousand different contexts in the language that have nothing to 
  !! do with "operations".
  !!
  !! The value component gets the name (or symbol) of the operator, including 
  !! the dots for the alphanumeric forms.
  !!
  !! The kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittOperator = 8
  
  !> dddd out the front of the statement.  Not currently used.
  !!
  !! Note that labels inside a statement are just returned 
  !! as kindless integers - picking them out requires too much context.  
  !! Later parsing code needs to check that the kindless integer can be a 
  !! valid label.
  !!
  !! The value component contains the digits and the kind component is 
  !! zero length.
  INTEGER, PARAMETER, PUBLIC :: ittStatementLabel = 9
  
  !> One of '(', ')', '[', ']', '(/' or '/)'.
  !!
  !! '/' is also a delimiter, but we've already called it an operator.
  !!
  !! The value component has the symbols and the kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittDelimiter = 10
  
  !> A ','.
  !!
  !! The value component has the symbol and the kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittComma = 11
  
  !> A '=' (but not '==' which is an operator).
  !!
  !! This is used for assignment and for "KEYWORD=arg" type clauses.
  !!
  !! The value component has the symbol and the kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittAssign = 12
  
  !> '=>', as used in SELECT TYPE, ASSOCIATE, USE renaming and pointer 
  !! assignment.
  !!
  !! The value component has the symbol and the the kind component is zero 
  !! length.
  INTEGER, PARAMETER, PUBLIC :: ittAssociate = 13
  
  !> ':', used in array declarations and subscripts and in the ONLY: clause.
  !!
  !! The value component has the symbol and the kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittColon = 14
  
  !> '::', used in declarations and type-specs in ALLOCATE statements.
  !!
  !! The value component has the symbol and the kind component is zero length.
  !!
  !! Parsing code should be prepared to treat a declare token as two colon 
  !! tokens in series in certain contexts (array subscripts).
  INTEGER, PARAMETER, PUBLIC :: ittDeclare = 15
  
  !> ';', or the end of a line without continuation.  Note that tokens of this 
  !! type may have no raw text or value text.
  !!
  !! The kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittEndStatement = 16
  
  !> '%', used to separate the component name from its parent.
  !!
  !! The value component has the symbol and the kind component is zero length.
  INTEGER, PARAMETER, PUBLIC :: ittPercent = 17
  
  !> A format specification.
  !!
  !! This is not a token as defined by the standard, but the behaviour 
  !! required of the scanner is sufficiently different that we treat this 
  !! specially.  Previously we regarded a format specification to be comprised 
  !! of other tokens such as delimiters, commas, keywords, character literals 
  !! and integer literals, but then free form sources spacing rules need to be 
  !! suppressed.
  !!
  !! The kind and value components are zero length.
  INTEGER, PARAMETER, PUBLIC :: ittFormatSpecification = 18
  
  ! Future possibilities for token types include directives and in-source 
  ! comments (the latter might better be a property of a nearby name token).
  
  !-----------------------------------------------------------------------------
  
  !> Names for each type of token for reporting.
  !!
  !! Names that don't appear in / / do not have a syntax rule that defines 
  !! them, they are just listed under a heading or their symbols appear 
  !! verbatim in the body of the standard.
  CHARACTER(*), PARAMETER, PUBLIC :: TokenTypeNames(0:18) = [  &
      'NFI                       ',  &    ! 0
      '/keyword/                 ',  &    !
      '/name/                    ',  &    ! ittName
      '/integer-literal-constant/',  &    ! ittIntegerLiteral
      '/real-literal-constant/   ',  &    ! ittRealLiteral
      '/char-literal-constant/   ',  &    ! ittCharLiteral
      '/logical-literal-constant/',  &    ! ittLogicalLiteral
      '/boz-literal-constant/    ',  &    ! ittBOZLiteral
      'operator                  ',  &    ! ittOperator
      '/label/                   ',  &    ! ittStatementLabel
      'delimiter                 ',  &    ! ittDelimiter
      '","                       ',  &    ! ittComma
      '"="                       ',  &    ! ittAssign
      '"=>"                      ',  &    ! ittAssociate
      '":"                       ',  &    ! ittColon
      '"::"                      ',  &    ! ittDeclare
      '";"                       ',  &    ! ittEndStatement
      '"%"                       ',  &    ! ittPercent
      'format specification      ' ]      ! ittFormatSpecification
  
  !-----------------------------------------------------------------------------
  ! The Token derived type.
  
  !> Type to represent a lexical token (see 3.2.1) from the source code.
  !!
  !! Constructors for the token type must allocate the text based components 
  !! (raw, value and kind) to at least a zero length string as the low level 
  !! procedures that operate on tokens assume that those components will always 
  !! be allocated.
  !!
  !! (This simplifies program logic, and potentially makes it easier to move 
  !! back from deferred length character to F95 fixed length character 
  !! variables).
  !!
  !! The use of type bound procedures here was probably a mistake as there is 
  !! no intention that the Token type be extended.  Oh well.  That said, 
  !! the tight binding of some of the operations on the type to the 
  !! is handy in the face of things being lost with "ONLY" clauses.
  TYPE, PUBLIC :: Token
    PRIVATE
    
    !> The type of the token.  See the itt constants.
    INTEGER :: type
    
    !> The location of the token in the source file.
    TYPE(SourceLocation) :: location
    
    !> The raw text of the token, with case preserved.
    !!
    !! Always allocated, but possibly zero length if the token has no 
    !! associated text - for example the ittEndStatement token that results 
    !! from reaching the end of line.
    !!
    !! This is as the token appears in the source, sans any continuation 
    !! characters.  It may be useful for error reporting.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: raw
    
    !> The text of the main part of the token, in case insensitive form.
    !!
    !! Always allocated but possible zero length if the token has no associated 
    !! text - as for the raw component.
    !!
    !! Note that you cannot simply compare the value component with a string 
    !! to decide what type of token you are dealing with as character literals 
    !! are potentially ambiguous with the other types (a character literal of 
    !! '*' has a value that is just the *, which is the same as the value of 
    !! the * token that is an operator.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: value
    
    !> The kind text of the token, for literals, in case insensitive form.
    !!
    !! Always allocated, but maybe to zero length if the token has no kind 
    !! text.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: kind
    
    !> Flag for the token being at the start of the line.
    LOGICAL :: first
    
    !> Flag for the token being at the end of the line.
    LOGICAL :: last
  CONTAINS
    !---------------------------------------------------------------------------
    ! Getters/Inquiry
    
    !> Specific binding for the OPERATOR(==) generic that compares the 
    !! type of a token with an integer.
    PROCEDURE, NON_OVERRIDABLE, PRIVATE :: equals_type
    !> Specific binding for the OPERATOR(==) generic that compares the value 
    !! component of a token with a string.
    PROCEDURE, NON_OVERRIDABLE, PRIVATE :: equals_text
    !> Compare the type or value of a token with an integer or string 
    !! respectively.
    GENERIC :: OPERATOR(==) => equals_type, equals_text
    
    !> Specific binding for the OPERATOR(/=) generic that compares the 
    !! type of a token with an integer.
    PROCEDURE, NON_OVERRIDABLE, PRIVATE :: notequals_type
    !> Specific binding for the OPERATOR(/=) generic that compares the 
    !! value component of a token with a string.
    PROCEDURE, NON_OVERRIDABLE, PRIVATE :: notequals_text
    !> Compare the type or value of a token with an integer or string 
    !! respectively.
    GENERIC :: OPERATOR(/=) => notequals_type, notequals_text
    
    !---------------------------------------------------------------------------
    ! Setters
    
    !> Specific binding for the ASSIGNMENT(=) generic that takes an integer 
    !! and sets the token's type to it.
    PROCEDURE, NON_OVERRIDABLE, PRIVATE :: assign_type
    !> Set the type of a token.
    GENERIC :: ASSIGNMENT(=) => assign_type
    
    !---------------------------------------------------------------------------
    ! Input/output
    !
    ! These are here just to annoy gfortran users.  They are barely used.
    !
    ! It turns out they annoy ifort users too.  Removed for now.
    
    !PROCEDURE, NON_OVERRIDABLE, PRIVATE :: write_formatted  &
    !    => tkn_write_formatted
    !GENERIC :: WRITE(FORMATTED) => write_formatted
    !
    !PROCEDURE, NON_OVERRIDABLE, PRIVATE :: write_unformatted  &
    !    => tkn_write_unformatted
    !GENERIC :: WRITE(UNFORMATTED) => write_unformatted
    !
    !PROCEDURE, NON_OVERRIDABLE, PRIVATE :: read_unformatted  &
    !    => tkn_read_unformatted
    !GENERIC :: READ(UNFORMATTED) => read_unformatted
    
  END TYPE Token
  
  !-----------------------------------------------------------------------------
  ! Interfaces for generics that operate on tokens.
  
  !> Construct a Token.
  !!
  !! @todo Rename to be the same as the type name once compilers support this 
  !! F2003 feature reliably (ifort 12.1.1 supports it, but there are too 
  !! many bugs).
  INTERFACE Tkn
    MODULE PROCEDURE Token_         ! Type and location.
    MODULE PROCEDURE Token_tfp      ! Type and location (via file).
    MODULE PROCEDURE Token_tfpt     ! Type, location and text.
    MODULE PROCEDURE Token_empty    ! Empty token.
  END INTERFACE Tkn
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Constructors
  
  
  !*****************************************************************************
  !!
  !> Constructs a token with type and position.
  !!
  !! @param[in]     type              The type of token (an itt* constant).
  !!
  !! @param[in]     location          The text position to use for the token.
  !!
  !! @param[in]     first             Flag to indicate whether the token is 
  !! the first token on a line.
  !!
  !! @param[in]     last              Flag to indicate whether the token is 
  !! the last token on a line.
  !!
  !! @returns An appropriately initialised token.  All string components 
  !! are allocated to zero length.
  
  FUNCTION Token_(type, location, first, last) RESULT(t)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: type
    TYPE(SourceLocation), INTENT(IN) :: location
    LOGICAL, INTENT(IN) :: first
    LOGICAL, INTENT(IN) :: last
    
    ! Function result
    TYPE(Token) :: t
    
    !***************************************************************************
    
    t%type = type
    t%location = location
    t%raw = ''
    t%value = ''
    t%kind = ''
    t%first = first
    t%last = last
    
  END FUNCTION Token_
  
  
  !*****************************************************************************
  !!
  !> Constructs a token with type, file and position, but no character.
  !!
  !! @param[in]     type              The type of token (an itt* constant).
  !!
  !! @param[in]     source_file       The source that provided the token.  The 
  !! line number for the token's location is take from the current line number 
  !! of the source.
  !!
  !! @param[in]     pos               Position on the current line.
  !!
  !! @param[in]     first             Flag to indicate whether the token is 
  !! the first token on a line.
  !!
  !! @param[in]     last              Flag to indicate whether the token is 
  !! the last token on a line.
  !!
  !! @returns An appropriately initialised token.  All character components 
  !! are allocated to zero length.
  
  FUNCTION Token_tfp(type, source_file, pos, first, last) RESULT(t)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: type
    CLASS(Source), INTENT(IN), POINTER :: source_file
    INTEGER, INTENT(IN) :: pos
    LOGICAL, INTENT(IN) :: first
    LOGICAL, INTENT(IN) :: last
    
    ! Function result
    TYPE(Token) :: t
    
    !***************************************************************************
    
    t%type = type
    t%location = SourceLocationIfortBug(source_file, pos)
    t%raw = ''
    t%value = ''
    t%kind = ''
    t%first = first
    t%last = last
    
  END FUNCTION Token_tfp
  
  
  !*****************************************************************************
  !!
  !> Constructs a token with type, location and text.
  !!
  !! @param[in]     type              The type of token (an itt* constant).
  !!
  !! @param[in]     location          The text position to use for the token.
  !!
  !! @param[in]     text              Initial token text.  The raw component 
  !! is set to this verbatim, the value component is set to the upper case 
  !! variant.
  !!
  !! @param[in]     first             Flag to indicate whether the token is 
  !! the first token on a line.
  !!
  !! @param[in]     last              Flag to indicate whether the token is 
  !! the last token on a line.
  !!
  !! @returns An appropriately initialised token.  The kind component is 
  !! allocated to zero length.
  
  FUNCTION Token_tfpt(type, location, text, first, last) RESULT(t)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: type
    TYPE(SourceLocation), INTENT(IN) :: location
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    LOGICAL, INTENT(IN) :: first
    LOGICAL, INTENT(IN) :: last
    
    ! Function result
    TYPE(Token) :: t
    
    !***************************************************************************
    
    t%type = type
    t%location = location
    t%raw = text
    t%value = text
    CALL UpperCase(t%value)
    t%kind = ''
    t%first = first
    t%last = last
    
  END FUNCTION Token_tfpt
  
  
  !*****************************************************************************
  !!
  !> Creates an empty token.
  !!
  !! @returns An appropriately initialised token.  All character components 
  !! are allocated to zero length, while he type of the token is set to ittNFI.
  
  FUNCTION Token_empty() RESULT(no_token)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    ! Function result
    TYPE(Token) :: no_token
    
    !***************************************************************************
    
    no_token%type = ittNFI
    ! because Sourcelocation has pointer components, this assignment stops 
    ! this procedure from being pure.
    no_token%location = SourceLocationIfortBug()
    no_token%raw = ''
    no_token%value = ''
    no_token%kind = ''
    no_token%first = .FALSE.
    no_token%last = .FALSE.
    
  END FUNCTION Token_empty
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Getters
  
  
  !*****************************************************************************
  !!
  !> Query the token's type - an itt* constant.
  !!
  !! @param[in]     the_token        The token of interest.
  !!
  !! @return The ittConstant for the type of the token.
  
  PURE FUNCTION QueryType(the_token) RESULT(type)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    INTEGER :: type
    
    !***************************************************************************
    
    type = the_token%type
    
  END FUNCTION QueryType
  
  
  !*****************************************************************************
  !!
  !> Query the token's value text - the string that primarily identifies the 
  !! token or the value that a literal token represents.
  !!
  !! @param[in]     the_token        The token of interest.
  !!
  !! @return The value string for the token.
  
  PURE FUNCTION QueryValue(the_token) RESULT(value)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    CHARACTER(LEN=LEN(the_token%value),KIND=scck) :: value
    
    !***************************************************************************
    
    value = the_token%value
    
  END FUNCTION QueryValue
  
  
  !*****************************************************************************
  !!
  !> Query the token's kind text.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @returns The kind text of the token.  Tokens that are not literal 
  !! constants will have a zero length kind string.
  
  PURE FUNCTION QueryKind(the_token) RESULT(kind)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    CHARACTER(LEN=LEN(the_token%kind),KIND=scck) :: kind
    
    !***************************************************************************
    
    kind = the_token%kind
    
  END FUNCTION QueryKind
  
  
  !*****************************************************************************
  !!
  !> Gets the raw text of the token.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @returns The raw text of the token, as it originally appeared in the 
  !! source.
  
  PURE FUNCTION QueryRaw(the_token) RESULT(raw)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    CHARACTER(LEN=LEN(the_token%raw),KIND=scck) :: raw
    
    !***************************************************************************
    
    raw = the_token%raw
    
  END FUNCTION QueryRaw
  
  
  !*****************************************************************************
  !!
  !> Query the error text of the token.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @returns The text of the token in a form suitable for error messages.
  
  PURE FUNCTION QueryErrorText(the_token) RESULT(raw)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    CHARACTER(LEN=LEN(the_token%raw),KIND=scck) :: raw
    
    !***************************************************************************
    
    raw = QueryRaw(the_token)
    
  END FUNCTION QueryErrorText
  
  
  !*****************************************************************************
  !!
  !> Query the location of the token.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @returns The location of the token.
  
  FUNCTION QueryLocation(the_token) RESULT(location)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    TYPE(SourceLocation) :: location
    
    !***************************************************************************
    
    location = the_token%location
    
  END FUNCTION QueryLocation
  
  
  !*****************************************************************************
  !!
  !> Gets the text description of the type of the token.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @returns The trimmed value of the relevant element of the TokenTypeNames 
  !! array.
  
  PURE FUNCTION QueryTypeText(the_token) RESULT(text)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    CHARACTER(:,KIND=scck), ALLOCATABLE :: text
    
    !***************************************************************************
    
    text = TRIM(TokenTypeNames(the_token%type))
    
  END FUNCTION QueryTypeText
  
  
  !*****************************************************************************
  !!
  !> Test whether a token's type matches a given type.  This is the 
  !! implementation of one of the specific bindings of the OPERATOR(==) 
  !! generic binding.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @param[in]     type              The type (one of the itt* constants) 
  !! to test.
  !!
  !! @returns .TRUE. if the type of @a the_token is @a type.
  
  ELEMENTAL FUNCTION equals_type(the_token, type) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(Token), INTENT(IN) :: the_token
    INTEGER, INTENT(IN) :: type
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = the_token%type == type
    
  END FUNCTION equals_type
  
  
  !*****************************************************************************
  !!
  !> Test whether a token's value text matches a given string.  This is the 
  !! implementation of one of the specific bindings of the OPERATOR(==) 
  !! generic binding.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @param[in]     text              The string to compare the value text 
  !! with.
  !!
  !! @returns .TRUE. if the value text of @a the_token matches @a text.
  
  ELEMENTAL FUNCTION equals_text(the_token, text) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(Token), INTENT(IN) :: the_token
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = the_token%value == text
    
  END FUNCTION equals_text
  
  
  !*****************************************************************************
  !!
  !> Test whether a token's type does not match a given type.  This is the 
  !! implementation of one of the specific bindings of the OPERATOR(/=) 
  !! generic binding.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @param[in]     type              The type (one of the itt* constants) 
  !! to test.
  !!
  !! @returns .TRUE. if the type of @a the_token is not @a type.
  
  ELEMENTAL FUNCTION notequals_type(the_token, type) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(Token), INTENT(IN) :: the_token
    INTEGER, INTENT(IN) :: type
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = the_token%type /= type
    
  END FUNCTION notequals_type
  
  
  !*****************************************************************************
  !!
  !> Test whether a token's value text does not match a given string.  This is 
  !! the implementation of one of the specific bindings of the OPERATOR(/=) 
  !! generic binding.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @param[in]     text              The string to compare the value text 
  !! with.  This should be in upper case.
  !!
  !! @returns .TRUE. if the value text of @a the_token does not match @a text.
  
  ELEMENTAL FUNCTION notequals_text(the_token, text) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(Token), INTENT(IN) :: the_token
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = the_token%value /= text
    
  END FUNCTION notequals_text
  
  
  !*****************************************************************************
  !!
  !> Test whether a token has kind text.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @returns .TRUE. if the kind text of @a the_token has non-zero length.
  
  FUNCTION HasKind(the_token) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = LEN(the_token%kind) /= 0
    
  END FUNCTION HasKind
  
  
  !*****************************************************************************
  !!
  !> Test whether a token was the first token read off a line.
  !!
  !! @param[in]     the_token         The token of interest.
  !!  
  !! @returns The value of the first component of @a the_token.
  
  FUNCTION IsFirst(the_token) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = the_token%first
    
  END FUNCTION IsFirst
  
  
  !*****************************************************************************
  !!
  !> Test whether a token was the last token read off a line.
  !!
  !! @param[in]     the_token         The token of interest.
  !!  
  !! @returns The value of the last component of @a the_token.
  
  FUNCTION IsLast(the_token) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = the_token%last
    
  END FUNCTION IsLast
  
  
  !*****************************************************************************
  !!
  !> Test whether a token matches a given type and value text.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @param[in]     type              The type (itt* value) to test.
  !!
  !! @param[in]     text              The text to test.
  !!
  !! @returns .TRUE. if the type of @a the_token matches @a type and if the 
  !! value text of @a the_token matches @a text, .FALSE. otherwise.
  
  FUNCTION IsToken(the_token, type, text) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    INTEGER, INTENT(IN) :: type
    CHARACTER(LEN=*,KIND=scck), INTENT(IN) :: text
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (the_token%type == type) THEN
      b = the_token%value == text
    ELSE
      b = .FALSE.
    END IF
    
  END FUNCTION IsToken
  
  
  !*****************************************************************************
  !!
  !> Test whether a token is a name token, with value text that matches a 
  !! given string.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! @param[in]     text              The text to test.
  !!
  !! @returns .TRUE. if the token is a name token (ittName) and has value 
  !! text that matches @a text, .FALSE. otherwise.
  
  FUNCTION IsKeyword(the_token, text) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Token), INTENT(IN) :: the_token
    CHARACTER(LEN=*,KIND=scck), INTENT(IN) :: text
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (the_token%type == ittName) THEN
      b = the_token%value == text
    ELSE
      b = .FALSE.
    END IF
    
  END FUNCTION IsKeyword
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Setters
  
  
  !*****************************************************************************
  !!
  !> Set the type of a token to a given type.  This is a specific binding 
  !! of the ASSIGNMENT(=) generic.
  !!
  !! @param[in,out] the_token         The token of interest.
  !!
  !! @param[in]     type              The desired type of the token.
  !!
  !! Note that the other components of @a the_token are not adjusted to be 
  !! consisted with a change in type - that is left to client code.
  
  SUBROUTINE assign_type(the_token, type)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(Token), INTENT(INOUT) :: the_token
    INTEGER, INTENT(IN) :: type
    
    !***************************************************************************
    
    the_token%type = type
    
  END SUBROUTINE assign_type
  
  
  !*****************************************************************************
  !!
  !> Append a string to the raw text and value text of a component.
  !!
  !! @param[in,out] the_token         The token of interest.
  !!
  !! @param[in]     text              The text to append.
  
  SUBROUTINE AppendRawValue(the_token, text)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    CHARACTER(*,scck), INTENT(IN) :: text
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(LEN(text),scck) :: tmp
    
    !***************************************************************************
    
    the_token%raw = the_token%raw // text
    tmp = text
    CALL UpperCase(tmp)
    the_token%value = the_token%value // tmp
    
  END SUBROUTINE AppendRawValue
  
  
  !*****************************************************************************
  !!
  !> Append a string to the raw text and kind text of a component.
  !!
  !! @param[in,out] the_token         The token of interest.
  !!
  !! @param[in]     text              The text to append.
  
  SUBROUTINE AppendRawKind(the_token, text)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    CHARACTER(*,scck), INTENT(IN) :: text
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(LEN(text),scck) :: tmp
    
    !***************************************************************************
    
    the_token%raw = the_token%raw // text
    tmp = text
    CALL UpperCase(tmp)
    the_token%kind = the_token%kind // tmp
    
  END SUBROUTINE AppendRawKind
  
  
  !*****************************************************************************
  !!
  !> Append a string to the raw text of a component.
  !!
  !! @param[in,out] the_token         The token of interest.
  !!
  !! @param[in]     text              The text to append.
  
  SUBROUTINE AppendRaw(the_token, text)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    CHARACTER(*,scck), INTENT(IN) :: text
    
    !***************************************************************************
    
    the_token%raw = the_token%raw // text
    
  END SUBROUTINE AppendRaw
  
  
  !*****************************************************************************
  !!
  !> Set the value text of a token to a given string.
  !!
  !! No case manipulation is done on the string.
  !!
  !! @param[in,out] the_token         The token of interest.
  !!
  !! @param[in]     text              The string.
  
  SUBROUTINE SetValue(the_token, text)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    
    !***************************************************************************
    
    the_token%value = text
    
  END SUBROUTINE SetValue
  
  
  !*****************************************************************************
  !!
  !> Set the kind text of a token to a given string.
  !!
  !! No case manipulation is done on the string.
  !!
  !! @param[in,out] the_token         The token of interest.
  !!
  !! @param[in]     text              The string.
  
  SUBROUTINE SetKind(the_token, text)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    
    !***************************************************************************
    
    the_token%kind = text
    
  END SUBROUTINE SetKind
  
  
  !*****************************************************************************
  !!
  !> Set the last flag for a token.
  !!
  !! @param[in]     the_token         The token of interest.
  !!
  !! The first flag can currently only be set using one of the constructor 
  !! forms.
  
  SUBROUTINE SetLast(the_token)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    
    !***************************************************************************
    
    the_token%last = .TRUE.
    
  END SUBROUTINE SetLast
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Manipulators
  
  
  !*****************************************************************************
  !!
  !> Set the kind text of a token to be its value text, and then clear its 
  !! value text.
  !!
  !! @param[in,out] the_token         The token of interest.
  !!
  !! This is used by the Scanner when building character literals, where the 
  !! token starts off as a name token until the separating _ is read.
  
  SUBROUTINE ValueToKind(the_token)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    
    !***************************************************************************
    
    the_token%kind = the_token%value
    the_token%value = ''
    
  END SUBROUTINE ValueToKind
  
  
  !*****************************************************************************
  !!
  !> Split a token into two tokens.
  !!
  !! @param[in,out] the_token         The token to split.  After this procedure 
  !! this will contain the front half of the original token.
  !!
  !! @param[in]     pos               The raw text position at which to split 
  !! the token.  Characters prior to pos remain in @a the_token, characters 
  !! from pos on go into @a back.
  !!
  !! @param[out]    back              The back half of the split token.
  
  SUBROUTINE Split(the_token, pos, back)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(INOUT) :: the_token
    INTEGER, INTENT(IN) :: pos
    TYPE(Token), INTENT(OUT) :: back
    
    !***************************************************************************
    
    IF (pos > LEN(the_token%raw)) THEN
      back = Token_empty()    ! Empty token.
      RETURN
    END IF
    
    !---------------------------------------------------------------------------
    ! Copy the second part of the token to be split over.
    
    back%type = ittName
    back%location = the_token%location
    ! Determine the position of the second token.  Here we are assuming that 
    ! the characters that make up the token are contiguous, which may not be 
    ! the case in fixed form source, but we'll live with that.
    back%location%column = back%location%column + pos - 1
    back%raw = the_token%raw(pos:)
    ! We could use tkn%value here and save on the uppercase.
    back%value = the_token%raw(pos:)
    CALL UpperCase(back%value)
    back%kind = ''
    ! We assume that the second token cannot have been the first token in 
    ! the line.  This is good enough - first/last are only used by INCLUDE 
    ! line checks, which requires that all the tokens appear on the same line 
    ! and there shouldn't be any splitting going on.
    back%first = .FALSE.
    ! If the orginal token was the last on the line, then this token is now 
    ! the last on the line.
    back%last = the_token%last
    
    !---------------------------------------------------------------------------
    ! Truncate the original token to be split to make the first token.
    
    the_token%raw = the_token%raw(:pos-1)
    ! We could use tkn%value here and save on the uppercase.
    the_token%value = the_token%raw(:pos-1)
    CALL UpperCase(the_token%value)
    ! If there is going to be another token following this one then this 
    ! cannot be the last token in the statement (which we _assume_ means it 
    ! cannot be the last token on the line, not that it matters).
    the_token%last = .FALSE.
    
  END SUBROUTINE Split
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! UDDTIO stuff.
  !
  ! We need to think about this a bit more.
  !
  ! For unformatted stuff, which humans are never going to read, we could 
  ! be a little verbose and dump just the raw text of the token structure 
  ! (perhaps with leading length).  A read of a token would then need to 
  ! decipher the type of token based on that text.  Because we know the 
  ! length of the token that isn't anywhere near the difficulty of the 
  ! original parse, but we would have to write all the type detection code 
  ! for that from scratch.
  !
  ! That approach would mean that we would lose the source context aspects 
  ! of the token, but they are probably not that interesting for a serialised 
  ! token anyway.  Given we refer to the name of the source file via 
  ! a pointer, we've probably lost source location information regardless.
  !
  ! (We could map the source file pointer to a source file index, and then 
  ! require that token writing code output the table of source files before 
  ! it output any tokens, but it is hard to understand how the information 
  ! flow works in this case - how do you tell the writing and reading 
  ! procedures what the current source file table is?)
  !
  ! Alternatively, we could dump every field (perhaps bar source location 
  ! given the above discussion) and make the reading code very simple.
  !
  ! Somewhere in the middle is probably ideal - dump the token type, 
  ! the raw text length, and the raw text, then on read use that type to 
  ! reassemble a reasonable representation of the complete token.
  !
  ! For formatted output, which is a human may want to read, the components 
  ! other than raw text perhaps are less interesting.  We could make their 
  ! output contingent on fields in the DT descriptor or similar.  Reading 
  ! raw text into a token practically puts us into free form parsing.
  !
  ! Formatted output might be useful for debugging.  Formatted input is 
  ! probably pointless.  Unformatted output might be useful for "precompiling" 
  ! (but you'd probably be better off dumping the results of a later stage, 
  ! such as the statement or parse tree) and unformatted input would go hand 
  ! in hand with that.
  
  
  !*****************************************************************************
  !!
  !> Write a token to a formatted file.
  !!
  !! @param[out]    iostat            IOSTAT code - non-zero on error.
  !!
  !! @param[in,out] iomsg             Error message - redefined if iostat is 
  !! non-zero.
  
  SUBROUTINE tkn_write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(Token), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: iotype
    INTEGER, INTENT(IN) :: v_list(:)
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(*), INTENT(INOUT) :: iomsg
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! v_list index.
    CHARACTER(10) :: fmt      ! Format buffer.
    INTEGER :: width          ! field width,
    
    !***************************************************************************
    
    IF (iotype == 'DTFULL') THEN
      DO i = 1, 7
        IF (i <= SIZE(v_list)) THEN
          width = v_list(i)
        ELSE
          width = 0
        END IF
          
        IF (i <= 5) THEN
          IF (width == 0) THEN
            fmt = '(A,1X)'
          ELSE
            WRITE (fmt, "('A',I0,',1X')") width
          END IF
        ELSE IF (i == 6) THEN
          WRITE (fmt, "('L',I0,',1X')") MAX(width, 1)
        ELSE
          WRITE (fmt, "('L',I0)") MAX(width, 1)
        END IF
          
        SELECT CASE (i)
        CASE (1)    ; WRITE (unit, fmt) TRIM(TokenTypeNames(dtv%type))
        CASE (2)    ; WRITE (unit, fmt) .ToString. dtv%location
        CASE (3)    ; WRITE (unit, fmt) dtv%raw
        CASE (4)    ; WRITE (unit, fmt) dtv%value
        CASE (5)    ; WRITE (unit, fmt) dtv%kind
        CASE (6)    ; WRITE (unit, fmt) dtv%first
        CASE (7)    ; WRITE (unit, fmt) dtv%last
        END SELECT
          
      END DO
    ELSE
      WRITE (unit, "(A)", IOSTAT=iostat, IOMSG=iomsg) dtv%raw
    END IF
    
  END SUBROUTINE tkn_write_formatted
  
  
  !*****************************************************************************
  !!
  !> Write a token to an unformatted file.
  !!
  !! We write the length of the raw token text followed by its raw text.
  !!
  !! @param[out]    iostat            IOSTAT code - non-zero on error.
  !!
  !! @param[in,out] iomsg             Error message - redefined if iostat is 
  !! non-zero.
  
  SUBROUTINE tkn_write_unformatted(dtv, unit, iostat, iomsg)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(Token), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(*), INTENT(INOUT) :: iomsg
    
    !***************************************************************************
    
    WRITE (unit, IOSTAT=iostat, IOMSG=iomsg) dtv%type
    IF (iostat /= 0) RETURN
    
    WRITE (unit, IOSTAT=iostat, IOMSG=iomsg) LEN(dtv%raw)
    IF (iostat /= 0) RETURN
    
    WRITE (unit, IOSTAT=iostat, IOMSG=iomsg) dtv%raw
    
  END SUBROUTINE tkn_write_unformatted
  
  
  !*****************************************************************************
  !!
  !> Read a token from an unformatted file.
  !!
  !! We require the same data layout as generated by tkn_write_unformatted.
  !!
  !! @param[in,out] dtv               The token being read.
  !!
  !! @param[in]     unit              The logical unit to read from.
  !!
  !! @param[out]    iostat            IOSTAT code - non-zero on error.
  !!
  !! @param[in,out] iomsg             Error message - redefined if iostat is 
  !! non-zero.
  
  SUBROUTINE tkn_read_unformatted(dtv, unit, iostat, iomsg)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(Token), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER(*), INTENT(INOUT) :: iomsg
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Token type.
    INTEGER(KIND(dtv%type)) :: type
    
    INTEGER :: num            ! Length of the name.
    
    ! Raw token text.
    CHARACTER(:,KIND=scck), ALLOCATABLE :: raw_text
    
    !***************************************************************************
    
    READ (unit, IOSTAT=iostat, IOMSG=iomsg) type
    IF (iostat /= 0) RETURN
    
    READ (unit, IOSTAT=iostat, IOMSG=iomsg) num
    IF (iostat /= 0) RETURN
    
    IF (ALLOCATED(dtv%raw)) DEALLOCATE(dtv%raw)
    ALLOCATE(CHARACTER(num,KIND=scck) :: raw_text)
    READ (unit, IOSTAT=iostat, IOMSG=iomsg) raw_text
    IF (iostat /= 0) RETURN
    
    CALL tkn_read_unformatted_worker(dtv, type, raw_text)
    
  END SUBROUTINE tkn_read_unformatted
  
  
  !*****************************************************************************
  !!
  !> Worker procedure for tkn_read_unformatted, where the argument is 
  !! INTENT(OUT).  This lets us rely upon the state of the various components 
  !! of token.
  !!
  !! @param[out]    dtv               The token being read, with the type 
  !! component defined.
  !!
  !! @param[in]     type              The token type.
  !!
  !! @param[in]     raw_text          The raw text for the token.
  
  SUBROUTINE tkn_read_unformatted_worker(dtv, type, raw_text)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(Token), INTENT(OUT) :: dtv
    INTEGER(KIND(dtv%type)), INTENT(IN) :: type
    CHARACTER(*,KIND=scck), INTENT(IN) :: raw_text
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: underbar       ! Underbar `_` position in raw text.
    
    !***************************************************************************
    
    ! Invent a location, set first and last components to reasonable default.
    dtv%Location = SourceLocationIfortBug()
    dtv%first = .FALSE.
    dtv%last = .FALSE.
    
    ! Store raw text.
    dtv%raw = raw_text
    
    SELECT CASE (type)
    CASE (ittKeyword, ittName)
      dtv%value = raw_text
      CALL UpperCase(dtv%value)
      dtv%kind = ''
    
    CASE (ittIntegerLiteral, ittRealLiteral, ittLogicalLiteral)
      underbar = INDEX(raw_text, '_')
      IF (underbar == 0) THEN
        dtv%value = raw_text
        CALL UpperCase(dtv%value)
        dtv%kind = ''
      ELSE
        dtv%value = raw_text(:underbar-1)
        CALL UpperCase(dtv%value)
        dtv%kind = raw_text(underbar+1:)
        CALL UpperCase(dtv%kind)
      END IF
      
    CASE (ittCharacterLiteral)
      IF (raw_text(1:1) == '"' .OR. raw_text(1:1) == "'") THEN
        dtv%value = unescape(raw_text)
        dtv%kind = ''
      ELSE
        underbar = INDEX(raw_text, '_')
        dtv%value = unescape(raw_text(underbar+1:))
        dtv%kind = raw_text(:underbar-1)
        CALL UpperCase(dtv%kind)
      END IF
      
    CASE (ittBOZLiteral, ittOperator)
      dtv%kind = raw_text(1:1)
      CALL UpperCase(dtv%kind)
      dtv%value = raw_text(3:LEN(raw_text)-1)
      CALL UpperCase(dtv%Value)
      
    CASE ( ittStatementLabel, ittDelimiter, ittComma, ittAssign,  &
        ittAssociate, ittColon, ittDeclare, ittEndStatement, ittPercent,  &
        ittFormatSpecification )
      dtv%value = raw_text
      dtv%kind = ''
      
    END SELECT
    
  END SUBROUTINE tkn_read_unformatted_worker
  
  
  !*****************************************************************************
  !!
  !> Unescape (doubled delimiting quotes to a single delimiting quote) 
  !! a character literal.
  !!
  !! @param[in]     in                The escaped literal.
  !!
  !! @returns The unescaped literal.
  
  FUNCTION unescape(in) RESULT(out)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*,scck), INTENT(IN) :: in
    
    ! Function result.
    CHARACTER(:,scck), ALLOCATABLE :: out
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: ii             ! In index.
    INTEGER :: io             ! Out index.
    
    ! Delimiting quote character.
    CHARACTER(KIND=scck) :: quote
    
    !***************************************************************************
    
    quote = in(1:1)
    
    ! Count the number of quote characters.  This will always be a multiple 
    ! of two.
    io = 0
    DO ii = 2, LEN(in) - 1
      IF (in(ii:ii) == quote) io = io + 1
    END DO
    
    ALLOCATE(CHARACTER(LEN(in)-2-io/2,scck) :: out)
    
    ii = 2
    io = 1
    DO WHILE (ii <= LEN(in) - 1)
      out(io:io) = in(ii:ii)
      IF (in(ii:ii) == quote) ii = ii + 1
      io = io + 1
      ii = ii + 1
    END DO
    
  END FUNCTION unescape
  
END MODULE Tokens
