! $Id: CmdLine.f90 2789 2019-03-19 20:49:12Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the CmdLine module.


!*******************************************************************************
!!
!> Generic command line parsing.
!!
!! Procedures and types in this module allow parsing of command lines that 
!! have items of the form:
!!
!! - Flag like options:  --option_name
!! - Option with value: --option_name[:] option_value
!! - Option with optional value:  --option_name[: option_value]
!! - Positional argument (things that have no leading hyphens and are not 
!!   option values).
!!
!! If an option has an optional value, then the colon must be used to 
!! separate the option name from the value and to avoid ambiguity around 
!! whether the next token is a positional argument.
!!
!! If a colon is used to separate an option from its value, then a space 
!! after the colon is optional.
!!
!! Single characters for option_name can be introduced with a single leading 
!! hyphen, ie "-o".
!!
!! Valid options are described using the CmdLineOption structure.
!!
!! A simple usage message can also be generated.
!!
!! Errors are indicated by a non-zero stat code and a descriptive message.  
!! Current possible stat codes are:
!! - 1: Unknown option.
!! - 2: Option missing argument.
!! - 3: Option has extra argument.
!! - 4: Option argument of wrong type.
!! - 5: Option specified too many times.
!! - 6: Unable to open file.
!! - 7: Unable to read from file.
!! - 8: Recursive reference to response file.
!!
!! Modelled loosely on boost::program_options.  Perhaps loosely is a little 
!! loose.
!!
!! The command line is broken into arguments.  What defines an argument 
!! depends on the source of the command line:
!! - Each argument as returned by the host fortran processor is counted as 
!!   an argument.
!! - If the command line is provided as a string, then arguments are 
!!   delimited by unquoted and unescaped blanks (spaces).  The escape 
!!   character is a back slash (so Hello\ World is one argument.  Use 
!!   double quotes for quoting (so "Hello World" is also one argument).
!! - Each record (line) read from a response file is an argument.
!!
!! Arguments that consist of all blanks, or that are zero length, are 
!! ignored.
!!
!! Arguments that have '@' as the first non-blank character are taken to 
!! be response file names.  Records are read from the response file and 
!! are substituted for that particular argument.  Response file substitution 
!! is recursive, in that records in a response file that start with '@' will 
!! result in another file being read and records substituted, however 
!! response files must not recursively refer back to themselves.
!!
!! Note that this response file substituion happens regardless of whether 
!! the argument is classified as a positional argument or as an option's value.
!!
!! Arguments that have '-' as the first non-blank character, followed 
!! immediately by another '-', followed by one or more non-blank 
!! characters, introduce long name options.  The name of the option is the 
!! sequence of non-blank characters after the '--', up until the first 
!! blank character or the first colon.
!!
!! Arguments that have '-' as the first non-blank character, followed by one 
!! or more non-blank, non-'-' characters introduce short name options.  The 
!! name of the option is the character that follows the '-'.
!!
!! Both long name and short name options may also require a value.  This 
!! is either the rest of the argument that follows the option name, or if 
!! there is no non-blank text following the option name, the next argument.  
!! For both long and short name options a separating colon that follows the 
!! argument name is ignored.
!!
!! Both long name and short name options may take an optional value.  In 
!! this case, the long option name must immediately be followed by colon 
!! (the optional value may be part of the next argument).  The short option 
!! name can either have the optional value in the same argument following 
!! the option name (blanks after the name character are ignored), in the 
!! same argument following a colon that comes immediately after the option 
!! name (blanks following the colon are ignore), or it can be the next 
!! argument if the short option name is immediately followed by a colon.
!!
!! If an option does not take a value, but there is what apears to be 
!! a value for the option in the same argument (or the option name is 
!! followed by a colon), then an error is raised.
!!
!! Arguments that are not option names or option values are classed as 
!! positional arguments.
!!
!! Examples:
!!    -a:cat
!! Short name option 'a' with either required or optional value 'cat'.  If 
!! the 'a' option doesn't take a value, then an error will be reported.
!!
!!    -a cat
!! Under string input rules, this is a short name option 'a' with required 
!! value 'cat', or with no value, and a positional argument 'cat'.  Under 
!! response file input (where the entire line is considered one argument) 
!! this is a short name option 'a' with required or optional value 'cat', 
!! if the 'a' option doesn't take a value, then an error will be reported.
!!
!!    --option-name:dog
!! Long name option 'option-name' with required or optional value 'dog'.  If 
!! the 'option-name' option doesn't take a value then an error will be 
!! reported.
!!
!!    abc def ghi
!! Under string input rules this is three positional arguments.  Under 
!! response file input rules this is one positional argument 'abc def ghi', 
!! which may also have leading and trailing blanks.
!!
!!
!! This was original written in the iso_varying_string era.  Some things 
!! are a little odd given the move to deferred length character.
!!
!! See CmdLineTests for tests.
!!
!! @todo Some way of referencing elements in an array of type CmdLineOpt 
!! based on their long and/or short name.  This would reduce the amount of 
!! fussing around the ordering and indexing of options in client code.
!!
!! @todo Find the file based input stuff I (think I) wrote.
!!
!! @todo Response file processing (@filename) in command line.
!!
!! @todo Some way of filtering the command line ahead of time for a 
!! particular option.  This will be required for --host support, as a 
!! particular host may add supported options.

MODULE CmdLine
  
  USE Strings
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and variables
  
  PUBLIC :: CmdLineOption
  PUBLIC :: WriteUsageMessage
  PUBLIC :: BreakLine
  PUBLIC :: ParseCmdLine
  PUBLIC :: GetCmdLineArgument
  
  !-----------------------------------------------------------------------------
  ! Option types
  
  !> Option has no argument.
  INTEGER, PARAMETER, PUBLIC :: ioptFlag    = 1
  !> Option must have an argument - any generic string.
  INTEGER, PARAMETER, PUBLIC :: ioptArg     = 2
  !> Option has an optional argument - any generic string.
  INTEGER, PARAMETER, PUBLIC :: ioptArgOpt  = 3
  !> Option must have an integer argument.
  INTEGER, PARAMETER, PUBLIC :: ioptArgInt  = 4
  !> Option must have a real argument.
  INTEGER, PARAMETER, PUBLIC :: ioptArgReal = 5
  
  !> Describes a command line option.
  TYPE :: CmdLineOption
    
    !---------------------------------------------------------------------------
    ! Input settings.
    
    !> Long name (follows two hyphens) of the option.
    !!
    !! If not allocated then the option has no long name equivalent.
    CHARACTER(:), ALLOCATABLE :: LongName
    
    !> Short name (follows a single hyphen) of the option.
    !!
    !! If not allocated then the option has no short name equivalent.
    !!
    !! @todo Consider using a blank to indicate no short name.
    !!
    !! Note this isn't deferred length!
    CHARACTER(1), ALLOCATABLE :: CharName
    
    !> Description of the option for a usage message.
    !!
    !! If not allocated then WriteUsageMessage will not print out the option 
    !! or its description.  This way, "internal" options can be created.
    CHARACTER(:), ALLOCATABLE :: Description
    
    !> Option flags.
    !!
    !! Use the iopt constants defined above.  By default options are treated 
    !! as flags.
    INTEGER :: OptionType = ioptFlag
    
    !> Repeat flag.
    !!
    !! If .TRUE. then the option may appear multiple times.  Arguments are 
    !! separated by CHAR(0) characters.
    LOGICAL :: CanRepeat
    
    !---------------------------------------------------------------------------
    ! Output settings.
    
    !> Number of times the option appears.
    INTEGER :: PresentCount = 0
    
    !> Indices of where the option appears.
    INTEGER, ALLOCATABLE :: Positions(:)
    
    !> Arguments associated with options that can have an argument.
    !!
    !! Not allocated if the option was not specified.  Allocated to a maximum 
    !! size of one if the option does not have the CanRepeat component set.
    TYPE(String), ALLOCATABLE :: Argument(:)
    
  END TYPE CmdLineOption
  
  !> Constructs a CmdLineOption object.
  !!
  !! @todo Rename when compile support is sufficient.
  INTERFACE CmdLineOption
    MODULE PROCEDURE CmdLineOption_all
  END INTERFACE CmdLineOption
  
  !> Parse a command line.
  INTERFACE ParseCmdLine
    MODULE PROCEDURE ParseCmdLine_cl
    MODULE PROCEDURE ParseCmdLine_str
    MODULE PROCEDURE ParseCmdLine_str1D
!    MODULE PROCEDURE ParseCmdLine_file     ! we wrote this... where is it??
  END INTERFACE ParseCmdLine
  
  !> Write a usage message for a command line.
  INTERFACE WriteUsageMessage
    MODULE PROCEDURE WriteUsageMessage_
  END INTERFACE WriteUsageMessage
  
  !> Break an output line into multiple lines at word boundaries.
  INTERFACE BreakLine
    MODULE PROCEDURE BreakLine_
  END INTERFACE BreakLine
  
  !> Get an argument from the command line.
  INTERFACE GetCmdLineArgument
    MODULE PROCEDURE GetCmdLineArgument_
  END INTERFACE GetCmdLineArgument
  
  !> Separator betweens option and their arguments.
  !!
  !! Other systems would set this to '='.
  !!
  !! Current this is LEN == 1.  Not sure what happens if it is longer.
  CHARACTER, PARAMETER :: option_separator_char = ':'
  
  !> Leading character of arguments that indicate a response filenames.
  CHARACTER, PARAMETER :: response_filename_char = '@'
  
  ! The following are only applicable to ParseCmdLine_str - where the 
  ! string is tokenised internally and not by a shell or similar.
  
  !> Character used to remove any special meaning from the character that 
  !! follows in the input string.  On DOS systems this could be the caret.
  CHARACTER, PARAMETER :: escape_char = '\'
  
  !> Character that starts a quoted string.
  CHARACTER, PARAMETER :: start_quote = '"'
  
  !> Character that ends a quoted string.
  CHARACTER, PARAMETER :: end_quote = '"'
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Construct a CmdLineOption object.
  !!
  !! @param[in]     long_name         Optional long name for the option.  
  !! If not present then the option has no long name.
  !!
  !! @param[in]     char_name         Optional short name for the option.  If 
  !! not present then the option has no short name.
  !!
  !! @param[in]     description       Optional description (for a usage 
  !! message) for the option.  If not present then the option will not 
  !! be displayed at all by WriteUsageMessage.
  !!
  !! @param[in]     option_type       Optional type of option (an iopt* 
  !! constant).  If not present then the option type is optFlag.
  !!
  !! @param[in]     can_repeat        Optional repeatable option flag.  If 
  !! .TRUE. the option can appear multiple times, otherwise (if .FALSE. or 
  !! not present) all the arguments from the various appearances will be 
  !! concatenated together, separated by CHAR(0).
  !!
  !! @returns The CmdLineOption object.
  
  PURE FUNCTION CmdLineOption_all( long_name, char_name, description,  &
      option_type, can_repeat ) RESULT(clo)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN), OPTIONAL :: long_name
    CHARACTER(1), INTENT(IN), OPTIONAL :: char_name
    CHARACTER(*), INTENT(IN), OPTIONAL :: description
    INTEGER, INTENT(IN), OPTIONAL :: option_type
    LOGICAL, INTENT(IN), OPTIONAL :: can_repeat
    
    ! Function result
    TYPE(CmdLineOption) :: clo
    
    !***************************************************************************
    
    ! Trundle through arguments, transferring those that are present across 
    ! to structure components and otherwise setting defaults.
    
    IF (PRESENT(long_name))     clo%LongName = long_name
    IF (PRESENT(char_name)) THEN
      ALLOCATE(clo%CharName)
      clo%CharName = char_name
    END IF
    IF (PRESENT(description))   clo%Description = description
    
    IF (PRESENT(option_type)) THEN
      clo%OptionType = option_type
    ELSE
      clo%OptionType = ioptFlag
    END IF
    
    IF (PRESENT(can_repeat)) THEN
      clo%CanRepeat = can_repeat
    ELSE
      clo%CanRepeat = .FALSE.
    END IF
    
  END FUNCTION CmdLineOption_all
  
  
  !*****************************************************************************
  !!
  !> Writes a formatted usage message to the console.
  !!
  !! @param[in]     option_list       An array of CmdLineOptions objects that 
  !! describe the options that the command line can take.
  !!
  !! @param[in]     unit              Optional logical unit number to write 
  !! the usage message to.  If missing then the console is used.
  
  SUBROUTINE WriteUsageMessage_(option_list, unit)
    
    USE CharUtils
    
    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY: OUTPUT_UNIT
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(CmdLineOption), INTENT(IN) :: option_list(:)
    INTEGER, INTENT(IN), OPTIONAL :: unit
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Line position where the option descriptions should start.  This is 
    ! determined based on the longest option name prefix.
    INTEGER :: description_start
    
    INTEGER :: i              ! Option list prefix.
    INTEGER :: local_unit     ! Local (defaulted if not present) copy of unit.
    
    ! Buffer for assembling the line output format specification.
    CHARACTER(LEN=64) :: format_string
    
    ! Buffer for the option name part.
    CHARACTER(:), ALLOCATABLE :: name_buffer
    
    ! Buffer for the option description part.
    CHARACTER(:), ALLOCATABLE :: desc_buffer
    
    ! Index into the description for a particular option.
    INTEGER :: description_ptr
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Line length for output.
    !
    ! Perhaps this could be INQUIRE(local_unit, RECL=line_length) ?
    !
    ! Take one off because otherwise we wrap.
    INTEGER, PARAMETER :: line_length = 80 - 1
    
    !***************************************************************************
    
    ! Set default for unit to be the output unit.
    IF (PRESENT(unit)) THEN
      local_unit = unit
    ELSE 
      local_unit = OUTPUT_UNIT
    END IF
      
    !---------------------------------------------------------------------------  
    ! Work out the line length required for the option flags
    
    description_start = 0
    DO i = 1, SIZE(option_list,1)
      ! Options without a description are not displayed.
      IF (.NOT. ALLOCATED(option_list(i)%Description)) CYCLE
      
      description_start = MAX( description_start,  &
          option_name_length(option_list(i)) )
    END DO
    
    ! Format string to position tab one space after end of name description.
    format_string = '(A,T' // ToString(description_start + 2) // ',A)'
    
    !---------------------------------------------------------------------------
    ! Format and output the descriptions.
    
    DO i = 1, SIZE(option_list,1)
      
      ! Options without a description are not displayed.
      IF (.NOT. ALLOCATED(option_list(i)%Description)) CYCLE
      
      name_buffer = ''
      description_ptr = 1
      
      ! Write out short name
      IF (ALLOCATED(option_list(i)%CharName)) THEN
        name_buffer = name_buffer // '-' // option_list(i)%CharName
      END IF
      ! Write out long name
      IF (ALLOCATED(option_list(i)%LongName)) THEN
        IF (ALLOCATED(option_list(i)%CharName))  &
            name_buffer = name_buffer // ' or '
        name_buffer = name_buffer // '--' // option_list(i)%LongName
      END IF
      
      SELECT CASE (option_list(i)%OptionType)
      CASE (ioptArg,ioptArgInt, ioptArgReal)      ! mandatory argument.
        name_buffer = name_buffer // '[' // option_separator_char // '] arg'
      CASE (ioptArgOpt)
        name_buffer = name_buffer // '[' // option_separator_char // 'arg]'
      END SELECT
      
      ! Always at least one line for each option
      CALL BreakLine( option_list(i)%Description, description_ptr,  &
          line_length - description_start, desc_buffer )
      WRITE (local_unit, format_string) name_buffer, desc_buffer
      
      DO WHILE (description_ptr <= LEN(option_list(i)%Description))
        CALL BreakLine( option_list(i)%Description, description_ptr,  &
            line_length - description_start, desc_buffer )
        WRITE (local_unit, format_string) ' ', desc_buffer
      END DO
      
      ! Blank record between option descriptions.
      WRITE (local_unit, "()")
    END DO
    
  END SUBROUTINE WriteUsageMessage_
  
  
  !*****************************************************************************
  !!
  !> Calculate the length of the descriptive text required for the option name.
  !!
  !! @param[in]     option            The option.
  !!
  !! @returns Length of the option name description (ie. text like 
  !! --opt[:arg]) or equivalent.
  
  FUNCTION option_name_length(option) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(CmdLineOption), INTENT(IN) :: option
    
    ! Function result
    INTEGER :: l
    
    !***************************************************************************
    
    l = 0
    ! '--name'
    IF (LEN(option%LongName) > 0) THEN
      l = l + LEN(option%LongName) + 2
      IF (LEN(option%CharName) > 0) THEN
        ! '-x or '
        l = l + 6
      END IF
    ELSE IF (LEN(option%CharName) > 0) THEN
      ! '-x'
      l = l + 2
    ELSE
      ! Option has no name?  Skip.
      RETURN
    END IF
    SELECT CASE (option%OptionType)
    CASE (ioptFlag) ! No additional text
    CASE (ioptArg, ioptArgInt, ioptArgReal)  ! Required argument
      ! '[:] arg'
      l = l + 7
    CASE (ioptArgOpt)
      ! '[:arg]'
      l = l + 6
    END SELECT
    
  END FUNCTION option_name_length
  
  
  !*****************************************************************************
  !!
  !> Breaks a string at a word boundary so that the string appears nicely with 
  !! a certain line length.
  !!
  !! @param[in]     in_str            The string to break.
  !!
  !! @param[in,out] start_pos         The position at which to start 
  !! processing @a in_str.  Updated to one past the last character processed 
  !! into @a out_str.
  !!
  !! @param[in]     line_length       The length of line to break @a in_str.
  !!
  !! @param[out]    out_str           The part of @a in_str that will fit 
  !! on the line.
  !!
  !! If we encounter a word that doesn't fit on the line (the word is longer 
  !! that @a line_length) then we just chop the word whereever convenient.
  
  PURE SUBROUTINE BreakLine_(in_str, start_pos, line_length, out_str)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: in_str
    INTEGER, INTENT(INOUT) :: start_pos
    INTEGER, INTENT(IN) :: line_length
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: out_str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Index into in_str
    
    ! Copy of the relevant part of the incoming string.
    CHARACTER(:), ALLOCATABLE :: temp_str
    
    ! The next word in the string.
    CHARACTER(:), ALLOCATABLE :: word
    
    !***************************************************************************
    
    temp_str = in_str(start_pos:)
    i = 1
    
    DO
      ! Find the next space
      CALL Split(temp_str, word, ' ')
      IF (i == 1) THEN
        ! We always print something, if necessary breaking the word
        IF (LEN(word) > line_length) THEN
          out_str = word(:line_length-1) // '-'
          start_pos = start_pos + Len(out_str)
          RETURN
        ELSE
          out_str = word
          i = i + LEN(word)
        END IF
      ELSE IF(i + LEN(word) < line_length) THEN
        out_str = out_str // ' ' // word
        i = i + 1 + LEN(word)
      ELSE
        start_pos = start_pos + i
        RETURN
      END IF
      IF (LEN(temp_str) == 0) THEN
        start_pos = LEN(in_str) + 1
        RETURN
      END IF
    END DO
    
  END SUBROUTINE BreakLine_
  
  
  !*****************************************************************************
  !!
  !> Parse the command line as retrieved by the Fortran processor.
  !!
  !! @param[in,out] option_list       List of command line options.
  !!
  !! @param[out]    arguments         List of command arguments (parts of the 
  !! command line that do not correspond to any particular option.
  !!
  !! @param[out]    stat              Error code - non-zero on errror.  In 
  !! addition to processor defined error codes this can include:
  !!
  !! @param[out]    message           Error message, set if stat is non-zero.
  !!
  !! We retreive the arguments from the processor using the Fortran 2003 
  !! standard intrinsics, and then get ParseCmdLine_str1D to do the 
  !! heavy lifting.
  
  SUBROUTINE ParseCmdLine_cl(option_list, arguments, stat, message, positions)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(CmdLineOption), INTENT(INOUT) :: option_list(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: arguments(:)
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: message
    INTEGER, INTENT(OUT), OPTIONAL, ALLOCATABLE :: positions(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Arguments as reported by the Fortran processor.
    TYPE(String), ALLOCATABLE :: args(:)
    
    INTEGER :: arg_count      ! Number of arguments reported by the processor.
    INTEGER :: i              ! Argument index.
    
    !***************************************************************************
    
    arg_count = COMMAND_ARGUMENT_COUNT()
    ALLOCATE(args(arg_count))
    DO i = 1, arg_count
      CALL GetCmdLineArgument(i, args(i)%item)
    END DO
    
    CALL ParseCmdLine(args, option_list, arguments, stat, message, positions)
    
  END SUBROUTINE ParseCmdLine_cl
  
  
  !*****************************************************************************
  !!
  !> Parse the command line as represented by a character string.
  !!
  !! @param[in]     str               The string representing the command 
  !! line to parse.
  !!
  !! @param[in,out] option_list       The list of command line options.
  !!
  !! @param[out]    arguments         List of command arguments (parts of the 
  !! command line that do not correspond to any particular option.
  !!
  !! @param[out]    stat              Error code - non-zero on errror.  In 
  !! addition to processor defined error codes this can include:
  !!
  !! @param[out]    message           Error message, set if stat is non-zero, 
  !! otherwise not allocated.
  !!
  !! Arguments are define by a very basic set of heuristics that are somewhat 
  !! similar to that used for unix shells:
  !!
  !! - Unless escaped or enclosed in double quotes, spaces are used to 
  !!   separate arguments.
  !!
  !! - The escape character is a backslash.  Anything escaped is treated as 
  !!   is.
  !!
  !! - The escape character still has significance inside a quoted string, so 
  !!   you can escape quotes that should be inside the string.
  !!
  !! - Two escape characters in a row represent a single escape 
  !!   character in the argument.
  !!
  !! - The outer quotes are stripped from arguments that are quoted.
  
  SUBROUTINE ParseCmdLine_str(str, option_list, arguments, stat, message)
    
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: str
    TYPE(CmdLineOption), INTENT(INOUT) :: option_list(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: arguments(:)
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: message
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! The arguments split out of the input string.
    TYPE(String), ALLOCATABLE :: args(:)
    
    INTEGER :: iarg           ! Index of current argument being constructed.
    INTEGER :: i              ! Index into the string.
    LOGICAL :: in_quote       ! Flag to indicate if we are inside a string.
    INTEGER :: skip           ! Number of characters to skip.
    
    !***************************************************************************
     
    !---------------------------------------------------------------------------
    ! Break the string up into space separated tokens.
    
    ! Some setup.
    in_quote = .FALSE.        ! Not in a string.
    iarg = 1                  ! Processing first argument.
    skip = 0                  ! No characters to skip.
    
    ! Initially allocate to size one, if no arguments are found then the 
    ! shrink of the array by one at the end sets this to size zero.  
    ! We build the arguments in place, so initially set the first argument to 
    ! zero length.
    CALL Append(args, '')
    
    args(1)%item = ''
    
    ! Move through the string.
    DO i = 1, LEN(str)
      ! Escape processing may mean we have characters to skip.
      IF (skip > 0) THEN
        skip = skip - 1
        CYCLE
      END IF
      
      ! Check for escape.
      IF (str(i:i) == escape_char) THEN
        ! Anything escaped is transferred across as-is.
        IF (i + 1 < LEN(str)) THEN
          ! Transfer the escaped character.
          args(iarg)%item = args(iarg)%item // str(i+1:i+1)
          ! Set skip so the fixed iterations of the do loop move past.
          skip = 1
          CYCLE
        END IF
      ! In quote processing.
      ELSE IF (in_quote) THEN
        IF (str(i:i) == end_quote) THEN
          in_quote = .FALSE.
        ELSE
          ! No special handling for space et.al.
          args(iarg)%item = args(iarg)%item // str(i:i)
        END IF
      ELSE   ! Normal processing.
        ! Argument separator?
        IF (str(i:i) == ' ') THEN
          IF (LEN(args(iarg)%item) > 0) THEN
            ! Move onto the next argument
            CALL Append(args, '')
            iarg = iarg + 1
          END IF
        ELSE IF (str(i:i) == start_quote) THEN
          in_quote = .TRUE.
        ELSE
          ! Normal argument character.  Append it.
          args(iarg)%item = args(iarg)%item // str(i:i)
        END IF
      END IF
    END DO
    
    ! If no text has been added to the last argument then chop it off.
    IF (LEN(args(iarg)%item) == 0) CALL Shrink(args, 1)
    
    !---------------------------------------------------------------------------
    ! Parse the tokens.
    
    CALL ParseCmdLine(args, option_list, arguments, stat, message)
    
  END SUBROUTINE ParseCmdLine_str
  
  
  !*****************************************************************************
  !!
  !> Parse command line arguments supplied in a file.
  !!
  !! @param[in]     filename          The name of the file.
  !!
  !! @param[in,out] option_list       An array of CmdLineOption objects that 
  !! describe the valid command line options.  This is INOUT to avoid 
  !! resetting variables that already have defaults.
  !!
  !! @param[out]    arguments         A list of positional arguments - command 
  !! line arguments that are not options (no leading hyphen) and are not 
  !! option arguments.  Always allocated if @a stat is zero, will be zero 
  !! length if there were no arguments.
  !!
  !! @param[out]    stat              Error code - non-zero on error:
  !!
  !! @param[out]    message           Error message.
  
  SUBROUTINE ParseCmdFile_(filename, option_list, arguments, stat, message)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*), INTENT(IN) :: filename
    TYPE(CmdLineOption), INTENT(INOUT) :: option_list(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: arguments(:)
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: message
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(String) :: args(1)
    
    !***************************************************************************
    
    args(1)%item = '@' // filename
    CALL ParseCmdLine(args, option_list, arguments, stat, message)
    
  END SUBROUTINE ParseCmdFile_
  
  
  !*****************************************************************************
  !!
  !> Parse command line arguments.
  !!
  !! @param[in]     args              A list of argument strings.
  !!
  !! @param[in,out] option_list       An array of CmdLineOption objects that 
  !! describe the valid command line options.  This is INOUT to avoid 
  !! resetting variables that already have defaults.
  !!
  !! @param[out]    arguments         A list of positional arguments - command 
  !! line arguments that are not options (no leading hyphen) and are not 
  !! option arguments.  Always allocated if @a stat is zero, will be zero 
  !! length if there were no arguments.
  !!
  !! @param[out]    stat              Error code - non-zero on error.
  !!
  !! @param[out]    message           Error message.
  !!
  !! @param[out]    positions         Optional indices of the positions 
  !! of the positional arguments.
  
  SUBROUTINE ParseCmdLine_str1D( args, option_list, arguments, stat,  &
      message, positions )
    
    USE Strings
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(String), INTENT(IN) :: args(:)
    TYPE(CmdLineOption), INTENT(INOUT) :: option_list(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: arguments(:)
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: message
    INTEGER, INTENT(OUT), OPTIONAL, ALLOCATABLE :: positions(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: iarg           ! Current command line argument
    INTEGER :: i              ! Index into the argument.
    
    ! Name of the option extracted from the current argument.
    CHARACTER(:), ALLOCATABLE :: option_name
    
    ! Option (if any) attached to the current argument.
    CHARACTER(:), ALLOCATABLE :: option_value
    
    ! .TRUE. if the argument contains a colon (or whatever we are using as the 
    ! option separator) to separate option name from its value.
    LOGICAL :: has_colon
    
    ! Expanded arguments, including those read in from response files.
    TYPE(String), ALLOCATABLE :: expanded_args(:)
    
    ! Dummy filename list.
    TYPE(String) :: filenames(0)
    
    !***************************************************************************
    
    stat = 0
    ALLOCATE(arguments(0))
    IF (PRESENT(positions)) ALLOCATE(positions(0))
    
    ! Allocate the position components of the options to zero size.
    DO i = 1, SIZE(option_list)
      ALLOCATE(option_list(i)%Positions(0))
    END DO
    
    ! Expanded args set is initially the args that we've been given.
    expanded_args = args
    CALL expand_response_files(expanded_args, filenames, stat, message)
    IF (stat /= 0) RETURN
    
    ! Loop over all arguments
    iarg = 1
    ! We can handle SIZE(expanded_args) == 0
    arg_loop: DO WHILE (iarg <= SIZE(expanded_args))
      
      ! Some shells may allow through zero sized arguments.  Take these to 
      ! be meaningless.
      IF (LEN_TRIM(expanded_args(iarg)%item) == 0) THEN
        iarg = iarg + 1
        CYCLE
      END IF
      
      ! Check for option switch and parse the option
      IF ( (expanded_args(iarg)%item(1:1) == '-')  &
          .AND. (LEN(expanded_args(iarg)%item) > 1) ) THEN
        
        IF (expanded_args(iarg)%item(2:2) == '-') THEN
          ! Long option
          CALL get_option_name(  &
              expanded_args(iarg)%item(3:),  &
              option_name,  &
              option_value,  &
              has_colon )
          DO i = 1, SIZE(option_list)
            IF (.NOT. ALLOCATED(option_list(i)%LongName)) CYCLE
            IF (option_list(i)%LongName == option_name) THEN
              CALL on_found_option(  &
                  option_name,  &
                  option_value,  &
                  has_colon,  &
                  iarg, stat )
              IF (stat /= 0) RETURN
              iarg = iarg + 1
              CYCLE arg_loop
            END IF
          END DO
          ! Unknown argument
          message = 'Unknown option "' // option_name // '".'
          stat = 1
          RETURN
        ELSE
          ! Should be a short option.
          CALL get_option_name(  &
              expanded_args(iarg)%item(2:),  &
              option_name,  &
              option_value,  &
              has_colon )
          IF (LEN(option_name) == 1) THEN
            ! Short option
            DO i = 1, SIZE(option_list)
              IF (.NOT. ALLOCATED(option_list(i)%CharName)) CYCLE
              IF (option_list(i)%CharName == option_name) THEN
                CALL on_found_option(  &
                    '-' // option_name,  &
                    option_value,  &
                    has_colon,  &
                    iarg, &
                    stat )
                IF (stat /= 0) RETURN
                iarg = iarg + 1
                CYCLE arg_loop
              END IF
            END DO
            ! Unknown option.
            message = 'Unknown option "' // option_name // '".'
            stat = 1
            RETURN
          ELSE
            ! Unknown option.
            message = 'Unknown option "' // option_name // '".'
            stat = 1
            RETURN
          END IF
        END IF
        
      ELSE
        
        ! Positional argument (not an option)
        CALL Append(arguments, expanded_args(iarg)%item)
        IF (PRESENT(positions)) positions = [positions, iarg]
        
      END IF
      
      iarg = iarg + 1
      
    END DO arg_loop
    
  CONTAINS
    
    !***************************************************************************
    !
    ! Parses the argument for the found option.
    !
    ! @param[in]      option_name       The name of the option found.
    !
    ! @param[in]      opt_arg           Argument for the option.
    !
    ! @param[in]      has_colon         Flag to indicate that the option 
    ! and the argument for the option were separated by a colon.
    !
    ! @param[in]      position          Index to report against the option.
    !
    ! @param[out]     stat              Error code.
    
    SUBROUTINE on_found_option(option_name, opt_arg, has_colon, position, stat)
      
      !-------------------------------------------------------------------------
      ! Characteristics
      
      CHARACTER(*), INTENT(IN) :: option_name
      CHARACTER(*), INTENT(IN) :: opt_arg
      LOGICAL, INTENT(IN) :: has_colon
      ! Value to avoid potential aliasing issue.
      INTEGER, INTENT(IN), VALUE :: position
      INTEGER, INTENT(OUT) :: stat
      
      !*************************************************************************
      
      stat = 0
      
      ! Increment present count
      IF ( .NOT. option_list(i)%CanRepeat  &
          .AND. option_list(i)%PresentCount > 0 ) THEN
        message = 'Option "' // option_name // '" specified multiple times.'
        stat = 5
        RETURN
      END IF
      option_list(i)%PresentCount = option_list(i)%PresentCount + 1
      
      IF (.NOT. ALLOCATED(option_list(i)%Positions)) THEN
        ALLOCATE(option_list(i)%Positions(1))
        option_list(i)%Positions(1) = position
      ELSE
        option_list(i)%Positions = [option_list(i)%Positions, position]
      END IF
      
      SELECT CASE(option_list(i)%OptionType)
      CASE (ioptArg, iOptArgInt, iOptArgReal)    ! Mandatory argument
        CALL get_option_argument(opt_arg, stat)
        
      CASE (ioptArgOpt)   ! Option has optional argument
        IF (has_colon) THEN
          CALL get_option_argument(opt_arg, stat)
        ELSE
          ! No argument provided - nothing to do.
        END IF
        
      CASE DEFAULT        ! Option has no argument
        IF (has_colon) THEN
          message = 'Option "' // option_name  &
              // '" has an unexpected argument.'
          stat = 3
        END IF
        
      END SELECT
      
      IF (stat /= 0) RETURN
      
      ! Argument validation
      SELECT CASE(option_list(i)%OptionType)
      CASE (ioptArgInt)
        IF (validate_integer(  &
              option_list(i)%Argument(SIZE(option_list(i)%Argument))%item ))  &
            stat = 4
      CASE (ioptArgReal)
        IF (validate_real(  &
              option_list(i)%Argument(SIZE(option_list(i)%Argument))%item ))  &
            stat = 4
      END SELECT
      
    END SUBROUTINE on_found_option
    
    
    !***************************************************************************
    !
    ! Gets an argument for the current option.
    !
    ! @param[in]      opt_arg           The argument for the option.
    !
    ! @param[out]     stat              Error code.
    !
    ! If @a opt_arg is of non-zero length, then uses that.  Otherwise obtains 
    ! the next command line argument and uses that.  If there is no next 
    ! command line argument then sets message to something appropriate and 
    ! @a stat to 1, otherwise sets stat to 0.
    
    SUBROUTINE get_option_argument(opt_arg, stat)
      
      !-------------------------------------------------------------------------
      ! Characteristics
      
      CHARACTER(*), INTENT(IN) :: opt_arg
      INTEGER, INTENT(OUT) :: stat
      
      !*************************************************************************
      
      IF (LEN(opt_arg) == 0) THEN
        IF (iarg + 1 <= SIZE(expanded_args)) THEN
          CALL Append(option_list(i)%Argument, expanded_args(iarg+1)%item)
          iarg = iarg + 1
        ELSE
          ! Missing argument
          message = 'Option "' // option_name // '" is missing its argument.'
          stat = 2
          RETURN
        END IF
      ELSE
        CALL Append(option_list(i)%Argument, opt_arg)
      END IF
      
      stat = 0
      
    END SUBROUTINE get_option_argument
    
  END SUBROUTINE ParseCmdLine_str1D
  
  
  !*****************************************************************************
  !!
  !> Expand arguments that refer to response files, in a recursive manner.
  !!
  !! @param[in,out] args              The arguments to expand.
  !!
  !! @param[in]     filenames         The names of the response files seen 
  !! so far.  This is used in an attempt to stop response file recursion.
  !!
  !! @param[out]    stat              Error code - non zero on error.
  !!
  !! @param[out]    message           Error message - defined when @a stat 
  !! is non-zero.
  
  RECURSIVE SUBROUTINE expand_response_files(args, filenames, stat, message)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(String), INTENT(INOUT), ALLOCATABLE :: args(:)
    TYPE(String), INTENT(IN) :: filenames(:)
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: message
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Argument index.
    
    ! Arguments from a response file.
    TYPE(String), ALLOCATABLE :: file_args(:)
    
    ! List of filenames for recursively processing arguments read in from 
    ! a response file.
    TYPE(String), ALLOCATABLE :: new_filenames(:)
    
    !***************************************************************************
    
    i = 1
    DO WHILE (i <= SIZE(args))
      ! Guard against zero size argument.
      IF (LEN(args(i)%item) == 0) THEN
        i = i + 1
        CYCLE
      END IF
      
      ! Skip arguments that don't have the leading character.
      IF (args(i)%item(1:1) /= response_filename_char) THEN
        i = i + 1
        CYCLE
      END IF
      
      ! Test whether the filename has been seen before.
      IF (ANY(filenames == args(i)%item(2:))) THEN
        stat = 8
        message = 'The response file "' // args(i)%item(2:)  &
            // '" is being referenced recursively.'
        RETURN
      END IF
      
      ! Read the response file into file_args.
      CALL read_response_file(args(i)%item(2:), file_args, stat, message)
      IF (stat /= 0) RETURN
      
      ! Recursively expand the arguments read in from the response file 
      ! (they might also reference a response file).  To avoid infinite 
      ! recursion, append the name of the file being read by this iteration 
      ! of the loop to the list of nested response files.
      new_filenames = filenames
      CALL Append(new_filenames, args(i)%item(2:))
      CALL expand_response_files(file_args, new_filenames, stat, message)
      IF (stat /= 0) RETURN
      
      ! Expand args with the arguments read in from the reponse file.
      args = [args(:i-1), file_args, args(i+1:)]
      
      ! Process the next argument from the original list.
      i = i + SIZE(file_args)
    END DO
    
  END SUBROUTINE expand_response_files
  
  
  !*****************************************************************************
  !!
  !> Read in a response file and return the contents as an array of strings.
  !!
  !! @param[in]     filename          Name of the response file.
  !!
  !! @param[out]    args              Strings (lines) read from the response 
  !! file.
  !!
  !! @param[out]    stat              Error code - non zero on error.
  !!
  !! @param[out]    message           Error message - defined when @a stat 
  !! is non-zero.
  
  SUBROUTINE read_response_file(filename, args, stat, message)
    
    USE UnitUtilities
    USE FileUtilities
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*), INTENT(IN) :: filename
    TYPE(String), INTENT(OUT), ALLOCATABLE :: args(:)
    INTEGER, INTENT(OUT) :: stat
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: message
    
    !---------------------------------------------------------------------------
    ! Local variables.
    
    INTEGER :: unit           ! Logical unit for reading the file.
    
    !***************************************************************************
    
    ! Allocate the message buffer to a size large enough that we think 
    ! we'll get the IOMSG.
    ALLOCATE(CHARACTER(1024) :: message)
    
    CALL GetUnit(unit)
    OPEN( unit,  &
        FILE=filename,  &
        POSITION='REWIND',  &
        STATUS='OLD',  &
        ACTION='READ',  &
        IOSTAT=stat,  &
        IOMSG=message )
    IF (stat /= 0) THEN
      message = 'Error opening file "' // filename // '":' // TRIM(message)
      stat = 6
      CALL ReleaseUnit(unit)
      RETURN
    END IF
    
    CALL GetLines(unit, args, stat, message)
    IF (stat /= 0) THEN
      message = 'Error reading from file "' // filename // '":' &
          // TRIM(message)
      stat = 7
    END IF 
    
    CLOSE(unit)
    CALL ReleaseUnit(unit)
    
  END SUBROUTINE read_response_file
  
  
  !*****************************************************************************
  !!
  !> Split a string into option name and value.
  !!
  !! @param[in]     text              The string to split.
  !!
  !! @param[out]    name              If a colon is present in @a text, then 
  !! the part of @a text prior to the colon.  Otherwise all of @ text.
  !!
  !! @param[out]    arg               If a colon is present in @a text, then 
  !! the part of @a text after the colon.  Otherwise ''.
  !!
  !! @param[out]    had_colon         .TRUE. if @a text contained a colon, 
  !! .FALSE. otherwise.
  
  SUBROUTINE get_option_name(text, name, arg, had_colon)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: text
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: arg
    LOGICAL, INTENT(OUT) :: had_colon
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Index of the colon (or whatever separator character we are using).
    INTEGER :: colon_pos
    
    !***************************************************************************
    
    colon_pos = INDEX(text, option_separator_char)
    IF (colon_pos == 0) THEN
      name = text
      arg = ''
      had_colon = .FALSE.
    ELSE
      name = text(:colon_pos-1)
      arg = text(colon_pos+1:)
      had_colon = .TRUE.
    END IF
    
  END SUBROUTINE get_option_name
  
  
  !*****************************************************************************
  !!
  !> Validates that a string represents an INTEGER.
  !!
  !! @param[in]     str               String to validate.
  !!
  !! @return .TRUE. if the string validly represents an INTEGER, .FALSE. 
  !! otherwise.
  !!
  !! We just use Fortran internal IO to do the validation.
  
  FUNCTION validate_integer(str) RESULT(rc)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    LOGICAL rc
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Format string template for attempting to reading str as an integer.
    CHARACTER(*), PARAMETER :: fmt_valid = "('(I',I0,'.0)')"
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(10) :: fmt    ! Format for attempting to read str as an integer.
    INTEGER :: i            ! Result of the read.
    INTEGER :: stat         ! IOSTAT code.
    
    !***************************************************************************
    
    WRITE (fmt, fmt_valid) LEN(str)
    READ (str, fmt, IOSTAT=stat) i
    rc = stat /= 0
    
  END FUNCTION validate_integer
  
  
  !*****************************************************************************
  !!
  !> Validates that a string represents a REAL.
  !!
  !! @param[in]     str               String to validate.
  !!
  !! @return .TRUE. if the string validly represents a REAL, .FALSE. 
  !! otherwise.
  !!
  !! We use the same definition of real literals as the Fortran standard - 
  !! in fact we just use Fortran internal IO to do the validation.
  
  FUNCTION validate_real(str) RESULT(rc)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    LOGICAL :: rc
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    ! Format string template for attempting to reading str as a real.
    CHARACTER(*), PARAMETER :: fmt_valid = "('(E',I0,'.0)')"
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(10) :: fmt    ! Format for attempting to read str as a real.
    REAL :: r               ! Result of the read.
    INTEGER :: stat         ! IOSTAT code.
    
    !***************************************************************************
    
    WRITE (fmt, fmt_valid) LEN(str)
    READ (str, fmt, IOSTAT=stat) r
    rc = stat /= 0
    
  END FUNCTION validate_real
  
  
  !*****************************************************************************
  !!
  !> Returns the given command line argument in a deferred length character.
  !!
  !! @param[in]     number            Argument number.
  !!
  !! @param[out]    value             The command argument value.  Not 
  !! allocated on error.
  !!
  !! @param[out]    status            Optional error code from the 
  !! GET_COMMAND_ARGUMENT call - non-zero on error.
  
  SUBROUTINE GetCmdLineArgument_(number, value, status)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(IN) :: number
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: value
    INTEGER, INTENT(OUT), OPTIONAL :: status
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: length         ! Length of the argument.
    INTEGER :: local_status   ! Local error code.
    
    !***************************************************************************
    
    length = 0
    CALL GET_COMMAND_ARGUMENT(number, LENGTH=length, STATUS=local_status)
    
    IF (PRESENT(status)) status = local_status
    IF (local_status /= 0) RETURN
    
    ALLOCATE(CHARACTER(length):: value)
    CALL GET_COMMAND_ARGUMENT(number, VALUE=value)
    
  END SUBROUTINE GetCmdLineArgument_
  
END MODULE CmdLine
