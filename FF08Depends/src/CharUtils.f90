! $Id: CharUtils.f90 1305 2014-09-02 12:58:46Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the CharUtils module.


!*******************************************************************************
!!
!> Various string utilities.
!!
!! This module has no dependencies on any other module, though it probably 
!! needs to depend on the CompilerKinds module - we haven't got around 
!! to putting 'scck' as the kind of every character variable, and perhaps 
!! we don't want to.
!!
!! Procedures were originally written for F95, which didn't have deferred 
!! length allocatable function results.  That made the ToString calls in 
!! particular quite tricky.  The code is probably more complicated than it 
!! needs to be as a consequence.
!!
!! @todo The number of procedures from this module that are actually 
!! required is something that should be tested one day.
!!
!! @todo Do we really need the REAL variants for ToString?  The format 
!! for an INTEGER is pretty obvious (logical perhaps to a lesser extent, 
!! but we have picked a certain convention), but code that allowed 
!! arbitrary real formatting would be a little fragile.

MODULE CharUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces.
  
  PUBLIC :: Split
  PUBLIC :: ToValue
  PUBLIC :: ToString
  PUBLIC :: UpperCase
  PUBLIC :: LowerCase
  PUBLIC :: CompareNoCase
  PUBLIC :: Rightmost
  
  PUBLIC :: OPERATOR(.EqNoCase.)
  
  !-----------------------------------------------------------------------------
  ! Interfaces
  
  !> Utility routine to convert a CHARACTER string to a numeric type.
  !!
  !! This generic interface is for a subroutine, all forms of which are 
  !! elemental.
  !!
  !! @param[in]     vs                The string to convert.
  !!
  !! @param[out]    val               A REAL, INTEGER or LOGICAL containing 
  !! the converted value.
  !!
  !! @param[out]    ierr              Error code - non zero on error.
  INTERFACE ToValue
    MODULE PROCEDURE ToValue_real
    MODULE PROCEDURE ToValue_int
    MODULE PROCEDURE ToValue_log
  END INTERFACE ToValue
  
  
  !> Utility routine to convert a value to a varying_string.
  !!
  !! This generic interface is for a function.
  !!
  !! @param[in]     val               A REAL, INTEGER or LOGICAL containing 
  !! the value to be converted.
  !!
  !! @param[in]     fmt               Optional (from the point of view of the 
  !! interface, not in terms of an OPTIONAL argument) format string for the 
  !! value.  If not present then list directed formatting is used.
  INTERFACE ToString
    MODULE PROCEDURE ToString_real
    MODULE PROCEDURE ToString_real_fmt
    MODULE PROCEDURE ToString_int
    MODULE PROCEDURE ToString_int_fmt
    MODULE PROCEDURE ToString_log
    MODULE PROCEDURE ToString_log_fmt
  END INTERFACE ToString
  
  !> Upper case a string.
  INTERFACE UpperCase
    MODULE PROCEDURE UpperCase_
  END INTERFACE UpperCase
  
  !> Lower case a string.
  INTERFACE LowerCase
    MODULE PROCEDURE LowerCase_
  END INTERFACE LowerCase
  
  !> Split a string at a particular character.
  INTERFACE Split
    MODULE PROCEDURE Split_
  END INTERFACE Split
  
  !> Get the rightmost characters from a string.
  INTERFACE Rightmost
    MODULE PROCEDURE Rightmost_
  END INTERFACE Rightmost
  
  !> Compare two strings in a case insensitive manner
  INTERFACE CompareNoCase
    MODULE PROCEDURE CompareNoCase_
  END INTERFACE CompareNoCase
  
  !> Operator form of CompareNoCase.
  INTERFACE OPERATOR(.EqNoCase.)
    MODULE PROCEDURE CompareNoCase_
  END INTERFACE OPERATOR(.EqNoCase.)
  
  !-----------------------------------------------------------------------------
  ! Private constants
  
  !> Format to use to convert a string to an integer or real value.
  !!
  !! This format is used to generate another format string that actually does 
  !! the conversion.  The integer item should be the width of the character 
  !! string.
  CHARACTER(*), PARAMETER :: fmt_tovalue = "('(G',I0,'.0)')"
  
  !> ASCII code for zero.
  INTEGER, PARAMETER :: ia_zero = IACHAR('0')
  
  !> ASCII code for nine
  INTEGER, PARAMETER :: ia_nine = IACHAR('9')
  
  !> Buffer length required for our variant of "list directed" writing of 
  !! a real.
  INTEGER :: real_list_buf_len = 15
  
  !> Format that we use for our variant of "list directed" writing of a 
  !! real.
  CHARACTER(*), PARAMETER :: fmt_real_list = "(1PG15.7E2)"
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Reads a REAL value from a CHARACTER string.
  !!
  !! @param[in]     str               The string to convert.
  !!
  !! @param[out]    val               The REAL value.
  !!
  !! @param[out]    ierr              Error code - non-zero on error, for 
  !! example if @a str does not represent a REAL value.
  !!
  !! Tolerant to using the comma as a decimal separator.
  !!
  !! (Of questionable use to ff08.)
  
  ELEMENTAL SUBROUTINE ToValue_real(str, val, ierr)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*), INTENT(IN) :: str
    REAL, INTENT(OUT) :: val
    INTEGER, INTENT(OUT) :: ierr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer for building the format specification for the actual READ.
    CHARACTER(10) :: fmt_string
    
    !***************************************************************************
    
    WRITE (fmt_string, fmt_tovalue) LEN(str)
    ! Does the string contain a comma?
    IF (INDEX(str, ',') == 0) THEN
      ! No comma
      READ (str, fmt_string, IOSTAT=ierr) val
    ELSE
      ! Try using european conventions
      READ (str, fmt_string, DECIMAL='COMMA', IOSTAT=ierr) val
    END IF
    
  END SUBROUTINE ToValue_real
  
  
  !*****************************************************************************
  !!
  !> Reads an INTEGER value from a CHARACTER string.
  !!
  !! @param[in]     str               The string to convert.
  !!
  !! @param[out]    val               The INTEGER value.
  !!
  !! @param[out]    ierr              Error code - non-zero on error, for 
  !! example if @a str does not represent an INTEGER value.
  
  ELEMENTAL SUBROUTINE ToValue_int(str, val, ierr)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*), INTENT(IN) :: str
    INTEGER, INTENT(OUT) :: val
    INTEGER, INTENT(OUT) :: ierr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer for building the format specification to read the value.
    CHARACTER(10) :: fmt_string
    
    !***************************************************************************
    
    WRITE (fmt_string, fmt_tovalue) LEN(str)
    READ (str, fmt_string, IOSTAT=ierr) val
    
  END SUBROUTINE ToValue_int
  
  
  !*****************************************************************************
  !!
  !> Reads a LOGICAL value from a varying string.
  !!
  !! @param[in]     str               The varying string object.
  !!
  !! @param[out]    val               The LOGICAL value.
  !!
  !! @param[out]    ierr              Error code - non-zero on error.
  !!
  !! Text representations are checked first (case-insensitively) - if @a str 
  !! contains 't' or 'true' then @a val is .TRUE. or if @a str contains 'f' or 
  !! 'false' then @a val is .FALSE..  Failing that, the logical is read as an 
  !! integer, and then C rules (anything non-zero is .TRUE.) are applied.
  !!
  !! (This procedure and its definitions of true and false representations is 
  !! unlikely to be relevant to ff08, it just came along for the ride.)
  
  ELEMENTAL SUBROUTINE ToValue_log(str, val, ierr)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*), INTENT(IN) :: str
    LOGICAL, INTENT(OUT) :: val
    INTEGER, INTENT(OUT) :: ierr
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer for building the format specification to read the value.
    CHARACTER(10) :: fmt_string
    
    ! Upper case copy of the input string.
    CHARACTER(LEN=LEN(str)) :: buffer
    
    ! Start character position for true/false testing (lets us skip the dot).
    INTEGER :: istart
    
    !***************************************************************************
    
    buffer = str
    IF (LEN(str) > 0) THEN
      IF (str(1:1) == '.') THEN
        istart = 2
      ELSE
        istart = 1
      END IF
    ELSE
      ! Zero length string
      ierr = 1
      RETURN
    END IF
    
    CALL UpperCase(buffer)
    IF (buffer(istart:) == 'T' .OR. buffer(istart:) == 'TRUE') THEN
      val = .TRUE.
      ierr = 0
    ELSE IF (buffer(istart:) == 'F' .OR. buffer(istart:) == 'FALSE') THEN
      val = .FALSE.
      ierr = 0
    ELSE
      WRITE (fmt_string, fmt_tovalue) LEN(buffer)
      READ (buffer, fmt_string, IOSTAT=ierr) val
    END IF
    
  END SUBROUTINE ToValue_log
  
  
  !*****************************************************************************
  !!
  !> Upper cases a string.
  !! 
  !! @param[in,out] str               The string to upper case.
  !!
  !! This function works if the character set is ASCII, and the characters 
  !! that need to be changed are in the 'a' to 'z' range of ASCII characters 
  !! (we need 'a' to 'z' and 'A' to 'Z' to be continuous and to match and we 
  !! only regard characters in the range 'a' to 'z' as characters that need 
  !! upper casing).
  
  ELEMENTAL SUBROUTINE UpperCase_(str)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(LEN=*), INTENT(INOUT) :: str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER i                 ! Index into string.
    INTEGER ic                ! Position of character in character set.
    
    !***************************************************************************
    
    DO i = 1, LEN(str)
      ic = ICHAR(str(i:i))
      ! See if we have a lower case character
      IF (ic >= ICHAR('a') .AND. ic <= ICHAR('z')) THEN
        ! Lower case character - make upper case
        str(i:i) = CHAR(ic + ICHAR('A') - ICHAR('a'))
      END IF
    END DO
    
  END SUBROUTINE UpperCase_
  
  
  !*****************************************************************************
  !!
  !> Lower cases a string.
  !! 
  !! @param[in,out] str               The string to lower case.
  !!
  !! This function works if the character set is ASCII, and the characters 
  !! that need to be changed are in the 'A' to 'Z' range of ASCII characters 
  !! (we need 'a' to 'z' and 'A' to 'Z' to be continuous and to match 
  !! and we only regard characters in the range 'A' to 'Z' as characters that 
  !! need lower casing).
  
  ELEMENTAL SUBROUTINE LowerCase_(str)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(LEN=*), INTENT(INOUT) :: str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER i                 ! Index into string.
    INTEGER ic                ! Position of character in character set.
    
    !***************************************************************************
    
    DO i = 1, LEN(str)
      ic = ICHAR(str(i:i))
      ! See if we have an upper case character
      IF (ic >= ICHAR('A') .AND. ic <= ICHAR('Z')) THEN
        ! Lower case character - make upper case
        str(i:i) = CHAR(ic + ICHAR('a') - ICHAR('A'))
      END IF
    END DO
    
  END SUBROUTINE LowerCase_
  
  
  !*****************************************************************************
  !!
  !> Provide a fixed length string representation of a REAL value.
  !!
  !! @param[in]     val               The value to be represented in the
  !! string.
  !!
  !! @return A string representation of @a val.
  !!
  !! There are some special cases:
  !! - zero is converted to '0.0' (string of length three).
  !! - positive integral values are converted to nnn.0.
  !! - negative integral values are converted to -nnn.0.
  !!
  !! I'm not sure how robust this is to extreme input.
  
  PURE FUNCTION ToString_real(val) RESULT(str)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    REAL, INTENT(IN) :: val
    
    ! Function result
    CHARACTER(LEN=len_real_val(val)) :: str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer to write out the real, if we decide that it doesn't look like 
    ! a simple number.
    CHARACTER(real_list_buf_len) :: buf
    
    !***************************************************************************
    
    ! Our own version of list directed formatting.
    IF (val == 0.0) THEN
      str = '0.0'
    ELSE IF (is_integral(val)) THEN
      str = ToString(NINT(val)) // '.0'
    ELSE
      WRITE (buf, fmt_real_list) val
      str = ADJUSTL(buf)
    END IF
    
  END FUNCTION ToString_real
  
  
  !*****************************************************************************
  !!
  !> Provide a fixed length string representation of a REAL value.
  !!
  !! @param[in]     val               The value to be represented in the 
  !! string.
  !!
  !! @param[in]     fmt               Edit descriptor to use to represent the 
  !! value.  A list-directed form of edit descriptor is used if @a fmt is '*'.
  !!
  !! @return A string representation of @a val.
  
  PURE FUNCTION ToString_real_fmt(val, fmt) RESULT(str)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    REAL, INTENT(IN) :: val
    CHARACTER(*), INTENT(IN) :: fmt
    
    ! Function result
    CHARACTER(LEN=len_real_fmt(val, fmt)) :: str
    
    !***************************************************************************
    
    IF (ADJUSTL(fmt) == '*') THEN
      str = ToString_real(val)
    ELSE
      ! str should be the right length
      WRITE (str, '(' // fmt // ')') val
    END IF
    
  END FUNCTION ToString_real_fmt
  
  
  !*****************************************************************************
  !!
  !> Provide a fixed length string representation of an INTEGER value.
  !!
  !! @param[in]     val               The value to be represented in the
  !! string.
  !!
  !! @return A string representation of @a val.
  
  PURE FUNCTION ToString_int(val) RESULT(str)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: val
    
    ! Function result
    CHARACTER(LEN=len_int_val(val)) :: str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Buffer for building the format specification to write the value.
    CHARACTER(10) :: fmt
    
    !***************************************************************************
    
    IF (val == 0) THEN
      str = '0'
    ELSE
      WRITE (fmt, "('(I',I0,')')") LEN(str)
      WRITE (str, fmt) val
    END IF
    
  END FUNCTION ToString_int
  
  
  !*****************************************************************************
  !!
  !> Provide a fixed length string representation of an INTEGER value.
  !!
  !! @param[in]     val               The value to be represented in the 
  !! string.
  !!
  !! @param[in]     fmt               Edit descriptor to use to represent the 
  !! value.  A list directed form is used if @a fmt is '*'.
  !!
  !! @return A string representation of @a val.
  
  PURE FUNCTION ToString_int_fmt(val, fmt) RESULT(str)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: val
    CHARACTER(*), INTENT(IN) :: fmt
    
    ! Function result
    CHARACTER(LEN=len_int_fmt(val, fmt)) :: str
    
    !***************************************************************************
    
    IF (ADJUSTL(fmt) == '*') THEN
      str = ToString(val)
    ELSE
      ! str should be the right length
      WRITE (str, '(' // fmt // ')') val
    END IF
    
  END FUNCTION ToString_int_fmt
  
  
  !*****************************************************************************
  !!
  !> Provide a fixed length string representation of a LOGICAL value.
  !!
  !! @param[in]     val               The value to be represented in the
  !! string.
  !!
  !! @return A string representation of @a val.
  
  FUNCTION ToString_log(val) RESULT(str)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    LOGICAL, INTENT(IN) :: val
    
    ! Function result
    CHARACTER(LEN=len_log_val()) :: str
    
    !***************************************************************************
    
    IF (val) THEN
      str = 'T'
    ELSE
      str = 'F'
    END IF
    
  END FUNCTION ToString_log
  
  
  !*****************************************************************************
  !!
  !> Provide a fixed length string representation of a LOGICAL value.
  !!
  !! @param[in]     val               The value to be represented in the 
  !! string.
  !!
  !! @param[in]     fmt               Edit descriptor to use to represent the 
  !! value.  A list directed form is used if @a fmt is '*'.
  !!
  !! @return A string representation of @a val.
  
  FUNCTION ToString_log_fmt(val, fmt) RESULT(str)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    LOGICAL, INTENT(IN) :: val
    CHARACTER(*), INTENT(IN) :: fmt
        
    ! Function result
    CHARACTER(LEN=len_log_fmt(fmt)) :: str
    
    !***************************************************************************
    
    IF (ADJUSTL(fmt) == '*') THEN
      str = ToString_log(val)
    ELSE
      ! str should be the right length
      WRITE (str, '(' // fmt // ')') val
      RETURN
    END IF
    
  END FUNCTION ToString_log_fmt
  
  
  !*****************************************************************************
  !!
  !> Calculates the length for the character variable that would be required 
  !! to hold a value formatted according to str.  This is just the width 
  !! component of the format.
  !!
  !! @param[in]     str               The string that contains the 
  !! descriptor (eg ES10.2, note no brackets).
  !!
  !! @returns The field width of the edit descriptor, or zero if no field 
  !! width was found.
  
  PURE FUNCTION len_fmt(str) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
  
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    INTEGER :: l
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! String index.
    
    !***************************************************************************
    
    ! Find the first numeric character
    DO i = 1, LEN(str)
      IF ( (IACHAR(str(i:i)) >= ia_zero)  &
          .AND. (IACHAR(str(i:i)) <= ia_nine) ) THEN
        ! Numeric character found - process from that point on
        l = len_fmt2(str(i:))
        RETURN
      END IF
    END DO
    
    l = 0 ! No width in format
    
  END FUNCTION len_fmt
  
  
  !*****************************************************************************
  !!
  !> Worker function for len_fmt - call to parse from the first numerical 
  !! character in a format on (after stripping out the type).
  !!
  !! @param[in]     str               The string that contains the partial 
  !! descriptor, starting with the width.
  !!
  !! You have a string like 10.2 - returns the value of the bit preceding the 
  !! decimal place (if it exists in fmt)
  
  PURE FUNCTION len_fmt2(str) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    INTEGER :: l
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! String index.
    
    !***************************************************************************
    
    ! Find the end of the string.  We know the first character is numeric.
    DO i = 2, LEN(str)
      IF ( (IACHAR(str(i:i)) < ia_zero)  &
          .OR. (IACHAR(str(i:i)) > ia_nine) ) THEN
        ! Non numeric character found - process up to that point
        l = len_fmt3(str(:i-1))
        RETURN
      END IF
    END DO
    ! Process the whole string...
    l = len_fmt3(str)
    
  END FUNCTION len_fmt2
  
  
  !*****************************************************************************
  !!
  !> Worker procedure for len_fmt2 - parses an integer string into a value.
  !!
  !! @param[in]     str               The integer string to be parsed.
  !!
  !! @return The integer in str, or zero if no integer could be parsed.
  !!
  !! You have a string like '10' - returns its value.  Does this by calling 
  !! ToValue.
  
  PURE FUNCTION len_fmt3(str) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    INTEGER :: l
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: stat           ! Error code.
    
    !***************************************************************************
    
    CALL ToValue(str, l, stat)
    IF (stat /= 0) l = 0
    
  END FUNCTION len_fmt3
  
  
  !*****************************************************************************
  !!
  !> Calculates the length for the character variable that would be required 
  !! to hold a value formatted according to str.  This is just the width 
  !! component of the format, if present, or our list directed length if not.
  !!
  !! @param[in]     str               The string that contains the 
  !! descriptor (eg ES10.2, note no brackets).
  !!
  !! @returns The field width of the edit descriptor, or zero if no field 
  !! width was found.
  
  PURE FUNCTION len_real_fmt(val, str) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    REAL, INTENT(IN) :: val
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    INTEGER :: l
    
    !***************************************************************************
    
    IF (ADJUSTL(str) == '*') THEN
      ! List directed formating requested.
      l = len_real_val(val)
    ELSE
      l = len_fmt(str)
    END IF
    
  END FUNCTION len_real_fmt
  
  
  !*****************************************************************************
  !!
  !> Calculates the length for the character variable that would be required 
  !! to hold a value formatted according to str.  This is just the width 
  !! component of the format, if present, or our list directed length if not.
  !!
  !! @param[in]     str               The string that contains the 
  !! descriptor (eg I10.2, note no brackets).
  !!
  !! @returns The field width of the edit descriptor, or zero if no field 
  !! width was found.
  
  PURE FUNCTION len_int_fmt(val, str) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: val
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    INTEGER :: l
    
    !***************************************************************************
    
    IF (ADJUSTL(str) == '*') THEN
      ! List directed formating requested.
      l = len_int_val(val)
    ELSE
      l = len_fmt(str)
    END IF
    
  END FUNCTION len_int_fmt
  
  
  !*****************************************************************************
  !!
  !> Calculates the length for the character variable that would be required 
  !! to hold a value formatted according to str.  This is just the width 
  !! component of the format, if present, or our list directed length if not.
  !!
  !! @param[in]     str               The string that contains the 
  !! descriptor (eg L10.2, note no brackets).
  !!
  !! @returns The field width of the edit descriptor, or zero if no field 
  !! width was found.
  
  PURE FUNCTION len_log_fmt(str) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: str
    
    ! Function result
    INTEGER :: l
    
    !***************************************************************************
    
    IF (ADJUSTL(str) == '*') THEN
      ! List directed formating requested.
      l = len_log_val()
    ELSE
      l = len_fmt(str)
    END IF
    
  END FUNCTION len_log_fmt
  
  
  !*****************************************************************************
  !!
  !> Worker procedure for ToString_real.
  !!
  !! @param[in]     val               Value that is being converted to a 
  !! string.
  !!
  !! @return The length of string required to hold a string representation of 
  !! @a val.
  !!
  !! The logic in this routine needs to be aligned with the logic that 
  !! actually creates the string in ToString_real.
  
  PURE FUNCTION len_real_val(val) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    REAL, INTENT(IN) :: val
    
    ! Function result
    INTEGER :: l
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(15) :: buf
    
    !***************************************************************************
    
    IF (val == 0.0) THEN
      l = 3   ! 0.0
    ELSE IF (is_integral(val)) THEN
      l = len_int_val(NINT(val)) + 2   ! nnnn.0
    ELSE
      WRITE (buf, fmt_real_list) val
      l = LEN_TRIM(ADJUSTL(buf))
    END IF
    
  END FUNCTION len_real_val
  
  
  !*****************************************************************************
  !!
  !> Worker procedure for len_tostring_int, that deals with the case where 
  !! the length should be determined by the value.
  !!
  !! @param[in]     val               Value that is being converted to a 
  !! string.
  !!
  !! @return The length of string required to hold a string representation of 
  !! @a val.
  !!
  !! The logic in this routine needs to be aligned with the logic that 
  !! actually creates the string in ToString_int.
  
  PURE FUNCTION len_int_val(val) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: val
    
    ! Function result
    INTEGER :: l
    
    !***************************************************************************
    
    IF (val == 0) THEN
      l = 1
    ELSE IF (val > 0) THEN
      l = INT(CEILING(LOG10(REAL(val + 1))))
    ELSE
      l = INT(CEILING(LOG10(-REAL(val - 1)))) + 1  ! for minus sign
    END IF
    
  END FUNCTION len_int_val
  
  
  !*****************************************************************************
  !!
  !> Calculates the length of character variable required to hold a 
  !! representatation of a LOGICAL value.
  !!
  !! @param[in]     fmt               Optional edit descriptor to use for 
  !! formatting the value.  If not present then the length is one.
  !!
  !! @returns The length of string required to represent @a val.
  
  PURE FUNCTION len_log_val() RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
     
    ! Function result
    INTEGER :: l
    
    !***************************************************************************
    
    l = 1
    
  END FUNCTION len_log_val
  
  
  !*****************************************************************************
  !!
  !> Utility routine that returns .TRUE. if a real number is very, very, close 
  !! to an integer number (so it could be represented without large numbers 
  !! of decimal places.
  !!
  !! @param[in]     val               The value of interest.
  !!
  !! @returns .TRUE. if @a val represents an integer value.
  
  PURE FUNCTION is_integral(val) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    REAL, INTENT(IN) :: val
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    IF (ABS(val) < 1E6) THEN
      b = ABS(REAL(NINT(val), KIND(val)) - val) < EPSILON(val)
    ELSE
      b = .FALSE.
    END IF
    
  END FUNCTION is_integral
  
  
  !*****************************************************************************
  !!
  !> Splits a string into two substrings with the substrings separated by the 
  !! occurrence of a character from a specified separator set.
  !!
  !! Modelled on the iso_varing_string procedure.
  !!
  !! @param[in,out] string            The string to be %split and the 
  !! remainder of the string after splitting, not including the separator 
  !! character.
  !!
  !! @param[out]    word              The characters passed over in the 
  !! search for a separator character from @a set.
  !!
  !! @param[in]     set               The set of characters to use as a 
  !! separator character for the splitting.
  !!
  !! @param[out]    separator         Optional character that actually 
  !! separated @a word from the remainder of @a string.
  !!
  !! @param[in]     back              An optional LOGICAL to indicate a 
  !! backward search if present with the value .TRUE.  A forward search is 
  !! conducted if not present or if the value is .FALSE..
  !!
  !! The effect of the procedure is to divide @a string at the first 
  !! occurrence of a character that is in @a set.  If no character from 
  !! @a set if found or if @a string is of zero length, the whole string is 
  !! returned in @a word, @a string is returned as zero length and 
  !! @a separator (if present) is returned as zero length.  The effect of the 
  !! procedure is such that, on return, either @a word // @a separator // 
  !! @a string is the same as the initial string for a forward search, of 
  !! @a string // @a separator // @a word is the same as the initial string 
  !! for a backward search.
  
  PURE SUBROUTINE Split_(string, word, set, separator, back)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(:), INTENT(INOUT), ALLOCATABLE :: string
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: word
    CHARACTER(LEN=*), INTENT(IN) :: set
    CHARACTER(:), INTENT(OUT), ALLOCATABLE, OPTIONAL :: separator
    LOGICAL, INTENT(IN), OPTIONAL :: back
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    LOGICAL :: local_back     ! Local copy of back, defaulted if not present.
    INTEGER :: i_separator    ! String index of the separator.
    
    !***************************************************************************
    
    ! Set default for the optional back argument.
    IF (PRESENT(back)) THEN
      local_back = back
    ELSE
      local_back = .false.
    END IF
    
    !---------------------------------------------------------------------------
    
    ! Go looking for the separator, forwards or reverse as appropriate.
    i_separator = SCAN(string, set, local_back)
    
    IF (i_separator /= 0) THEN
      ! We found a separator.
      IF (local_back) THEN
        ! Reverse search
        word = string(i_separator+1:)
        if (PRESENT(separator)) separator = string(i_separator:i_separator)
        string = string(:i_separator-1)
      ELSE
        ! Forward search
        word = string(:i_separator-1)
        if (PRESENT(separator)) separator = string(i_separator:i_separator)
        string = string(i_separator+1:)
      END IF
    ELSE
      ! No separator.
      word = string
      IF (PRESENT(separator)) separator = ''
      string = ''
    END IF
    
  END SUBROUTINE Split_
  
  
  !*****************************************************************************
  !!
  !> Compares two strings in a case insensitive manner.
  !!
  !! @param[in]     lhs               One of the strings to test.
  !!
  !! @param[in]     rhs               The other string to test.
  !!
  !! @returns .TRUE. if the strings are the same regardless of case, .FALSE. 
  !! otherwise.
  !!
  !! The comparison is done by upper casing a copy of both strings.
  
  ELEMENTAL FUNCTION CompareNoCase_(lhs, rhs) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: lhs
    CHARACTER(*), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    
    ! Copy of the lhs, upper cased.
    CHARACTER(LEN(lhs)) :: lhs_uppercased
    
    ! Copy of the rhs, upper cased.
    CHARACTER(LEN(rhs)) :: rhs_uppercased
    
    !***************************************************************************
    
    lhs_uppercased = lhs
    rhs_uppercased = rhs
    
    CALL UpperCase(lhs_uppercased)
    CALL UpperCase(rhs_uppercased)
    
    b = lhs_uppercased == rhs_uppercased
    
  END FUNCTION CompareNoCase_
  
  
  !*****************************************************************************
  !!
  !> Extract the rightmost n characters from a string.
  !!
  !! @param[in]     str               The string to extract the characters 
  !! from.
  !!
  !! @param[in]     n                 The number of characters to extract.
  !!
  !! @returns The rightmost @a n characters of @a str, or the entirety of 
  !! @a str if the length of @a str is less than @a n.
  
  PURE FUNCTION Rightmost_(str, n) RESULT(right)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(*), INTENT(IN) :: str
    INTEGER, INTENT(IN) :: n
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: right
    
    !***************************************************************************
    
    ALLOCATE(CHARACTER(MIN(n,LEN(str))) :: right)
    
    right = str(LEN(str) - LEN(right) + 1:)
    
  END FUNCTION Rightmost_
  
END MODULE CharUtils
