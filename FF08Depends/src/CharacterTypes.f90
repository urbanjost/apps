! $Id: CharacterTypes.f90 2823 2019-03-24 07:41:09Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the CharacterTypes module.


!*******************************************************************************
!!
!> Procedures for classifying characters.
!!
!! These routines are used by the Scanner.
!!
!! We don't have a classification for the comment character, because it is 
!! source form dependent (there are several (position dependent) such 
!! characters in fixed form source).
!!
!! All the procedures defined in this module are elemental.  Character 
!! arguments are all specified to be of "source code character kind" (scck).
!!
!! Some of these procedures are a little pointless as they simply duplicate 
!! intrinsic equality comparison, but there you go.
!!
!! Test coverage of this module is implicit in test coverage of the scanner 
!! extensions.

MODULE CharacterTypes
  
  USE CompilerKinds
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: IsBlankChar
  PUBLIC :: IsLetterChar
  PUBLIC :: IsNameChar
  PUBLIC :: IsDigitChar
  PUBLIC :: IsSignChar
  PUBLIC :: IsKNCLChar
  PUBLIC :: IsExponentChar
  PUBLIC :: IsQuoteChar
  PUBLIC :: IsUnderscoreChar
  
  !-----------------------------------------------------------------------------
  
  ! All the following are defined to be length one only.
  
  !> Single quote character.
  CHARACTER(KIND=scck), PARAMETER :: single_quote = scck_''''
  
  !> Double quote character.
  CHARACTER(KIND=scck), PARAMETER :: double_quote = scck_'"'
  
  !> Underscore character.
  CHARACTER(KIND=scck), PARAMETER :: underscore = scck_'_'
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Tests whether the given character is classed as a blank.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if @a ch is a blank, .FALSE. otherwise.
  !!
  !! At the moment we consider a blank to be a space or a tab.
  
  ELEMENTAL FUNCTION IsBlankChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = (ch == scck_' ') .OR. (ch == ACHAR(9, scck))
    
  END FUNCTION IsBlankChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether the current character could be part of a keyword or the 
  !! first character in a name.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if @a char is a letter (3.1.2), .FALSE. otherwise.
  
  ELEMENTAL FUNCTION IsLetterChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    ! Use ASCII collation sequence to define the ranges for big letters and 
    ! little letters.
    b = (LGE(ch, scck_'A') .AND. LLE(ch, scck_'Z'))  &
        .OR. (LGE(ch, scck_'a') .AND. LLE(ch, scck_'z'))
    
  END FUNCTION IsLetterChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether the current character could be the second or subsequent 
  !! character in a name.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if ch is an alphanumeric-character (3.1.1), .FALSE. 
  !! otherwise.
  !!
  !! The first character in a name must also return .TRUE. from 
  !! is_keyword_char.
  !!
  !! A keyword char is also known in the standard as an alphanumeric-character 
  !! (R301 in 3.1.1).
  
  ELEMENTAL FUNCTION IsNameChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = IsLetterChar(ch)  &
        .OR. IsDigitChar(ch)  &
        .OR. (ch == underscore)
    
  END FUNCTION IsNameChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether a character is a digit.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if ch is a digit (3.1.3), .FALSE. otherwise.
  
  ELEMENTAL FUNCTION IsDigitChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    ! Use ASCII collation sequence to define the range.
    b = LGE(ch, scck_'0') .AND. LLE(ch, scck_'9')
    
  END FUNCTION IsDigitChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether a character could be used as a string delimiter.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if @a ch is a single quote (') or double quote ("), 
  !! .FALSE. otherwise.
  
  ELEMENTAL FUNCTION IsQuoteChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = (ch == single_quote) .OR. (ch == double_quote)
    
  END FUNCTION IsQuoteChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether a character could be an exponent in a real literal.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if @a ch is 'D' or 'E' (case insensitive), .FALSE. 
  !! otherwise.
  
  ELEMENTAL FUNCTION IsExponentChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = (ch == scck_'D')  &
        .OR. (ch == scck_'d')  &
        .OR. (ch == scck_'E')  &
        .OR. (ch == scck_'e')
    
  END FUNCTION IsExponentChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether a character could be part of a keyword, name, constant or 
  !! label.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if @a ch is a letter, quote or digit, .FALSE. otherwise.
  !!
  !! Characters considered part of a 'KNCL' include letters, digits, the 
  !! underscore and the quote characters.
  !!
  !! real-literals can also contain a + or - as part of their exponent, 
  !! but that must always follow an exponent char (E,D), which is handled 
  !! separately in get_real_exponent_seq.  Logical literals contain a leading 
  !! and a trailing dot.  These are not included in this test.
  !
  ! This is not currently used.
  
  ELEMENTAL FUNCTION IsKNCLChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = IsNameChar(ch) .OR. IsQuoteChar(ch)
    
  END FUNCTION IsKNCLChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether a character is a sign char in a real-literal exponent.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if @a ch is '+' or '-', .FALSE. otherwise.
  
  ELEMENTAL FUNCTION IsSignChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = (ch == scck_'+') .OR. (ch == scck_'-')
    
  END FUNCTION IsSignChar
  
  
  !*****************************************************************************
  !!
  !> Tests whether a character is the underscore character.
  !!
  !! @param[in]     ch                The character to test.
  !!
  !! @returns .TRUE. if @a ch is an underscore (_), .FALSE. otherwise.
  !!
  !! Possibly one of the less useful functions in this module.
  
  ELEMENTAL FUNCTION IsUnderscoreChar(ch) RESULT(b)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(KIND=scck), INTENT(IN) :: ch
    
    ! Function result
    LOGICAL :: b
    
    !***************************************************************************
    
    b = ch == underscore
    
  END FUNCTION IsUnderscoreChar
  
END MODULE CharacterTypes
