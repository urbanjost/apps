! $Id: LabelsUtils.f90 2181 2016-06-05 01:32:08Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the LabelsUtils module.


!*******************************************************************************
!!
!> Defines procedures related to scanning labels.

MODULE LabelsUtils
  
  USE SourceLocations
  
  IMPLICIT NONE
  
  PRIVATE
  
  PUBLIC :: ScanLabel
  
  !-----------------------------------------------------------------------------
  
  !> Stores the results of scanning a label.
  TYPE, PUBLIC :: Label
    PRIVATE
    !> The value of the label.
    !!
    !! The default value of zero indicates an invalid label.
    INTEGER :: value = 0
    !> The location in source of the label.
    TYPE(SourceLocation) :: location
  CONTAINS
    PROCEDURE, PRIVATE :: value_of => label_value_of
    GENERIC :: OPERATOR(.ValueOf.) => value_of
    
    PROCEDURE :: GetLocation => label_GetLocation
    GENERIC :: OPERATOR(.LocationOf.) => GetLocation
  END TYPE Label
  
  INTERFACE Label
    MODULE PROCEDURE Label_constructor
  END INTERFACE Label
  
  !> Syntax rule name for a /label/.
  CHARACTER(*), PARAMETER, PUBLIC :: label_comp = 'R312'
  
  !> The maximum number of characters that can appear in a label.
  INTEGER, PARAMETER, PUBLIC :: MaxLabelLength = 5
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Scanning of statement labels (R312).
  !!
  !! @param[in]     text              Text for the label.  This is taken as 
  !! text rather than as a token so that it can be called from fixed form 
  !! parsing code, where the statement label appears in its own field.  
  !! The label characters need to have been packed to the left in the 
  !! variable (don't pass in "1 2 3").  Note that a statement can contain 
  !! at most five digits.
  !!
  !! @param[in]     loc               Location of the label (typically the 
  !! first character in it) to use for error reporting.
  !!
  !! @param[out]    label_value       The numeric identifier for the label, 
  !! which will be non-zero without other errors.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE ScanLabel(text, loc, label_value, err_list)
    
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SourceLocations
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*,KIND=scck), INTENT(IN) :: text
    TYPE(SourceLocation), INTENT(IN) :: loc
    TYPE(Label), INTENT(OUT) :: label_value
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    CHARACTER(10) :: num_text ! Buffer for maximum length text.
    CHARACTER(20) :: fmt      ! Format specification for scanning the label.
    INTEGER :: stat           ! Scanning (internal io) error code.
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    ! This test gets rid of labels that have leading signs.
    IF (VERIFY(text, '0123456789') /= 0) THEN
      CALL Add( err_list,  &
          CODE=errSyntax,  &
          COMPONENT=label_comp,  &
          LOCATION=loc,  &
          MSG='Statement label "' // text  &
            // '" contains non-digit characters.' )
      RETURN
    END IF
    
    IF (LEN_TRIM(text) > MaxLabelLength) THEN
      WRITE(num_text, "(I0)") MaxLabelLength
      CALL Add( err_list,  &
          CODE=errSyntax,  &
          LEVEL=errLevelWarning,  &
          COMPONENT=label_comp,  &
          LOCATION=loc,  &
          MSG='The statement label "' // text  &
            // '" contains more than ' // TRIM(num_text)  &
            // ' characters.' )
    END IF
    
    WRITE (fmt, "('(I',I0,')')") LEN(text)
    READ (text, fmt, IOSTAT=stat) label_value%value
    
    IF (stat /= 0) THEN
      CALL Add( err_list,  &
          CODE=errSyntax,  &
          COMPONENT=label_comp,  &
          LOCATION=loc,  &
          MSG='Bad statement label "' // text // '".' )
      RETURN
    END IF
    
    IF (label_value%value == 0) THEN
      CALL Add( err_list,  &
          CODE=errAllDigitsZero,  &
          COMPONENT='C304 on ' // label_comp,  &
          LOCATION=loc,  &
          MSG='At least one digit in a label shall be non-zero.' )
      RETURN
    END IF
    
    label_value%location = loc
    
  END SUBROUTINE ScanLabel
  
  
  !*****************************************************************************
  !!
  !> Construct a Label object given a non-zero value and a location.
  !!
  !! @param[in]     value             The value of the label.  This must be 
  !! non-zero.
  !!
  !! @param[in]     location          The source location for the label.
  !!
  !! @returns A constructed label.
  
  FUNCTION Label_constructor(value, location) RESULT(l)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    INTEGER, INTENT(IN) :: value
    TYPE(SourceLocation), INTENT(IN) :: location
    
    ! Function result.
    TYPE(Label) :: l
    
    !***************************************************************************
    
    l%value = value
    l%location = location
    
  END FUNCTION Label_constructor
  
  
  !*****************************************************************************
  !!
  !> Extract the integer value of the label.
  !!
  !! @param[in]     the_label         The label of interest.
  !!
  !! @returns The label, as an integer.
  !!
  !! Made elemental so that we can get the values of an array of labels as 
  !! an array of integers.  This is used in the output procedures for 
  !! computed goto, a vital part of the modern language.
  
  ELEMENTAL FUNCTION label_value_of(the_label) RESULT(the_value)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(Label), INTENT(IN) :: the_label
    
    ! Function result
    INTEGER :: the_value
    
    !***************************************************************************
    
    the_value = the_label%value
    
  END FUNCTION label_value_of
  
  
  !*****************************************************************************
  !!
  !> Get the location of the identifier.
  !!
  !! @param[in]     the_label         The label to be queried.
  !!
  !! @returns The value of the location component.
  
  FUNCTION label_GetLocation(the_label) RESULT(location)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(Label), INTENT(IN) :: the_label
    
    ! Function result.
    TYPE(SourceLocation) :: location
    
    !***************************************************************************
    
    location = the_label%location
    
  END FUNCTION label_GetLocation
  
END MODULE LabelsUtils
