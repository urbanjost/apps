! $Id: Errors.f90 1397 2014-10-18 16:38:31Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Errors module.


!*******************************************************************************
!!
!> Types and procedures for handling error codes and messages.
!!
!! Way back when, we figured that it would be better to take advantage of the 
!! inherent support for arrays in the Fortran language, and define an error 
!! object that represented a single error (multiple errors - use an array).
!!
!! However, that means that procedures that need to return lists of errors 
!! (the near ubiquitous err_list argument) need to have that argument 
!! allocatable.
!!
!! For reasons of consistency we also decided that argument must always be 
!! returned as a zero length array if there were no errors, rather than just 
!! being unallocated (this saves us from constantly having to test the 
!! allocation status of that argument after a procedure call).
!!
!! That then means that we always have this ALLOCATE(err_list(0)) type of 
!! call littering the landscape.  This has introduced a significant bias 
!! into the wear rates of particular keys on my keyboard.
!!
!! Perhaps we should define an explicit error list type that encapsulates 
!! such a list.
!!
!! See the ErrorTests module for some tests.
!!
!! Compiler bugs associated with polymorphic components have complicated 
!! things in this module - some of the code is perhaps unnecessarily 
!! obtuse.  In some cases those bugs have morphed with time, which means 
!! that the original work arounds have similarly morphed.  There's also 
!! the possibility that there are some outright coding errors - hard to 
!! tell.  But if users have errors in their code then they deserve the 
!! odd obscure crash anyway.
!!
!! @todo Clean it all up once we are happy with compiler support.

MODULE Errors
  
  USE Strings
  USE ErrorLevels
  USE ErrorLocations
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! Expose module procedures and interfaces
  
  PUBLIC :: ConstructError
  PUBLIC :: Add
  PUBLIC :: Remove
  PUBLIC :: Max
  PUBLIC :: MaxVal
  PUBLIC :: NoError
  PUBLIC :: Failure
  PUBLIC :: Fatal
  PUBLIC :: Empty
  
  PUBLIC :: OPERATOR(>)
  PUBLIC :: OPERATOR(>=)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  PUBLIC :: OPERATOR(<=)
  PUBLIC :: OPERATOR(<)
  PUBLIC :: OPERATOR(.Describe.)
  
  PUBLIC :: ASSIGNMENT(=)
  
  !-----------------------------------------------------------------------------
  
  !> An element for a vector or array of locations.
  TYPE location_element
    !> An item in the vector.
    CLASS(ErrorLocation), ALLOCATABLE :: item
  END TYPE location_element
  
  !-----------------------------------------------------------------------------
  ! The error type.
  
  !> Represents a particular error or message.
  !!
  !! An error object comprises at a minimum a level, indicating the severity 
  !! of the error.  The object can also contain a code (a number) a message, 
  !! a location and a component.
  !!
  !! The component and location components are arrays to allow for 
  !! chaining, where lower level components (or locations) are referenced 
  !! via higher level components (or files).
  !!
  !! Note that the code, level and line members are given default 
  !! definitions.  This stops us running into undefined variable issues with 
  !! some of the utility procedures.  The default definition is for 
  !! "no error".
  !!
  !! The ConstructError function also gives zero string lengths to the 
  !! @a component and @a msg components.
  !!
  !! When an operation that returns an error level of errLevelError it means 
  !! that the operation failed, but there is the possibility that if the 
  !! operation was repeated with different parameters then the operation could 
  !! ultimately succeed.  As a result, it is important that operations that 
  !! return errLevelError leave the internal state of the program in a 
  !! consistent, well-defined and continuable condition.  If that is not 
  !! possible then the operation should return errLevelFatal - there is no 
  !! hope of success, and the whole parent task (however that might be 
  !! defined - often the entire process or program) should be abandoned.
  !!
  !! Clearly, being able to recover from an error is preferably to losing the 
  !! entire process, so if possible errLevelError is preferred to 
  !! errLevelFatal.
  !!
  !! If messages are provided in conjunction with error level of None, then 
  !! the message is assumed to be a simple text string that should be 
  !! reported back to the user without the usual error context.  To get 
  !! context, set the error level to errLevelInfo.
  !!
  !! @todo Consider whether our chaining model is sufficient.  In the general 
  !! case it might be more suitable to have child errors - so that each level 
  !! of what is the current chain can have a code, a message, a level, etc.  
  !! Alternatively, perhaps that's better handled by using the array form 
  !! of error return (though that imposes requirements on parent errors being 
  !! more severe than lower errors if the comparison operators are to work).  
  !! Another alternative might be to chain things like the code and message, 
  !! but leave the level as a single concept for a particular error object.
  TYPE, PUBLIC :: Error
    PRIVATE
    
    !> User defined error code.
    !!
    !! The convention is that the error code ties in with the first component 
    !! (the lowest level component) in the chain.
    INTEGER :: code = 0
    
    !> Error severity.
    TYPE(ErrorLevel) :: level = errLevelNone
    
    !> Message describing the error.
    !!
    !! If not allocated, then the error has no associated message.
    CHARACTER(:), ALLOCATABLE :: msg
    
    !> Program component (or chain of components) that generated the error.
    !!
    !! The component in the first element is the lowest level 
    !! component (a sub-component of other components in the array).
    !!
    !! If not allocated then no component information was available.
    !!
    !! The component is not considered when testing for empty errors - an 
    !! error object that consists of just a component is not considered 
    !! to contain any useful information.
    TYPE(String), ALLOCATABLE :: component(:)
    
    !> File (or chain of files) that generated the error.
    !!
    !! The file in the first element is the lowest level file 
    !! (other files may have included that file).
    !!
    !! If not allocated then no location information was available.
    !!
    !! Location is not considered when testing for empty errors - an 
    !! error object that consists of just a location is not considered to 
    !! contain useful information.
    TYPE(location_element), ALLOCATABLE :: location(:)
  END TYPE Error
  
  !-----------------------------------------------------------------------------
  ! Construction and assignment
  
  !> Construct an error object.
  !!
  !! @todo Convert to be a structure constructor when compiler support 
  !! sufficient.
  INTERFACE ConstructError
    MODULE PROCEDURE Error_err
  END INTERFACE ConstructError
  
  !> Add an error to an array of errors
  INTERFACE Add
    MODULE PROCEDURE Add_all
    MODULE PROCEDURE Add_list
  END INTERFACE Add
  
  !> Remove an error from an array of errors
  INTERFACE Remove
    MODULE PROCEDURE Remove_err
  END INTERFACE Remove
  
  !> Assignment
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE Assignment_to_integer
    MODULE PROCEDURE Assignment_to_level
    ! ifort bug work around - assignment to dt with polymorphic component.
    ! @todo fix when the compiler eventually gets fixed.  One day.  Maybe.
    module procedure assignment_bug
  END INTERFACE ASSIGNMENT(=)
  
  ! The following two are to avoid having to USE ErrorLevels all the time.
  
  !> Test for failure.
  INTERFACE Failure
    MODULE PROCEDURE Failure_err
    MODULE PROCEDURE Failure_err_list
  END INTERFACE Failure
  
  !> Test for disaster.
  INTERFACE Fatal
    MODULE PROCEDURE Fatal_err
    MODULE PROCEDURE Fatal_err_list
  END INTERFACE Fatal
  
  !> Test for an empty error (no information).
  INTERFACE Empty
    MODULE PROCEDURE Empty_err
  END INTERFACE Empty
  
  !-----------------------------------------------------------------------------
  ! Utility procedures
  
  !> Obtain worst error from two errors.
  INTERFACE Max
    MODULE PROCEDURE Max_err
  END INTERFACE
  
  !> Obtain worst error from a list.
  INTERFACE MaxVal
    MODULE PROCEDURE MaxVal_err
  END INTERFACE MaxVal
  
  !> Return an value that represents "no error".  This value will be treated 
  !! as an empty error by various routines.
  INTERFACE NoError
    MODULE PROCEDURE NoError_
  END INTERFACE NoError
  
  !-----------------------------------------------------------------------------
  ! Defined operations
  
  ! Note that shortcut versions (non-elemental) are "available" only for 
  ! > and >= (i.e. you could see if a list (array) of errors exceeds a certain 
  ! threshold).  In the end the elemental version proved more useful, so 
  ! we commented them out of the interface blocks below.
  
  !> Test an error object against something else in a "greater than" context.
  INTERFACE OPERATOR(>)
    MODULE PROCEDURE gt_error_err
    MODULE PROCEDURE gt_error_level
    MODULE PROCEDURE gt_error_code
  END INTERFACE OPERATOR(>)
  
  !> Test an error object against something else in a "greater than or equal 
  !! to" context.
  INTERFACE OPERATOR(>=)
    MODULE PROCEDURE ge_error_err
    MODULE PROCEDURE ge_error_level
    MODULE PROCEDURE ge_error_code
  END INTERFACE OPERATOR(>=)
  
  !> Equality comparison of an error with a level or an integer code.
  !!
  !! We don't provide equality comparison with error objects themselves 
  !! to avoid ambiguity about what an exact match entails.
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE eq_error_level
    MODULE PROCEDURE eq_error_code
  END INTERFACE OPERATOR(==)
  
  !> Inequality comparison of an error with a level or an integer code.
  !!
  !! We don't provide inequality comparison with error objects themselves 
  !! to avoid ambiguity about what an inexact match entails.
  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE ne_error_level
    MODULE PROCEDURE ne_error_code
  END INTERFACE OPERATOR(/=)
  
  !> Test an error object against something else in a "less than or equal 
  !! to" context.
  INTERFACE OPERATOR(<=)
    MODULE PROCEDURE le_error_err
    MODULE PROCEDURE le_error_level
    MODULE PROCEDURE le_error_code
  END INTERFACE OPERATOR(<=)
  
  !> Test an error object against something else in a "less than" context.
  INTERFACE OPERATOR(<)
    MODULE PROCEDURE lt_error_err
    MODULE PROCEDURE lt_error_level
    MODULE PROCEDURE lt_error_code
  END INTERFACE OPERATOR(<)
  
  !> Generate a text description of an error message.
  INTERFACE OPERATOR(.Describe.)
    !> @details See Errors::describe_err.
    MODULE PROCEDURE describe_err
  END INTERFACE OPERATOR(.Describe.)
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Construction and finalisation.
  
  
  !*****************************************************************************
  !!
  !> Constructs an Error object.
  !!
  !! @param[in]     original          Optional error object to use as a 
  !! "template" for the new error object.  Other parameters, if present, 
  !! override the settings in this object.  If not present then the other 
  !! parameters specify a new error object from scratch.  If present and 
  !! contains an empty error value, and no other arguments are present (or 
  !! they are present but with values equivalent to that for an empty error 
  !! value) then the result will be an empty error value.
  !!
  !! @param[in]     code              Optional error code.  The meaning of 
  !! this is up to the user.  If not present then the constructed error 
  !! either has the same code as @a original, or zero if @a original is 
  !! also not present.
  !!
  !! @param[in]     level             Optional severity level.  If not present 
  !! then the constructed object either has the same severity as @a original, 
  !! or a severity of "Error" if @a original is also not present.
  !!
  !! @param[in]     msg               Optional descriptive error message. If 
  !! not present then the constructed error object has the same message as 
  !! @a original, or no description if @a original is also not present.
  !!
  !! @param[in]     component         Optional component causing the error.  
  !! If not present then the constructed error object has either the same 
  !! component as @a original, or no source component if @a original is also 
  !! not present.  If present, then the component information may be chained 
  !! with the original error's component (if present) - see comments for 
  !! @a chain.
  !!
  !! @param[in]     location          Optional location of the error.  If 
  !! not present then the constructed error object has either the same 
  !! location as @a original, or no location information if @a original is 
  !! also not present.  If present then the location information may be 
  !! chained with the original error's location (if present) - see 
  !! coments for @a chain.
  !!
  !! @param[in]     chain             Optional flag to indicate if the 
  !! component information from @a component or the location information from 
  !! @a location should be chained to existing components or location 
  !! information in @a original or just override the existing information.  
  !! If not present then the component and location will be chained.  Has no 
  !! effect if @a components or @a location are not present.
  !!
  !! Note that calling Construct_err with no arguments does not give the 
  !! same value as calling NoError, or the same value as a default initialised 
  !! Error entity.  An error value initialised by Construct_err is an "error".
  !!
  !! If @a original is present, but is an empty error, and the only 
  !! arguments present with non-empty values are @a component or @a location, 
  !! then the resulting error is still considered an empty error.    This 
  !! means that:
  !! @code
  !! err = Error(err_list, empty_error, COMPONENT='my component')
  !! @endcode
  !! behaves.
  
  PURE FUNCTION Error_err( original, code, level, msg, component, &
      location, chain ) RESULT(err)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN), OPTIONAL :: original
    INTEGER, INTENT(IN), OPTIONAL :: code
    TYPE(ErrorLevel), INTENT(IN), OPTIONAL :: level
    CHARACTER(*), INTENT(IN), OPTIONAL :: msg
    CHARACTER(*), INTENT(IN), OPTIONAL :: component
    CLASS(ErrorLocation), INTENT(IN), OPTIONAL :: location
    LOGICAL, INTENT(IN), OPTIONAL :: chain
    
    ! Function result
    TYPE(Error) :: err
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    LOGICAL :: local_chain    ! Local copy of chain, defaulted appropriately.
    
    ! Flag to indicate that the original error object was empty.
    LOGICAL :: original_empty
    
    ! Flag to indicate that the new error object is empty.
    LOGICAL :: new_empty
    
    !***************************************************************************
    
    ! Determine the empty state of the original error.  If there no original 
    ! error was provided then it is considered empty.
    IF (PRESENT(original)) THEN
      original_empty = Empty(original)
    ELSE
      original_empty = .TRUE.
    END IF
    
    ! Determine whether chaining of component and location should happen.
    IF (PRESENT(chain)) THEN
      local_chain = chain
    ELSE
      local_chain = .TRUE.
    END IF
    
    ! Start off assuming that the result of this function contains no useful 
    ! information 
    new_empty = .TRUE.
    
    IF (PRESENT(code)) THEN
      err%code = code
      IF (code /= 0) new_empty = .FALSE.
    ELSE IF (PRESENT(original)) THEN
      err%code = original%code
    ELSE
      err%code = 0
    END IF
    
    IF (PRESENT(level)) THEN
      err%level = level
      IF (level /= errLevelNone) new_empty = .FALSE.
    ELSE IF (PRESENT(original)) THEN
      err%level = original%level
    ELSE
      err%level = errLevelError
    END IF
    
    ! If no message provided, then we leave err%msg unallocated.
    IF (PRESENT(msg)) THEN
      err%msg = msg
      new_empty = .FALSE.
    ELSE IF (PRESENT(original)) THEN
      IF (ALLOCATED(original%msg)) err%msg = original%msg
    END IF
    
    ! We do component and location last so we can test for emptiness after 
    ! processing all the other arguments.  The component and location don't 
    ! affect emptiness testing.  If both the original error and new errors 
    ! are empty then there's no point saving the component and location 
    ! information.
    IF (new_empty .AND. original_empty) RETURN
    
    ! If no component provided then we leave err%component unallocated.
    IF (PRESENT(component)) THEN
      IF (PRESENT(original)) THEN
        IF (ALLOCATED(original%component)) THEN
          ! New component specified and the original error has component 
          ! information.  ! We chain components if requested and the original 
          ! error was not empty.
          IF (local_chain .AND. (.NOT. original_empty)) THEN
            ! Chaining requested.  Chaining only happens if we are not 
            ! empty.
            err%component = [original%component, String(component)]
          ELSE
            ! Chaining not requested or the original object was empty.
            err%component = [String(component)]
          END IF
        ELSE
          ! New component specified, original error had no component.
          err%component = [String(component)]
        END IF
      ELSE
        ! New component provided, no original error.
        err%component = [String(component)]
      END IF
    ELSE IF (PRESENT(original)) THEN
      IF (ALLOCATED(original%component)) THEN
        ! No new component, original error had component information.
        err%component = original%component
      END IF
    END IF
    
    ! locations cause too much havoc.
    !return
    
    ! Comment out bits are intention, sans ifort 12.1 and 13.0.0 bugs.
    IF (PRESENT(location)) THEN
      IF (PRESENT(original)) THEN
        IF (ALLOCATED(original%location)) THEN
          IF (local_chain) THEN
            ! New location specified, original error had a location and 
            ! chaining requested.  Append the new location to the original 
            ! location.
!            err%location = [original%location, location_element(location)]
            ALLOCATE( err%location(SIZE(original%location)),  &
                SOURCE=original%location )
            CALL add_location(err%location, location)
          ELSE
            ! New location specified, original error had a location but 
            ! chaining not requested.  New location overrules original 
            ! location.
!            err%location = [err%location, location_element(location)]
            CALL add_location(err%location, location)
          END IF
        ELSE
          ! New location specified, original error had no location.
!          err%location = [location_element(location)]
          CALL add_location(err%location, location)
        END IF
      ELSE
        ! New location specified, no original error.
!        err%location = [location_element(location)]
        CALL add_location(err%location, location)
      END IF
    ELSE
      IF (PRESENT(original)) THEN
        IF (ALLOCATED(original%location)) THEN
          ! No new location, original error had a location.
!          err%location = original%location
          ALLOCATE( err%location(SIZE(original%location)),  &
              SOURCE=original%location )
        END IF
      END IF
    END IF
    
  END FUNCTION Error_err
  
  
  !*****************************************************************************
  !!
  !> Implements defined assignment from an error object to an integer.  This 
  !! is one way of extracting the error code.
  !!
  !! @param[out]    code              The integer to be assigned with the 
  !! error code.
  !!
  !! @param[in]     err               The error object to assign.
  !!
  !! The error code is transferred to the integer argument.
  
  ELEMENTAL SUBROUTINE Assignment_to_integer(code, err)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    INTEGER, INTENT(OUT) :: code
    TYPE(Error), INTENT(IN) :: err
    
    !***************************************************************************
    
    code = err%code
    
  END SUBROUTINE Assignment_to_integer
  
  
  !*****************************************************************************
  !!
  !> Implements defined assignment from an error object to an ErrorLevel 
  !! object.  This is one way of extracting the error level.
  !!
  !! @param[out]    level             The error level object to be assigned 
  !! the error level of @a err.
  !!
  !! @param[in]     err               The Error object.
  
  ELEMENTAL SUBROUTINE Assignment_to_level(level, err)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(ErrorLevel), INTENT(OUT) :: level
    TYPE(Error), INTENT(IN) :: err
    
    !***************************************************************************
    
    level = err%level
    
  END SUBROUTINE Assignment_to_level
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Utility procedures
  
  
  !*****************************************************************************
  !!
  !> Adds an error, either specified by a single error object or specified by 
  !! the individual parameters of the error, or the appropriate combination 
  !! of both, to an ErrorGroup.
  !!
  !! @param[in,out] err_list          The error list.  Does not have to be 
  !! allocated on entry, will always be allocated on exit, though possibly 
  !! to zero size.
  !!
  !! @param[in]     original          Optional error object to be added.  
  !! Other parameters, if present, override the settings in this object.  
  !! If not present then the other parameters specify a new error object 
  !! from scratch.  If this is an empty error value and no other arguments 
  !! are present (or are present, but have the equivalent empty error values) 
  !! then nothing is added to @a err_list.
  !!
  !! @param[in]     code              Optional error code.  The meaning of 
  !! this is up to the user.  If not present then the constructed error 
  !! either has the same code as @a original, or zero if @a original is 
  !! also not present.
  !!
  !! @param[in]     level             Optional severity level.  If not present 
  !! then the constructed object either has the same severity as @a original, 
  !! or a severity of "Error" if @a original is also not present.
  !!
  !! @param[in]     msg               Optional descriptive error message. If 
  !! not present then the constructed error object has the same message as 
  !! @a original, or no description if @a original is also not present.
  !!
  !! @param[in]     component         Optional component causing the error.  
  !! If not present then the constructed error object has either the same 
  !! component as @a original, or no source component if @a original is also 
  !! not present.  See also comments below.
  !!
  !! @param[in]     line              Optional line number of the error.  If 
  !! not present the the constructed error object's line number is either the 
  !! same line as @a original, or zero if @a original is also not present.
  !!
  !! If @a level is specified as errLevelNone and all other values are either 
  !! not present or are at their not present values then the resulting error 
  !! is considered 'empty' and nothing will be added to the list.  In this 
  !! case the allocation status of @a err_list is unchanged.
  !!
  !! @param[in]     chain             Optional flag to indicate if the 
  !! components from @a component should be chained to existing components 
  !! of @a original or just override the component.  If not present then 
  !! the component will be chained.  Has not effect if @a components or 
  !! @a original is not present.  See also comments below.
  !!
  !! Calling this procedure with no arguments actually adds an error to the 
  !! list.  This is different from calling this procedure with @a original 
  !! having its default initialised value (which represents an empty error 
  !! value). 
  !!
  !! If @a original is present, but is an empty error, and chaining is active 
  !! (@a chain not present or present with the value .TRUE.) and the only 
  !! argument present with non-empty values is @a component, then the error 
  !! is still regarded as an empty error, and no error will be added to the 
  !! list.  This means that:
  !! @code
  !! CALL Add(err_list, empty_error, COMPONENT='my component')
  !! @endcode
  !! behaves.
  
  SUBROUTINE Add_all( err_list, original, code, level, msg, component,  &
      location, chain )
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    TYPE(Error), INTENT(IN), OPTIONAL :: original
    INTEGER, INTENT(IN), OPTIONAL :: code
    TYPE(ErrorLevel), INTENT(IN), OPTIONAL :: level
    CHARACTER(*), INTENT(IN), OPTIONAL :: msg
    CHARACTER(*), INTENT(IN), OPTIONAL :: component
    CLASS(ErrorLocation), INTENT(IN), OPTIONAL :: location
    LOGICAL, INTENT(IN), OPTIONAL :: chain
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    !TYPE(Error) :: err        ! Temporary error object built from the args.
    
    ! Workaround requires allocatable.
    type(error), allocatable :: err2
    
    !***************************************************************************
    
    !===========================================================================
    ! ifort 15.0 bug workaround for 
    !err = ConstructError( ORIGINAL=original, CODE=code, LEVEL=level,  &
    !      MSG=msg, COMPONENT=component, LOCATION=location, CHAIN=chain )
    
    allocate(err2, source=constructerror( original=original, code=code, level=level,  &
        msg=msg, component=component, location=location, chain=chain ) )
    
    !===========================================================================
    
    !===========================================================================
    ! ifort 12.0.4 bug workaround.
    !IF (PRESENT(location)) THEN
    !  err = ConstructError( ORIGINAL=original, CODE=code, LEVEL=level,  &
    !      MSG=msg, COMPONENT=component, LOCATION=location, CHAIN=chain )
    !ELSE
    !  err = ConstructError( ORIGINAL=original, CODE=code, LEVEL=level,  &
    !      MSG=msg, COMPONENT=component, CHAIN=chain )
    !END IF
    !===========================================================================
    
    IF (Empty(err2)) RETURN
    
    CALL grow_list(err_list, 1)
    err_list(UBOUND(err_list,1)) = err2
    
  END SUBROUTINE Add_all
  
  
  !*****************************************************************************
  !!
  !> Adds one list of errors to another list.
  !!
  !! @param[in,out] err_list          The list of errors to be added to.  
  !! We can cope with unallocated input.  The output array is always 
  !! allocated, thought the size may be zero if @a from_list contains 
  !! only empty errors.
  !!
  !! @param[in]     from_list         The list of errors to be added from.
  !!
  !! @param[in]     component         Optional component to override any 
  !! components specified in @a from_list.  If not present then the components 
  !! in @a from_list are left unchanged.  Ignored if @a from_list is empty 
  !! or if @a from_list only contains empty errors and chaining is not 
  !! active.
  !!
  !! @param[in]     location          Optional error location to override 
  !! or chaing with the locations specified in @a from_list.  If not present 
  !! then the locations in @a from_list are left unchanged.  Ignored if 
  !! @a from_list is empty or if @a from_list only contains empty errors 
  !! and chaining is not active.
  !!
  !! @param[in]     chain             Optional flag to indicate if the 
  !! components from @a component should be chained to existing components 
  !! or just override the components.  If not present then components will 
  !! be chained.  Has not effect if @a components and @a location not present.
  !!
  !! Unlike the Error_err constructor we do not do empty testing when 
  !! considering whether to chain components or locations.
  
  SUBROUTINE Add_list(err_list, from_list, component, location, chain)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    TYPE(Error), INTENT(IN) :: from_list(:)
    CHARACTER(*), INTENT(IN), OPTIONAL :: component
    CLASS(ErrorLocation), INTENT(IN), OPTIONAL :: location
    LOGICAL, INTENT(IN), OPTIONAL :: chain
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! List index.
    INTEGER :: ibase          ! Existing index upper bound.
    LOGICAL :: local_chain    ! Local variant of chain, defaulted.
    
    !***************************************************************************
    
    ! Set default for component chaining.
    IF (PRESENT(chain)) THEN
      local_chain = chain
    ELSE
      local_chain = .TRUE.
    END IF
    
    ! The errors get appended to the end of the list.  For convenience save 
    ! the upper bound of the existing list to use as an offset.
    IF (ALLOCATED(err_list)) THEN
      ibase = UBOUND(err_list, 1)
    ELSE
      ibase = 0
    END IF
    
    ! Make space for the errors to be added.
    CALL grow_list(err_list, SIZE(from_list))
    ! Copy the errors to be added (the "original errors") over.
    DO i = 1, SIZE(from_list)     ! may be zero trip.
      err_list(ibase+i) = from_list(i)
      
      IF (PRESENT(component)) THEN
        IF (ALLOCATED(err_list(ibase+i)%component)) THEN
          ! New component specified and original error has component 
          ! information.
          IF (local_chain) THEN
            ! Chaining requested.  ifort 13.0.0 bug prevents us from doing:
            ! err_list(ibase+i)%component = [  &
            !    err_list(ibase+i)%component,  &
            !   String(component) ]
            ! but this is an easy (and probably more efficient) workaround:
            CALL Append(err_list(ibase+i)%component, component)
          ELSE
            ! Chaining not requested.
            err_list(ibase+i)%component = [String(component)]
          END IF
        ELSE
          ! New component specified, original error had no component.
          err_list(ibase+i)%component = [String(component)]
        END IF
      END IF
      
      ! locations cause too much havoc.
      !cycle
      
      IF (PRESENT(location)) THEN
        IF (local_chain) THEN
          CALL add_location(err_list(ibase+i)%location, location)
        ELSE
          ALLOCATE(err_list(ibase+i)%location(1))
          ALLOCATE(err_list(ibase+i)%location(1)%item, SOURCE=location)
          ! Replace above with following due ifort 12.1.5 bug.
          !err_list(ibase+i)%location = [ location_element(location) ]
        END IF
      END IF
    END DO
    
  END SUBROUTINE Add_list
  
  
  !*****************************************************************************
  !!
  !> Worker routine for Add_all and Add_list to grow the destination list of 
  !! errors.
  !!
  !! @param[in,out] err_list          The error list to grow.
  !!
  !! @param[in]     count             The number of elements to be added to 
  !! the list.
  
  SUBROUTINE grow_list(err_list, count)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    INTEGER, INTENT(IN) :: count
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the list.
    TYPE(Error), ALLOCATABLE :: tmp_list(:)
    
    !***************************************************************************
    
    ! Reallocate the list to be one bigger
    IF (.NOT. ALLOCATED(err_list)) THEN
      ALLOCATE(tmp_list(count))
    ELSE
      ALLOCATE(tmp_list(LBOUND(err_list,1):UBOUND(err_list,1) + count))
      ! Copy existing errors over.
      tmp_list(:UBOUND(err_list,1)) = err_list
    END IF
    ! Transfer allocation over to the argument.
    CALL MOVE_ALLOC(tmp_list, err_list)
    
  END SUBROUTINE grow_list
  
  
  !*****************************************************************************
  !!
  !> Remove an error from the list.
  !!
  !! @param[in,out] err_list          The error list.
  !!
  !! @param[in]     idx               Index of the error to remove.
  
  SUBROUTINE Remove_err(err_list, idx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Error), INTENT(INOUT), ALLOCATABLE :: err_list(:)
    INTEGER, INTENT(IN) :: idx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for shrinking the list.
    TYPE(Error), ALLOCATABLE :: tmp_list(:)
    
    !***************************************************************************
    
    IF ((idx < LBOUND(err_list, 1)) .OR. (idx > UBOUND(err_list, 1))) RETURN
    
    ! Reallocate the list to be one smaller
    ALLOCATE(tmp_list(LBOUND(err_list,1):UBOUND(err_list,1)-1))
    IF (idx > LBOUND(err_list, 1)) THEN
      tmp_list(:idx-1) = err_list(:idx-1)
    END IF
    IF (idx < UBOUND(err_list, 1)) THEN
      tmp_list(idx:) = err_list(idx+1:)
    END IF
    ! Transfer allocation over to the argument.
    CALL MOVE_ALLOC(tmp_list, err_list)
    
  END SUBROUTINE Remove_err
  
  
  !*****************************************************************************
  !!
  !> Returns the worst error from a list of errors, chosen on the basis 
  !! of severity.
  !!
  !! @param[in]     err_list          The list of errors.
  !!
  !! @returnsThe error with the worst severity in the list.  If the list is 
  !! empty (zero sized), then an empty error value is returned.  Where errors 
  !! have equally the worst severity, the first error in the list is returned.
  
  PURE FUNCTION MaxVal_err(err_list) RESULT(err)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err_list(:)
    
    ! Function result
    TYPE(Error) :: err
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! List index.
    INTEGER :: imax           ! Index of the maximum value.
    
    ! Maximum value.
    TYPE(ErrorLevel) :: max_level
    
    !***************************************************************************
    
    imax = 0
    max_level = errLevelNone
    DO i = 1, SIZE(err_list)
      IF ((err_list(i) > max_level) .OR. (imax == 0)) THEN
        imax = i
        max_level = err_list(i)
      END IF
    END DO
    
    IF (imax == 0) RETURN     ! Default error construct is empty
    err = err_list(imax)      ! Here's the baddy.
    
  END FUNCTION MaxVal_err
  
  
  !*****************************************************************************
  !!
  !> Returns the worst error from a pair of errors.
  !!
  !! @param[in]     err1              First error to compare.
  !!
  !! @param[in]     err2              Second error to compare.
  !!
  !! @returns The worst of the two errors, based on their severity.  @a err1 
  !! is returned if the errors have equal severity.
  
  ELEMENTAL FUNCTION Max_err(err1, err2) RESULT(err)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err1
    TYPE(Error), INTENT(IN) :: err2
    
    ! Function result
    TYPE(Error) :: err
    
    !***************************************************************************
    
    ! Comparison of errors is the same as comparing their severity.
    IF (err1 >= err2) THEN
      err = err1
    ELSE
      err = err2
    END IF
    
  END FUNCTION Max_err
  
  
  !*****************************************************************************
  !!
  !> Returns a "no error" or empty error value.  This error value contains 
  !! no useful information.
  !!
  !! Can be useful for avoiding compiler warnings, or to reset previous error 
  !! conditions.
  !!
  !! @returns An error object with severity none and all other aspects at 
  !! their default values.  This is the same as a default initialised Error 
  !! entity.
  
  PURE FUNCTION NoError_() RESULT(err)
    
    TYPE(Error) :: err
    
    !***************************************************************************
    
    ! We do this explicitly to avoid compiler warnings.  Technically not 
    ! required as the default initialisation should do everything we need.
    err = ConstructError(LEVEL=errLevelNone)
    
  END FUNCTION NoError_
  
  
  !*****************************************************************************
  !!
  !> Tests whether an error object represents failure of an operation.
  !!
  !! Failure is defined as a severity level of error or greater.  Warnings 
  !! and informational error objects are not failures.
  !!
  !! @param[in]     err               The error object.
  !!
  !! @returns.TRUE. if the level of @a err is greater than errLevelWarning.
  
  ELEMENTAL FUNCTION Failure_err(err) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = err > errLevelWarning
    
  END FUNCTION Failure_err
  
  
  !*****************************************************************************
  !!
  !> Tests whether an error object represents failure of an operation.
  !!
  !! Failure is defined as a severity level of error or greater.  Warnings 
  !! and informational error objects are not failures.
  !!
  !! @param[in]     err_list          The list of error objects.
  !!
  !! @returns.TRUE. if any object in @a err is greater than errLevelWarning.
  
  PURE FUNCTION Failure_err_list(err_list) RESULT(res)
  
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err_list(:)
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = ANY(err_list > errLevelWarning)
  
  END FUNCTION Failure_err_list
  
  
  !*****************************************************************************
  !!
  !> Tests whether an error object represents fatal failure of a program.
  !!
  !! Normal errors, warnings and informational error objects are not fatal 
  !! failures.
  !!
  !! @param[in]     err               The error object.
  !!
  !! @returns.TRUE. if the level of @a err is greater than errLevelError.
  
  ELEMENTAL FUNCTION Fatal_err(err) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = err > errLevelError
    
  END FUNCTION Fatal_err
  
  
  !*****************************************************************************
  !!
  !> Tests whether an error object represents fatal failure of a program.
  !!
  !! Normal errors, warnings and informational error objects are not fatal 
  !! failures.
  !!
  !! @param[in]     err_list          The list of error objects.
  !!
  !! @returns.TRUE. if any object in @a err is greater than errLevelError.
  
  PURE FUNCTION Fatal_err_list(err_list) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err_list(:)
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = ANY(err_list > errLevelError)
    
  END FUNCTION Fatal_err_list
  
  
  !*****************************************************************************
  !!
  !> Tests whether an error object contains some useful information.
  !!
  !! @param[in]     err               The error object to test.
  !!
  !! Useful information means that any of the fields bar component or 
  !! location have been initialised to something other than the default values.
  !!
  !! Procedures may return empty error objects to indicate success.
  !!
  !! @returns.TRUE. if @a err contains something useful, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION Empty_err(err) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    ! We do this backwards (test for non-emptiness) to hopefully promote 
    ! short circuiting - as soon as we find something that isn't empty 
    ! we can get out of here.
    res = (err%level /= errLevelNone)  &
        .OR. (err%code /= 0)  &
        .OR. ALLOCATED(err%msg)
    
    ! Flip from a non-empty test to an empty test.
    res = .NOT. res
    
  END FUNCTION Empty_err
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Comparisons of errors with other errors (their severity levels).
  
  
  !*****************************************************************************
  !!
  !> Greater than comparison of two error objects, carried out on the basis of 
  !! their severity.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand error object.
  !!
  !! @returns.TRUE. if @a lhs has a worse (>) severity than @a rhs, .FALSE. 
  !! otherwise.
  
  ELEMENTAL FUNCTION gt_error_err(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(Error), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level > rhs%Level
    
  END FUNCTION gt_error_err
  
  
  !*****************************************************************************
  !!
  !> Greater than or equal comparison of two error objects, carried out on 
  !! the basis of their severity.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand error object.
  !!
  !! @returns.TRUE. if @a lhs has a worse or equal (>=) severity than 
  !! @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION ge_error_err(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(Error), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level >= rhs%Level
    
  END FUNCTION ge_error_err
  
  
  !*****************************************************************************
  !!
  !> Less than or equal comparison of two error objects, carried out on 
  !! the basis of their severity.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand error object.
  !!
  !! @returns.TRUE. if @a lhs has a better or equal (<=) severity than 
  !! @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION le_error_err(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(Error), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level <= rhs%Level
    
  END FUNCTION le_error_err
  
  
  !*****************************************************************************
  !!
  !> Less than comparison of two error objects, carried out on the basis of 
  !! their severity.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand error object.
  !! 
  !! @returns.TRUE. if @a lhs has a better (<) severity than @a rhs, 
  !! .FALSE. otherwise.
  
  ELEMENTAL FUNCTION lt_error_err(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(Error), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level < rhs%Level
    
  END FUNCTION lt_error_err
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Comparisons of errors with severity levels (directly).
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(>) - greater than comparison 
  !! of an error with a level.
  !!
  !! @param[in]     lhs               The error object.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if the error has a worse (>) severity than @a rhs, 
  !! .FALSE. otherwise.
  
  ELEMENTAL FUNCTION gt_error_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level > rhs
    
  END FUNCTION gt_error_level
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(>=) - greater than or equal to comparison 
  !! of an error with a level.
  !!
  !! @param[in]     lhs               The error object.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if the error has a worse or same (>=) severity than 
  !! @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION ge_error_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level >= rhs
    
  END FUNCTION ge_error_level
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(==) - equal to comparison 
  !! of an error with a level.
  !!
  !! @param[in]     lhs               The error object.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if the error has the same (==) severity as
  !! @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION eq_error_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level == rhs
    
  END FUNCTION eq_error_level
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(/=) - not equal to comparison 
  !! of an error with a level.
  !!
  !! @param[in]     lhs               The error object.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if the error has a different (/=) severity than 
  !! @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION ne_error_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level /= rhs
    
  END FUNCTION ne_error_level
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(<=) - less than or equal to comparison 
  !! of an error with a level.
  !!
  !! @param[in]     lhs               The error object.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if the error has a better or same (<=) severity than 
  !! @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION le_error_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level <= rhs
    
  END FUNCTION le_error_level
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(<) - greater than or equal to comparison 
  !! of an error with a level.
  !!
  !! @param[in]     lhs               The error object.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if the error has a better (<) severity than 
  !! @a rhs, .FALSE. otherwise.
  
  ELEMENTAL FUNCTION lt_error_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%level < rhs
    
  END FUNCTION lt_error_level
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  
  
  !*****************************************************************************
  !!
  !> Greater than compare a list of errors against a scalar level and return a 
  !! scalar result.
  !!
  !! Use this to test whether an array of errors contains an error that exceeds 
  !! a certain threshold of severity.
  !!
  !! @param[in]     lhs               The list of errors.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if any error in the list has a worse (>) severity than
  !! @a rhs, .FALSE. otherwise.
  
  PURE FUNCTION gt_errors_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs(:)
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! List index.
    
    !***************************************************************************
    
    DO i = 1, SIZE(lhs)
      IF (lhs(i)%level > rhs) THEN
        res = .TRUE.
        RETURN
      END IF
    END DO
    res = .FALSE.   ! Zero sized list ends up here too.
    
  END FUNCTION gt_errors_level
  
  
  !*****************************************************************************
  !!
  !> Greater than or equal compare a list of errors against a scalar level and 
  !! return a scalar result.  This is not currently used.
  !!
  !! Use this to test whether an array of errors contains an error that meets 
  !! or exceeds a certain threshold of severity.
  !!
  !! @param[in]     lhs               The list of errors.
  !!
  !! @param[in]     rhs               The severity level to test.
  !!
  !! @returns.TRUE. if any error in the list has a worse (>) severity than
  !! @a rhs, .FALSE. otherwise.
  
  PURE FUNCTION ge_errors_level(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs(:)
    TYPE(ErrorLevel), INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! List index.
    
    !***************************************************************************
    
    DO i = 1, SIZE(lhs)
      IF (lhs(i)%level >= rhs) THEN
        res = .TRUE.
        RETURN
      END IF
    END DO
    res = .FALSE.   ! Zero sized list ends up here too.
    
  END FUNCTION ge_errors_level
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! Comparisons of errors with codes.
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(>) - greater than comparison of 
  !! the code of an error object with an integer.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand side integer.
  !!
  !! @returns .TRUE. if the code component of @a lhs is greater than @a rhs.
  
  ELEMENTAL FUNCTION gt_error_code(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    INTEGER, INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%code > rhs
    
  END FUNCTION gt_error_code
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(>=) - greater than or equal to comparison of 
  !! the code of an error object with an integer.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand side integer.
  !!
  !! @returns .TRUE. if the code component of @a lhs is greater than or equal 
  !! to @a rhs.
  
  ELEMENTAL FUNCTION ge_error_code(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    INTEGER, INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%code >= rhs
    
  END FUNCTION ge_error_code
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(==) - equal to comparison of the 
  !! code of an error object with an integer.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand side integer.
  !!
  !! @returns .TRUE. if the code component of @a lhs is equal to @a rhs.
  
  ELEMENTAL FUNCTION eq_error_code(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    INTEGER, INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%code == rhs
    
  END FUNCTION eq_error_code
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(/=) - not equal to comparison of the 
  !! code of an error object with an integer.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand side integer.
  !!
  !! @returns .TRUE. if the code component of @a lhs is not equal 
  !! to @a rhs.
  
  ELEMENTAL FUNCTION ne_error_code(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    INTEGER, INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%code /= rhs
    
  END FUNCTION ne_error_code
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(<=) - less than or equal to comparison of the 
  !! code of an error object with an integer.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand side integer.
  !!
  !! @returns .TRUE. if the code component of @a lhs is less than or equal 
  !! to @a rhs.
  
  ELEMENTAL FUNCTION le_error_code(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    INTEGER, INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%code <= rhs
    
  END FUNCTION le_error_code
  
  
  !*****************************************************************************
  !!
  !> Implementation of OPERATOR(<) - less than comparison of the code of an 
  !! error object with an integer.
  !!
  !! @param[in]     lhs               The left hand error object.
  !!
  !! @param[in]     rhs               The right hand side integer.
  !!
  !! @returns .TRUE. if the code component of @a lhs is less than @a rhs.
  
  ELEMENTAL FUNCTION lt_error_code(lhs, rhs) RESULT(res)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: lhs
    INTEGER, INTENT(IN) :: rhs
    
    ! Function result
    LOGICAL :: res
    
    !***************************************************************************
    
    res = lhs%code < rhs
    
  END FUNCTION lt_error_code
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  ! String related stuff
  
  
  !*****************************************************************************
  !!
  !> Generate a text representation of the error.
  !!
  !! There are four sections, the location list (series of file + line + 
  !! column), the error type (its level plus code), the message and the 
  !! component list.  All sections are  optional, and the various bits in the 
  !! sections are also optional.
  !!
  !! If the error level is errLevelNone, then only the error message is 
  !! printed.
  !!
  !! In between the location, error type and message sections that 
  !! are present we put ': '.  In between the rest of the description and 
  !! the component list we put a space.
  !!
  !! If either line or column is present, the line and column numbers are 
  !! put inside a set of parentheses, '(' and ')'.  If both line and column 
  !! information are present then a comma separates the two.  If there is 
  !! no column information, then there is no comma, but a comma is still 
  !! included if there is line information but no column information.
  !!
  !! If there are multiple locations, they are separated from each other with 
  !! the word 'from'.
  !!
  !! If present, the error code follows a '#' sign.
  !!
  !! If both the error level and code are present they are separated by 
  !! a space.
  !!
  !! If present, the component is put inside square brackets '[' and ']'. 
  !! If there are multiple locations the are separated from each other with 
  !! the word 'via'.
  !! @code
  !! Info: You are a chicken
  !!
  !! file.txt(1,2): Error #10: The relevant message [comp1]
  !!
  !! file1.txt(1,2) from file2.txt: Warning: The message [comp1 via comp2]
  !! @endcode
  
  PURE FUNCTION describe_err(err) RESULT(str)
    
    USE CharUtils
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    TYPE(Error), INTENT(IN) :: err
    
    ! Function result
    CHARACTER(:), ALLOCATABLE :: str
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Error location index.
    
    !***************************************************************************
    
    ! Errors with a severity of None just have the message printed.
    IF (err%level == errLevelNone) THEN
      IF (ALLOCATED(err%msg)) THEN
        str = err%msg
      ELSE
        str = ''
      END IF
    ELSE
      ! Error location
      IF (ALLOCATED(err%location)) THEN
        IF (SIZE(err%location) > 0) THEN
          str = err%location(1)%item%ToString()
          DO i = 2, SIZE(err%location)
            str = str // ' from ' // err%location(2)%item%ToString()
          END DO
        ELSE
          str = ''
        END IF
      ELSE
        str = ''
      END IF
      
      ! Error level and code.  We always print at least the error level.
      IF (LEN(str) > 0) str = str // ': '
      str = str // .Describe. err%level
      IF (err%code /= 0) str = str //  ' ' // ToString(err%code)
      
      ! Error message
      IF (ALLOCATED(err%msg)) THEN
        str = str // ': ' // err%msg
      END IF
      
      ! Component
      IF (ALLOCATED(err%component)) THEN
        str = str // ' [' // err%component(1)%item
        DO i = 2, SIZE(err%component)
          str = str // ' via ' // err%component(i)%item
        END DO
        str = str // ']'
      END IF
    END IF
    
  END FUNCTION describe_err
  
  
  !*****************************************************************************
  !!
  !> Add a location to a vector of locations.
  !!
  !! @param[in,out] loc_vec           The vector of locations.  Doesn't have 
  !! to be allocated.
  !!
  !! @param[in]     loc               The location to add to the vector.
  !!
  !! Work around for ifort 12.0.4 bug.  The work around includes work arounds 
  !! for bugs in 13.0.0 exposed by the original work around.  If your head 
  !! is not spinning from all the working arounding, then you are doing better 
  !! than me.
  !!
  !! (That said - this might also be more efficient than the original 
  !! short cut code.)
  
  PURE SUBROUTINE add_location(loc_vec, loc)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(location_element), INTENT(INOUT), ALLOCATABLE :: loc_vec(:)
    CLASS(ErrorLocation), INTENT(IN) :: loc
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Temporary for growing the location array.
    TYPE(location_element), ALLOCATABLE :: tmp(:)
    
    INTEGER :: i              ! loc_vec index.
    
    !***************************************************************************
    
    IF (ALLOCATED(loc_vec)) THEN
      ALLOCATE(tmp(SIZE(loc_vec)+1))
      
      DO i = 1, SIZE(loc_vec)
        ALLOCATE(tmp(i)%item, SOURCE=loc_vec(i)%item)
      END DO
    ELSE
      ALLOCATE(tmp(1))
    END IF
    
    ALLOCATE(tmp(SIZE(tmp))%item, SOURCE=loc)
    
    CALL MOVE_ALLOC(tmp, loc_vec)
    
  END SUBROUTINE add_location
  
  
  !*****************************************************************************
  !!
  !> Bug workaround to do intrinsic assignment.
  !!
  !! @param[out]    lhs               The left hand side error object.
  !!
  !! @param[in]     rhs               The right hand side error object.
  !!
  !! Delete this and the associated generic interface when the ifort bug is 
  !! fixed.
  
  ELEMENTAL SUBROUTINE assignment_bug(lhs, rhs)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Error), INTENT(OUT) :: lhs
    TYPE(Error), INTENT(IN) :: rhs
    
    !***************************************************************************
    
    lhs%code = rhs%code
    lhs%level = rhs%level
    IF (ALLOCATED(rhs%msg)) lhs%msg = rhs%msg
    IF (ALLOCATED(rhs%component)) THEN
      ALLOCATE(lhs%component(SIZE(rhs%component)), SOURCE=rhs%component)
    END IF
    IF (ALLOCATED(rhs%location)) THEN
      ALLOCATE(lhs%location(SIZE(rhs%location)), SOURCE=rhs%location)
    END IF
    
  END SUBROUTINE assignment_bug
  
END MODULE Errors
