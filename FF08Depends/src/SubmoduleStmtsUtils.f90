! $Id: SubmoduleStmtsUtils.f90 2960 2020-04-17 20:43:28Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SubmoduleStmtsUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the submodule identifier and the parent 
!! identifier from a submodule statement.
!!
!! This has been split out from the SubmoduleStmts module to improve source 
!! file granularity.  We were also hoping that we could squish an ifort 13.0.0 
!! ice out of the FF08Depends project, but he just popped up elsewhere.

MODULE SubmoduleStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: GetSubmoduleAncestorName
  PUBLIC :: GetSubmoduleIdentifier
  PUBLIC :: GetSubmoduleParentIdentifier
  PUBLIC :: GetSubmoduleName
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Gets the name of the submodule.
  !!
  !! @param[in]     tlist             The tokens that make up the 
  !! /submodule-stmt/.
  !! 
  !! @param[out]    name              The name of the submodule.  Not 
  !! allocated if the name could not be sensibly determined.  Note that this 
  !! is of default character kind - this procedure is also responsible for 
  !! kind conversion.
  
  SUBROUTINE GetSubmoduleName(tlist, name)
    
    USE Tokens
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: start          ! Index of the first opening (.
    INTEGER :: finish         ! Index of the corresponding closing ).
    
    !***************************************************************************
    
    CALL GetNextParensRange(tlist, start, finish)
    IF ((start /= 0) .AND. (finish /= 0)) THEN
      IF (SIZE(tlist) > finish + 1) THEN      ! If there are tokens after ')'.
        IF (tlist(finish+2) == ittName) THEN  ! If the token after is a name.
          name = QueryValue(tlist(finish+2))   ! Kind conversion here.
          RETURN
        END IF
      END IF
    END IF
    
    ! If we get to here we couldn't work out the submodule name - return 
    ! unallocated name.
    
  END SUBROUTINE GetSubmoduleName
  
  
  !*****************************************************************************
  !!
  !> Gets the identifier of this submodule.
  !!
  !! @param[in]     tlist             The sequence of tokens that make up the 
  !! /submodule-stmt/.
  !!
  !! @param[out]    identifier        The identifier of the submodule.
  !!
  !! A submodule's identifier is its ancestor module followed by its own 
  !! name, separated by a colon.
  
  SUBROUTINE GetSubmoduleIdentifier(tlist, identifier)
    
    USE Strings
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: identifier
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the ancestor module.
    CHARACTER(:), ALLOCATABLE :: ancestor
    
    ! Name of this submodule.
    CHARACTER(:), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetSubmoduleAncestorName(tlist, ancestor)
    IF (.NOT. ALLOCATED(ancestor)) RETURN
    
    CALL GetSubmoduleName(tlist, name)
    IF (.NOT. ALLOCATED(name)) RETURN
    
    ! The identifier of this module is the identifier of its parent with the 
    ! name of this module tacked on the end.
    identifier = ancestor // ':' // name
    
  END SUBROUTINE GetSubmoduleIdentifier
  
  
  !*****************************************************************************
  !!
  !> Gets the identifier of the parent of this module.
  !!
  !! @param[in]     tlist             The sequence of tokens that make up the 
  !! /submodule-stmt/.
  !!
  !! @param[out]    identifier        The identifier of the parent of the 
  !! module.
  !!
  !! An earlier variant of this actually used ParseParentIdentifier, but 
  !! that brought with it links to ParseUtils that we wanted to break.
  
  SUBROUTINE GetSubmoduleParentIdentifier(tlist, identifier)
    
    USE CompilerKinds
    USE Errors
    USE MatchUtils
    USE Strings
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: identifier
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Name of the ancestor module.
    CHARACTER(:), ALLOCATABLE :: ancestor
    
    ! Name of this submodule.
    CHARACTER(:), ALLOCATABLE :: parent
    
    INTEGER :: start          ! Start of the identifier in tlist.
    INTEGER :: finish         ! Finish of the identifier in tlist.
    
    !***************************************************************************
    
    CALL GetNextParensRange(tlist, start, finish)
    IF ((start == 0) .OR. (finish == 0)) RETURN
    
    ! If the first thing doesn't look like a name, we've failed.
    IF (.NOT. MatchName(tlist, start)) RETURN
    
    ancestor = QueryValue(tlist(start))              ! Kind conversion.
    
    IF (finish >= start + 1) THEN
      IF (IsToken(tlist(start + 1), ittColon, scck_':')) THEN
        IF (MatchName(tlist, start + 2)) THEN
          parent = QueryValue(tlist(start+2))        ! Kind conversion.
        ELSE
          RETURN          ! Bad syntax - parent name wasn't a name.
        END IF
      ELSE
        RETURN            ! bad syntax - ":" wasn't a ":".
      END IF
    END IF
    
    ! ancestor always allocated, parent might not be (it's optional).
    IF (ALLOCATED(parent)) THEN
      identifier = ancestor // ':' // parent
    ELSE
      identifier = ancestor
    END IF
    
  END SUBROUTINE GetSubmoduleParentIdentifier
  
  
  !*****************************************************************************
  !!
  !> Get the name of the ancestor module of the submodule.
  !!
  !! @param[in]     tlist             List of tokens that make up the 
  !! submodule-stmt.
  !!
  !! @param[out]    name              The ancestor module name for the 
  !! submodule.
  !!
  !! Ancestor names are determined by splitting up the /parent-identifier/ 
  !! part of the /submodule-stmt/.
  
  SUBROUTINE GetSubmoduleAncestorName(tlist, name)
    
    USE Tokens
    USE MatchUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: start          ! Index of the first (.
    INTEGER :: finish         ! Index of the corresponding closing ).
    
    !***************************************************************************
    
    CALL GetNextParensRange(tlist, start, finish)
    IF ((start /= 0) .AND. (finish /= 0)) THEN
      IF (tlist(start) == ittName) THEN
        name = QueryValue(tlist(start))      ! Kind conversion
        RETURN
      END IF
    END IF
    
    ! If we get to here just return with unallocated name.
    
  END SUBROUTINE GetSubmoduleAncestorName
  
END MODULE SubmoduleStmtsUtils
