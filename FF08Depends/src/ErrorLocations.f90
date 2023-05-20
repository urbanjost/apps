! $Id: ErrorLocations.f90 1334 2014-09-14 03:04:53Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ErrorLocations module.


!*******************************************************************************
!!
!> Defines the ErrorLocation type as a parent for types that can describe a 
!! point in a source.
!!
!! This module has no dependencies.
!!
!! ifort 12.1 bugs mean that this is difficult to use in anger, or using 
!! this module results in anger.

MODULE ErrorLocations
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  ! The error location type.
  
  !> Type that represents a location in source code or some other input source.
  TYPE, PUBLIC, ABSTRACT :: ErrorLocation
  CONTAINS
    !> Create a text representation of the ErrorLocation.
    PROCEDURE(errloc_ToString), DEFERRED :: ToString
    
    !> Operator access to ToString.
    GENERIC :: OPERATOR(.ToString.) => ToString
  END TYPE ErrorLocation
  
  !> Interfaces for deferred bindings of ErrorLocation.
  ABSTRACT INTERFACE
    !> Create a text representation of the location.
    !!
    !! @param[in]     loc               The location to be represented in text.
    !!
    !! @returns A text representation of @a loc, the details of which are left 
    !! up to extensions of ErrorLocation.
    PURE FUNCTION errloc_ToString(loc) RESULT(str)
      IMPORT :: ErrorLocation
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(ErrorLocation), INTENT(IN) :: loc
      CHARACTER(:), ALLOCATABLE :: str
    END FUNCTION errloc_ToString
  END INTERFACE
  
END MODULE ErrorLocations
