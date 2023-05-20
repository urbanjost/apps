! $Id: CompilerKinds.f90 1061 2013-09-27 00:32:39Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the CompilerKinds module.


!*******************************************************************************
!!
!> Kinds for various types used in the compiler.
!!
!! This module has no dependencies on other modules.
!!
!! One day we might change some of these kinds away from the default 
!! kind values.  When we do that expect many things to break...

MODULE CompilerKinds
  
  IMPLICIT NONE
  
  ! All entities are public.
  
  !> File name character kind.
  !!
  !! Character kind used for strings that designate filenames to the compiler.
  !!
  !! Also looks like a rude word.  Giggle.
  INTEGER, PARAMETER :: fnck = KIND('a')
  
  !> Source code character kind.
  !!
  !! Character kind used for the source code supplied to the compiler.
  INTEGER, PARAMETER :: scck = KIND('a')
  
END MODULE CompilerKinds
