! $Id: Version.f90 1794 2015-08-21 11:29:26Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the Version module.


!*******************************************************************************
!!
!> Version information for the library.

MODULE Version
  
  USE Revision
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: Major = 1     !< Major version number.
  INTEGER, PARAMETER :: Minor = 0     !< Minor version number.
  INTEGER, PARAMETER :: Micro = 1     !< Micro version number.
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Returns the library version in the form major.minor.micro.
  !!
  !! @returns A string so formatted!
  
  FUNCTION SimpleVersionString() RESULT(s)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CHARACTER(:), ALLOCATABLE :: s
    
    ! Buffer for building output.
    CHARACTER(30) :: buffer
    
    !***************************************************************************
    
    WRITE (buffer, "(I0,'.',I0,'.',I0)") Major, Minor, Micro
    s = TRIM(buffer)
    
  END FUNCTION SimpleVersionString
  
  
  !*****************************************************************************
  !!
  !> Write some version information to the console.
  !!
  !! @param[in]     program_name      Name of the program to use in the 
  !! message.
  
  SUBROUTINE VersionMessage(program_name)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CHARACTER(*), INTENT(IN) :: program_name
    
    !***************************************************************************
    
    PRINT "(A,' version ',A)", program_name, SimpleVersionString()
    
    PRINT "('Source from ',A,' revision ',A,'.')",  &
        SourceRepository,  &
        SourceRevision
    
    PRINT "('Packaged on ',A,' at ',A,'.')", PackageDate, PackageTime
    
  END SUBROUTINE VersionMessage
  
END MODULE Version
