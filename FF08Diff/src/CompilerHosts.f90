! $Id: CompilerHosts.f90 2171 2016-06-02 12:03:37Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the CompilerHosts module


!*******************************************************************************
!!
!> Defines the CompilerHost type and related procedures.
!!
!! A compiler host object provides the characteristics platform hosting the 
!! compilation.  This includes things like:
!!
!! - Wildcard expansion of file specifications.
!!
!! - Creating source (form) objects from source file names.
!!
!! - Opening include files.
!!
!! Some preliminary work has been done to permit the KIND for filenames 
!! to be something other than default character, but it is incomplete - 
!! notably the Strings and CharUtils modules would need specialisation.

MODULE CompilerHosts
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: ExpandSpecsToNames
  
  TYPE, PUBLIC, ABSTRACT :: CompilerHost
  CONTAINS
    !> Give a file specification, creates a list of relevant file names.
    PROCEDURE(ch_ExpandFileSpec), DEFERRED :: ExpandFileSpec
    !> Given a filename, creates an appropriate source form object that can 
    !! be used to read the file.
    PROCEDURE(ch_OpenSourceForm), DEFERRED :: OpenSourceForm
    !> Given the char-literal token from an include-line, create an apropriate 
    !! source object to read the file.
    PROCEDURE(ch_OpenIncludeFile), DEFERRED :: OpenIncludeFile
    !> Compare two filenames to see if they are equal.
    PROCEDURE(ch_CompareFilenames), DEFERRED :: CompareFilenames
  END TYPE CompilerHost
  
  !> Interfaces for deferred bindings of CompilerHost.
  ABSTRACT INTERFACE
    
    !***************************************************************************
    !!
    !> Interface for CompilerHost%ExpandFileSpec - given a file specification 
    !! (which will have come off the command line) return a list of relevant 
    !! file names.
    !!
    !! @param[in]     host              The compiler host object.
    !!
    !! @param[in]     filespec          The file specification.
    !!
    !! @param[out]    file_list         List of file names.
    !!
    !! @param[out]    err_list          List of errors.
    !!
    !! What a file specification means is to the discretion of the host - 
    !! some hosts may regard all file specifications as literal file names, 
    !! others may do wildcard expansion, etc.
    
    SUBROUTINE ch_ExpandFileSpec(host, filespec, file_list, err_list)
      USE CompilerKinds
      USE Errors
      USE Strings
      IMPORT :: CompilerHost
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(CompilerHost), INTENT(IN) :: host
      CHARACTER(*,KIND=fnck), INTENT(IN) :: filespec
      TYPE(String), INTENT(OUT), ALLOCATABLE :: file_list(:)
      TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    END SUBROUTINE ch_ExpandFileSPec
    
    !***************************************************************************
    !!
    !> Interface for CompilerHost%OpenSourceForm - given the name of a source 
    !! file (which will have come off the command line or from ExpandFileSpec) 
    !! create an appropriate SourceForm object that references an appropriate 
    !! Source object to parse the file.
    !!
    !! @param[in]     host              The host object.
    !!
    !! @param[in]     filename          The name of the file to open.
    !!
    !! @param[out]    source_form       A source form object for the 
    !! particular file.
    !!
    !! @param[out]    err_list          List of errors.
    
    SUBROUTINE ch_OpenSourceForm(host, filename, source_form, err_list)
      USE CompilerKinds
      USE Errors
      USE Scanner
      IMPORT :: CompilerHost
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(CompilerHost), INTENT(IN) :: host
      CHARACTER(*,KIND=fnck), INTENT(IN) :: filename
      CLASS(SourceForm), INTENT(OUT), ALLOCATABLE :: source_form
      TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    END SUBROUTINE ch_OpenSourceForm
    
    !***************************************************************************
    !!
    !> Interface for CompilerHost%OpenIncludeFile - given the token from an 
    !! INCLUDE line create a Source object that allows the contents of the 
    !! relevant include file to be read.
    !!
    !! @param[in]     host              The host object.
    !!
    !! @param[in]     include_token     The token for the filename from the 
    !! include line.  This may include a character kind in the form of 
    !! a literal constant.
    !!
    !! @param[in]     parent_source     The source object for the file that 
    !! contains the include line.
    !!
    !! @param[out]    include_source    A source object that will provides 
    !! whatever included source is represented by the include line.  NULL on 
    !! error.
    !!
    !! @param[out]    err_list          List of errors.
    
    SUBROUTINE ch_OpenIncludeFile( host, include_token, parent_source,  &
        include_source, err_list )
      USE Errors
      USE CompilerKinds
      USE Sources
      USE Tokens
      IMPORT :: CompilerHost
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(CompilerHost), INTENT(IN) :: host
      TYPE(Token), INTENT(IN) :: include_token
      CLASS(Source), INTENT(IN), POINTER :: parent_source
      CLASS(Source), INTENT(OUT), POINTER :: include_source
      TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    END SUBROUTINE ch_OpenIncludeFile
    
    !***************************************************************************
    !!
    !> Interface for CompilerHost%CompareFilenames - compare two file names
    !! for equality.
    !!
    !! @param[in]     host              The host object.
    !!
    !! @param[in]     file1             One of the filenames to compare.
    !!
    !! @param[in]     file2             The other filename to compare.
    !!
    !! @returns .TRUE. if the filenames are equal according to the rules of
    !! the host, .FALSE. otherwise.
    
    FUNCTION ch_CompareFilenames(host, file1, file2) RESULT(b)
      USE CompilerKinds
      IMPORT :: CompilerHost
      IMPLICIT NONE
      !-------------------------------------------------------------------------
      CLASS(CompilerHost), INTENT(IN) :: host
      CHARACTER(*,KIND=fnck), INTENT(IN) :: file1
      CHARACTER(*,KIND=fnck), INTENT(IN) :: file2
      ! Function result
      LOGICAL :: b
    END FUNCTION ch_CompareFilenames
  END INTERFACE
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Expand a list of file specifications into a list of file names. 
  !!
  !! @param[in]     host              The compiler host object.
  !!
  !! @param[in]     specs             List of file specifications.
  !!
  !! @param[out]    names             List of file names.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! A file specification may result in zero or more file names.  The nature 
  !! of that mapping is defined by the host.  The naive default mapping is 
  !! one to one (a file spec is a file name).
  
  SUBROUTINE ExpandSpecsToNames(host, specs, names, err_list)
    
    USE Strings
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(CompilerHost), INTENT(IN) :: host
    TYPE(String), INTENT(IN) :: specs(:)
    TYPE(String), INTENT(OUT), ALLOCATABLE :: names(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Spec index.
    
    ! Names from each spec.
    TYPE(String), ALLOCATABLE :: items(:)
    
    ! Error list for procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    ALLOCATE(names(0))
    DO i = 1, SIZE(specs)
      CALL host%ExpandFileSpec(specs(i)%item, items, sub_err_list)
      CALL Add(err_list, sub_err_list)
      CALL Append(names, items)
    END DO
    
  END SUBROUTINE ExpandSpecsToNames
  
END MODULE CompilerHosts
