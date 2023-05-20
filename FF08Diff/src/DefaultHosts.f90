! $Id: DefaultHosts.f90 2798 2019-03-22 17:18:41Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the DefaultHosts module.


!*******************************************************************************
!!
!> Defines the DefaultHost type, which provides default (and easily 
!! implemented, more importantly!) behaviour for the host.

MODULE DefaultHosts
  
  USE CompilerHosts
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> Describes the "default" behaviour of the host.
  TYPE, PUBLIC, EXTENDS(CompilerHost) :: DefaultHost
    !> Dummy to avoid not used warnings.
    INTEGER :: i = 0
  CONTAINS
    ! Bindings inherited from CompilerHost.
    PROCEDURE :: ExpandFileSpec => dh_ExpandFileSpec
    PROCEDURE :: OpenSourceForm => dh_OpenSourceForm
    PROCEDURE :: OpenIncludeFile => dh_OpenIncludeFile
    PROCEDURE :: CompareFilenames => dh_CompareFilenames
    
    PROCEDURE :: GetFileExtension => dh_GetFileExtension
    PROCEDURE :: GetFileName => dh_GetFileName
    PROCEDURE :: GetFileDirectory => dh_GetFileDirectory
  END TYPE DefaultHost
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of DefaultHost%ExpandFileSpec - converts a file 
  !! specification into a list of zero or more file names.
  !!
  !! @param[in]     host              The host object.
  !!
  !! @param[in]     filespec          The file specification.
  !!
  !! @param[out]    file_list         The list of files that correspond to 
  !! @a file_spec.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! The behaviour of the default host is that a file spec is a file name - 
  !! we assume that things like wildcard expansion etc are carried out by the 
  !! shell before the program is called.
  
  SUBROUTINE dh_ExpandFileSpec(host, filespec, file_list, err_list)
    
    USE Strings
    USE Errors
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DefaultHost), INTENT(IN) :: host
    CHARACTER(*), INTENT(IN) :: filespec
    TYPE(String), INTENT(OUT), ALLOCATABLE :: file_list(:)
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! We never go wrong.
    ALLOCATE(err_list(0))
    
    ! One file spec gives rise to one file name.
    ALLOCATE(file_list(1))
    file_list(1)%item = filespec
    
  END SUBROUTINE dh_ExpandFileSpec
  
  
  !*****************************************************************************
  !!
  !> Implementation of DefaultHost%OpenSourceForm - creates a source form 
  !! object for a particular file.
  !!
  !! @param[in]     host              The host object.
  !!
  !! @param[in]     filename          The name of the file to open.
  !!
  !! @param[out]    source_form       A source form object for the particular 
  !! file.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! The type of the source form object returned depends on the extension 
  !! of @a filename (where the extension is whatever follows the final 
  !! dot in the @a filename.
  !!
  !! If the extension is 'FOR' or 'F' then a FixedFormSrc object is returned.
  !!
  !! If the extension is 'F90', 'F95', 'F03' or 'F08' then a FreeFormSrc 
  !! object is returned.
  !!
  !! The comparison of extensions is not case sensitive.
  !!
  !! If the extension is anything else then a unrecognised file error is 
  !! returned.
  
  SUBROUTINE dh_OpenSourceForm(host, filename, source_form, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE Scanner
    USE ScanFixedForm
    USE ScanFreeForm
    USE Sources
    USE SourceFiles
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DefaultHost), INTENT(IN) :: host
    CHARACTER(*), INTENT(IN) :: filename
    CLASS(SourceForm), INTENT(OUT), ALLOCATABLE :: source_form
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(Error) :: err        ! Error opject for procedure calls.
    
    ! A SourceFile object for the nominated file.
    TYPE(SourceFile), POINTER :: current_file
    
    ! The extension of the nominated file.
    CHARACTER(:), ALLOCATABLE :: ext
    
    ! A Source object that points to the same object as current_file.
    CLASS(Source), POINTER :: p_source
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    ! Open the file.
    
    CALL OpenSourceFile(filename, current_file, err)
    CALL Add(err_list, err)
    IF (Failure(err)) RETURN
    
    p_source => current_file
    
    !---------------------------------------------------------------------------
    ! Create a source form object attached to the file.
    
    ext = host%GetFileExtension(filename)
    CALL UpperCase(ext)
    SELECT CASE (ext)
    CASE ('FOR', 'F')
      ! ifort 12.1.0 bug workaround - should be able to do:
      ! ALLOCATE(source_form, SOURCE=FixedFormSrc(p_source))
      ALLOCATE(FixedFormSource:: source_form)
      SELECT TYPE (source_form)
      TYPE IS (FixedFormSource)
        source_form = FixedFormSource(p_source)
      END SELECT
      
    CASE ('F90', 'F95', 'F03', 'F08')
      ! ifort 12.1.0 bug workaround - should be able to do:
      ! ALLOCATE(source_form, SOURCE=FreeFormSrc(p_source))
      ALLOCATE(FreeFormSource:: source_form)
      SELECT TYPE (source_form)
      TYPE IS (FreeFormSource)
        source_form = FreeFormSource(p_source)
      END SELECT
      
    CASE DEFAULT
      CALL Add( err_list, CODE=errUnknownFileType,  &
          MSG='Unrecognised extension ' // ext // '.  You need to specify &
            &the source form manually' )
      
    END SELECT
    
  END SUBROUTINE dh_OpenSourceForm
  
  
  !*****************************************************************************
  !!
  !> Implementation of DefaultHost%OpenIncludeFile - create a source object 
  !! for a file nominated in an include line.
  !!
  !! @param[in]     host              The host object.
  !!
  !! @param[in]     include_token     The token for the filename from the 
  !! include line.  This may include a character kind.
  !!
  !! @param[in]     parent_source     The source object for the file that 
  !! contains the include line.
  !!
  !! @param[out]    include_source    A source object that will provides 
  !! whatever included source is represented by the include line.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! @todo Do something reasonable with @a include_kind.
  !!
  !! @todo Search for include file on an include path, that is prefixed 
  !! with the part of the parent file.
  
  SUBROUTINE dh_OpenIncludeFile( host, include_token, parent_source,  &
      include_source, err_list )
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE CompilerKinds
    USE Tokens
    USE Sources
    USE SourceFiles
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DefaultHost), INTENT(IN) :: host
    TYPE(Token), INTENT(IN) :: include_token
    CLASS(Source), INTENT(IN), POINTER :: parent_source
    CLASS(Source), INTENT(OUT), POINTER :: include_source
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    TYPE(Error) :: err        ! Error object for child procedure calls.
    INTEGER :: is             ! Source index.
    
    ! Pointer to source used for querying existing sources.
    CLASS(Source), POINTER :: ptr
    
    ! Relocated file anme (perhaps with current source prefix).
    CHARACTER(:), ALLOCATABLE :: resolved_filename
    
    ! Pointer to source file used for opening a new source.
    TYPE(SourceFile), POINTER :: p_source_file
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    IF (HasKind(include_token)) THEN
      CALL Add( err_list,  &
          CODE=errUnsupportedIncludeKind,  &
          MSG='A kind-param of ' // QueryKind(include_token) // ' on the &
            &char-literal-constant is not supported on an INCLUDE line',  &
          LOCATION=QueryLocation(include_token),  &
          COMPONENT='3.4p8' )
      include_source => NULL()
      RETURN
    END IF
    
    !---------------------------------------------------------------------------
    ! See if we can find the file in the existing file list.
    
    ! See if the file is already known. If it is then see if the file is 
    ! already open - that would indicate recursion, which is not legal.  
    ! If the file is not already open then return that file.
    DO is = 1, SourceCount()
      CALL GetSource(is, ptr)
      IF (host%CompareFilenames(ptr%name, QueryValue(include_token))) THEN
        IF (ptr%line > 0) THEN
          ! @todo kind of the token.
          CALL Add( err_list, CODE=errNestedInclude,  &
              MSG='The source file "' // QueryValue(include_token) // '" is &
                &being INCLUDEd in a recursive manner that results in &
                &inclusion of the same source text',  &
              LOCATION=QueryLocation(include_token),  &
              COMPONENT='3.4p6' )
          include_source => NULL()
        ELSE
          include_source => ptr
        END IF
        RETURN
      END IF
    END DO
    
    !---------------------------------------------------------------------------
    ! Otherwise add the file to the global list of source files and return
    ! a pointer to it.
    
    resolved_filename = QueryValue(include_token)  ! kind conversion.
    CALL resolve_include_filename(  &
        host,  &
        (resolved_filename),  &
        parent_source%name,  &
        resolved_filename )
    
    ! @todo Kind conversion issues.
    CALL OpenSourceFile(resolved_filename, p_source_file, err)
    CALL Add(err_list, err)
    IF (Failure(err)) THEN
      include_source => NULL()
    ELSE
      include_source => p_source_file
    END IF
    
  END SUBROUTINE dh_OpenIncludeFile
  
  
  !***************************************************************************
  !!
  !> Resolve the name of an include file.
  !!
  !! @param[in]     host              The host object.
  !!
  !! @param[in]     in_filename       The filename from the include line.
  !!
  !! @param[in]     parent_filename   The filename of the parent source.
  !!
  !! @param[out]    out_filename      The resolved include filename.
  
  SUBROUTINE resolve_include_filename( host, in_filename, parent_filename,  &
      out_filename )
    
    USE CompilerKinds
    
    !-------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DefaultHost), INTENT(IN) :: host
    CHARACTER(*,KIND=fnck), INTENT(IN) :: in_filename
    CHARACTER(*,KIND=fnck), INTENT(IN) :: parent_filename
    CHARACTER(:,KIND=fnck), INTENT(OUT), ALLOCATABLE :: out_filename
    
    !-------------------------------------------------------------------------
    ! Locals
    
    CHARACTER(:,KIND=fnck), ALLOCATABLE :: source_dir
    
    LOGICAL :: exist          ! File existence flag.
    
    !***************************************************************************
    
    ! Does the file exist in a directory relative to the source?
    source_dir = host%GetFileDirectory(parent_filename)
    
    IF (source_dir /= '') THEN
      out_filename = source_dir // in_filename
      INQUIRE(FILE=out_filename, EXIST=exist)
      IF (exist) RETURN
    END IF
    
    ! @todo Test directories added via --include command line option 
    ! or similar.
    
    ! Assume the file is relative to the current directory.
    out_filename = in_filename
    
  END SUBROUTINE 
  
  
  !***************************************************************************
  !!
  !> Implementation of DefaultHost%CompareFilenames - compare two file names
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
  
  FUNCTION dh_CompareFilenames(host, file1, file2) RESULT(b)
    
    USE CharUtils       ! Needs to be specialised for fnck character kind!
    USE CompilerKinds
    
    !-------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DefaultHost), INTENT(IN) :: host
    CHARACTER(*,KIND=fnck), INTENT(IN) :: file1
    CHARACTER(*,KIND=fnck), INTENT(IN) :: file2
    
    ! Function result
    LOGICAL :: b
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Upper case variant of file1
    CHARACTER(:,KIND=fnck), ALLOCATABLE :: upper1
    
    ! Upper case variant of file2
   CHARACTER(:,KIND=fnck), ALLOCATABLE :: upper2
    
    !***************************************************************************
    
    upper1 = file1
    CALL UpperCase(upper1)
    
    upper2 = file2
    CALL UpperCase(upper2)
    
    b = upper1 == upper2
    
  END FUNCTION dh_CompareFilenames
  
  
  !*****************************************************************************
  !!
  !> Get the extension part of a filename in a path.
  !!
  !! @param[in]     host              The host object.
  !!
  !! @param[in]     path              The string representation of the path.
  !!
  !! @returns The extension in the given path.  The extension is defined as 
  !! whatever follows the last dot in the path.
  
  FUNCTION dh_GetFileExtension(host, path) RESULT(ext)
    
    USE CompilerKinds
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DefaultHost), INTENT(IN) :: host
    CHARACTER(*,KIND=fnck), INTENT(IN) :: path
    
    ! Function result
    CHARACTER(:,KIND=fnck), ALLOCATABLE :: ext
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Filename part of path.
    CHARACTER(:,KIND=fnck), ALLOCATABLE :: filename
    
    INTEGER :: pos            ! Location of the dot.
    
    !***************************************************************************
    
    ! Look for the dot.
    filename = host%GetFileName(path)
    pos = SCAN(filename, fnck_'.', BACK=.TRUE.)
    
    ! A filename that ends with a dot has no extension and a filename that 
    ! has only a dot at the start is treated as having no extension.
    IF ((pos == 0) .OR. (pos == LEN(filename)) .OR. (pos == 1)) THEN
      ext = ''
    ELSE
      ext = filename(pos+1:)
    END IF
    
  END FUNCTION dh_GetFileExtension
  
  
  !*****************************************************************************
  !!
  !> Get the parent directory part of a path.
  !!
  !! @param[in]     host              The host object.
  !!
  !! @param[in]     path              The string representation of the path.
  !!
  !! @returns The parent directory in the given path.  The parent directory 
  !! is defined as whatever is before the last foreslash or backslash in 
  !! the filename, plus that last foreslash or backslash.
  
  FUNCTION dh_GetFileDirectory(host, path) RESULT(dir)
    
    USE CompilerKinds
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DefaultHost), INTENT(IN) :: host
    CHARACTER(*,KIND=fnck), INTENT(IN) :: path
    
    CHARACTER(:,KIND=fnck), ALLOCATABLE :: dir
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: pos            ! Position of the last "/" or "\".
    
    !***************************************************************************
    
    ! Look for the slashes.
    pos = SCAN(path, fnck_'/\', BACK=.TRUE.)
    IF (pos == 0) THEN
      dir = ''
    ELSE
      dir = path(:pos)
    END IF
    
  END FUNCTION dh_GetFileDirectory
  
  
  !*****************************************************************************
  !!
  !> Get the file name part of a path.
  !!
  !! @param[in]     host              The host object.
  !!
  !! @param[in]     path              The string representation of the path.
  !!
  !! @returns The filename part of the path.  This is defined as everything 
  !! following the last slash or backslash in the filename.
  
  FUNCTION dh_GetFileName(host, path) RESULT(name)
    
    USE CompilerKinds
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DefaultHost), INTENT(IN) :: host
    CHARACTER(*,KIND=fnck), INTENT(IN) :: path
    
    CHARACTER(:,KIND=fnck), ALLOCATABLE :: name
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: pos            ! Position of the last "/" or "\".
    
    !***************************************************************************
    
    ! Look for the slashes.
    pos = SCAN(path, fnck_'/\', BACK=.TRUE.)
    name = path(pos+1:)
    
  END FUNCTION dh_GetFileName
  
END MODULE DefaultHosts
