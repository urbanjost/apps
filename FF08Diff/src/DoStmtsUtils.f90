! $Id: DoStmtsUtils.f90 2800 2019-03-22 18:13:05Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the DoStmtUtils module.


!*******************************************************************************
!!
!> Procedure for extracting the terminating label of a /label-do-stmt/.
!!
!! This has been split out from the DoStmts module to improve source file 
!! granularity.

MODULE DoStmtsUtils
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  PUBLIC :: GetLabel
  
  !> Extract the terminating label of a /label-do-stmt/.
  INTERFACE GetLabel
    MODULE PROCEDURE GetLabel_
  END INTERFACE GetLabel
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Gets the label value of a /label-do-stmt/ (R816).
  !!
  !! [ /do-construct-name/ : ] DO /label/ [ /loop-control/ ]
  !!
  !! @param[in]     tlist             The sequence of tokens for the 
  !! /label-do-stmt/.
  !!
  !! @param[out]    the_label         The label for the statement that will 
  !! mark the end of the do-block.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE GetLabel_(tlist, label_value, err_list)
    
    USE CharUtils
    USE CompilerKinds
    USE Errors
    USE ErrorCodes
    USE LabelsUtils
    USE Scanner
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    TYPE(Token), INTENT(IN) :: tlist(:)
    INTEGER, INTENT(OUT) :: label_value
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: it             ! Label token index.
    TYPE(Label) :: the_label  ! Scanned label.
    
    ! Error list for child procedure calls.
    TYPE(Error), ALLOCATABLE :: sub_err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    IF (SIZE(tlist) < 2) THEN
      CALL missing_label_error
      RETURN
    END IF
      
    ! Determine the position of the label in the list of tokens - we need 
    ! to check for a construct name.
    IF (IsToken(tlist(2), ittColon, scck_':')) THEN
      IF (SIZE(tlist) < 4) THEN
        CALL missing_label_error
        RETURN
      END IF
      it = 4
    ELSE
      it = 2
    END IF
    
    ! Validate that the label looks like a label.
    CALL ScanLabel(  &
        QueryRaw(tlist(it)),  &
        QueryLocation(tlist(it)),  &
        the_label,  &
        sub_err_list )
    CALL Add(err_list, sub_err_list, COMPONENT='R816')
    
    label_value = .ValueOf. the_label
    
  CONTAINS
    
    SUBROUTINE missing_label_error
      CALL Add( err_list,  &
          CODE=errLabelDoMissingLabel,  &
          LOCATION=QueryLocation(tlist(SIZE(tlist))),  &
          MSG='A /label-do-stmt/ is missing its label.',  &
          COMPONENT='R816' )
    END SUBROUTINE missing_label_error
    
  END SUBROUTINE GetLabel_
  
END MODULE DoStmtsUtils
