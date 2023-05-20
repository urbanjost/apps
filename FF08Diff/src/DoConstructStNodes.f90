! $Id: DoConstructStNodes.f90 2844 2019-03-27 21:01:19Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the DoStNodes module.


!*******************************************************************************
!!
!> Defines the DoConstructStNode type to represent /do-construct/'s in the 
!! statement tree.

MODULE DoConstructStNodes
  
  USE ExecStNodes
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> StNode for a Do construct.
  TYPE, PUBLIC, EXTENDS(BasicBlockStNode) :: DoConstructStNode
    !> If the do-construct was started by a label-do-stmt then this holds 
    !! the nominated label number.  Otherwise zero, which is not a valid 
    !! label value.
    INTEGER :: label_value = 0
  CONTAINS
    ! Bindings inherited from StNode
    PROCEDURE :: BuildTree => do_BuildTree
    PROCEDURE :: Check => do_Check
    PROCEDURE :: GetPart => do_GetPart
    PROCEDURE :: CheckLabel => do_CheckLabel
    PROCEDURE :: Terminate => do_Terminate
  END TYPE DoConstructStNode
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! DoStNode procedures.
  
  
  !*****************************************************************************
  !!
  !> Implementation of DoStNode%BuildTree - incorporate the current 
  !! statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !! 
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  !!
  !! There are two forms of do construct - block and nonblock.  The block 
  !! form then has two variants - a labelled form or a non-labelled form.  
  !! The block non-labelled construct begins with a /nonlabel-do-stmt/ 
  !! (the modern form) and finishes with an /end-do-stmt/.  The labelled 
  !! block construct begines with a /label-do-stmt/ and may finish 
  !! with either an /end-do-stmt/ or with a /continue-stmt/, in both cases 
  !! the statement that finishes the construct must have the same label 
  !! as nominated in the /label-do-stmt/.
  !!
  !! The /nonblock-do-construct/ is like the labelled block construct, 
  !! except that the final statement is an action statement.  Constructs 
  !! might share the terminating action statement (note that terminating 
  !! action statement might be a continue statement).  This form is 
  !! deprecated.
  !!
  !! The nonlabelled case is consistent with other block constructs.
  !!
  !! In the labelled cases, we rely upon the CheckLabel binding to 
  !! identify when the construct is complete.
  
  RECURSIVE SUBROUTINE do_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE DoStmtsUtils
    USE Statements
    USE StatementData
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DoConstructStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ! All paths below allocate err_list.
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      ! First time we've been called.  If we are a labelled do construct 
      ! (a construct that starts with a /label-do-stmt/) then store the 
      ! label for later termination detection, otherwise leave st%label 
      ! at its default of zero (not a valid label value).
      IF (stmt%ist == istLabelDo) THEN
        CALL GetLabel(stmt%tlist, st%label_value, err_list)
        IF (Failure(err_list)) THEN
          ! Couldn't get the label - pretend the label-do-stmt didn't exist.
          new_tip => st%parent
          RETURN
        END IF
      END IF
    END IF
    
    IF (st%label_value /= 0) THEN
      CALL DoBuildTree(st, stmt, istLabelDo, istEndDo, new_tip, err_list)
      ! If the tree building process terminated this block, then check that 
      ! the label on the statement matches the label we are looking for.  This 
      ! can happen if the opening statement is a label-do-stmt and the closing 
      ! statement is a END DO.
      IF (ALLOCATED(st%last)) THEN
        IF (ALLOCATED(st%last%statement_label)) THEN
          IF (.ValueOf. st%last%statement_label /= st%label_value) THEN
            CALL Add( err_list,  &
                CODE=errSyntax,  &
                COMPONENT='C815 on R814',  &
                LOCATION=QueryLocation(st%last%tlist(1)),  &
                MSG='If the opening do-stmt of a /do-construct/ is a &
                  &/label-do-stmt/ and the closing statement is an &
                  &/end-do-stmt/ then the labels must match.' )
          END IF
        ELSE
          ! The opening statement had a label but the closing statement 
          ! does not.
          CALL Add( err_list,  &
              CODE=errSyntax,  &
              COMPONENT='C815 on R814',  &
              LOCATION=QueryLocation(st%last%tlist(1)),  &
              MSG='If the opening do-stmt of a /do-construct/ is a &
                &/label-do-stmt/ and the closing statement is an &
                &/end-do-stmt/ then the closing statement must &
                &have a matching statement label.' )
        END IF
      END IF
    ELSE
      CALL DoBuildTree(st, stmt, istNonlabelDo, istEndDo, new_tip, err_list)
    END IF
    
  END SUBROUTINE do_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of DoStNode%GetPart - get the syntax part of this 
  !! statement tree node.
  !!
  !! @returns The part associated with the node.
  
  FUNCTION do_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(DoConstructStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ! We used to describe this part as a block-do-construct, but we really 
    ! didn't know until after termination.  This resulted in inaccurate 
    ! error messages, so we've gone to the more general /do-construct/.
    ipt = iptDoConstruct
    
  END FUNCTION do_GetPart
  
  
  !*****************************************************************************
  !!
  !> Check the label associated with a statement tree node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     label             The label to test.
  !!
  !! @returns .TRUE. if we are a do construct introduced by a non-labelled 
  !! do statement and the label in that statement matches the current 
  !! statement's label, .FALSE. otherwise.
  !!
  !! Overriden because a labelled do construct is terminated by a labelled 
  !! statement.
  
  FUNCTION do_CheckLabel(st, statement_label)
    
    USE LabelsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DoConstructStNode), INTENT(IN), TARGET :: st
    TYPE(Label), INTENT(IN) :: statement_label
    
    ! Function result.
    LOGICAL :: do_CheckLabel
    
    !***************************************************************************
    
    ! Check if we were terminated.  If we are a non-labelled do construct 
    ! then st%label will be zero.  Zero is not a valid label value (C304) 
    ! so we will never be asked to test that.
    do_CheckLabel = .ValueOf. statement_label == st%label_value
    
  END FUNCTION do_CheckLabel
  
  
  !*****************************************************************************
  !!
  !> Implementation of DoConstructStNode%Terminate - called when a parent 
  !! node decides that it has finished.
  !!
  !! @param[in]     st                The statement tree node being terminated.
  !!
  !! @paramin]      eof               The statement tree node was terminated 
  !! by end of file.  @a stmt is not defined.
  !!
  !! @param[in]     stmt              The statement that was incorporated into 
  !! the tree and triggered termination.  Not defined if @a eof is true.
  !!
  !! @param[out]    err_list          List of errors.
  
  SUBROUTINE do_Terminate(st, eof, stmt, err_list)
    
    USE CharUtils
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE StatementData
    USE Statements
    USE SyntaxParts
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DoConstructStNode), INTENT(INOUT), TARGET :: st
    LOGICAL, INTENT(IN) :: eof
    TYPE(StData), INTENT(IN) :: stmt
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    ! Component for error reporting.
    CHARACTER(:), ALLOCATABLE :: comp
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    !---------------------------------------------------------------------------
    
    comp = 'R' // ToString(st%GetPart())
    IF (eof) THEN
      CALL Add( err_list,  &
          CODE=errUnterminatedBlock,  &
          COMPONENT=comp,  &
          MSG='An unterminated block exists for a /'  &
              // GetPartName(st) // '/.' )
      RETURN
    END IF
    
    ! What terminates the parent might also terminate us.
    IF (st%label_value /= 0) THEN
      IF (ALLOCATED(stmt%statement_label)) THEN
        IF (st%label_value == .ValueOf. stmt%statement_label) RETURN
      END IF
    END IF
    
    CALL Add( err_list,  &
        CODE=errUnterminatedBlock,  &
        COMPONENT=comp,  &
        LOCATION=QueryLocation(stmt%tlist(1)),  &
        MSG='An unterminated block exists for a /'  &
            // GetPartName(st) // '/.' )
    
  END SUBROUTINE do_Terminate
  
  
  !****************************************************************************
  !!
  !> Implementation of DoConstructStNode%Check - check that the node complies 
  !! with the rules of the language.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    err_list          List of errors
  
  SUBROUTINE do_Check(st, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE SingleStNodes, ONLY: SingleStNode
    USE Statements
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(DoConstructStNode), INTENT(IN) :: st
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (ALLOCATED(st%last)) RETURN
    
    IF (SIZE(st%exec%children) == 0) THEN
      STOP 'Internal error in DoConstructStNodes%%do_Check, &
          &the node had no children.'
    END IF
    
    SELECT TYPE (last_child => st%exec%children(SIZE(st%exec%children))%item)
    CLASS IS (SingleStNode)
      SELECT CASE (last_child%stmt%ist)
      CASE (istContinue)
        ! This is ok.
        
      CASE ( istArithmeticIf, istCycle, istErrorStop, istExit,  &
          istGoto, istReturn, istStop )
        ! The above are explicitly prohibited.
        CALL Add( err_list,  &
            CODE=errSyntax,  &
            COMPONENT='C816 on R826',  &
            LOCATION=last_child%GetLocation(),  &
            MSG='A /' // GetStName(last_child%stmt%ist)  &
              // '/ is not permitted to be the termination statement for &
              &a /action-term-do-construct/.' )
      
      CASE DEFAULT
        CALL Add( err_list,  &
            LEVEL=errLevelWarning,  &
            CODE=errDoActionTermination,  &
            COMPONENT='R826',  &
            LOCATION=last_child%GetLocation(),  &
            MSG='Termination of a /do-construct/ on a statement other than &
              &CONTINUE or END DO is obsolescent.' )
        
      END SELECT
      
    CLASS IS (DoConstructStNode)
      CALL Add( err_list,  &
          CODE=errDoSharedTermination,  &
          COMPONENT='R827',  &
          LEVEL=errLevelWarning,  &
          LOCATION=st%GetLocation(),  &
          MSG='The sharing of a termination statement by more than one &
            &/do-construct/ is obsolescent.' )
      
    CLASS DEFAULT
      ! The location here will be the location of the start of the construct 
      ! whose last statement also terminates the do construct.  This isn't 
      ! particularly precise, but doing something better might be tricky 
      ! (perhaps store the last location in the do construct during 
      ! build tree?).
      CALL Add( err_list,  &
          LEVEL=errLevelWarning,  &
          CODE=errDoActionTermination,  &
          COMPONENT='R826',  &
          LOCATION=last_child%GetLocation(),  &
          MSG='Termination of a /do-construct/ on a statement other than &
            &CONTINUE or END DO is obsolescent.' )
      
    END SELECT
    
  END SUBROUTINE do_Check
  
END MODULE DoConstructStNodes
