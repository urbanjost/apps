! $Id: InterfaceStNodes.f90 2795 2019-03-22 09:15:08Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the InterfaceStNodes module


!*******************************************************************************
!!
!> Defines the InterfaceStNode type to represent /interface-block/'s in the
!! statement tree.

MODULE InterfaceStNodes
  
  USE StatementData
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  !> StNode for an /interface-block/.
  TYPE, PUBLIC, EXTENDS(StNode) :: InterfaceStNode
    !> The interface-stmt that starts the interface block.  Not allocated 
    !! the first time that the BuildTree method is called.
    TYPE(StData), ALLOCATABLE :: first
    !> The various procedure interfaces inside the interface block.  These 
    !! can be subroutine or function interface-body's, or procedure-stmt's.
    !!
    !! Always allocated on a successful build, may be size zero.
    TYPE(StNodeList), ALLOCATABLE :: bodies(:)
    !> The end-interface-stmt that ends the interface block.  Gets allocated 
    !! the last time that BuildTree is called.
    TYPE(StData), ALLOCATABLE :: last
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => intf_BuildTree
    PROCEDURE :: GetPart => intf_GetPart
    PROCEDURE :: DumpTree => intf_DumpTree
    PROCEDURE :: Query => intf_Query
    PROCEDURE :: Modify => intf_Modify
    PROCEDURE :: GetLocation => intf_GetLocation
    PROCEDURE :: GetLabel => intf_GetLabel
  END TYPE InterfaceStNode
  
CONTAINS
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceStNode%BuildTree - incorporate the current 
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
  
  RECURSIVE SUBROUTINE intf_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE ErrorCodes
    USE ErrorLevels
    USE InterfaceBodyStNodes
    USE SingleStNodes
    USE Statements
    USE StatementData
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InterfaceStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !---------------------------------------------------------------------------
    ! Local constants
    
    CHARACTER(*), PARAMETER :: comp = 'R458'
    
    !***************************************************************************
    
    ALLOCATE(err_list(0))
    
    IF (.NOT. ALLOCATED(st%first)) THEN
      
      ALLOCATE(st%first)
      IF (stmt%ist /= istInterface) THEN
        STOP 'Internal error in InterfaceStNodes%%intf_BuildTree, &
            &the first statement of an /interface-block/ isn''t an &
            &/interface-stmt/.'
      END IF
      CALL Move(st%first, stmt)
      new_tip => st
      
    ELSE IF (stmt%ist == istEndInterface) THEN
      
      IF (ALLOCATED(st%last)) THEN
        STOP 'Internal error in InterfaceStNodes%%intf_BuildTree, &
            &the last statement of an /interface-block/ was &
            &encountered twice.'
      END IF
      ALLOCATE(st%last)
      CALL Move(st%last, stmt)
      
      IF (.NOT. ALLOCATED(st%bodies)) ALLOCATE(st%bodies(0))
      
      new_tip => st%parent
      
    ELSE IF (stmt%ist == istFunction) THEN
      
      CALL AddNode(st%bodies)
      ALLOCATE(FunctionInterfaceBodyStNode :: st%bodies(SIZE(st%bodies))%item)
      st%bodies(SIZE(st%bodies))%item%parent => st
      CALL st%bodies(SIZE(st%bodies))%item%BuildTree(stmt, new_tip, err_list)
      
    ELSE IF (stmt%ist == istSubroutine) THEN
      
      CALL AddNode(st%bodies)
      ALLOCATE(SubroutineInterfaceBodyStNode :: st%bodies(SIZE(st%bodies))%item)
      st%bodies(SIZE(st%bodies))%item%parent => st
      CALL st%bodies(SIZE(st%bodies))%item%BuildTree(stmt, new_tip, err_list)
      
    ELSE IF (stmt%ist == istProcedure) THEN
      
      CALL AddNode(st%bodies)
      ALLOCATE(SingleStNode :: st%bodies(SIZE(st%bodies))%item)
      st%bodies(SIZE(st%bodies))%item%parent => st
      CALL st%bodies(SIZE(st%bodies))%item%BuildTree(stmt, new_tip, err_list)
      
    ELSE
      
      CALL Add( err_list, CODE=errInvalidContext, COMPONENT=comp,  &
          LOCATION=QueryLocation(stmt%tlist(1)),  &
          MSG='A /' // GetStName(stmt%ist) // '/ is not valid inside an &
            &/interface-block/.' )
      
    END IF
    
  END SUBROUTINE intf_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceStNode%GetPart - return the number of the 
  !! syntax part associated with this node.
  !!
  !! @param[in]     st                The statement tree node of interest.
  !!
  !! @returns The part associated with the node.
  
  FUNCTION intf_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(InterfaceStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ipt = iptInterfaceBlock
    
  END FUNCTION intf_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceStNode%DumpTree - print debugging information 
  !! about this node.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[in]     unit              The unit to dump to.
  !!
  !! @param[in]     pfx               Prefix to write at the start 
  !! of each record (for indentation).
  
  SUBROUTINE intf_DumpTree(st, unit, pfx)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InterfaceStNode), INTENT(IN), TARGET :: st
    INTEGER, INTENT(IN) :: unit
    CHARACTER(*), INTENT(IN) :: pfx
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Interface body index.
    
    !***************************************************************************
    
    WRITE (unit, fmt_partname) pfx, GetPartName(st)
    
    IF (ALLOCATED(st%first))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%first)
    IF (ALLOCATED(st%bodies)) THEN
      DO i = 1, SIZE(st%bodies)
        CALL st%bodies(i)%item%DumpTree(unit, pfx // '  ')
      END DO
    END IF
    IF (ALLOCATED(st%last))  &
        WRITE (unit, fmt_stmtname) pfx // '  ', GetStName(st%last)
    
  END SUBROUTINE intf_DumpTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceStNode%Query - apply visitor object operations 
  !! to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because of nested interface bodies.
  RECURSIVE SUBROUTINE intf_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InterfaceStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Interface body index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to interface bodies.
    DO i = 1, SIZE(st%bodies)
      CALL st%bodies(i)%Query(visitor)
    END DO
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE intf_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceStNode%Modify - apply modify visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because of nested interface bodies.
  RECURSIVE SUBROUTINE intf_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InterfaceStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !---------------------------------------------------------------------------
    ! Local variables
    
    INTEGER :: i              ! Interface body index.
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement.
    CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to interface bodies.
    DO i = 1, SIZE(st%bodies)
      CALL st%bodies(i)%Modify(visitor)
    END DO
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE intf_Modify
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceStNode%GetLocation - get the 
  !! starting source location of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The source location of the start of the node.
  
  FUNCTION intf_GetLocation(st) RESULT(loc)
    
    USE SourceLocations
    USE Tokens
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(InterfaceStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(SourceLocation) :: loc
    
    !***************************************************************************
    
    loc = QueryLocation(st%first%tlist(1))
    
  END FUNCTION intf_GetLocation
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceStNode%GetLabel - get the label 
  !! of the node.
  !!
  !! @param[in]     st                The node of interest.
  !!
  !! @returns The statement label of the the node.
  
  FUNCTION intf_GetLabel(st) RESULT(label)
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(InterfaceStNode), INTENT(IN) :: st
    
    ! Function result.
    TYPE(LabelWrapper) :: label
    
    !***************************************************************************
    
    ! ifort bug - allocatable component in structure constructor.
    if (allocated(st%first%statement_label)) then
      label = LabelWrapper(st%first%statement_label)
    end if
    
  END FUNCTION intf_GetLabel
  
END MODULE InterfaceStNodes
