! $Id: InterfaceBodyStNodes.f90 2195 2016-06-06 14:51:14Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the InterfaceSpecificationStNodes module.


!*******************************************************************************
!!
!> Defines the FunctionInterfaceBodyStNode and the 
!! SubroutineInterfaceBodyStNode types to represent the two varieties of 
!! interface body in the statement tree.
!!
!! These nodes are very similar to program unit nodes, so they use the 
!! ProgUnitStNode type as a parent and the associated BuildTree procedure.

MODULE InterfaceBodyStNodes
  
  USE ProgUnitStNodes
  USE StNodes
  
  IMPLICIT NONE
  
  PRIVATE
  
  !-----------------------------------------------------------------------------
  
  TYPE, EXTENDS(ProgUnitLikeStNode), PUBLIC, ABSTRACT :: InterfaceBodyStNode
  CONTAINS
    PROCEDURE :: GetPart => ifbody_GetPart
    PROCEDURE :: Query => ifbody_Query
    PROCEDURE :: Modify => ifbody_Modify
    PROCEDURE(ifbody_GetName), DEFERRED :: GetName
  END TYPE InterfaceBodyStNode
  
  ! Interfaces for deferred bindings in InterfaceBodyStNode.
  INTERFACE
    !> Interface for InterfaceBodyStNode%GetName - get the name of the 
    !! subprogram declared by the interface body.
    !!
    !! Must not be called unless the tree building pass has been successfully 
    !! completed for the subject node.
    !!
    !! @param[in]     st              The statement tree node for the 
    !! program unit.
    !!
    !! @param[out]    name            The subprogram name.  Note that this 
    !! is default character kind, regardless of the kind of the underlying 
    !! source.
    !!
    !! If a name cannot be determined (perhaps due to a syntax issue) then 
    !! the @a name argument will be unallocated on return.  Otherwise the 
    !! name will be allocated and will not be zero length.
    SUBROUTINE ifbody_GetName(st, name)
      IMPORT :: InterfaceBodyStNode
      IMPLICIT NONE
      !---------------------------------------------------------------------------
      CLASS(InterfaceBodyStNode), INTENT(IN) :: st
      CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    END SUBROUTINE ifbody_GetName
  END INTERFACE
  
  !> Statement tree node for an interface body for a function.
  TYPE, PUBLIC, EXTENDS(InterfaceBodyStNode) :: FunctionInterfaceBodyStNode
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => fn_BuildTree
    PROCEDURE :: GetName => fn_GetName
  END TYPE FunctionInterfaceBodyStNode
  
  !> Statement tree node for an interface body for a subroutine.
  TYPE, PUBLIC, EXTENDS(InterfaceBodyStNode) :: SubroutineInterfaceBodyStNode
  CONTAINS
    ! Bindings inherited from StNode.
    PROCEDURE :: BuildTree => sub_BuildTree
    PROCEDURE :: GetName => sub_GetName
  END TYPE SubroutineInterfaceBodyStNode
  
CONTAINS
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for InterfaceBodyStNode
  
  
  !*****************************************************************************
  !!
  !> Interface for InterfaceBodyStNode%GetPart - get the syntax part 
  !! of the current statement tree node.
  !!
  !! @returns The part associated with the node.
  
  FUNCTION ifbody_GetPart(st) RESULT(ipt)
    
    USE SyntaxParts
    
    !---------------------------------------------------------------------------
    ! Characteristics
    
    CLASS(InterfaceBodyStNode), INTENT(IN) :: st
    
    ! Function result
    INTEGER :: ipt
    
    !***************************************************************************
    
    ! Note that we don't have a different part for function interface 
    ! bodies and subroutine interface bodies.  This is a consequence of 
    ! the way the language syntax rules are written.
    ipt = iptInterfaceBody
    
  END FUNCTION ifbody_GetPart
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceBodyStNode%Query - apply visitor object 
  !! operations to this node and its children.
  !!
  !! @param[in]     st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the query action.
  
  ! Recursive because of nested interface bodies.
  RECURSIVE SUBROUTINE ifbody_Query(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InterfaceBodyStNode), INTENT(IN), TARGET :: st
    CLASS(StNodeQueryVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Query(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE ifbody_Query
  
  
  !*****************************************************************************
  !!
  !> Implementation of InterfaceBodyStNode%Modify - apply modify visitor 
  !! object operations to this node and its children.
  !!
  !! @param[in,out] st                The statement tree node being traversed.
  !!
  !! @param[in,out] visitor           The object to execute the modify action.
  
  ! Recursive because of nested interface bodies.
  RECURSIVE SUBROUTINE ifbody_Modify(st, visitor)
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(InterfaceBodyStNode), INTENT(INOUT), TARGET :: st
    CLASS(StNodeModifyVisitor), INTENT(INOUT) :: visitor
    
    !***************************************************************************
    
    ! Execute against ourselves.
    CALL visitor%ExecuteNode(st)
    
    ! Opening statement, if present.
    IF (ALLOCATED(st%first)) CALL visitor%ExecuteStmt(st, st%first)
    
    ! Forward to specificaction section, if present.
    IF (ALLOCATED(st%spec)) CALL st%spec%Modify(visitor)
    
    ! Final statement.
    CALL visitor%ExecuteStmt(st, st%last)
    
  END SUBROUTINE ifbody_Modify
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for FunctionInterfaceBodyStNode
  
  
  !*****************************************************************************
  !!
  !> Interface for FunctionInterfaceBodyStNode%BuildTree - incorporate the 
  !! current statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE fn_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    USE StNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FunctionInterfaceBodyStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istFunction,  &
        istEndFunction,  &
        new_tip,  &
        err_list )
    
  END SUBROUTINE fn_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of FunctionInterfaceBodyStNode::GetName - get the 
  !! name of the separate module subprogram.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the function, 
  !! or not allocated on error.
  
  SUBROUTINE fn_GetName(st, name)
    
    USE CompilerKinds
    USE FunctionStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(FunctionInterfaceBodyStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetFunctionName(st%first%tlist, name)
    
  END SUBROUTINE fn_GetName
  
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !
  ! Procedures for SubroutineInterfaceBodyStNode.
  
  
  !*****************************************************************************
  !!
  !> Interface for SubroutineInterfaceBodyStNode%BuildTree - incorporate the 
  !! current statement into the statement tree.
  !!
  !! @param[in,out] st                The statement tree node.
  !!
  !! @param[in,out] stmt              The current statement.
  !!
  !! @param[out]    new_tip           The new tip of the statement tree after 
  !! the statement has been processed.
  !!
  !! @param[out]    err_list          List of errors.
  
  RECURSIVE SUBROUTINE sub_BuildTree(st, stmt, new_tip, err_list)
    
    USE Errors
    USE Statements
    USE StatementData
    USE StNodes
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SubroutineInterfaceBodyStNode), INTENT(INOUT), TARGET :: st
    TYPE(StData), INTENT(INOUT) :: stmt
    CLASS(StNode), INTENT(OUT), POINTER :: new_tip
    TYPE(Error), INTENT(OUT), ALLOCATABLE :: err_list(:)
    
    !***************************************************************************
    
    CALL DoBuildTree(  &
        st,  &
        stmt,  &
        istSubroutine,  &
        istEndSubroutine,  &
        new_tip,  &
        err_list )
    
  END SUBROUTINE sub_BuildTree
  
  
  !*****************************************************************************
  !!
  !> Implementation of SubroutineInterfacebodyStNode::GetName - get the 
  !! name of the separate module subprogram.
  !!
  !! @param[in]     st                The statement tree node.
  !!
  !! @param[out]    name              The name of the subroutine, 
  !! or not allocated on error.
  
  SUBROUTINE sub_GetName(st, name)
    
    USE SubroutineStmtsUtils
    
    !---------------------------------------------------------------------------
    ! Arguments
    
    CLASS(SubroutineInterfacebodyStNode), INTENT(IN) :: st
    CHARACTER(:), INTENT(OUT), ALLOCATABLE :: name
    
    !***************************************************************************
    
    CALL GetSubroutineName(st%first%tlist, name)
    
  END SUBROUTINE sub_GetName
  
END MODULE InterfaceBodyStNodes
