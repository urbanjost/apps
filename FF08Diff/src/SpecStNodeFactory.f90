! $Id: SpecStNodeFactory.f90 2905 2019-04-12 22:32:58Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the SpecStNodeFactory subroutine.


!*******************************************************************************
!!
!> External procedure to create statement tree nodes for specification 
!! statements and constructs.  
!!
!! This exists as an external procedure to break a cyclic dependency loop 
!! between statement tree nodes (program units are used in interface 
!! blocks, which are used in specification sections which are used in 
!! program units).
!!
!! @param[in]     ist                 The statement identifier that describes 
!! the type of statement tree node that needs to be created.
!!
!! @param[in]     implicit_part       True to indicate that the statement 
!! being considered is part of the /implicit-part/ of the 
!! /specification-part/, false if the statement is part of the following 
!! sequence of /declaration-construct/s.
!!
!! @param[out]    node                Allocated to the appropriate 
!! specification statement tree node, or not allocated if the statement is 
!! not recognised as a statement that can be in a /declaration-construct/.
!!
!! If @a implicit_part is true, a SingleStNode is always allocated.
!!
!! When @a implicit_part is false, this procedure recognises all the 
!! statements that can be part of a /declaration-construct/ (R207), 
!! namely:
!! - /derived-type-def/ ()
!! - /entry-stmt/
!! - /enum-def/ ()
!! - /format-stmt/
!! - /interface-block/ (/interface-stmt/)
!! - /parameter-stmt/
!! - /procedure-declaration-stmt/
!! - /other-specification-stmt/
!! - /type-declaration-stmt/
!! - /stmt-function-stmt/
!!
!! We rely on the definition of the isgOtherSpecification group for the 
!! /other-specification-stmt/ rule.  Each of these will be allocated a 
!! SingleStNode.
!!
!! An interface for this external procedure is provided in the SpecStNodes 
!! module.
!!
!! Note that this routine can be used to test whether a statement is a 
!! specification statement, through the allocation status of the @a node 
!! argument.

SUBROUTINE SpecStNodeFactory(ist, implicit_part, node)
  
  ! Only clauses to prevent this procedure from picking up our own interface.
  USE DerivedTypeDefStNodes, ONLY: DerivedTypeDefStNode
  USE InterfaceStNodes, ONLY: InterfaceStNode
  USE SingleStNodes, ONLY: SingleStNode
  USE EnumDefStNodes, ONLY: EnumDefStNode
  
  USE Statements
  USE StNodes, ONLY: StNode
  
  IMPLICIT NONE
  
  !-----------------------------------------------------------------------------
  ! Arguments
  
  INTEGER, INTENT(IN) :: ist
  LOGICAL, INTENT(IN) :: implicit_part
  CLASS(StNode), INTENT(OUT), ALLOCATABLE :: node
  
  !*****************************************************************************
  
  IF (implicit_part) THEN
    ALLOCATE(SingleStNode:: node)
  ELSE
    SELECT CASE (ist)
    CASE (istInterface)
      ALLOCATE(InterfaceStNode:: node)
    
    CASE (istDerivedType)
      ALLOCATE(DerivedTypeDefStNode:: node)
    
    CASE (istEnumDef)
      ALLOCATE(EnumDefStNode:: node)
    
    ! Contiguous added on the assumption that it was an oversight.
    CASE ( istEntry, istFormat, istParameter, istProcedureDeclaration,  &
        istTypeDeclaration, istStmtFunction, istAccess, istAllocatable,  &
        istAsynchronous, istBind, istCodimension, istContiguous, istCommon,  &
        istData, istDimension, istEquivalence, istExternal, istIntent,  &
        istIntrinsic, istNamelist, istOptional, istPointer, istProtected,  &
        istSave, istTarget, istVolatile, istValue )
      ALLOCATE(SingleStNode:: node)
    
    ! Default case - leave the node argument unallocated.
    
    END SELECT
  END IF
  
END SUBROUTINE SpecStNodeFactory
