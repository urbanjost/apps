! $Id: ErrorCodes.f90 2877 2019-04-05 21:27:32Z ian $
! ff08 source code copyright 2012 M.E.G.M.S.  See LICENCE.txt for licence.
!> @file
!! Defines the ErrorCodes module.


!*******************************************************************************
!!
!> Defines all the error codes that the processor and associated tools
!! can generate.
!!
!! Having these codes in the one place helps with documentation (and avoids
!! duplicate codes) but it is a pain in the backside from the point of view
!! of project coupling.

MODULE ErrorCodes

  IMPLICIT NONE

  ! All entities public.

  !-----------------------------------------------------------------------------
  ! Errors that result from the compiler host



  !-----------------------------------------------------------------------------
  ! Errors that result primarily during Scanning.

  !> Generic syntax error.
  !!
  !! There are about ten thousand reasons why this error could be generated.
  !! Originally we were going to give a unique code to each error, but that
  !! seemed like overkill.  Now it more or less represents a violation of the
  !! R* syntax rules.
  INTEGER, PARAMETER :: errSyntax = 1000

  !> A line in a free form source file is is longer than 132 default
  !! characters, or longer than the processor dependent length for
  !! non-default character source (3.3.2.1).
  !!
  !! This warning is specifically given when there are non-blank characters
  !! past position 132 in free form source or non-blank characters past
  !! position 72 in fixed form source.  When there are only blanks past
  !! position 132 or 72 respectivly then the warning
  !! errLineWithBlanksTooLong is given instead.
  INTEGER, PARAMETER :: errLineTooLong = 1001

  !> Then end of a line was reached without reading in the closing
  !! character literal.
  INTEGER, PARAMETER :: errUnterminatedCharLiteral = 1002

  !> A bare ampersand was encountered (3.3.2.4).
  INTEGER, PARAMETER :: errBareAmpersand = 1003

  !> A continued character constant did not start on the continued
  !! line with an ampersand.
  INTEGER, PARAMETER :: errNoAmpersand = 1004

  !> A literal constant is missing part of its kind specification.
  INTEGER, PARAMETER :: errBadLiteralKind = 1005

  !> No line found following continued line.
  INTEGER, PARAMETER :: errNoFollowingLine = 1006

  !> A lexical token was terminated with a continuation character, with
  !! no ampersand on the next line and non-blank characters in column one.
  !! Did the user mean to continue the token?
  INTEGER, PARAMETER :: errSplitTokenNoAmpersand = 1007

  !> Two lexical tokens that must be separated by a blank, weren't.
  INTEGER, PARAMETER :: errMissingBlank = 1008

  !> Some sort of internal logic failure detected.
  !!
  !! Typically we bail out using a STOP statement, but sometimes it is more
  !! useful to generate this and then let the parent procedure decide what
  !! to do.
  INTEGER, PARAMETER :: errInternal = 1009

  !> If both exponent-letter and a kind-param appear, then exponent-letter
  !! shall be E (C412/R413 4.4.2.3).
  INTEGER, PARAMETER :: errWrongExponentLetter = 1010

  !> If both exponent-letter and a kind-param appear, then exponent-letter
  !! shall be E (C412/R413 4.4.2.3).
  INTEGER, PARAMETER :: errMissingExponent = 1011

  !> Character encountered outside of a character-literal that is not
  !! part of the processors character set.
  INTEGER, PARAMETER :: errStrangeCharacter = 1012

  !> The initial line of a continued statement in fixed form source
  !! appears to be a program unit end statement (perhaps because it is
  !! one - you are not allowed to continue them).
  INTEGER, PARAMETER :: errContinuedAppearsLikeEnd = 1013

  !> A file ended before a statement ended.
  !!
  !! This may be an indication of an internal error.
  INTEGER, PARAMETER :: errFileEndedButStatementDidnt = 1014

  ! No idea why we skipped numbers.  Perhaps I can't count.

  !> The string part of a binary boz-literal-constant did not
  !! contain only the digits 0 or 1 (C4100/R464 4.7).
  INTEGER, PARAMETER :: errBadBinaryLiteral = 1020

  !> The string part of a binary boz-literal-constant did not
  !! contain only the digits 0 through 7 (C4101/R465 4.7).
  INTEGER, PARAMETER :: errBadOctalLiteral = 1021

  !> The string part of a binary boz-literal-constant did not
  !! contain only the digits 0 or 1 (R467 4.7).
  INTEGER, PARAMETER :: errBadHexLiteral = 1022

  !> A statement appears in an invalid context.
  INTEGER, PARAMETER :: errWrongPosition = 1023

  !> The first line of a fixed form source file was marked as a continuation
  !! line.
  INTEGER, PARAMETER :: errFirstLineContinued = 1024

  !> An empty statement was labelled.
  INTEGER, PARAMETER :: errLabelWithNoStatement = 1025

  !> A constant or name was not separated from a following constant or name
  !! by a blank in free form source.
  INTEGER, PARAMETER :: errMissingBlankBetweenTokens = 1026

  !> A statement label did not contain a non-zero digit (C304/R312).
  INTEGER, PARAMETER :: errAllDigitsZero = 1027

  !> The char-literal on an include statement had a named constant.
  !!
  !! Generated in Parser::TestInclude.
  INTEGER, PARAMETER :: errBadIncludeCharLiteral = 1028

  !> What looks awfully like an include line was not on its own line.
  INTEGER, PARAMETER :: errBadIncludeLine = 1029

  !> Nested INCLUDE lines refer to the same file.
  INTEGER, PARAMETER :: errNestedInclude = 1030

  !> A kind-param in the char-literal-constant of an INCLUDE line is not
  !! supported by this processor.
  INTEGER, PARAMETER :: errUnsupportedIncludeKind = 1031

  !> A statement was empty, perhaps because a line started with a semicolon.
  INTEGER, PARAMETER :: errEmptyStatement = 1032

  !> There were blanks at the end of the line that took the total line
  !! length in free form source past 132 characters, or in fixed form source
  !! past 72 characters.  This is a warning.  If there were non-blank
  !! characters past those positions then the errLineTooLong warning is
  !! returned instead.
  INTEGER, PARAMETER :: errLineWithBlanksTooLong = 1033

  !> There were too many continuation lines.
  !!
  !! This is a warning.
  INTEGER, PARAMETER :: errTooManyContinuationLines = 1034

  !-----------------------------------------------------------------------------
  ! Errors that result primarily during classification

  !> The type of statement couldn't be determined.  This error is actually
  !! raised by client code of the classifier - the classifier just says
  !! that the statement type is istUnclassifed.
  !!
  !! "unclassified" or "unclassifiable"?
  INTEGER, PARAMETER :: errUnclassifiedStatement = 1101

  !-----------------------------------------------------------------------------
  ! Errors that result primarily during grouping and contexting.

  !> An unterminated block was encountered.
  !!
  !! The unterminated program will be exterminated.
  INTEGER, PARAMETER :: errUnterminatedBlock = 1201

  !> A data-stmt was found in the execution-part.
  !!
  !! This is a warning for an obsolescent feature.
  INTEGER, PARAMETER :: errDataStmtInExecutionPart = 1202

  !> A statement appears in an invalid context.
  INTEGER, PARAMETER :: errInvalidContext = 1203

  !> A statement appears twice.
  !!
  !! (Two program statements, etc?)
  INTEGER, PARAMETER :: errDuplicateStatement = 1204

  !> Conflicting statements appear.
  !!
  !! For example - both SEQUENCE and PRIVATE in a /derived-type-def/.
  INTEGER, PARAMETER :: errConflictingStatement = 1205

  !> The matching label for a labelled do construct was not found
  !! prior to the end of the scope.
  !!
  !! This is similar to errUnterminatedBlock.
  INTEGER, PARAMETER :: errMatchingDoLabelNotFound = 1206

  !> An internal subprogram had an internal subprogram part.
  INTEGER, PARAMETER :: errInternalSubprogramHadInternalSubprogram = 1207

  !> A program unit has been defined more than once.
  INTEGER, PARAMETER :: errProgramUnitMultipleDefinition = 1211

  !> An action statement other than CONTINUE or END DO was used to terminate
  !! a /do-construct/ introduced by a /label-do-stmt/ (i.e. we have a form of
  !! /nonblock-do-construct/.
  INTEGER, PARAMETER :: errDoActionTermination = 1212

  !> Multiple /do-construct/s shared the same termination statement - i.e.
  !! this is a form of /nonblock-do-construct/.
  INTEGER, PARAMETER :: errDoSharedTermination = 1213

  !> A name used for a subprogram (or more generally program unit) conflicts
  !! with the name of another subprogram that is being defined in the same
  !! scope.
  INTEGER, PARAMETER :: errSubprogramOrUnitAlreadyDefined = 1214

  !> An /entry-stmt/ was encountered.
  !!
  !! This is a warning for an obsolescent feature.
  INTEGER, PARAMETER :: errObsolescentEntryStmt = 1215
  
  !> An /arithmetic-if-stmt/ was encountered.
  !!
  !! This is a warning for an obsolescent feature.
  INTEGER, PARAMETER :: errObsolescentArithmeticIfStmt = 1216
  
  !> A /computed-goto-stmt/ was encountered.
  !!
  !! This is a warning for an obsolescent feature.
  INTEGER, PARAMETER :: errObsolescentComputedGotoStmt = 1217

  !-----------------------------------------------------------------------------
  ! Errors that result during parsing.

  !> Missing closing parenthesis.
  INTEGER, PARAMETER :: errMissingCloseParens = 1301

  !> Unexpected extra tokens.
  !!
  !! "Unexpected extra tokens following "xxxx"."
  INTEGER, PARAMETER :: errUnexpectedExtraTokens = 1302

  !> The unit or construct name in an END statement of some sort, or the
  !! construct name in an intermediate construct statement doesn't
  !! match the unit name or construct name provided in the opening statement.
  !!
  !! In the case of a program unit, it is not valid to provide a name in the
  !! /end-program-stmt/ unless a /program-stmt/ was present at the start
  !! of the /main-program/.
  INTEGER, PARAMETER :: errNameMismatch = 1303

  !> The order of letters in an implicit statement with a range of letters
  !! did not follow alphabetical order.
  INTEGER, PARAMETER :: errNotAlphabetical = 1304

  !> Attributes in a /type-declaration-stmt/ that can be overridden later
  !! were inconsistent.  This is a warning.
  INTEGER, PARAMETER :: errSillyAttributes = 1305

  !> Invalid child statement of an if-stmt.
  INTEGER, PARAMETER :: errIfBadChildStmt = 1311

  !> Use of obsolescent form of character length declaration.
  INTEGER, PARAMETER :: errObsolescentCharLength = 1312

  !> Type is not extensible, but ABSTRACT was specified.
  !!
  !! This corresponds to C430.
  INTEGER, PARAMETER :: errNotExtensible = 1313

  !> The type name clashes with the name of an intrinsic type or with
  !! the DOUBLEPRECISION type specifier.
  !!
  !! This corresponds to C426.
  INTEGER, PARAMETER :: errInvalidTypeName = 1314

  !> Use of the obsolescent * for a dummy argument, or the equivalent
  !! alternate return specifier for an actual argumnet.
  INTEGER, PARAMETER :: errObsolescentAlternateReturn = 1315

  !> Missing the label in a label-do-stmt.
  !!
  !! The classifier won't identify a label-do-stmt that doesn't have a label,
  !! so this will only arise if label-do-stmt parsing is invoked on tokens
  !! directly.
  INTEGER, PARAMETER :: errLabelDoMissingLabel = 1341

  !> The label in a label-do-stmt doesn't look like a label.
  INTEGER, PARAMETER :: errLabelDoBadLabel = 1342

  !> The label in a label-do-stmt is an invalid value.
  INTEGER, PARAMETER :: errLabelDoBadLabelValue = 1343

  !> A specification part, other than a specification part for an interface
  !! body that is not an interface body for a separate module procedure,
  !! contained an /import-stmt/.
  INTEGER, PARAMETER :: errImportNotPermitted = 1344

  !-----------------------------------------------------------------------------
  ! Errors that are generated during advertising.

  !> The type of an entity has been previously specified.
  INTEGER, PARAMETER :: errTypeAlreadySpecified = 1401

  !> An attribute of an identifer, that cannot be multiply specified, was
  !! multiply specified.
  !!
  !! This is usually due to C514.
  INTEGER, PARAMETER :: errAttributeAlreadySpecified = 1402

  !> An identifier has already been declared as a type.
  INTEGER, PARAMETER :: errAlreadyDeclaredAsType = 1403

  !> The type in a type declaration statement or similar has not been declared.
  INTEGER, PARAMETER :: errUnknownTypeName = 1404

  !> Inconsistent (incompatible) attributes
  INTEGER, PARAMETER :: errAttributeIncompatible = 1405

  !-----------------------------------------------------------------------------
  ! Errors that are generated during semantic node creations.

  !> The declaration of a local identifier clashes with an identifier acquired
  !! by use association.
  INTEGER, PARAMETER :: errUseClash = 1501

  !> A non-polymorphic data object is having its type declared prior to
  !! the type being defined.
  INTEGER, PARAMETER :: errTypeReferenceBeforeDefinition = 1502

  !> A component name in a data or proc component declaration clashed with
  !! the existing name of a component or binding.
  !!
  !! This is here rather than under the advertising section, as components
  !! and bindings are not considered local identifiers of the scoping unit
  !! of the derived type definition under our rules.
  INTEGER, PARAMETER :: errComponentNameClash = 1503

  !> Overlapping IMPLICIT spec's (violation of 5.5p2).
  INTEGER, PARAMETER :: errOverlappingImplicitSpecs = 1505

  !> Undeclared entity with IMPLICIT NONE in force
  INTEGER, PARAMETER :: errEntityNotDeclared = 1506

  !> Name already declared as a type in the current scope and an attempt was
  !! made to use it for some other object.
  INTEGER, PARAMETER :: errLocalNameClash = 1507

  !> Name already declared as a global name.
  INTEGER, PARAMETER :: errGlobalNameClash = 1508

  !> The name used for a kind param in a literal constant was not known.
  INTEGER, PARAMETER :: errKindParamNotKnown = 1509

  !> The name used for a kind param in a literal constant was not a constant.
  INTEGER, PARAMETER :: errKindParamNotConstant = 1510

  !> The name used for a kind param in a literal constant was not scalar.
  INTEGER, PARAMETER :: errKindParamNotScalar = 1511

  !> The name used for a kind param in a literal constant was not scalar.
  INTEGER, PARAMETER :: errKindParamNotInteger = 1512

  !> The variety for an entity has already been specified.  For some reason we
  !! are trying to specify it again.
  INTEGER, PARAMETER :: errVarietyAlreadySpecified = 1514

  !> The base object of a designator in a constant expression was not
  !! itself a constant.
  INTEGER, PARAMETER :: errDesignatorNotAConstant = 1515

  !> The definition of a module had not previously been made available.
  !!
  !! This corresponds to the `mod` file not being found.
  INTEGER, PARAMETER :: errUnknownModule = 1516

  !> A named type parameter was not known for the relevent type.
  INTEGER, PARAMETER :: errUnknownTypeParamName = 1517

  !> The initializer for an entity was not compatible with the entity by
  !! the rules of intrinsic assignment.
  INTEGER, PARAMETER :: errInitializerNotCompatible = 1518

  !> An entity listed in a use statement did not exist in the nominated
  !! module.
  INTEGER, PARAMETER :: errEntityNotInModule = 1519

  !> Function reference syntax was applied to something that cannot be
  !! referenced as a function.
  INTEGER, PARAMETER :: errIdentifierNotFunctionLike = 1520

  !> Different entities have been made available by use association using
  !! the same name.
  INTEGER, PARAMETER :: errUseConflict = 1521

  !> An access spec was specified for an identifier in a scope which
  !! does not permit an access spec.
  INTEGER, PARAMETER :: errNoAccess = 1522

  !> A structure constructor was encountered for an abstract type.
  INTEGER, PARAMETER :: errAbstractType = 1523

  !> Implicitly defined object later given different type.
  INTEGER, PARAMETER :: errTypeDifferent = 1524

  !> The name in a /procedure-stmt/ was not known as a non-intrinsic 
  !! specific procedure with an explicit interface.
  INTEGER, PARAMETER :: errNameNotAProcedure = 1525
  
  !> A unary or binary defined operator had more than 63 letters.
  INTEGER, PARAMETER :: errOperatorTooLong = 1526.
  
  !> A name had more than 63 letters.
  INTEGER, PARAMETER :: errNameTooLong = 1527.
  
  !> The identifier used in a /proc-interface/ was not known as a 
  !! intrisnic type or procedure.
  INTEGER, PARAMETER :: errUnknownProcInterface = 1528
  
  !> The identifier used in a PASS specification was not known as 
  !! a dummy procedure of the relevant procedure interface.
  INTEGER, PARAMETER :: errUnknownPassName = 1529
  
  !> An identifier used as an operator was not known as an operator 
  !! in the current scope.
  INTEGER, PARAMETER :: errUnknownOperator = 1530
  
  !> There was a mismatch between the subroutine/function nature of 
  !! a specific procedure behind a generic interface, and other 
  !! specific procedures behind the interface.
  INTEGER, PARAMETER :: errInconsistentProcedureType = 1531
  
  !-----------------------------------------------------------------------------
  ! Resolution?

  !> The value used for a kind parameter for an intrinsic type is not
  !! supported.
  INTEGER, PARAMETER :: errKindValueNotSupported = 1601

  !> A matching specific procedure in a reference to a generic was not
  !! found.
  !!
  !! This error means that the name (or operator) was identified as generic
  !! and a generic definition was located, but that generic interface did
  !! not have a suitable specific.
  INTEGER, PARAMETER :: errNoMatchingSpecific = 1602

  !> A matching generic interface for an operator was not found.
  !!
  !! This error means that no generic interface was located for an identifier
  !! that had to be generic - for example one that sits behind an operator.
  INTEGER, PARAMETER :: errNoMatchingGeneric = 1603

  !> A function reference in a constant expression was not a reference to
  !! an intrinsic function.
  INTEGER, PARAMETER :: errFunctionNotConstant = 1604

  !> The part on the left hand side of a % in a data reference was not an
  !! object that had parts.
  INTEGER, PARAMETER :: errObjectHasNoParts = 1605

  !> The name of the component, binding or parameter on the right hand side
  !! of a % was not known.
  INTEGER, PARAMETER :: errPartNotKnown = 1606

  !> The value of an object was referenced in a specification or constant
  !! expression prior to the definition of the value of the object.
  INTEGER, PARAMETER :: errValueReferenceBeforeDefinition = 1607

  !> The type and type parameters of an object are being referenced in
  !! a specification or constant expression prior to the specification
  !! of the type of the object.
  INTEGER, PARAMETER :: errTypeSpecReferenceBeforeDefinition = 1608

  !> The shape of an object is being referenced in a specification
  !! expression prior to the specification of the shape of the object.
  INTEGER, PARAMETER :: errShapeReferenceBeforeDefinition = 1609

  !-----------------------------------------------------------------------------
  ! Errors that are generated during compilation proper

  !> An overflow occurred during evaluation of a constant expression.
  INTEGER, PARAMETER :: errOverflow = 2001

  !-----------------------------------------------------------------------------
  ! Errors generated in the file dependency pass.

  !> A unit specified as a root unit was not found.
  INTEGER, PARAMETER :: errRootUnitNotFound = 8001

  !> A file specified as a root file was not found.
  INTEGER, PARAMETER :: errRootFileNotFound = 8002

  !> A cyclic dependency between files exists.
  INTEGER, PARAMETER :: errCyclicFileDependency = 8003

  !> A cyclic dependency between modules exists.
  INTEGER, PARAMETER :: errCyclicModuleDependency = 8004

  !> A module is referenced prior to its definition within a source file.
  INTEGER, PARAMETER :: errReferenceBeforeDefinition = 8005

  !-----------------------------------------------------------------------------
  ! Errors that are generated at other times

  !> A compiler driver was asked to do something with a file, but it did not
  !! recognise the extension and therefore couldn't work out what to do.
  INTEGER, PARAMETER :: errUnknownFileType = 9001
  
  !> Some sort of error in a file for a serialised module.
  INTEGER, PARAMETER :: errBadModule = 9002
  
  !-----------------------------------------------------------------------------
  ! Errors specific to IL.

  !> A statement had a numeric label (in the Fortran sense).
  INTEGER, PARAMETER :: errNumericLabelNotPermitted = 9101

  !> A statement had a tag, but nothing else.
  INTEGER, PARAMETER :: errTagWithoutStatement = 9102

  !> Table type incompatible with the type of block.
  INTEGER, PARAMETER :: errIncompatibleTable = 9103

  !> A file did not start with a BLOCK statement.
  INTEGER, PARAMETER :: errNoBlockStatement = 9104

  !> A block did not start with a TABLE statement.
  INTEGER, PARAMETER :: errNoTableStatement = 9105

  !> A BLOCK or TABLE statement was tagged.
  INTEGER, PARAMETER :: errBlockOrTableTag = 9106

  !> TABLE or BLOCK missing type.
  INTEGER, PARAMETER :: errBadOrMissingType = 9107

  !> Table already specified in a particular block.
  INTEGER, PARAMETER :: errTableAlreadySpecified = 9108

  !> Too many arguments provided.
  INTEGER, PARAMETER :: errTooManyArguments = 9109

  !> Missing argument.
  INTEGER, PARAMETER :: errMissingArgument = 9110

  !> Bad table reference table type.
  INTEGER, PARAMETER :: errBadTableType = 9111

  !> Bad table reference record number.
  INTEGER, PARAMETER :: errBadRecordNumber = 9112

  !> Unknown tag specified in a reference.
  INTEGER, PARAMETER :: errBadTag = 9113

  !> Literal constant missing kind index.
  INTEGER, PARAMETER :: errNoKindIndex = 9114

  !> Literal constant with non-numeric kind.
  INTEGER, PARAMETER :: errBadKindIndex = 9115

  !> Literal constant with out of range kind index.
  INTEGER, PARAMETER :: errKindIndexOutOfRange = 9116

  !-----------------------------------------------------------------------------
  ! Errors specific to the ff08 target - link stage

  !> Referenced block not found.
  INTEGER, PARAMETER :: errUnknownBlockName = 9201

  !> Too many main programs.
  INTEGER, PARAMETER :: errMultipleMainPrograms = 9202

  !> Not enough main programs.
  INTEGER, PARAMETER :: errMissingMainProgram = 9203

  !> Got to the end of a block without RETURN instruction.
  INTEGER, PARAMETER :: errRanOffEndOfBlock = 9204
  
END MODULE ErrorCodes
