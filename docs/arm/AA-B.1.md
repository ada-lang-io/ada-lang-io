---
sidebar_position:  138
---

# B.1  Interfacing Aspects

{AI05-0229-1} An interfacing aspect is a representation aspect that is one of the aspects Import, Export, Link_Name, External_Name, or Convention.

{AI05-0229-1} {AI05-0269-1} Specifying the Import aspect  to have the value True is used to import an entity defined in a foreign language into an Ada program, thus allowing a foreign-language subprogram to be called from Ada, or a foreign-language variable to be accessed from Ada. In contrast, specifying the Export aspect to have the value True is used to export an Ada entity to a foreign language, thus allowing an Ada subprogram to be called from a foreign language, or an Ada object to be accessed from a foreign language. The Import and Export aspects are intended primarily for objects and subprograms, although implementations are allowed to support other entities. The Link_Name and External_Name aspects are used to specify the link name and external name, respectively, to be used to identify imported or exported entities in the external environment. 

Aspect Description for Import: Entity is imported from another language.

Aspect Description for Export: Entity is exported to another language.

Aspect Description for External_Name: Name used to identify an imported or exported entity.

Aspect Description for Link_Name: Linker symbol used to identify an imported or exported entity.

{AI05-0229-1} The Convention aspect is used to indicate that an Ada entity should use the conventions of another language. It is intended primarily for types and "callback" subprograms. For example, "with Convention =&gt Fortran" on the declaration of an array type Matrix implies that Matrix should be represented according to the conventions of the supported Fortran implementation, namely column-major order.

Aspect Description for Convention: Calling convention or other convention used for interfacing to other languages.

{AI12-0445-1} A [pragma](./AA-2.8#S0019) Linker_Options is used to specify the system linker parameters necessary when a given compilation unit is included in a partition.


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Linker_Options is as follows: 

Paragraphs 5 through 7 were moved to Annex J, "Obsolescent Features". 

  pragma Linker_Options(string_[expression](./AA-4.4#S0132));

A [pragma](./AA-2.8#S0019) Linker_Options is allowed only at the place of a [declarative_item](./AA-3.11#S0087).

This paragraph was deleted.{8652/0058} {AI95-00036-01} {AI05-0229-1} 


#### Name Resolution Rules

{AI05-0229-1} The Import and Export aspects are of type Boolean.

{AI05-0229-1} The Link_Name and External_Name aspects are of type String. 

Ramification: There is no language-defined support for external or link names of type Wide_String, or of other string types. Implementations may, of course, have additional aspects for that purpose. Note that allowing both String and Wide_String in the same [aspect_definition](./AA-13.1#S0348) would cause ambiguities. 

{AI05-0229-1} The expected type for the string_[expression](./AA-4.4#S0132) in pragma Linker_Options is String.


#### Legality Rules

{AI05-0229-1} The aspect Convention shall be specified by a convention_[identifier](./AA-2.3#S0002) which shall be the name of a convention. The convention names are implementation defined, except for certain language-defined ones, such as Ada and Intrinsic, as explained in 6.3.1, "Conformance Rules". [Additional convention names generally represent the calling conventions of foreign languages, language implementations, or specific run-time models.] The convention of a callable entity is its calling convention. 

Implementation defined: Implementation-defined convention names.

Discussion: We considered representing the convention names using an enumeration type declared in System. Then, convention_[identifier](./AA-2.3#S0002) would be changed to convention_[name](./AA-4.1#S0091), and we would make its expected type be the enumeration type. We didn't do this because it seems to introduce extra complexity, and because the list of available languages is better represented as the list of children of package Interfaces - a more open-ended sort of list. 

If L is a convention_[identifier](./AA-2.3#S0002) for a language, then a type T is said to be compatible with convention L, (alternatively, is said to be an L-compatible type) if any of the following conditions are met: 

T is declared in a language interface package corresponding to L and is defined to be L-compatible (see B.3, B.3.1, B.3.2, B.4, B.5),

{AI05-0229-1} Convention L has been specified for T, and T is eligible for convention L; that is: 

{AI12-0135-1} T is an enumeration type such that all internal codes (whether assigned by default or explicitly) are within an implementation-defined range that includes at least the range of values 0 .. 2**151;

T is an array type with either an unconstrained or statically-constrained first subtype, and its component type is L-compatible,

T is a record type that has no discriminants and that only has components with statically-constrained subtypes, and each component type is L-compatible,

{AI05-0002-1} T is an access-to-object type, its designated type is L-compatible, and its designated subtype is not an unconstrained array subtype,

T is an access-to-subprogram type, and its designated profile's parameter and result types are all L-compatible. 

T is derived from an L-compatible type,

{AI12-0207-1} T is an anonymous access type, and T is eligible for convention L,

Reason: We say this so that the presence of an anonymous access component does not necessarily prevent a type from being eligible for convention L. We want the anonymous access type to take the convention from the enclosing type, but if we only said that, the definition would be circular (one can only portably apply the convention L to a record type R if the components of R already have convention L; but the anonymous components of R have to take the convention from R). We include the part of about T being eligible for convention L so that we don't force convention L on some type that is incompatible with it. 

The implementation permits T as an L-compatible type.

Discussion: For example, an implementation might permit Integer as a C-compatible type, though the C type to which it corresponds might be different in different environments. 

{AI05-0229-1} If the Convention aspect is specified for a type, then the type shall either be compatible with or eligible for the specified convention. 

Ramification: {AI05-0229-1} If a type is derived from an L-compatible type, the derived type is by default L-compatible, but it is also permitted to specify the Convention aspect for the derived type.

{AI05-0229-1} It is permitted to specify the Convention aspect for an incomplete type, but in the complete declaration each component must be L-compatible.

{AI05-0229-1} If each component of a record type is L-compatible, then the record type itself is only L-compatible if it has a specified Convention. 

{AI12-0207-1} If convention L is specified for a type T, for each component of T that has an anonymous access type, the convention of the anonymous access type is L. If convention L is specified for an object that has an anonymous access type, the convention of the anonymous access type is L.

Ramification: This applies to both anonymous access-to-object and anonymous access-to-subprogram types. 

{AI05-0229-1} Notwithstanding any rule to the contrary, a declaration with a True Import aspect shall not have a completion. 

Discussion: {AI05-0229-1} For declarations of deferred constants and subprograms, we explicitly mention that no completion is allowed when aspect Import is True. For other declarations that require completions, we ignore the possibility of the aspect Import being True. Nevertheless, if an implementation chooses to allow specifying aspect Import to be True for the declaration of a task, protected type, incomplete type, private type, etc., it may do so, and the normal completion is then not allowed for that declaration. 

{AI05-0229-1}  An entity with a True Import aspect (or Export aspect) is said to be imported (respectively, exported). An entity shall not be both imported and exported.

The declaration of an imported object shall not include an explicit initialization expression. [Default initializations are not performed.] 

Proof: This follows from the "Notwithstanding ..." wording in the Dynamics Semantics paragraphs below. 

{AI05-0229-1} The type of an imported or exported object shall be compatible with the specified Convention aspect, if any. 

Ramification: This implies, for example, that importing an Integer object might be illegal, whereas importing an object of type Interfaces.C.int would be permitted. 

{AI05-0229-1} For an imported or exported subprogram, the result and parameter types shall each be compatible with the specified Convention aspect, if any.

{AI05-0229-1} The [aspect_definition](./AA-13.1#S0348) (if any) used to directly specify an Import, Export, External_Name, or Link_Name aspect shall be a static expression. The string_[expression](./AA-4.4#S0132) of a [pragma](./AA-2.8#S0019) Linker_Options shall be static. An External_Name or Link_Name aspect shall be specified only for an entity that is either imported or exported.


#### Static Semantics

Paragraphs 28 and 29 were deleted. 

{AI05-0229-1} The Convention aspect represents the calling convention or representation convention of the entity. For an access-to-subprogram type, it represents the calling convention of designated subprograms. In addition: 

A True Import aspect indicates that the entity is defined externally (that is, outside the Ada program). This aspect is never inherited; if not directly specified, the Import aspect is False.

A True Export aspect indicates that the entity is used externally. This aspect is never inherited; if not directly specified, the Export aspect is False.

For an entity with a True Import or Export aspect, an external name, link name, or both may also be specified. 

An external name is a string value for the name used by a foreign language program either for an entity that an Ada program imports, or for referring to an entity that an Ada program exports.

A link name is a string value for the name of an exported or imported entity, based on the conventions of the foreign language's compiler in interfacing with the system's linker tool.

The meaning of link names is implementation defined. If neither a link name nor the Address attribute of an imported or exported entity is specified, then a link name is chosen in an implementation-defined manner, based on the external name if one is specified. 

Implementation defined: The meaning of link names.

Ramification: For example, an implementation might always prepend "_", and then pass it to the system linker. 

Implementation defined: The manner of choosing link names when neither the link name nor the address of an imported or exported entity is specified.

Ramification: Normally, this will be the entity's defining name, or some simple transformation thereof. 

Pragma Linker_Options has the effect of passing its string argument as a parameter to the system linker (if one exists), if the immediately enclosing compilation unit is included in the partition being linked. The interpretation of the string argument, and the way in which the string arguments from multiple Linker_Options pragmas are combined, is implementation defined. 

Implementation defined: The effect of pragma Linker_Options.


#### Dynamic Semantics

{AI05-0229-1} Notwithstanding what this document says elsewhere, the elaboration of a declaration with a True Import aspect does not create the entity. Such an elaboration has no other effect than to allow the defining name to denote the external entity. 

Ramification: This implies that default initializations are skipped. (Explicit initializations are illegal.) For example, an imported access object is not initialized to null.

This paragraph was deleted.{AI05-0229-1} 

Discussion: {AI05-0229-1} This "notwithstanding" wording is better than saying "unless aspect Import is True" on every definition of elaboration. It says we recognize the contradiction, and this rule takes precedence. 


#### Erroneous Execution

{AI95-00320-01} {AI05-0229-1} {AI12-0219-1} It is the programmer's responsibility to ensure that the use of interfacing aspects does not violate Ada semantics; otherwise, program execution is erroneous. For example, passing an object with mode in to imported code that modifies it causes erroneous execution. Similarly, calling an imported subprogram that is not pure from a pure package causes erroneous execution. 


#### Implementation Advice

{AI05-0229-1} If an implementation supports Export for a given language, then it should also allow the main subprogram to be written in that language. It should support some mechanism for invoking the elaboration of the Ada library units included in the system, and for invoking the finalization of the environment task. On typical systems, the recommended mechanism is to provide two subprograms whose link names are "adainit" and "adafinal". Adainit should contain the elaboration code for library units. Adafinal should contain the finalization code. These subprograms should have no effect the second and subsequent time they are called. 

Implementation Advice: If  Export is supported for a language, the main program should be able to be written in that language. Subprograms named "adainit" and "adafinal" should be provided for elaboration and finalization of the environment task.

Ramification: For example, if the main subprogram is written in C, it can call adainit before the first call to an Ada subprogram, and adafinal after the last. 

{AI05-0229-1} {AI05-0269-1} Automatic elaboration of preelaborated packages should be provided when specifying the Export aspect as True is supported. 

Implementation Advice: Automatic elaboration of preelaborated packages should be provided when specifying the Export aspect as True is supported.

{AI05-0229-1} {AI12-0135-1} {AI12-0444-1} For each supported convention L other than Intrinsic, an implementation should support specifying the Import and Export aspects for objects of L-compatible types and for subprograms, and the Convention aspect for L-eligible types and for subprograms, presuming the other language has corresponding features. Specifying the  Convention aspect should be supported for enumeration types whose internal codes fall within the range 0 .. 2**151, but no recommendation is made for other scalar types. 

Implementation Advice: For each supported convention L other than Intrinsic, specifying the aspects Import and Export should be supported for objects of L-compatible types and for subprograms, and aspect Convention should be supported for L-eligible types and for subprograms.

Reason: {AI05-0229-1} Specifying aspect Convention is not necessary for scalar types, since the language interface packages declare scalar types corresponding to those provided by the respective foreign languages. 

Implementation Note: {AI95-00114-01} If an implementation supports interfacing to the C++ entities not supported by B.3, it should do so via the convention identifier C_Plus_Plus (in additional to any C++-implementation-specific ones). 

Reason: {AI95-00114-01} The reason for giving the advice about C++ is to encourage uniformity among implementations, given that the name of the language is not syntactically legal as an [identifier](./AA-2.3#S0002). 

NOTE 1   {AI05-0229-1} {AI12-0440-1} Implementations can place restrictions on interfacing aspects; for example, requiring each exported entity to be declared at the library level. 

Proof: Arbitrary restrictions are allowed by 13.1. 

Ramification: Such a restriction might be to disallow them altogether. Alternatively, the implementation might allow them only for certain kinds of entities, or only for certain conventions. 

NOTE 2   {AI05-0229-1} The Convention aspect in combination with the Import aspect indicates the conventions for accessing external entities. It is possible that the actual entity is written in assembly language, but reflects the conventions of a particular language. For example, with Convention =&gt Ada can be used to interface to an assembly language routine that obeys the Ada compiler's calling conventions.

NOTE 3   {AI05-0229-1} {AI12-0440-1} To obtain "call-back" to an Ada subprogram from a foreign language environment, the Convention aspect can be specified both for the access-to-subprogram type and the specific subprogram(s) to which 'Access is applied.

Paragraphs 45 and 46 were deleted. 

NOTE 4   See also 13.8, "Machine Code Insertions". 

Ramification: {AI05-0229-1} The Intrinsic convention (see 6.3.1) implies that the entity is somehow "built in" to the implementation. Thus, it generally does not make sense for users to specify Intrinsic along with specifying that the entity is imported. The intention is that only implementations will specify Intrinsic for an imported entity. The language also defines certain subprograms to be Intrinsic. 

Discussion: {AI05-0229-1} There are many imaginable interfacing aspects that don't make any sense. For example, setting the Convention of a protected procedure to Ada is probably wrong. Rather than enumerating all such cases, however, we leave it up to implementations to decide what is sensible. 

NOTE 5   {AI05-0229-1} If both External_Name and Link_Name are specified for a given entity, then the External_Name is ignored.

This paragraph was deleted.{AI95-00320-01} 


#### Examples

{AI12-0080-1} Example of interfacing aspects: 

```ada
{AI05-0229-1} {AI05-0269-1} package Fortran_Library is
  function Sqrt (X : Float) return Float
    with Import =&gt True, Convention =&gt Fortran;
  type Matrix is array (Natural range &lt&gt, Natural range &lt&gt) of Float
    with Convention =&gt Fortran;
  function Invert (M : Matrix) return Matrix
    with Import =&gt True, Convention =&gt Fortran;
end Fortran_Library;

```


#### Extensions to Ada 83

Interfacing pragmas are new to Ada 95. Pragma Import replaces Ada 83's pragma Interface. Existing implementations can continue to support pragma Interface for upward compatibility. 


#### Wording Changes from Ada 95

{8652/0058} {AI95-00036-01} Corrigendum: Clarified that [pragma](./AA-2.8#S0019)s Import and Export work like a subprogram call; parameters cannot be omitted unless named notation is used. (Reordering is still not permitted, however.)

{AI95-00320-01} Added wording to say all bets are off if foreign code doesn't follow the semantics promised by the Ada specifications. 


#### Incompatibilities With Ada 2005

{AI05-0002-1} Correction: Access types that designate unconstrained arrays are no longer defined to be L-compatible. Such access-to-arrays require bounds information, which is likely to be incompatible with a foreign language. The change will allow (but not require) compilers to reject bad uses, which probably will not work anyway. Note that implementations can still support any type that it wants as L-compatible; such uses will not be portable, however. As such, there should be little existing code that will be impacted (compilers probably already rejected cases that could not be translated, whether or not the language allowed doing so formally). 


#### Extensions to Ada 2005

{AI05-0229-1} Aspects Convention, Import, Export, Link_Name, and External_Name are new; [pragma](./AA-2.8#S0019)s Convention, Import, and Export are now obsolescent. 


#### Incompatibilities With Ada 2012

{AI12-0207-1} Correction: The convention of anonymous access components is that of the enclosing type (in Ada 2012, it was Ada). Similarly, the convention of the anonymous access type of an object is that of the object (again, in Ada 2012 it was Ada). While this is formally incompatible, it should be more useful in portable code; it makes little sense to have a component of an Ada access type in a record with a C convention. For most implementations, this will have no real effect as convention Ada anonymous access types were allowed as C-compatible anyway. But such code was not portable, as this was not required in Ada 2012. 


#### Extensions to Ada 2012

{AI12-0135-1} Corrigendum: Added a suggestion that convention be supported for enumeration types. This will make the use of enumeration types portable for implementations that support interfacing to a particular language. 


#### Wording Changes from Ada 2012

{AI12-0219-1} Correction: Added some examples to the erroneous execution text; this is a very important rule as it means that Ada compilers can assume that provided interfacing declarations reflect the actual foreign code. 

