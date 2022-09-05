---
sidebar_position:  205
---

# J.15  Aspect-related Pragmas

{AI05-0229-1} Pragmas can be used as an alternative to aspect_specifications to specify certain aspects. 


#### Name Resolution Rules

{AI12-0417-1} Certain [pragma](./AA-2.8#S0019)s are defined to be program unit pragmas. A [name](./AA-4.1#S0091) given as the argument of a program unit pragma shall resolve to denote the declarations or renamings of one or more program units that occur immediately within the declarative region or [compilation](./AA-10.1#S0285) in which the [pragma](./AA-2.8#S0019) immediately occurs, or it shall resolve to denote the declaration of the immediately enclosing program unit (if any); the [pragma](./AA-2.8#S0019) applies to the denoted program unit(s). If there are no [name](./AA-4.1#S0091)s given as arguments, the [pragma](./AA-2.8#S0019) applies to the immediately enclosing program unit. 

Ramification: The fact that this is a Name Resolution Rule means that the [pragma](./AA-2.8#S0019) will not apply to declarations from outer declarative regions. 


#### Legality Rules

{AI12-0417-1} A program unit pragma shall appear in one of these places: 

{AI12-0417-1} At the place of a [compilation_unit](./AA-10.1#S0286), in which case the [pragma](./AA-2.8#S0019) shall immediately follow in the same [compilation](./AA-10.1#S0285) (except for other [pragma](./AA-2.8#S0019)s) a [library_unit_declaration](./AA-10.1#S0288) that is a [subprogram_declaration](./AA-6.1#S0195), [generic_subprogram_declaration](./AA-12.1#S0311), or [generic_instantiation](./AA-12.3#S0315), and the [pragma](./AA-2.8#S0019) shall have an argument that is a [name](./AA-4.1#S0091) denoting that declaration. 

Ramification: The [name](./AA-4.1#S0091) has to denote the immediately preceding [library_unit_declaration](./AA-10.1#S0288). 

{8652/0033} {AI95-00136-01} {AI12-0417-1} Immediately within the visible part of a program unit and before any nested declaration (but not within a generic formal part), in which case the argument, if any, shall be a [direct_name](./AA-4.1#S0092) that denotes the immediately enclosing program unit declaration. 

Ramification: The argument is optional in this case. 

{AI12-0417-1} At the place of a declaration other than the first, of a [declarative_part](./AA-3.11#S0086) or program unit declaration, in which case the [pragma](./AA-2.8#S0019) shall have an argument, which shall be a [direct_name](./AA-4.1#S0092) that denotes one or more of the following (and nothing else): a [subprogram_declaration](./AA-6.1#S0195), a [generic_subprogram_declaration](./AA-12.1#S0311), or a [generic_instantiation](./AA-12.3#S0315), of the same [declarative_part](./AA-3.11#S0086) or program unit declaration. 

Ramification: If you want to denote a [subprogram_body](./AA-6.3#S0216) that is not a completion, or a [package_declaration](./AA-7.1#S0229), for example, you have to put the [pragma](./AA-2.8#S0019) inside. 

{AI05-0132-1} {AI12-0417-1} Certain program unit pragmas are defined to be library unit pragmas. If a library unit pragma applies to a program unit, the program unit shall be a library unit. 

Ramification: This, together with the rules for program unit pragmas above, implies that if a library unit pragma applies to a [subprogram_declaration](./AA-6.1#S0195) (and similar things), it has to appear immediately after the [compilation_unit](./AA-10.1#S0286), whereas if the [pragma](./AA-2.8#S0019) applies to a [package_declaration](./AA-7.1#S0229), a [subprogram_body](./AA-6.3#S0216) that is not a completion (and similar things), it has to appear inside, as the first [declarative_item](./AA-3.11#S0087). 


#### Static Semantics

{8652/0034} {AI95-00041-01} {AI12-0417-1} A library unit pragma that applies to a generic unit does not apply to its instances, unless a specific rule for the pragma specifies the contrary. 


#### Implementation Advice

{8652/0034} {AI95-00041-01} {AI12-0417-1} When applied to a generic unit, a program unit pragma that is not a library unit pragma should apply to each instance of the generic unit for which there is not an overriding pragma applied directly to the instance. 

Implementation Advice: When applied to a generic unit, a program unit pragma that is not a library unit pragma should apply to each instance of the generic unit for which there is not an overriding pragma applied directly to the instance.


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Many existing pragmas have been converted into aspects; the pragmas have moved here.

{AI05-0132-1} {AI12-0417-1} Correction: A library unit pragma must apply directly to a library unit, even if no name is given in the pragma. 


#### Wording Changes from Ada 2012

{AI12-0417-1} The terms "program unit pragma" and "library unit pragma" were moved here as all of the [pragma](./AA-2.8#S0019)s that use these terms are now in this annex. 


## J.15.1  Pragma Inline


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Inline, which is a program unit pragma (see 10.1.5), is as follows: 

  pragma Inline ([name](./AA-4.1#S0091){, [name](./AA-4.1#S0091)}); 


#### Legality Rules

{AI05-0229-1} The [pragma](./AA-2.8#S0019) shall apply to one or more callable entities or generic subprograms. 


#### Static Semantics

{AI05-0229-1} [Pragma](./AA-2.8#S0019) Inline specifies that the Inline aspect (see 6.3.2) for each entity denoted by each [name](./AA-4.1#S0091) given in the [pragma](./AA-2.8#S0019) has the value True.

Ramification: Note that inline expansion is desired no matter what [name](./AA-4.1#S0091) is used in the call. This allows one to request inlining for only one of several overloaded subprograms as follows:

```ada
package IO is
   procedure Put(X : in Integer);
   procedure Put(X : in String);
   procedure Put(X : in Character);
private
   procedure Character_Put(X : in Character) renames Put;
   pragma Inline(Character_Put);
end IO;

```

```ada
with IO; use IO;
procedure Main is
   I : Integer;
   C : Character;
begin
   ...
   Put(C); -- Inline expansion is desired.
   Put(I); -- Inline expansion is NOT desired.
end Main;

```


#### Implementation Permissions

{AI05-0229-1} An implementation may allow a [pragma](./AA-2.8#S0019) Inline that has an argument which is a [direct_name](./AA-4.1#S0092) denoting a [subprogram_body](./AA-6.3#S0216) of the same [declarative_part](./AA-3.11#S0086).

Reason: This is allowed for Ada 83 compatibility. This is only a permission as this usage was considered obsolescent even for Ada 95. 

Discussion: We only need to allow this in [declarative_part](./AA-3.11#S0086)s, because a [body](./AA-3.11#S0089) is only allowed in another [body](./AA-3.11#S0089), and these all have [declarative_part](./AA-3.11#S0086)s. 

NOTE 1   {AI05-0229-1} {AI12-0440-1} The name in a [pragma](./AA-2.8#S0019) Inline can denote more than one entity in the case of overloading. Such a [pragma](./AA-2.8#S0019) applies to all of the denoted entities. 


#### Incompatibilities With Ada 83

{AI95-00309-01} {AI05-0229-1} A pragma Inline cannot refer to a [subprogram_body](./AA-6.3#S0216) outside of that body. The pragma can be given inside of the subprogram body. Ada 2005 adds an Implementation Permission to allow this usage for compatibility (and Ada 95 implementations also can use this permission), but implementations do not have to allow such [pragma](./AA-2.8#S0019)s. 


#### Extensions to Ada 83

{AI05-0229-1} A [pragma](./AA-2.8#S0019) Inline is allowed inside a [subprogram_body](./AA-6.3#S0216) if there is no corresponding [subprogram_declaration](./AA-6.1#S0195). This is for uniformity with other program unit pragmas. 


#### Extensions to Ada 95

{AI95-00309-01} {AI05-0229-1} Amendment Correction: Implementations are allowed to let [Pragma](./AA-2.8#S0019) Inline apply to a [subprogram_body](./AA-6.3#S0216). 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragma Inline was moved here from 6.3.2; aspect Inline lives there now. 


## J.15.2  Pragma No_Return


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) No_Return, which is a representation pragma (see 13.1), is as follows: 

{AI12-0269-1}   pragma No_Return (subprogram_[local_name](./AA-13.1#S0345){, subprogram_[local_name](./AA-13.1#S0345)}); 


#### Legality Rules

{AI05-0229-1} {AI12-0269-1} Each subprogram_[local_name](./AA-13.1#S0345) shall denote one or more subprograms or generic subprograms. [The subprogram_[local_name](./AA-13.1#S0345) shall not denote a null procedure nor an instance of a generic unit.] 


#### Static Semantics

{AI05-0229-1} {AI12-0269-1} [Pragma](./AA-2.8#S0019) No_Return specifies that the No_Return aspect (see 6.5.1) for each subprogram denoted by each [local_name](./AA-13.1#S0345) given in the [pragma](./AA-2.8#S0019) has the value True. 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragma No_Return was moved here from 6.5.1; aspect No_Return lives there now. 


#### Wording Changes from Ada 2012

{AI12-0269-1} This subclause is new. Pragma No_Return was moved here from 6.5.1; aspect No_Return lives there now. 


## J.15.3  Pragma Pack


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Pack, which is a representation pragma (see 13.1), is as follows: 

  pragma Pack (first_subtype_[local_name](./AA-13.1#S0345)); 


#### Legality Rules

{AI05-0229-1} The first_subtype_[local_name](./AA-13.1#S0345) of a [pragma](./AA-2.8#S0019) Pack shall denote a composite subtype. 


#### Static Semantics

{AI05-0229-1} [Pragma](./AA-2.8#S0019) Pack specifies that the Pack aspect (see 13.2) for the type denoted by first_subtype_[local_name](./AA-13.1#S0345) has the value True. 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragma Pack was moved here from 13.2; aspect Pack lives there now. 


## J.15.4  Pragma Storage_Size


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Storage_Size is as follows: 

  pragma Storage_Size ([expression](./AA-4.4#S0132));

{AI05-0229-1} A [pragma](./AA-2.8#S0019) Storage_Size is allowed only immediately within a [task_definition](./AA-9.1#S0246). 


#### Name Resolution Rules

{AI05-0229-1} The [expression](./AA-4.4#S0132) of a [pragma](./AA-2.8#S0019) Storage_Size is expected to be of any integer type. 


#### Static Semantics

{AI05-0229-1} The [pragma](./AA-2.8#S0019) Storage_Size sets the Storage_Size aspect (see 13.3) of the type defined by the immediately enclosing [task_definition](./AA-9.1#S0246) to the value of the [expression](./AA-4.4#S0132) of the [pragma](./AA-2.8#S0019). 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragma Storage_Size was moved here from 13.3; aspect Storage_Size lives there now. 


## J.15.5  Interfacing Pragmas


#### Syntax

{AI05-0229-1} An interfacing pragma is a representation pragma that is one of the [pragma](./AA-2.8#S0019)s Import, Export, or Convention. Their forms are as follows: 

  pragma Import(
     [Convention =&gt] convention_[identifier](./AA-2.3#S0002), [Entity =&gt] [local_name](./AA-13.1#S0345)
  [, [External_Name =&gt] external_name_string_[expression](./AA-4.4#S0132)]
  [, [Link_Name =&gt] link_name_string_[expression](./AA-4.4#S0132)]);

  pragma Export(
     [Convention =&gt] convention_[identifier](./AA-2.3#S0002), [Entity =&gt] [local_name](./AA-13.1#S0345)
  [, [External_Name =&gt] external_name_string_[expression](./AA-4.4#S0132)]
  [, [Link_Name =&gt] link_name_string_[expression](./AA-4.4#S0132)]);

  pragma Convention([Convention =&gt] convention_[identifier](./AA-2.3#S0002),[Entity =&gt] [local_name](./AA-13.1#S0345));

{AI05-0229-1} For [pragma](./AA-2.8#S0019)s Import and Export, the argument for Link_Name shall not be given without the pragma_argument_[identifier](./AA-2.3#S0002) unless the argument for External_Name is given. 


#### Name Resolution Rules

{AI05-0229-1}  The expected type for an external_name_string_[expression](./AA-4.4#S0132) and a link_name_string_[expression](./AA-4.4#S0132) in an interfacing pragma is String. 


#### Legality Rules

{AI05-0229-1} The convention_[identifier](./AA-2.3#S0002) of an interfacing pragma shall be the name of a convention (see B.1).

{AI05-0229-1} A [pragma](./AA-2.8#S0019) Import shall be the completion of a declaration. Notwithstanding any rule to the contrary, a [pragma](./AA-2.8#S0019) Import may serve as the completion of any kind of (explicit) declaration if supported by an implementation for that kind of declaration. If a completion is a [pragma](./AA-2.8#S0019) Import, then it shall appear in the same [declarative_part](./AA-3.11#S0086), [package_specification](./AA-7.1#S0230), [task_definition](./AA-9.1#S0246), or [protected_definition](./AA-9.4#S0251) as the declaration. For a library unit, it shall appear in the same [compilation](./AA-10.1#S0285), before any subsequent [compilation_unit](./AA-10.1#S0286)s other than [pragma](./AA-2.8#S0019)s. If the [local_name](./AA-13.1#S0345) denotes more than one entity, then the [pragma](./AA-2.8#S0019) Import is the completion of all of them.

{AI05-0229-1} The external_name_string_[expression](./AA-4.4#S0132) and link_name_string_[expression](./AA-4.4#S0132) of a [pragma](./AA-2.8#S0019) Import or Export shall be static.

{AI05-0229-1} The [local_name](./AA-13.1#S0345) of each of these pragmas shall denote a declaration that may have the similarly named aspect specified. 


#### Static Semantics

{AI05-0229-1} An interfacing pragma specifies various aspects of the entity denoted by the [local_name](./AA-13.1#S0345) as follows: 

The Convention aspect (see B.1) is convention_[identifier](./AA-2.3#S0002).

A [pragma](./AA-2.8#S0019) Import specifies that the Import aspect (see B.1) is True.

A [pragma](./AA-2.8#S0019) Export specifies that the Export aspect (see B.1) is True.

For both [pragma](./AA-2.8#S0019) Import and Export, if an external name is given in the pragma, the External_Name aspect (see B.1) is specified to be external_name_string_[expression](./AA-4.4#S0132). If a link name is given in the pragma, the Link_Name aspect (see B.1) is specified to be the link_name_string_[expression](./AA-4.4#S0132). 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragmas Import, Export, and Convention were moved here from B.1; aspects Import, Export, Convention, Link_Name, and External_Name live there now. 


## J.15.6  Pragma Unchecked_Union


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Unchecked_Union, which is a representation pragma (see 13.1), is as follows: 

  pragma Unchecked_Union (first_subtype_[local_name](./AA-13.1#S0345)); 


#### Legality Rules

{AI05-0229-1} The first_subtype_[local_name](./AA-13.1#S0345) of a [pragma](./AA-2.8#S0019) Unchecked_Union shall denote an unconstrained discriminated record subtype having a [variant_part](./AA-3.8#S0071). 


#### Static Semantics

{AI05-0229-1} A [pragma](./AA-2.8#S0019) Unchecked_Union specifies that the Unchecked_Union aspect (see B.3.3) for the type denoted by first_subtype_[local_name](./AA-13.1#S0345) has the value True. 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragma Unchecked_Union was moved here from B.3.3; aspect Unchecked_Union lives there now. 


## J.15.7  Pragmas Interrupt_Handler and Attach_Handler


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Interrupt_Handler is as follows: 

  pragma Interrupt_Handler (handler_[name](./AA-4.1#S0091));

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Attach_Handler is as follows: 

  pragma Attach_Handler (handler_[name](./AA-4.1#S0091), [expression](./AA-4.4#S0132)); 


#### Name Resolution Rules

{AI05-0229-1} For the Interrupt_Handler and Attach_Handler pragmas, the handler_[name](./AA-4.1#S0091) shall resolve to denote a protected procedure with a parameterless profile.

{AI05-0229-1} For the Attach_Handler pragma, the expected type for the expression is Interrupts.Interrupt_Id (see C.3.2). 


#### Legality Rules

{AI05-0033-1} {AI05-0229-1} The Attach_Handler and Interrupt_Handler pragmas are only allowed immediately within the [protected_definition](./AA-9.4#S0251) where the corresponding subprogram is declared. The corresponding [protected_type_declaration](./AA-9.4#S0249) or [single_protected_declaration](./AA-9.4#S0250) shall be a library-level declaration, and shall not be declared within a generic body. In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit. 

Discussion: In the case of a [protected_type_declaration](./AA-9.4#S0249), an [object_declaration](./AA-3.3#S0032) of an object of that type need not be at library level.

{AI05-0033-1} We cannot allow these pragmas in a generic body, because legality rules are not checked for instance bodies, and these should not be allowed if the instance is not at the library level. The protected types can be declared in the private part if this is desired. Note that while the 'Access to use the handler would provide the check in the case of Interrupt_Handler, there is no other check for Attach_Handler. Since these pragmas are so similar, we want the rules to be the same. 


#### Static Semantics

{AI05-0229-1} For an implementation that supports Annex C, a pragma Interrupt_Handler specifies the Interrupt_Handler aspect (see C.3.1) for the protected procedure handler_[name](./AA-4.1#S0091) to have the value True. For an implementation that supports Annex C, a pragma Attach_Handler specifies the Attach_Handler aspect (see C.3.1) for the protected procedure handler_[name](./AA-4.1#S0091) to have the value of the given [expression](./AA-4.4#S0132)[ as evaluated at object creation time]. 


#### Incompatibilities With Ada 2005

{AI05-0033-1} Correction: Added missing generic contract wording for the pragma Attach_Handler and Interrupt_Handler. This means that nested instances with these pragmas in the private part are now illegal. This is not likely to occur in practice. 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragmas Interrupt_Handler and Attach_Handler were moved here from C.3.1; aspects Interrupt_Handler and Attach_Handler live there now. 


## J.15.8  Shared Variable Pragmas


#### Syntax

{AI05-0229-1} {AI12-0425-1} The following [pragma](./AA-2.8#S0019)s are defined with the given forms: 

  pragma Atomic ([local_name](./AA-13.1#S0345));

  pragma Volatile ([local_name](./AA-13.1#S0345));

{AI05-0009-1}   pragma Independent (component_[local_name](./AA-13.1#S0345));

  pragma Atomic_Components (array_[local_name](./AA-13.1#S0345));

  pragma Volatile_Components (array_[local_name](./AA-13.1#S0345));

{AI05-0009-1}   pragma Independent_Components ([local_name](./AA-13.1#S0345));

Discussion: {AI05-0009-1} {AI05-0229-1} Pragmas Independent and Independent_Components are born obsolescent; they are defined to provide consistency with the existing shared variable pragmas. As with all obsolescent features, these pragmas are not optional; all Ada implementations need to implement them. Also note that these pragmas were defined as a Correction; as such, they are expected to be implemented as part of Ada 2005 implementations (and they would not be obsolescent there). 


#### Name Resolution Rules

{AI05-0009-1} {AI05-0229-1} The [local_name](./AA-13.1#S0345) in an Atomic or Volatile pragma shall resolve to denote either an [object_declaration](./AA-3.3#S0032), a noninherited [component_declaration](./AA-3.8#S0070), or a [full_type_declaration](./AA-3.2#S0024). The component_[local_name](./AA-13.1#S0345) in an Independent pragma shall resolve to denote a noninherited [component_declaration](./AA-3.8#S0070). The array_[local_name](./AA-13.1#S0345) in an Atomic_Components or Volatile_Components pragma shall resolve to denote the declaration of an array type or an array object of an anonymous type. The [local_name](./AA-13.1#S0345) in an Independent_Components pragma shall resolve to denote the declaration of an array or record type or an array object of an anonymous type. 


#### Static Semantics

{AI05-0229-1} These [pragma](./AA-2.8#S0019)s are representation pragmas (see 13.1). Each of these [pragma](./AA-2.8#S0019)s specifies that the similarly named aspect (see C.6) of the type, object, or component denoted by its argument is True. 


#### Legality Rules

{AI05-0229-1} The [local_name](./AA-13.1#S0345) of each of these [pragma](./AA-2.8#S0019)s shall denote a declaration that may have the similarly named aspect specified. 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. These pragmas were moved here from C.6; various aspects live there now. 


## J.15.9  Pragma CPU

Discussion: {AI05-0229-1} This pragma is born obsolescent; it is defined to provide consistency with existing real-time pragmas. As with all obsolescent features, this pragma is not optional; all Ada implementations need to implement it. 


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) CPU is as follows: 

  pragma CPU ([expression](./AA-4.4#S0132));


#### Name Resolution Rules

{AI05-0229-1} The expected type for the [expression](./AA-4.4#S0132) of a [pragma](./AA-2.8#S0019) CPU is System.Multiprocessors.CPU_Range. 


#### Legality Rules

{AI05-0229-1} {AI12-0281-1} A CPU pragma is allowed only immediately within a [task_definition](./AA-9.1#S0246), [protected_definition](./AA-9.4#S0251), or the [declarative_part](./AA-3.11#S0086) of a [subprogram_body](./AA-6.3#S0216).

{AI05-0229-1} For a CPU pragma that appears in the [declarative_part](./AA-3.11#S0086) of a [subprogram_body](./AA-6.3#S0216), the [expression](./AA-4.4#S0132) shall be static. 


#### Static Semantics

{AI05-0229-1} {AI12-0281-1} For an implementation that supports Annex D, a [pragma](./AA-2.8#S0019) CPU specifies the value of the CPU aspect (see D.16). If the [pragma](./AA-2.8#S0019) appears in a [task_definition](./AA-9.1#S0246), the [expression](./AA-4.4#S0132) is associated with the aspect for the task type or [single_task_declaration](./AA-9.1#S0245) that contains the [pragma](./AA-2.8#S0019). If the pragma appears in a [protected_definition](./AA-9.4#S0251), the [expression](./AA-4.4#S0132) is associated with the aspect for the protected type or [single_protected_declaration](./AA-9.4#S0250) that contains the [pragma](./AA-2.8#S0019). Otherwise, the [expression](./AA-4.4#S0132) is associated with the aspect for the subprogram that contains the [pragma](./AA-2.8#S0019). 


#### Extensions to Ada 2005

{AI05-0009-1} Pragma CPU is new. 


## J.15.10  Pragma Dispatching_Domain

Discussion: {AI05-0167-1} This pragma is born obsolescent; it is defined to provide consistency with existing real-time pragmas. As with all obsolescent features, this pragma is not optional; all Ada implementations need to implement it. 


#### Syntax

{AI05-0167-1} The form of a [pragma](./AA-2.8#S0019) Dispatching_Domain is as follows: 

  pragma Dispatching_Domain ([expression](./AA-4.4#S0132));


#### Name Resolution Rules

{AI05-0167-1} The expected type for the [expression](./AA-4.4#S0132) is System.Multiprocessors.Dispatching_Domains.Dispatching_Domain. 


#### Legality Rules

{AI05-0167-1} A Dispatching_Domain pragma is allowed only immediately within a [task_definition](./AA-9.1#S0246). 


#### Static Semantics

{AI05-0167-1} For an implementation that supports Annex D, a pragma Dispatching_Domain specifies the value of the Dispatching_Domain aspect (see D.16.1). The [expression](./AA-4.4#S0132) is associated with the aspect for the task type or [single_task_declaration](./AA-9.1#S0245) that contains the pragma. 


#### Extensions to Ada 2005

{AI05-0009-1} Pragma Dispatching_Domain is new. 


## J.15.11  Pragmas Priority and Interrupt_Priority


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Priority is as follows: 

  pragma Priority ([expression](./AA-4.4#S0132));

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Interrupt_Priority is as follows: 

  pragma Interrupt_Priority [([expression](./AA-4.4#S0132));]


#### Name Resolution Rules

{AI05-0229-1} The expected type for the [expression](./AA-4.4#S0132) in a Priority or Interrupt_Priority pragma is Integer. 


#### Legality Rules

{AI05-0229-1} A Priority pragma is allowed only immediately within a [task_definition](./AA-9.1#S0246), a [protected_definition](./AA-9.4#S0251), or the [declarative_part](./AA-3.11#S0086) of a [subprogram_body](./AA-6.3#S0216). An Interrupt_Priority pragma is allowed only immediately within a [task_definition](./AA-9.1#S0246) or a [protected_definition](./AA-9.4#S0251).

{AI05-0229-1} For a Priority pragma that appears in the [declarative_part](./AA-3.11#S0086) of a [subprogram_body](./AA-6.3#S0216), the [expression](./AA-4.4#S0132) shall be static, and its value shall be in the range of System.Priority. 


#### Static Semantics

{AI05-0229-1} For an implementation that supports Annex D, a [pragma](./AA-2.8#S0019) Priority specifies the value of the Priority aspect (see D.1) and a [pragma](./AA-2.8#S0019) Interrupt_Priority specifies the value of the Interrupt_Priority aspect as follows:

If the [pragma](./AA-2.8#S0019) appears in a [task_definition](./AA-9.1#S0246), the [expression](./AA-4.4#S0132) is associated with the aspect for the task type or [single_task_declaration](./AA-9.1#S0245) that contains the [pragma](./AA-2.8#S0019);

If the [pragma](./AA-2.8#S0019) appears in a [protected_definition](./AA-9.4#S0251), the [expression](./AA-4.4#S0132) is associated with the aspect for the protected type or [single_protected_declaration](./AA-9.4#S0250) that contains the [pragma](./AA-2.8#S0019);

If the [pragma](./AA-2.8#S0019) appears in the [declarative_part](./AA-3.11#S0086) of a [subprogram_body](./AA-6.3#S0216), the [expression](./AA-4.4#S0132) is associated with the aspect for the subprogram that contains the [pragma](./AA-2.8#S0019).

{AI05-0229-1} If there is no [expression](./AA-4.4#S0132) in an Interrupt_Priority pragma, the Interrupt_Priority aspect has the value Interrupt_Priority'Last. 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragmas Interrupt_Priority and Priority were moved here from D.1; aspects Interrupt_Priority and Priority live there now. 


## J.15.12  Pragma Relative_Deadline


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Relative_Deadline is as follows: 

  pragma Relative_Deadline (relative_deadline_[expression](./AA-4.4#S0132));


#### Name Resolution Rules

{AI05-0229-1} The expected type for a relative_deadline_[expression](./AA-4.4#S0132) is Real_Time.Time_Span. 


#### Legality Rules

{AI05-0229-1} A Relative_Deadline pragma is allowed only immediately within a [task_definition](./AA-9.1#S0246) or the [declarative_part](./AA-3.11#S0086) of a [subprogram_body](./AA-6.3#S0216). 


#### Static Semantics

{AI05-0229-1} For an implementation that supports Annex D, a [pragma](./AA-2.8#S0019) Relative_Deadline specifies the value of the Relative_Deadline aspect (see D.2.6). If the [pragma](./AA-2.8#S0019) appears in a [task_definition](./AA-9.1#S0246), the [expression](./AA-4.4#S0132) is associated with the aspect for the task type or [single_task_declaration](./AA-9.1#S0245) that contains the [pragma](./AA-2.8#S0019); otherwise, the [expression](./AA-4.4#S0132) is associated with the aspect for the subprogram that contains the [pragma](./AA-2.8#S0019). 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragma Relative_Deadline was moved here from D.2.6; aspect Relative_Deadline lives there now. 


## J.15.13  Pragma Asynchronous


#### Syntax

{AI05-0229-1} The form of a [pragma](./AA-2.8#S0019) Asynchronous, which is a representation pragma (see 13.1), is as follows: 

  pragma Asynchronous ([local_name](./AA-13.1#S0345)); 


#### Static Semantics

{AI05-0229-1} For an implementation that supports Annex E, a pragma Asynchronous specifies that the Asynchronous aspect (see E.4.1) for the procedure or type denoted by [local_name](./AA-13.1#S0345) has the value True. 


#### Legality Rules

{AI05-0229-1} The [local_name](./AA-13.1#S0345) of a pragma Asynchronous shall denote a declaration that may have aspect Asynchronous specified. 


#### Wording Changes from Ada 2005

{AI05-0229-1} {AI05-0299-1} This subclause is new. Pragma Asynchronous was moved here from E.4.1; aspect Asynchronous lives there now. 


## J.15.14  Elaboration Control Pragmas

{AI12-0409-1} This subclause defines pragmas that specify aspects that help control the elaboration order of [library_item](./AA-10.1#S0287)s.

Discussion: {AI12-0417-1} Pragmas that do not have associated aspects still appear in 10.2.1. These pragmas are presented in the order that the aspects are defined in 10.2.1. 


#### Syntax

{AI12-0417-1} The following [pragma](./AA-2.8#S0019)s are defined with the given forms: 

  pragma Preelaborate[(library_unit_[name](./AA-4.1#S0091))];

  pragma Preelaborable_Initialization([direct_name](./AA-4.1#S0092));

  pragma Pure[(library_unit_[name](./AA-4.1#S0091))];

  pragma Elaborate_Body[(library_unit_[name](./AA-4.1#S0091))];

{AI12-0417-1} [Pragma](./AA-2.8#S0019)s Preelaborate, Pure, and Elaborate_Body are library unit pragmas. 

Ramification: [Pragma](./AA-2.8#S0019) Preelaborable_Initialization is not a library unit pragma. 


#### Static Semantics

{AI12-0417-1} A [pragma](./AA-2.8#S0019) Preelaborate specifies that a library unit is preelaborated, namely that the Preelaborate aspect (see 10.2.1) of the library unit is True.

{AI12-0417-1} A [pragma](./AA-2.8#S0019) Pure specifies that a library unit is declared pure, namely that the Pure aspect (see 10.2.1) of the library unit is True.

{AI12-0417-1} A [pragma](./AA-2.8#S0019) Elaborate_Body specifies that [a library unit requires a completion, namely that] the Elaborate_Body aspect (see 10.2.1) of the library unit is True. 


#### Legality Rules

{AI12-0409-1} A [pragma](./AA-2.8#S0019) Preelaborable_Initialization specifies that the Preelaborable_Initialization aspect (see 10.2.1) for a composite type is True. This pragma shall appear in the visible part of a package or generic package.

{AI12-0409-1} If the pragma appears in the first declaration list of a [package_specification](./AA-7.1#S0230), then the [direct_name](./AA-4.1#S0092) shall denote the first subtype of a composite type, and the type shall be declared immediately within the same package as the pragma. The composite type shall be one for which the Preelaborable_Initialization aspect can be directly specified as True. In addition to the places where Legality Rules normally apply (see 12.3), these rules also apply in the private part of an instance of a generic unit.

{AI12-0409-1} If the pragma appears in a [generic_formal_part](./AA-12.1#S0313), then the [direct_name](./AA-4.1#S0092) shall denote a type declared in the same [generic_formal_part](./AA-12.1#S0313) as the pragma, and be one for which the Preelaborable_Initialization aspect can be directly specified as True. 

NOTE 1   {AI12-0409-1} Pragmas Elaborate and Elaborate_All, which do not have associated aspects, are found in 10.2.1. 


#### Wording Changes from Ada 2012

{AI12-0409-1} This subclause is new. These pragmas were moved here from 10.2.1. 


## J.15.15  Distribution Pragmas

{AI12-0417-1} This subclause defines pragmas that specify properties of units for distributed systems.

Discussion: {AI12-0417-1} These pragmas are presented in the order that the aspects are defined in Annex E. 


#### Syntax

{AI12-0417-1} The following [pragma](./AA-2.8#S0019)s are defined with the given forms: 

  pragma Shared_Passive[(library_unit_[name](./AA-4.1#S0091))];

  pragma Remote_Types[(library_unit_[name](./AA-4.1#S0091))];

  pragma Remote_Call_Interface[(library_unit_[name](./AA-4.1#S0091))];

  pragma All_Calls_Remote[(library_unit_[name](./AA-4.1#S0091))];

{AI12-0417-1} Each of these pragmas is a library unit pragma. 


#### Static Semantics

{AI12-0417-1} A categorization pragma is a pragma that specifies a corresponding categorization aspect.

{AI12-0417-1} The pragmas Shared_Passive, Remote_Types, and Remote_Call_Interface are categorization pragmas. In addition, the pragma Pure (see J.15.14) is considered a categorization pragma. 

{AI12-0417-1} A [pragma](./AA-2.8#S0019) Shared_Passive specifies that a library unit is a shared passive library unit, namely that the Shared_Passive aspect (see E.2.1) of the library unit is True.

{AI12-0417-1} A [pragma](./AA-2.8#S0019) Remote_Types specifies that a library unit is a remote types library unit, namely that the Remote_Types aspect (see E.2.2) of the library unit is True.

A [pragma](./AA-2.8#S0019) Remote_Call_Interface specifies that a library unit is a remote call interface, namely that the Remote_Call_Interface aspect (see E.2.3) of the library unit is True.

A [pragma](./AA-2.8#S0019) All_Calls_Remote specifies that the All_Calls_Remote aspect (see E.2.3) of the library unit is True.


#### Wording Changes from Ada 2012

{AI12-0417-1} This subclause is new. These pragmas were moved here from Annex E, "Distributed Systems". 

