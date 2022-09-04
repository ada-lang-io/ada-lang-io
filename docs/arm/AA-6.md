---
sidebar_position:  7
---

# 6 Subprograms

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
A subprogram is a program unit or intrinsic operation whose execution is invoked by a subprogram call. There are two forms of subprogram: procedures and functions. A procedure call is a statement; a function call is an expression and returns a value. The definition of a subprogram can be given in two parts: a subprogram declaration defining its interface, and a subprogram_body defining its execution. [Operators and enumeration literals are functions.] 

To be honest: A function call is an expression, but more specifically it is a name. 

Version=[5],Kind=(AddedNormal),Group=[S],Term=[subprogram], Def=[a unit of a program that can be brought into execution in various contexts, with the invocation being a subprogram call that can parameterize the effect of the subprogram through the passing of operands], Note1=[There are two forms of subprograms: functions, which return values, and procedures, which do not.] Version=[5],Kind=(AddedNormal),Group=[S],Term=[function], Def=[a form of subprogram that returns a result and can be called as part of an expression] Version=[5],Kind=(AddedNormal),Group=[S],Term=[procedure], Def=[a form of subprogram that does not return a result and can only be invoked by a statement]

A callable entity is a subprogram or entry (see 9.5.2). A callable entity is invoked by a call; that is, a subprogram call or entry call. A callable construct is a construct that defines the action of a call upon a callable entity: a subprogram_body, entry_body, or accept_statement. 

Ramification: Note that "callable entity" includes predefined operators, enumeration literals, and abstract subprograms. "Call" includes calls of these things. They do not have callable constructs, since they don't have completions. 


## 6.1  Subprogram Declarations

[A subprogram_declaration declares a procedure or function.] 


#### Syntax

subprogram_declaration ::= subprogram_specification;

abstract_subprogram_declaration ::= subprogram_specification is abstract;

subprogram_specification ::= 
    procedure defining_program_unit_name parameter_profile
  | function defining_designator parameter_and_result_profile

designator ::= [parent_unit_name . ]identifier | operator_symbol

defining_designator ::= defining_program_unit_name | defining_operator_symbol

defining_program_unit_name ::= [parent_unit_name . ]defining_identifier

[The optional parent_unit_name is only allowed for library units (see 10.1.1).] 

operator_symbol ::= string_literal

The sequence of characters in an operator_symbol shall correspond to an operator belonging to one of the six classes of operators defined in clause 4.5(spaces are not allowed and the case of letters is not significant).

defining_operator_symbol ::= operator_symbol

parameter_profile ::= [formal_part]

parameter_and_result_profile ::= [formal_part] return subtype_mark

formal_part ::= 
   (parameter_specification {; parameter_specification})

parameter_specification ::= 
    defining_identifier_list : mode  subtype_mark [:= default_expression]
  | defining_identifier_list : access_definition [:= default_expression]

mode ::= [in] | in out | out


#### Name Resolution Rules

A formal parameter is an object [directly visible within a subprogram_body] that represents the actual parameter passed to the subprogram in a call; it is declared by a parameter_specification. For a formal parameter, the expected type for its default_expression, if any, is that of the formal parameter. 


#### Legality Rules

The parameter mode of a formal parameter conveys the direction of information transfer with the actual parameter: in, in out, or out. Mode in is the default, and is the mode of a parameter defined by an access_definition. The formal parameters of a function, if any, shall have the mode in. 

Ramification: Access parameters are permitted. This restriction to in parameters is primarily a methodological restriction, though it also simplifies implementation for some compiler technologies. 

A default_expression is only allowed in a parameter_specification for a formal parameter of mode in.

A subprogram_declaration or a generic_subprogram_declaration requires a completion: [a body, a renaming_declaration (see 8.5), or a pragma Import (see B.1)]. [A completion is not allowed for an abstract_subprogram_declaration.] 

Ramification: Abstract subprograms are not declared by subprogram_declarations, and so do not require completion. Protected subprograms are declared by subprogram_declarations, and so require completion. Note that an abstract subprogram is a subprogram, and a protected subprogram is a subprogram, but a generic subprogram is not a subprogram. 

A name that denotes a formal parameter is not allowed within the formal_part in which it is declared, nor within the formal_part of a corresponding body or accept_statement. 

Ramification: By contrast, generic_formal_parameter_declarations are visible to subsequent declarations in the same generic_formal_part. 


#### Static Semantics

The profile of (a view of) a callable entity is either a parameter_profile or parameter_and_result_profile[; it embodies information about the interface to that entity - for example, the profile includes information about parameters passed to the callable entity. All callable entities have a profile - enumeration literals, other subprograms, and entries. An access-to-subprogram type has a designated profile.] Associated with a profile is a calling convention. A subprogram_declaration declares a procedure or a function, as indicated by the initial reserved word, with name and profile as given by its specification.

The nominal subtype of a formal parameter is the subtype denoted by the subtype_mark, or defined by the access_definition, in the parameter_specification.

An access parameter is a formal in parameter specified by an access_definition. An access parameter is of an anonymous general access-to-variable type (see 3.10). [Access parameters  allow dispatching calls to be controlled by access values.]

The subtypes of a profile are: 

For any non-access parameters, the nominal subtype of the parameter.

For any access parameters, the designated subtype of the parameter type.

For any result, the result subtype.

[ The types of a profile are the types of those subtypes.]

[A subprogram declared by an abstract_subprogram_declaration is abstract; a subprogram declared by a subprogram_declaration is not. See 3.9.3, "Abstract Types and Subprograms".]


#### Dynamic Semantics

The elaboration of a subprogram_declaration or an abstract_subprogram_declaration has no effect. 

NOTE 1   A parameter_specification with several identifiers is equivalent to a sequence of single parameter_specifications, as explained in 3.3.

NOTE 2   Abstract subprograms do not have bodies, and cannot be used in a nondispatching call (see 3.9.3, "Abstract Types and Subprograms").

NOTE 3   The evaluation of default_expressions is caused by certain calls, as described in 6.4.1. They are not evaluated during the elaboration of the subprogram declaration.

NOTE 4   Subprograms can be called recursively and can be called concurrently from multiple tasks. 


#### Examples

Examples of subprogram declarations: 

```ada
procedure Traverse_Tree;
procedure Increment(X : in out Integer);
procedure Right_Indent(Margin : out Line_Size);          --  see 3.5.4
procedure Switch(From, To : in out Link);                --  see 3.10.1

```

```ada
function Random return Probability;                      --  see 3.5.7

```

```ada
function Min_Cell(X : Link) return Cell;                 --  see 3.10.1
function Next_Frame(K : Positive) return Frame;          --  see 3.10
function Dot_Product(Left, Right : Vector) return Real;  --  see 3.6

```

```ada
function "*"(Left, Right : Matrix) return Matrix;        --  see 3.6

```

Examples of in parameters with default expressions: 

```ada
procedure Print_Header(Pages  : in Natural;
            Header : in Line    :=  (1 .. Line'Last =&gt ' ');  --  see 3.6
            Center : in Boolean := True);

```


#### Extensions to Ada 83

The syntax for abstract_subprogram_declaration is added. The syntax for parameter_specification is revised to allow for access parameters (see 3.10)

Program units that are library units may have a parent_unit_name to indicate the parent of a child (see Section 10). 


#### Wording Changes from Ada 83

We have incorporated the rules from RM83-6.5, "Function Subprograms" here and in 6.3, "Subprogram Bodies"

We have incorporated the definitions of RM83-6.6, "Parameter and Result Type Profile - Overloading of Subprograms" here.

The syntax rule for defining_operator_symbol is new. It is used for the defining occurrence of an operator_symbol, analogously to defining_identifier. Usage occurrences use the direct_name or selector_name syntactic categories. The syntax rules for defining_designator and defining_program_unit_name are new. 

Version=[5],Kind=(AddedNormal),Group=[S],Term=[precondition], Def=[an assertion that is expected to be True when a given subprogram is called] Version=[5],Kind=(AddedNormal),Group=[S],Term=[postcondition], Def=[an assertion that is expected to be True when a given subprogram returns normally] 


#### Static Semantics








## 6.2  Formal Parameter Modes

[A parameter_specification declares a formal parameter of mode in, in out, or out.] 


#### Static Semantics

A parameter is passed either by copy or by reference. [When a parameter is passed by copy, the formal parameter denotes a separate object from the actual parameter, and any information transfer between the two occurs only before and after executing the subprogram. When a parameter is passed by reference, the formal parameter denotes (a view of) the object denoted by the actual parameter; reads and updates of the formal parameter directly reference the actual parameter object.]

A type is a by-copy type if it is an elementary type, or if it is a descendant of a private type whose full type is a by-copy type. A parameter of a by-copy type is passed by copy.

A type is a by-reference type if it is a descendant of one of the following: 

a tagged type;

a task or protected type;

a nonprivate type with the reserved word limited in its declaration; 

Ramification: A limited private type is by-reference only if it falls under one of the other categories. 

a composite type with a subcomponent of a by-reference type;

a private type whose full type is a by-reference type. 

A parameter of a by-reference type is passed by reference. Each value of a by-reference type has an associated object. For a parenthesized expression, qualified_expression, or type_conversion, this object is the one associated with the operand.

Ramification: By-reference parameter passing makes sense only if there is an object to reference; hence, we define such an object for each case.

Since tagged types are by-reference types, this implies that every value of a tagged type has an associated object. This simplifies things, because we can define the tag to be a property of the object, and not of the value of the object, which makes it clearer that object tags never change.

We considered simplifying things even more by making every value (and therefore every expression) have an associated object. After all, there is little semantic difference between a constant object and a value. However, this would cause problems for untagged types. In particular, we would have to do a constraint check on every read of a type conversion (or a renaming thereof) in certain cases.

We do not want this definition to depend on the view of the type; privateness is essentially ignored for this definition. Otherwise, things would be confusing (does the rule apply at the call site, at the site of the declaration of the subprogram, at the site of the return_statement?), and requiring different calls to use different mechanisms would be an implementation burden.

C.6, "Shared Variable Control" says that a composite type with an atomic or volatile subcomponent is a by-reference type, among other things.

Every value of a limited by-reference type is the value of one and only one limited object. The associated object of a value of a limited by-reference type is the object whose value it represents. Two values of a limited by-reference type are the same if and only if they represent the value of the same object.

We say "by-reference" above because these statements are not always true for limited private types whose underlying type is nonlimited (unfortunately). 

For parameters of other types, it is unspecified whether the parameter is passed by copy or by reference. 

Discussion: There is no need to incorporate the discussion of AI83-00178, which requires pass-by-copy for certain kinds of actual parameters, while allowing pass-by-reference for others. This is because we explicitly indicate that a function creates an anonymous constant object for its result, unless the type is a return-by-reference type (see 6.5). We also provide a special dispensation for instances of Unchecked_Conversion to return by reference, even if the result type is not a return-by-reference type (see 13.9). 


#### Bounded (Run-Time) Errors

If one name denotes a part of a formal parameter, and a second name denotes a part of a distinct formal parameter or an object that is not part of a formal parameter, then the two names are considered distinct access paths. If an object is of a type for which the parameter passing mechanism is not specified, then it is a bounded error to assign to the object via one access path, and then read the value of the object via a distinct access path, unless the first access path denotes a part of a formal parameter that no longer exists at the point of the second access [(due to leaving the corresponding callable construct).] The possible consequences are that Program_Error is raised, or the newly assigned value is read, or some old value of the object is read. 

Discussion: For example, if we call "P(X =&gt Global_Variable, Y =&gt Global_Variable)", then within P, the names "X", "Y", and "Global_Variable" are all distinct access paths. If Global_Variable's type is neither pass-by-copy nor pass-by-reference, then it is a bounded error to assign to Global_Variable and then read X or Y, since the language does not specify whether the old or the new value would be read. On the other hand, if Global_Variable's type is pass-by-copy, then the old value would always be read, and there is no error. Similarly, if Global_Variable's type is defined by the language to be pass-by-reference, then the new value would always be read, and again there is no error. 

Reason: We are saying assign here, not update, because updating any subcomponent is considered to update the enclosing object.

The "still exists" part is so that a read after the subprogram returns is OK.

If the parameter is of a by-copy type, then there is no issue here - the formal is not a view of the actual. If the parameter is of a by-reference type, then the programmer may depend on updates through one access path being visible through some other access path, just as if the parameter were of an access type. 

Implementation Note: The implementation can keep a copy in a register of a parameter whose parameter-passing mechanism is not specified. If a different access path is used to update the object (creating a bounded error situation), then the implementation can still use the value of the register, even though the in-memory version of the object has been changed. However, to keep the error properly bounded, if the implementation chooses to read the in-memory version, it has to be consistent -- it cannot then assume that something it has proven about the register is true of the memory location. For example, suppose the formal parameter is L, the value of L(6) is now in a register, and L(6) is used in an indexed_component as in "A(L(6)) := 99;", where A has bounds 1..3. If the implementation can prove that the value for L(6) in the register is in the range 1..3, then it need not perform the constraint check if it uses the register value. However, if the memory value of L(6) has been changed to 4, and the implementation uses that memory value, then it had better not alter memory outside of A.

Note that the rule allows the implementation to pass a parameter by reference and then keep just part of it in a register, or, equivalently, to pass part of the parameter by reference and another part by copy. 

Reason: We do not want to go so far as to say that the mere presence of aliasing is wrong. We wish to be able to write the following sorts of things in standard Ada: 

```ada
procedure Move ( Source  : in  String;
                 Target  : out String;
                 Drop    : in  Truncation := Error;
                 Justify : in  Alignment  := Left;
                 Pad     : in  Character  := Space);
-- Copies elements from Source to Target (safely if they overlap)

```

This is from the standard string handling package. It would be embarrassing if this couldn't be written in Ada!

The "then" before "read" in the rule implies that the implementation can move a read to an earlier place in the code, but not to a later place after a potentially aliased assignment. Thus, if the subprogram reads one of its parameters into a local variable, and then updates another potentially aliased one, the local copy is safe - it is known to have the old value. For example, the above-mentioned Move subprogram can be implemented by copying Source into a local variable before assigning into Target.

For an assignment_statement assigning one array parameter to another, the implementation has to check which direction to copy at run time, in general, in case the actual parameters are overlapping slices. For example: 

```ada
procedure Copy(X : in out String; Y: String) is
begin
    X := Y;
end Copy;

```

It would be wrong for the compiler to assume that X and Y do not overlap (unless, of course, it can prove otherwise). 

NOTE 1   A formal parameter of mode in is a constant view (see 3.3); it cannot be updated within the subprogram_body.


#### Extensions to Ada 83

The value of an out parameter may be read. An out parameter is treated like a declared variable without an explicit initial expression. 


#### Wording Changes from Ada 83

Discussion of copy-in for parts of out parameters is now covered in 6.4.1, "Parameter Associations".

The concept of a by-reference type is new to Ada 95.

We now cover in a general way in 3.7.2 the rule regarding erroneous execution when a discriminant is changed and one of the parameters depends on the discriminant. 


## 6.3  Subprogram Bodies

[A subprogram_body specifies the execution of a subprogram.] 


#### Syntax

subprogram_body ::= 
    subprogram_specification is
       declarative_part
    begin
        handled_sequence_of_statements
    end [designator];

If a designator appears at the end of a subprogram_body, it shall repeat the defining_designator of the subprogram_specification. 


#### Legality Rules

[In contrast to other bodies,] a subprogram_body need not be the completion of a previous declaration[, in which case the body declares the subprogram]. If the body is a completion, it shall be the completion of a subprogram_declaration or generic_subprogram_declaration. The profile of a subprogram_body that completes a declaration shall conform fully to that of the declaration. 


#### Static Semantics

A subprogram_body is considered a declaration. It can either complete a previous declaration, or itself be the initial declaration of the subprogram. 


#### Dynamic Semantics

The elaboration of a nongeneric subprogram_body has no other effect than to establish that the subprogram can from then on be called without failing the Elaboration_Check. 

Ramification: See 12.2 for elaboration of a generic body. Note that protected subprogram_bodies never get elaborated; the elaboration of the containing protected_body allows them to be called without failing the Elaboration_Check. 

[The execution of a subprogram_body is invoked by a subprogram call.] For this execution the declarative_part is elaborated, and the handled_sequence_of_statements is then executed. 


#### Examples

Example of procedure body: 

```ada
procedure Push(E : in Element_Type; S : in out Stack) is
begin
   if S.Index = S.Size then
      raise Stack_Overflow;
   else
      S.Index := S.Index + 1;
      S.Space(S.Index) := E;
   end if;
end Push;

```

Example of a function body: 

```ada
function Dot_Product(Left, Right : Vector) return Real is
   Sum : Real := 0.0;
begin
   Check(Left'First = Right'First and Left'Last = Right'Last);
   for J in Left'Range loop
      Sum := Sum + Left(J)*Right(J);
   end loop;
   return Sum;
end Dot_Product;

```


#### Extensions to Ada 83

A renaming_declaration may be used instead of a subprogram_body. 


#### Wording Changes from Ada 83

The syntax rule for subprogram_body now uses the syntactic category handled_sequence_of_statements.

The declarative_part of a subprogram_body is now required; that doesn't make any real difference, because a declarative_part can be empty.

We have incorporated some rules from RM83-6.5 here.

RM83 forgot to restrict the definition of elaboration of a subprogram_body to nongenerics. 


### 6.3.1  Conformance Rules

[When subprogram profiles are given in more than one place, they are required to conform in one of four ways: type conformance, mode conformance, subtype conformance, or full conformance.] 


#### Static Semantics

[As explained in B.1, "Interfacing Pragmas", a convention can be specified for an entity.] [For a callable entity or access-to-subprogram type, the convention is called the calling convention.] The following conventions are defined by the language: 

The default calling convention for any subprogram not listed below is Ada. [A pragma Convention, Import, or Export may be used to override the default calling convention (see B.1)]. 

Ramification: See also the rule about renamings-as-body in 8.5.4. 

The Intrinsic calling convention represents subprograms that are "built in" to the compiler. The default calling convention is Intrinsic for the following: 

an enumeration literal;

a "/=" operator declared implicitly due to the declaration of "=" (see 6.6);

any other implicitly declared subprogram unless it is a dispatching operation of a tagged type;

an inherited subprogram of a generic formal tagged type with unknown discriminants; 

an attribute that is a subprogram;

a subprogram declared immediately within a protected_body.

[The Access attribute is not allowed for Intrinsic subprograms.] 

Ramification: The Intrinsic calling convention really represents any number of calling conventions at the machine code level; the compiler might have a different instruction sequence for each intrinsic. That's why the Access attribute is disallowed. We do not wish to require the implementation to generate an out of line body for an intrinsic.

Whenever we wish to disallow the Access attribute in order to ease implementation, we make the subprogram Intrinsic. Several language-defined subprograms have "pragma Convention(Intrinsic, ...);". An implementation might actually implement this as "pragma Import(Intrinsic, ...);", if there is really no body, and the implementation of the subprogram is built into the code generator.

Subprograms declared in protected_bodies will generally have a special calling convention so as to pass along the identification of the current instance of the protected type. The convention is not protected since such local subprograms need not contain any "locking" logic since they are not callable via "external" calls; this rule prevents an access value designating such a subprogram from being passed outside the protected unit.

The "implicitly declared subprogram" above refers to predefined operators (other than the "=" of a tagged type) and the inherited subprograms of untagged types. 

The default calling convention is protected for a protected subprogram, and for an access-to-subprogram type with the reserved word protected in its definition.

The default calling convention is entry for an entry.

Of these four conventions, only Ada and Intrinsic are allowed as a convention_identifier in a pragma Convention, Import, or Export. 

Discussion: The names of the protected and entry calling conventions cannot be used in the interfacing pragmas. Note that protected and entry are reserved words. 

Two profiles are type conformant if they have the same number of parameters, and both have a result if either does, and corresponding parameter and result types are the same, or, for access parameters, corresponding designated types are the same. 

Discussion: For access parameters, the designated types have to be the same for type conformance, not the access types, since in general each access parameter has its own anonymous access type, created when the subprogram is called. Of course, corresponding parameters have to be either both access parameters or both not access parameters.

Two profiles are mode conformant if they are type-conformant, and corresponding parameters have identical modes, and, for access parameters, the designated subtypes statically match 

Two profiles are subtype conformant if they are mode-conformant, corresponding subtypes of the profile statically match, and the associated calling conventions are the same. The profile of a generic formal subprogram is not subtype-conformant with any other profile. 

Ramification: 

Two profiles are fully conformant if they are subtype-conformant, and corresponding parameters have the same names and have default_expressions that are fully conformant with one another. 

Ramification: Full conformance requires subtype conformance, which requires the same calling conventions. However, the calling convention of the declaration and body of a subprogram or entry are always the same by definition. 

Two expressions are fully conformant if, [after replacing each use of an operator with the equivalent function_call:] 

each constituent construct of one corresponds to an instance of the same syntactic category in the other, except that an expanded name may correspond to a direct_name (or character_literal) or to a different expanded name in the other; and

each direct_name, character_literal, and selector_name that is not part of the prefix of an expanded name in one denotes the same declaration as the corresponding direct_name, character_literal, or selector_name in the other; and 

Ramification: Note that it doesn't say "respectively" because a direct_name can correspond to a selector_name, and vice-versa, by the previous bullet. This rule allows the prefix of an expanded name to be removed, or replaced with a different prefix that denotes a renaming of the same entity. However, it does not allow a direct_name or selector_name to be replaced with one denoting a distinct renaming (except for direct_names and selector_names in prefixes of expanded names). Note that calls using operator notation are equivalent to calls using prefix notation.

Given the following declarations: 

```ada
package A is
    function F(X : Integer := 1) return Boolean;
end A;

```

```ada
with A;
package B is
    package A_View renames A;
    function F_View(X : Integer := 9999) return Boolean renames F;
end B;

```

```ada
with A, B; use A, B;
procedure Main is ...

```

Within Main, the expressions "F", "A.F", "B.A_View.F", and "A_View.F" are all fully conformant with one another. However, "F" and "F_View" are not fully conformant. If they were, it would be bad news, since the two denoted views have different default_expressions. 

each primary that is a literal in one has the same value as the corresponding literal in the other.

Ramification: The literals may be written differently. 

Ramification: Note that the above definition makes full conformance a transitive relation. 

Two known_discriminant_parts are fully conformant if they have the same number of discriminants, and discriminants in the same positions have the same names, statically matching subtypes, and default_expressions that are fully conformant with one another. 

Two discrete_subtype_definitions are fully conformant if they are both subtype_indications or are both ranges, the subtype_marks (if any) denote the same subtype, and the corresponding simple_expressions of the ranges (if any) fully conform. 

Ramification: In the subtype_indication case, any ranges have to be corresponding; that is, two subtype_indications cannot conform unless both or neither has a range. 

Discussion: This definition is used in 9.5.2, "Entries and Accept Statements" for the conformance required between the discrete_subtype_definitions of an entry_declaration for a family of entries and the corresponding entry_index_specification of the entry_body. 


#### Implementation Permissions

An implementation may declare an operator declared in a language-defined library unit to be intrinsic. 


#### Extensions to Ada 83

The rules for full conformance are relaxed - they are now based on the structure of constructs, rather than the sequence of lexical elements. This implies, for example, that "(X, Y: T)" conforms fully with "(X: T; Y: T)", and "(X: T)" conforms fully with "(X: in T)". 


### 6.3.2  Inline Expansion of Subprograms

[Subprograms may be expanded in line at the call site.] 


#### Syntax

The form of a pragma Inline, which is a program unit pragma (see 10.1.5), is as follows: 

  pragma Inline(name {, name}); 


#### Legality Rules

The pragma shall apply to one or more callable entities or generic subprograms. 


#### Static Semantics

If a pragma Inline applies to a callable entity, this indicates that inline expansion is desired for all calls to that entity. If a pragma Inline applies to a generic subprogram, this indicates that inline expansion is desired for all calls to all instances of that generic subprogram.

Ramification: Note that inline expansion is desired no matter what name is used in the call. This allows one to request inlining for only one of several overloaded subprograms as follows: 

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

Ramification: The meaning of a subprogram can be changed by a pragma Inline only in the presence of failing checks (see 11.6). 


#### Implementation Permissions

For each call, an implementation is free to follow or to ignore the recommendation expressed by the pragma. 

Ramification: Note, in particular, that the recommendation cannot always be followed for a recursive call, and is often infeasible for entries. Note also that the implementation can inline calls even when no such desire was expressed by a pragma, so long as the semantics of the program remains unchanged. 

NOTE 1   The name in a pragma Inline can denote more than one entity in the case of overloading. Such a pragma applies to all of the denoted entities. 


#### Extensions to Ada 83

A pragma Inline is allowed inside a subprogram_body if there is no corresponding subprogram_declaration. This is for uniformity with other program unit pragmas. 


## 6.4  Subprogram Calls

A subprogram call is either a procedure_call_statement or a function_call; [it invokes the execution of the subprogram_body. The call specifies the association of the actual parameters, if any, with formal parameters of the subprogram.] 


#### Syntax

procedure_call_statement ::= 
    procedure_name;
  | procedure_prefix actual_parameter_part;

function_call ::= 
    function_name
  | function_prefix actual_parameter_part

actual_parameter_part ::= 
    (parameter_association {, parameter_association})

parameter_association ::= 
   [formal_parameter_selector_name =&gt] explicit_actual_parameter

explicit_actual_parameter ::= expression | variable_name

A parameter_association is named or positional according to whether or not the formal_parameter_selector_name is specified. Any positional associations shall precede any named associations. Named associations are not allowed if the prefix in a subprogram call is an attribute_reference. 

Ramification: This means that the formal parameter names used in describing predefined attributes are to aid presentation of their semantics, but are not intended for use in actual calls. 


#### Name Resolution Rules

The name or prefix given in a procedure_call_statement shall resolve to denote a callable entity that is a procedure, or an entry renamed as (viewed as) a procedure. The name or prefix given in a function_call shall resolve to denote a callable entity that is a function. [When there is an actual_parameter_part, the prefix can be an implicit_dereference of an access-to-subprogram value.] 

Ramification: The function can be an operator, enumeration literal, attribute that is a function, etc. 

A subprogram call shall contain at most one association for each formal parameter. Each formal parameter without an association shall have a default_expression (in the profile of the view denoted by the name or prefix). [This rule is an overloading rule (see 8.6).] 


#### Dynamic Semantics

For the execution of a subprogram call, the name or prefix of the call is evaluated, and each parameter_association is evaluated (see 6.4.1). If a default_expression is used, an implicit parameter_association is assumed for this rule. These evaluations are done in an arbitrary order. The subprogram_body is then executed. Finally, if the subprogram completes normally, then after it is left, any necessary assigning back of formal to actual parameters occurs (see 6.4.1).

Discussion: The implicit association for a default is only for this run-time rule. At compile time, the visibility rules are applied to the default at the place where it occurs, not at the place of a call. 

To be honest: If the subprogram is inherited, see 3.4, "Derived Types and Classes".

If the subprogram is protected, see 9.5.1, "Protected Subprograms and Protected Actions".

If the subprogram is really a renaming of an entry, see 9.5.3, "Entry Calls".

Normally, the subprogram_body that is executed by the above rule is the one for the subprogram being called. For an enumeration literal, implicitly declared (but noninherited) subprogram, or an attribute that is a subprogram, an implicit body is assumed. For a dispatching call, 3.9.2, "Dispatching Operations of Tagged Types" defines which subprogram_body is executed. 

The exception Program_Error is raised at the point of a function_call if the function completes normally without executing a return_statement. 

Discussion: We are committing to raising the exception at the point of call, for uniformity - see AI83-00152. This happens after the function is left, of course.

Note that there is no name for suppressing this check, since the check imposes no time overhead and minimal space overhead (since it can usually be statically eliminated as dead code). 

A function_call denotes a constant, as defined in 6.5; the nominal subtype of the constant is given by the result subtype of the function. 


#### Examples

Examples of procedure calls: 

```ada
Traverse_Tree;                                               --  see 6.1
Print_Header(128, Title, True);                              --  see 6.1

```

```ada
Switch(From =&gt X, To =&gt Next);                               --  see 6.1
Print_Header(128, Header =&gt Title, Center =&gt True);          --  see 6.1
Print_Header(Header =&gt Title, Center =&gt True, Pages =&gt 128); --  see 6.1

```

Examples of function calls: 

```ada
Dot_Product(U, V)   --  see 6.1 and 6.3
Clock               --  see 9.6
F.all               --  presuming F is of an access-to-subprogram type - see 3.10

```

Examples of procedures with default expressions: 

```ada
procedure Activate(Process : in Process_Name;
                   After   : in Process_Name := No_Process;
                   Wait    : in Duration := 0.0;
                   Prior   : in Boolean := False);

```

```ada
procedure Pair(Left, Right : in Person_Name := new Person);   --  see 3.10.1

```

Examples of their calls: 

```ada
Activate(X);
Activate(X, After =&gt Y);
Activate(X, Wait =&gt 60.0, Prior =&gt True);
Activate(X, Y, 10.0, False);

```

```ada
Pair;
Pair(Left =&gt new Person, Right =&gt new Person);

```

NOTE 1   If a default_expression is used for two or more parameters in a multiple parameter_specification, the default_expression is evaluated once for each omitted parameter. Hence in the above examples, the two calls of Pair are equivalent. 


#### Examples

Examples of overloaded subprograms: 

```ada
procedure Put(X : in Integer);
procedure Put(X : in String);

```

```ada
procedure Set(Tint   : in Color);
procedure Set(Signal : in Light);

```

Examples of their calls: 

```ada
Put(28);
Put("no possible ambiguity here");

```

```ada
Set(Tint   =&gt Red);
Set(Signal =&gt Red);
Set(Color'(Red));

```

```ada
--  Set(Red) would be ambiguous since Red may
--  denote a value either of type Color or of type Light

```


#### Wording Changes from Ada 83

We have gotten rid of parameters "of the form of a type conversion" (see RM83-6.4.1(3)). The new view semantics of type_conversions allows us to use normal type_conversions instead.

We have moved wording about run-time semantics of parameter associations to 6.4.1.

We have moved wording about raising Program_Error for a function that falls off the end to here from RM83-6.5. 


### 6.4.1  Parameter Associations

[ A parameter association defines the association between an actual parameter and a formal parameter.] 


#### Language Design Principles

The parameter passing rules for out parameters are designed to ensure that the parts of a type that have implicit initial values (see 3.3.1) don't become "de-initialized" by being passed as an out parameter.


#### Name Resolution Rules

The formal_parameter_selector_name of a parameter_association shall resolve to denote a parameter_specification of the view being called.

The actual parameter is either the explicit_actual_parameter given in a parameter_association for a given formal parameter, or the corresponding default_expression if no parameter_association is given for the formal parameter. The expected type for an actual parameter is the type of the corresponding formal parameter. 

To be honest: The corresponding default_expression is the one of the corresponding formal parameter in the profile of the view denoted by the name or prefix of the call. 

If the mode is in, the actual is interpreted as an expression; otherwise, the actual is interpreted only as a name, if possible. 

Ramification: This formally resolves the ambiguity present in the syntax rule for explicit_actual_parameter.  Note that we don't actually require that the actual be a name if the mode is not in; we do that below.


#### Legality Rules

If the mode is in out or out, the actual shall be a name that denotes a variable. 

Discussion: We no longer need "or a type_conversion whose argument is the name of a variable", because a type_conversion is now a name, and a type_conversion of a variable is a variable. 

Reason: The requirement that the actual be a (variable) name is not an overload resolution rule, since we don't want the difference between expression and name to be used to resolve overloading. For example: 

```ada
procedure Print(X : in Integer; Y : in Boolean := True);
procedure Print(Z : in out Integer);
. . .
Print(3); -- Ambiguous!
  

```

The above call to Print is ambiguous even though the call is not compatible with the second Print which requires an actual that is a (variable) name ("3" is an expression, not a name). This requirement is a legality rule, so overload resolution fails before it is considered, meaning that the call is ambiguous. 

The type of the actual parameter associated with an access parameter shall be convertible (see 4.6) to its anonymous access type. 


#### Dynamic Semantics

For the evaluation of a parameter_association: 

The actual parameter is first evaluated.

For an access parameter, the access_definition is elaborated, which creates the anonymous access type.

For a parameter [(of any mode)] that is passed by reference (see 6.2), a view conversion of the actual parameter to the nominal subtype of the formal parameter is evaluated, and the formal parameter denotes that conversion. 

Discussion: We are always allowing sliding, even for [in] out by-reference parameters. 

For an in or in out parameter that is passed by copy (see 6.2), the formal parameter object is created, and the value of the actual parameter is converted to the nominal subtype of the formal parameter and assigned to the formal. 

Ramification: The conversion mentioned here is a value conversion. 

For an out parameter that is passed by copy, the formal parameter object is created, and: 

For an access type, the formal parameter is initialized from the value of the actual, without a constraint check;

Reason: This preserves the Language Design Principle that an object of an access type is always initialized with a "reasonable" value. 

For a composite type with discriminants or that has implicit initial values for any subcomponents (see 3.3.1), the behavior is as for an in out parameter passed by copy. 

Reason: This ensures that no part of an object of such a type can become "de-initialized" by being part of an out parameter. 

Ramification: This includes an array type whose component type is an access type, and a record type with a component that has a default_expression, among other things. 

For any other type, the formal parameter is uninitialized. If composite, a view conversion of the actual parameter to the nominal subtype of the formal is evaluated [(which might raise Constraint_Error)], and the actual subtype of the formal is that of the view conversion. If elementary, the actual subtype of the formal is given by its nominal subtype. 

Ramification: This case covers scalar types, and composite types whose subcomponent's subtypes do not have any implicit initial values. The view conversion for composite types ensures that if the lengths don't match between an actual and a formal array parameter, the Constraint_Error is raised before the call, rather than after. 

A formal parameter of mode in out or out with discriminants is constrained if either its nominal subtype or the actual parameter is constrained.

After normal completion and leaving of a subprogram, for each in out or out parameter that is passed by copy, the value of the formal parameter is converted to the subtype of the variable given as the actual parameter and assigned to it. These conversions and assignments occur in an arbitrary order. 

Ramification: The conversions mentioned above during parameter passing might raise Constraint_Error - (see 4.6). 

Ramification: If any conversion or assignment as part of parameter passing propagates an exception, the exception is raised at the place of the subprogram call; that is, it cannot be handled inside the subprogram_body. 

Proof: Since these checks happen before or after executing the subprogram_body, the execution of the subprogram_body does not dynamically enclose them, so it can't handle the exceptions. 

Discussion: The variable we're talking about is the one denoted by the variable_name given as the explicit_actual_parameter. If this variable_name is a type_conversion, then the rules in 4.6 for assigning to a view conversion apply. That is, if X is of subtype S1, and the actual is S2(X), the above-mentioned conversion will convert to S2, and the one mentioned in 4.6 will convert to S1. 


#### Extensions to Ada 83

In Ada 95, a program can rely on the fact that passing an object as an out parameter does not "de-initialize" any parts of the object whose subtypes have implicit initial values. (This generalizes the RM83 rule that required copy-in for parts that were discriminants or of an access type.) 


#### Wording Changes from Ada 83

We have eliminated the subclause on Default Parameters, as it is subsumed by earlier clauses and subclauses. 


## 6.5  Return Statements

A return_statement is used to complete the execution of the innermost enclosing subprogram_body, entry_body, or accept_statement. 


#### Syntax

return_statement ::= return [expression];


#### Name Resolution Rules

The expression, if any, of a return_statement is called the return expression. The result subtype of a function is the subtype denoted by the subtype_mark after the reserved word return in the profile of the function. The expected type for a return expression is the result type of the corresponding function. 

To be honest: The same applies to generic functions. 


#### Legality Rules

A return_statement shall be within a callable construct, and it applies to the innermost one. A return_statement shall not be within a body that is within the construct to which the return_statement applies.

A function body shall contain at least one return_statement that applies to the function body, unless the function contains code_statements. A return_statement shall include a return expression if and only if it applies to a function body.

Reason: The requirement that a function body has to have at least one return_statement is a "helpful" restriction. There was been some interest in lifting this restriction, or allowing a raise statement to substitute for the return_statement. However, there was enough interest in leaving it as is that we decided not to change it. 


#### Dynamic Semantics

For the execution of a return_statement, the expression (if any) is first evaluated and converted to the result subtype. 

Ramification: The conversion might raise Constraint_Error - (see 4.6). 

If the result type is class-wide, then the tag of the result is the tag of the value of the expression. 

If the result type is a specific tagged type:

Ramification: . 

If it is limited, then a check is made that the tag of the value of the return expression identifies the result type. Constraint_Error is raised if this check fails.

If it is nonlimited, then the tag of the result is that of the result type. 

Ramification: This is true even if the tag of the return expression is different. 

Reason: These rules ensure that a function whose result type is a specific tagged type always returns an object whose tag is that of the result type. This is important for dispatching on controlling result, and, if nonlimited, allows the caller to allocate the appropriate amount of space to hold the value being returned (assuming there are no discriminants). 

A type is a return-by-reference type if it is a descendant of one of the following: 

a tagged limited type;

a task or protected type;

a nonprivate type with the reserved word limited in its declaration;

a composite type with a subcomponent of a return-by-reference type;

a private type whose full type is a return-by-reference type. 

Ramification: The above rules are such that there are no "Ada 83" types other than those containing tasks that are return-by-reference. This helps to minimize upward incompatibilities relating to return-by-reference. 

If the result type is a return-by-reference type, then a check is made that the return expression is one of the following: 

a name that denotes an object view whose accessibility level is not deeper than that of the master that elaborated the function body; or 

Discussion: This rule was unnecessarily confusing, and the parenthetical remark "(or a value with an associated object, see 6.2)" was added - and then the entire concept was deleted. 

a parenthesized expression or qualified_expression whose operand is one of these kinds of expressions.

The exception Program_Error is raised if this check fails. 

Discussion: Compare the definition of return-by-reference with that of by-reference.

The return-by-reference types are all limited types except those that are limited only because of a limited private type with a nonlimited untagged full type. 

Reason: This check can often be performed at compile time. It is defined to be a runtime check to avoid generic contract model problems. In a future version of the standard, we anticipate that function return of a local variable will be illegal for all limited types, eliminating the need for the runtime check except for dereferences of an access parameter. 

For a function with a return-by-reference result type the result is returned by reference; that is, the function call denotes a constant view of the object associated with the value of the return expression. For any other function, the result is returned by copy; that is, the converted value is assigned into an anonymous constant created at the point of the return_statement, and the function call denotes that object. 

Ramification: The assignment operation does the necessary value adjustment, as described in 7.6, "User-Defined Assignment and Finalization". 7.6.1 describes when the anonymous constant is finalized. 

Finally, a transfer of control is performed which completes the execution of the callable construct to which the return_statement applies, and returns to the caller.


#### Examples

Examples of return statements: 

```ada
return;                         -- in a procedure body, entry_body, or accept_statement

```

```ada
return Key_Value(Last_Index);   -- in a function body

```


#### Incompatibilities With Ada 83

In Ada 95, if the result type of a function has a part that is a task, then an attempt to return a local variable will raise Program_Error. In Ada 83, if a function returns a local variable containing a task, execution is erroneous according to AI83-00867. However, there are other situations where functions that return tasks (or that return a variant record only one of whose variants includes a task) are correct in Ada 83 but will raise Program_Error according to the new rules.

The rule change was made because there will be more types (protected types, limited controlled types) in Ada 95 for which it will be meaningless to return a local variable, and making all of these erroneous is unacceptable. The current rule was felt to be the simplest that kept upward incompatibilities to situations involving returning tasks, which are quite rare. 


#### Wording Changes from Ada 83

This clause has been moved here from chapter 5, since it has mainly to do with subprograms.

A function now creates an anonymous object. This is necessary so that controlled types will work.

We have clarified that a return_statement applies to a callable construct, not to a callable entity.

There is no need to mention generics in the rules about where a return_statement can appear and what it applies to; the phrase "body of a subprogram or generic subprogram" is syntactic, and refers exactly to "subprogram_body". 


#### Syntax

  

Aspect Description for No_Return: 


## 6.6  Overloading of Operators

An operator is a function whose designator is an operator_symbol. [Operators, like other functions, may be overloaded.] 


#### Name Resolution Rules

Each use of a unary or binary operator is equivalent to a function_call with function_prefix being the corresponding operator_symbol, and with (respectively) one or two positional actual parameters being the operand(s) of the operator (in order). 

To be honest: We also use the term operator (in Section 4 and in 6.1) to refer to one of the syntactic categories defined in 4.5, "Operators and Expression Evaluation" whose names end with "_operator:" logical_operator, relational_operator, binary_adding_operator, unary_adding_operator, multiplying_operator, and highest_precedence_operator. 


#### Legality Rules

The subprogram_specification of a unary or binary operator shall have one or two parameters, respectively. A generic function instantiation whose designator is an operator_symbol is only allowed if the specification of the generic function has the corresponding number of parameters.

Default_expressions are not allowed for the parameters of an operator (whether the operator is declared with an explicit subprogram_specification or by a generic_instantiation).

An explicit declaration of "/=" shall not have a result type of the predefined type Boolean. 


#### Static Semantics

A declaration of "=" whose result type is Boolean implicitly declares a declaration of "/=" that gives the complementary result. 

NOTE 1   The operators "+" and "" are both unary and binary operators, and hence may be overloaded with both one- and two-parameter functions. 


#### Examples

Examples of user-defined operators: 

```ada
function "+" (Left, Right : Matrix) return Matrix;
function "+" (Left, Right : Vector) return Vector;

--  assuming that A, B, and C are of the type Vector
--  the following two statements are equivalent:

A := B + C;
A := "+"(B, C);

```


#### Extensions to Ada 83

Explicit declarations of "=" are now permitted for any combination of parameter and result types.

Explicit declarations of "/=" are now permitted, so long as the result type is not Boolean. 

