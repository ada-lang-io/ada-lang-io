---
sidebar_position:  59
---

# 7.2  Package Bodies

[In contrast to the entities declared in the visible part of a package, the entities declared in the [package_body](./AA-7.2#S0231) are visible only within the [package_body](./AA-7.2#S0231) itself. As a consequence, a package with a [package_body](./AA-7.2#S0231) can be used for the construction of a group of related subprograms in which the logical operations available to clients are clearly isolated from the internal entities.] 


#### Syntax

{AI05-0267-1} package_body<a id="S0231"></a> ::= 
    package body [defining_program_unit_name](./AA-6.1#S0201)
        [[aspect_specification](./AA-13.1#S0346)] is
       [declarative_part](./AA-3.11#S0086)
   [begin
        [handled_sequence_of_statements](./AA-11.2#S0304)]
    end [[[parent_unit_name](./AA-10.1#S0291).][identifier](./AA-2.3#S0002)];

If an [identifier](./AA-2.3#S0002) or [parent_unit_name](./AA-10.1#S0291).[identifier](./AA-2.3#S0002) appears at the end of a [package_body](./AA-7.2#S0231), then this sequence of lexical elements shall repeat the [defining_program_unit_name](./AA-6.1#S0201). 


#### Legality Rules

{AI12-0417-1} A [package_body](./AA-7.2#S0231) shall be the completion of a previous [package_declaration](./AA-7.1#S0229) or [generic_package_declaration](./AA-12.1#S0312). A library [package_declaration](./AA-7.1#S0229) or library [generic_package_declaration](./AA-12.1#S0312) shall not have a body unless it requires a body[; the Elaborate_Body aspect can be used to require a [library_unit_declaration](./AA-10.1#S0288) to have a body (see 10.2.1) if it would not otherwise require one]. 

Ramification: The first part of the rule forbids a [package_body](./AA-7.2#S0231) from standing alone - it has to belong to some previous [package_declaration](./AA-7.1#S0229) or [generic_package_declaration](./AA-12.1#S0312).

A nonlibrary [package_declaration](./AA-7.1#S0229) or nonlibrary [generic_package_declaration](./AA-12.1#S0312) that does not require a completion may have a corresponding body anyway. 


#### Static Semantics

{AI05-0299-1} In any [package_body](./AA-7.2#S0231) without [statement](./AA-5.1#S0167)s there is an implicit [null_statement](./AA-5.1#S0170). For any [package_declaration](./AA-7.1#S0229) without an explicit completion, there is an implicit [package_body](./AA-7.2#S0231) containing a single [null_statement](./AA-5.1#S0170). For a noninstance, nonlibrary package, this body occurs at the end of the [declarative_part](./AA-3.11#S0086) of the innermost enclosing program unit or [block_statement](./AA-5.6#S0191); if there are several such packages, the order of the implicit package_bodies is unspecified. [(For an instance, the implicit [package_body](./AA-7.2#S0231) occurs at the place of the instantiation (see 12.3). For a library package, the place is partially determined by the elaboration dependences (see Clause 10).)] 

Discussion: Thus, for example, we can refer to something happening just after the begin of a [package_body](./AA-7.2#S0231), and we can refer to the [handled_sequence_of_statements](./AA-11.2#S0304) of a [package_body](./AA-7.2#S0231), without worrying about all the optional pieces. The place of the implicit body makes a difference for tasks activated by the package. See also RM83-9.3(5).

The implicit body would be illegal if explicit in the case of a library package that does not require (and therefore does not allow) a body. This is a bit strange, but not harmful. 


#### Dynamic Semantics

For the elaboration of a nongeneric [package_body](./AA-7.2#S0231), its [declarative_part](./AA-3.11#S0086) is first elaborated, and its [handled_sequence_of_statements](./AA-11.2#S0304) is then executed. 

NOTE 1   A variable declared in the body of a package is only visible within this body and, consequently, its value can only be changed within the [package_body](./AA-7.2#S0231). In the absence of local tasks, the value of such a variable remains unchanged between calls issued from outside the package to subprograms declared in the visible part. The properties of such a variable are similar to those of a "static" variable of C.

NOTE 2   The elaboration of the body of a subprogram explicitly declared in the visible part of a package is caused by the elaboration of the body of the package. Hence a call of such a subprogram by an outside program unit raises the exception Program_Error if the call takes place before the elaboration of the [package_body](./AA-7.2#S0231) (see 3.11). 


#### Examples

Example of a package body (see 7.1): 

```ada
package body Rational_Numbers is

```

```ada
   procedure Same_Denominator (X,Y : in out Rational) is
   begin
      --  reduces X and Y to the same denominator:
      ...
   end Same_Denominator;

```

```ada
   function "="(X,Y : Rational) return Boolean is
      U : Rational := X;
      V : Rational := Y;
   begin
      Same_Denominator (U,V);
      return U.Numerator = V.Numerator;
   end "=";

```

```ada
   function "/" (X,Y : Integer) return Rational is
   begin
      if Y &gt 0 then
         return (Numerator =&gt X,  Denominator =&gt Y);
      else
         return (Numerator =&gt -X, Denominator =&gt -Y);
      end if;
   end "/";

```

```ada
   function "+" (X,Y : Rational) return Rational is ... end "+";
   function "-" (X,Y : Rational) return Rational is ... end "-";
   function "*" (X,Y : Rational) return Rational is ... end "*";
   function "/" (X,Y : Rational) return Rational is ... end "/";

```

```ada
end Rational_Numbers;

```


#### Wording Changes from Ada 83

The syntax rule for [package_body](./AA-7.2#S0231) now uses the syntactic category [handled_sequence_of_statements](./AA-11.2#S0304).

The [declarative_part](./AA-3.11#S0086) of a [package_body](./AA-7.2#S0231) is now required; that doesn't make any real difference, since a [declarative_part](./AA-3.11#S0086) can be empty.

RM83 seems to have forgotten to say that a [package_body](./AA-7.2#S0231) can't stand alone, without a previous declaration. We state that rule here.

RM83 forgot to restrict the definition of elaboration of package_bodies to nongeneric ones. We have corrected that omission.

The rule about implicit bodies (from RM83-9.3(5)) is moved here, since it is more generally applicable. 


#### Extensions to Ada 2005

{AI05-0267-1} An optional [aspect_specification](./AA-13.1#S0346) can be used in a [package_body](./AA-7.2#S0231). This is described in 13.1.1. 

