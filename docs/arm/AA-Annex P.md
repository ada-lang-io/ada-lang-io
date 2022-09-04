---
sidebar_position:  28
---

# Annex P Syntax Summary

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
This Annex summarizes the complete syntax of the language.


## P.1  Syntax Rules

See  for a description of the notation used. 

2.1:
character ::= graphic_character | format_effector | other_control_function

2.1:
graphic_character ::= identifier_letter | digit | space_character | special_character

2.3:
identifier ::= 
   identifier_letter {[underline] letter_or_digit}

2.3:
letter_or_digit ::= identifier_letter | digit

2.4:
numeric_literal ::= decimal_literal | based_literal

2.4.1:
decimal_literal ::= numeral [.numeral] [exponent]

2.4.1:
numeral ::= digit {[underline] digit}

2.4.1:
exponent ::= E [+] numeral | E  numeral

2.4.2:
based_literal ::= 
   base # based_numeral [.based_numeral] # [exponent]

2.4.2:
base ::= numeral

2.4.2:
based_numeral ::= 
   extended_digit {[underline] extended_digit}

2.4.2:
extended_digit ::= digit | A | B | C | D | E | F

2.5:
character_literal ::= 'graphic_character'

2.6:
string_literal ::= "{string_element}"

2.6:
string_element ::= "" | non_quotation_mark_graphic_character

2.7:
comment ::= --{non_end_of_line_character}

2.8:
pragma ::= 
   pragma identifier [(pragma_argument_association {, pragma_argument_association})];

2.8:
pragma_argument_association ::= 
     [pragma_argument_identifier =&gt] name
   | [pragma_argument_identifier =&gt] expression

3.1:
basic_declaration ::= 
     type_declaration	| subtype_declaration
   | object_declaration	| number_declaration
   | subprogram_declaration	| abstract_subprogram_declaration
   | package_declaration	| renaming_declaration
   | exception_declaration	| generic_declaration
   | generic_instantiation

3.1:
defining_identifier ::= identifier

3.2.1:
type_declaration ::=  full_type_declaration
   | incomplete_type_declaration
   | private_type_declaration
   | private_extension_declaration

3.2.1:
full_type_declaration ::= 
     type defining_identifier [known_discriminant_part] is type_definition;
   | task_type_declaration
   | protected_type_declaration

3.2.1:
type_definition ::= 
     enumeration_type_definition	| integer_type_definition
   | real_type_definition	| array_type_definition
   | record_type_definition	| access_type_definition
   | derived_type_definition

3.2.2:
subtype_declaration ::= 
   subtype defining_identifier is subtype_indication;

3.2.2:
subtype_indication ::=  subtype_mark [constraint]

3.2.2:
subtype_mark ::= subtype_name

3.2.2:
constraint ::= scalar_constraint | composite_constraint

3.2.2:
scalar_constraint ::= 
     range_constraint | digits_constraint | delta_constraint

3.2.2:
composite_constraint ::= 
     index_constraint | discriminant_constraint

3.3.1:
object_declaration ::= 
    defining_identifier_list : [aliased] [constant] subtype_indication [:= expression];
  | defining_identifier_list : [aliased] [constant] array_type_definition [:= expression];
  | single_task_declaration
  | single_protected_declaration

3.3.1:
defining_identifier_list ::= 
  defining_identifier {, defining_identifier}

3.3.2:
number_declaration ::= 
     defining_identifier_list : constant := static_expression;

3.4:
derived_type_definition ::= [abstract] new parent_subtype_indication [record_extension_part]

3.5:
range_constraint ::=  range range

3.5:
range ::=  range_attribute_reference
   | simple_expression .. simple_expression

3.5.1:
enumeration_type_definition ::= 
   (enumeration_literal_specification {, enumeration_literal_specification})

3.5.1:
enumeration_literal_specification ::=  defining_identifier | defining_character_literal

3.5.1:
defining_character_literal ::= character_literal

3.5.4:
integer_type_definition ::= signed_integer_type_definition | modular_type_definition

3.5.4:
signed_integer_type_definition ::= range static_simple_expression .. static_simple_expression

3.5.4:
modular_type_definition ::= mod static_expression

3.5.6:
real_type_definition ::= 
   floating_point_definition | fixed_point_definition

3.5.7:
floating_point_definition ::= 
  digits static_expression [real_range_specification]

3.5.7:
real_range_specification ::= 
  range static_simple_expression .. static_simple_expression

3.5.9:
fixed_point_definition ::= ordinary_fixed_point_definition | decimal_fixed_point_definition

3.5.9:
ordinary_fixed_point_definition ::= 
   delta static_expression  real_range_specification

3.5.9:
decimal_fixed_point_definition ::= 
   delta static_expression digits static_expression [real_range_specification]

3.5.9:
digits_constraint ::= 
   digits static_expression [range_constraint]

3.6:
array_type_definition ::= 
   unconstrained_array_definition | constrained_array_definition

3.6:
unconstrained_array_definition ::= 
   array(index_subtype_definition {, index_subtype_definition}) of component_definition

3.6:
index_subtype_definition ::= subtype_mark range &lt&gt

3.6:
constrained_array_definition ::= 
   array (discrete_subtype_definition {, discrete_subtype_definition}) of component_definition

3.6:
discrete_subtype_definition ::= discrete_subtype_indication | range

3.6:
component_definition ::= [aliased] subtype_indication

3.6.1:
index_constraint ::=  (discrete_range {, discrete_range})

3.6.1:
discrete_range ::= discrete_subtype_indication | range

3.7:
discriminant_part ::= unknown_discriminant_part | known_discriminant_part

3.7:
unknown_discriminant_part ::= (&lt&gt)

3.7:
known_discriminant_part ::= 
   (discriminant_specification {; discriminant_specification})

3.7:
discriminant_specification ::= 
   defining_identifier_list : subtype_mark [:= default_expression]
 | defining_identifier_list : access_definition [:= default_expression]

3.7:
default_expression ::= expression

3.7.1:
discriminant_constraint ::= 
   (discriminant_association {, discriminant_association})

3.7.1:
discriminant_association ::= 
   [discriminant_selector_name {| discriminant_selector_name} =&gt] expression

3.8:
record_type_definition ::= [[abstract] tagged] [limited] record_definition

3.8:
record_definition ::= 
    record
       component_list
    end record
  | null record

3.8:
component_list ::= 
      component_item {component_item}
   | {component_item} variant_part
   |  null;

3.8:
component_item ::= component_declaration | representation_clause

3.8:
component_declaration ::= 
   defining_identifier_list : component_definition [:= default_expression];

3.8.1:
variant_part ::= 
   case discriminant_direct_name is
       variant
      {variant}
   end case;

3.8.1:
variant ::= 
   when discrete_choice_list =&gt
      component_list

3.8.1:
discrete_choice_list ::= discrete_choice {| discrete_choice}

3.8.1:
discrete_choice ::= expression | discrete_range | others

3.9.1:
record_extension_part ::= with record_definition

3.9.3:
 ::= 

3.10:
access_type_definition ::= 
    access_to_object_definition
  | access_to_subprogram_definition

3.10:
access_to_object_definition ::= 
    access [general_access_modifier] subtype_indication

3.10:
general_access_modifier ::= all | constant

3.10:
access_to_subprogram_definition ::= 
    access [protected] procedure parameter_profile
  | access [protected] function  parameter_and_result_profile

3.10:
access_definition ::= access subtype_mark

3.10.1:
incomplete_type_declaration ::= type defining_identifier [discriminant_part];

3.11:
declarative_part ::= {declarative_item}

3.11:
declarative_item ::= 
    basic_declarative_item | body

3.11:
basic_declarative_item ::= 
    basic_declaration | representation_clause | use_clause

3.11:
body ::= proper_body | body_stub

3.11:
proper_body ::= 
    subprogram_body | package_body | task_body | protected_body

4.1:
name ::= 
     direct_name	| explicit_dereference
   | indexed_component	| slice
   | selected_component	| attribute_reference
   | type_conversion	| function_call
   | character_literal

4.1:
direct_name ::= identifier | operator_symbol

4.1:
prefix ::= name | implicit_dereference

4.1:
explicit_dereference ::= name.all

4.1:
implicit_dereference ::= name

4.1.1:
indexed_component ::= prefix(expression {, expression})

4.1.2:
slice ::= prefix(discrete_range)

4.1.3:
selected_component ::= prefix . selector_name

4.1.3:
selector_name ::= identifier | character_literal | operator_symbol

4.1.4:
attribute_reference ::= prefix'attribute_designator

4.1.4:
attribute_designator ::= 
    identifier[(static_expression)]
  | Access | Delta | Digits

4.1.4:
range_attribute_reference ::= prefix'range_attribute_designator

4.1.4:
range_attribute_designator ::= Range[(static_expression)]

4.3:
aggregate ::= record_aggregate | extension_aggregate | array_aggregate

4.3.1:
record_aggregate ::= (record_component_association_list)

4.3.1:
record_component_association_list ::= 
    record_component_association {, record_component_association}
  | null record

4.3.1:
record_component_association ::= 
    [component_choice_list =&gt] expression

4.3.1:
component_choice_list ::= 
     component_selector_name {| component_selector_name}
   | others

4.3.2:
extension_aggregate ::= 
    (ancestor_part with record_component_association_list)

4.3.2:
ancestor_part ::= expression | subtype_mark

4.3.3:
array_aggregate ::= 
    positional_array_aggregate | named_array_aggregate

4.3.3:
positional_array_aggregate ::= 
    (expression, expression {, expression})
  | (expression {, expression}, others =&gt expression)

4.3.3:
named_array_aggregate ::= 
    array_component_association {, array_component_association})

4.3.3:
array_component_association ::= 
    discrete_choice_list =&gt expression

4.4:
expression ::= 
     relation {and relation} 	| relation {and then relation}
   | relation {or relation} 	| relation {or else relation}
   | relation {xor relation}

4.4:
relation ::= 
     simple_expression [relational_operator simple_expression]
   | simple_expression [not] in range
   | simple_expression [not] in subtype_mark

4.4:
simple_expression ::= [unary_adding_operator] term {binary_adding_operator term}

4.4:
term ::= factor {multiplying_operator factor}

4.4:
factor ::= primary [** primary] | abs primary | not primary

4.4:
primary ::= 
    numeric_literal | null | string_literal | aggregate
  | name | qualified_expression | allocator | (expression)

4.5:
logical_operator ::= 	 and | or  | xor

4.5:
relational_operator ::= 	 =   | /=  | &lt   | &lt= | &gt | &gt=

4.5:
binary_adding_operator ::= 	 +   |    | &

4.5:
unary_adding_operator ::= 	 +   | 

4.5:
multiplying_operator ::= 	 *   | /   | mod | rem

4.5:
highest_precedence_operator ::= 	 **  | abs | not

4.6:
type_conversion ::= 
    subtype_mark(expression)
  | subtype_mark(name)

4.7:
qualified_expression ::= 
   subtype_mark'(expression) | subtype_mark'aggregate

4.8:
allocator ::= 
   new subtype_indication | new qualified_expression

5.1:
sequence_of_statements ::= statement {statement}

5.1:
statement ::= 
   {label} simple_statement | {label} compound_statement

5.1:
simple_statement ::= null_statement
   | assignment_statement	| exit_statement
   | goto_statement	| procedure_call_statement
   | return_statement	| entry_call_statement
   | requeue_statement	| delay_statement
   | abort_statement	| raise_statement
   | code_statement

5.1:
compound_statement ::= 
     if_statement	| case_statement
   | loop_statement	| block_statement
   | accept_statement	| select_statement

5.1:
null_statement ::= null;

5.1:
label ::= &lt&ltlabel_statement_identifier&gt&gt

5.1:
statement_identifier ::= direct_name

5.2:
assignment_statement ::= 
   variable_name := expression;

5.3:
if_statement ::= 
    if condition then
      sequence_of_statements
   {elsif condition then
      sequence_of_statements}
   [else
      sequence_of_statements]
    end if;

5.3:
condition ::= boolean_expression

5.4:
case_statement ::= 
   case expression is
       case_statement_alternative
      {case_statement_alternative}
   end case;

5.4:
case_statement_alternative ::= 
   when discrete_choice_list =&gt
      sequence_of_statements

5.5:
loop_statement ::= 
   [loop_statement_identifier:]
      [iteration_scheme] loop
         sequence_of_statements
       end loop [loop_identifier];

5.5:
iteration_scheme ::= while condition
   | for loop_parameter_specification

5.5:
loop_parameter_specification ::= 
   defining_identifier in [reverse] discrete_subtype_definition

5.6:
block_statement ::= 
   [block_statement_identifier:]
       [declare
            declarative_part]
        begin
            handled_sequence_of_statements
        end [block_identifier];

5.7:
exit_statement ::= 
   exit [loop_name] [when condition];

5.8:
goto_statement ::= goto label_name;

6.1:
subprogram_declaration ::= subprogram_specification;

6.1:
abstract_subprogram_declaration ::= subprogram_specification is abstract;

6.1:
subprogram_specification ::= 
    procedure defining_program_unit_name parameter_profile
  | function defining_designator parameter_and_result_profile

6.1:
designator ::= [parent_unit_name . ]identifier | operator_symbol

6.1:
defining_designator ::= defining_program_unit_name | defining_operator_symbol

6.1:
defining_program_unit_name ::= [parent_unit_name . ]defining_identifier

6.1:
operator_symbol ::= string_literal

6.1:
defining_operator_symbol ::= operator_symbol

6.1:
parameter_profile ::= [formal_part]

6.1:
parameter_and_result_profile ::= [formal_part] return subtype_mark

6.1:
formal_part ::= 
   (parameter_specification {; parameter_specification})

6.1:
parameter_specification ::= 
    defining_identifier_list : mode  subtype_mark [:= default_expression]
  | defining_identifier_list : access_definition [:= default_expression]

6.1:
mode ::= [in] | in out | out

6.3:
subprogram_body ::= 
    subprogram_specification is
       declarative_part
    begin
        handled_sequence_of_statements
    end [designator];

6.4:
procedure_call_statement ::= 
    procedure_name;
  | procedure_prefix actual_parameter_part;

6.4:
function_call ::= 
    function_name
  | function_prefix actual_parameter_part

6.4:
actual_parameter_part ::= 
    (parameter_association {, parameter_association})

6.4:
parameter_association ::= 
   [formal_parameter_selector_name =&gt] explicit_actual_parameter

6.4:
explicit_actual_parameter ::= expression | variable_name

6.5:
return_statement ::= return [expression];

7.1:
package_declaration ::= package_specification;

7.1:
package_specification ::= 
    package defining_program_unit_name is
      {basic_declarative_item}
   [private
      {basic_declarative_item}]
    end [[parent_unit_name.]identifier]

7.2:
package_body ::= 
    package body defining_program_unit_name is
       declarative_part
   [begin
        handled_sequence_of_statements]
    end [[parent_unit_name.]identifier];

7.3:
private_type_declaration ::= 
   type defining_identifier [discriminant_part] is [[abstract] tagged] [limited] private;

7.3:
private_extension_declaration ::= 
   type defining_identifier [discriminant_part] is
     [abstract]  new ancestor_subtype_indication with private;

8.4:
use_clause ::= use_package_clause | use_type_clause

8.4:
use_package_clause ::= use package_name {, package_name};

8.4:
use_type_clause ::= use type subtype_mark {, subtype_mark};

8.5:
renaming_declaration ::= 
      object_renaming_declaration
    | exception_renaming_declaration
    | package_renaming_declaration
    | subprogram_renaming_declaration
    | generic_renaming_declaration

8.5.1:
object_renaming_declaration ::= defining_identifier : subtype_mark renames object_name;

8.5.2:
exception_renaming_declaration ::= defining_identifier : exception renames exception_name;

8.5.3:
package_renaming_declaration ::= package defining_program_unit_name renames package_name;

8.5.4:
subprogram_renaming_declaration ::= subprogram_specification renames callable_entity_name;

8.5.5:
generic_renaming_declaration ::= 
    generic package	defining_program_unit_name renames generic_package_name;
  | generic procedure	defining_program_unit_name renames generic_procedure_name;
  | generic function	defining_program_unit_name renames generic_function_name;

9.1:
task_type_declaration ::= 
   task type defining_identifier [known_discriminant_part] [is task_definition];

9.1:
single_task_declaration ::= 
   task defining_identifier [is task_definition];

9.1:
task_definition ::= 
     {task_item}
  [ private
     {task_item}]
  end [task_identifier]

9.1:
task_item ::= entry_declaration | representation_clause

9.1:
task_body ::= 
   task body defining_identifier is
     declarative_part
   begin
     handled_sequence_of_statements
   end [task_identifier];

9.4:
protected_type_declaration ::= 
  protected type defining_identifier [known_discriminant_part] is protected_definition;

9.4:
single_protected_declaration ::= 
  protected defining_identifier is protected_definition;

9.4:
protected_definition ::= 
    { protected_operation_declaration }
[ private
    { protected_element_declaration } ]
  end [protected_identifier]

9.4:
protected_operation_declaration ::= subprogram_declaration
     | entry_declaration
     | representation_clause

9.4:
protected_element_declaration ::= protected_operation_declaration
     | component_declaration

9.4:
protected_body ::= 
  protected body defining_identifier is
   { protected_operation_item }
  end [protected_identifier];

9.4:
protected_operation_item ::= subprogram_declaration
     | subprogram_body
     | entry_body
     | representation_clause

9.5.2:
entry_declaration ::= 
   entry defining_identifier [(discrete_subtype_definition)] parameter_profile;

9.5.2:
accept_statement ::= 
   accept entry_direct_name [(entry_index)] parameter_profile [do
     handled_sequence_of_statements
   end [entry_identifier]];

9.5.2:
entry_index ::= expression

9.5.2:
entry_body ::= 
    entry defining_identifier entry_body_formal_partentry_barrier is
       declarative_part
    begin
       handled_sequence_of_statements
    end [entry_identifier];

9.5.2:
entry_body_formal_part ::= [(entry_index_specification)] parameter_profile

9.5.2:
entry_barrier ::= when condition

9.5.2:
entry_index_specification ::= for defining_identifier in discrete_subtype_definition

9.5.3:
entry_call_statement ::= entry_name [actual_parameter_part];

9.5.4:
requeue_statement ::= requeue entry_name [with abort];

9.6:
delay_statement ::= delay_until_statement | delay_relative_statement

9.6:
delay_until_statement ::= delay until delay_expression;

9.6:
delay_relative_statement ::= delay delay_expression;

9.7:
select_statement ::= 
   selective_accept
  | timed_entry_call
  | conditional_entry_call
  | asynchronous_select

9.7.1:
selective_accept ::= 
  select
   [guard]
     select_alternative
{ or
   [guard]
     select_alternative }
[ else
   sequence_of_statements ]
  end select;

9.7.1:
guard ::= when condition =&gt

9.7.1:
select_alternative ::= 
   accept_alternative
  | delay_alternative
  | terminate_alternative

9.7.1:
accept_alternative ::= 
  accept_statement [sequence_of_statements]

9.7.1:
delay_alternative ::= 
  delay_statement [sequence_of_statements]

9.7.1:
terminate_alternative ::= terminate;

9.7.2:
timed_entry_call ::= 
  select
   entry_call_alternative
  or
   delay_alternative
  end select;

9.7.2:
entry_call_alternative ::= 
  entry_call_statement [sequence_of_statements]

9.7.3:
conditional_entry_call ::= 
  select
   entry_call_alternative
  else
   sequence_of_statements
  end select;

9.7.4:
asynchronous_select ::= 
  select
   triggering_alternative
  then abort
   abortable_part
  end select;

9.7.4:
triggering_alternative ::= triggering_statement [sequence_of_statements]

9.7.4:
triggering_statement ::= entry_call_statement | delay_statement

9.7.4:
abortable_part ::= sequence_of_statements

9.8:
abort_statement ::= abort task_name {, task_name};

10.1.1:
compilation ::= {compilation_unit}

10.1.1:
compilation_unit ::= 
    context_clause library_item
  | context_clause subunit

10.1.1:
library_item ::= [private] library_unit_declaration
  | library_unit_body
  | [private] library_unit_renaming_declaration

10.1.1:
library_unit_declaration ::= 
     subprogram_declaration	| package_declaration
   | generic_declaration	| generic_instantiation

10.1.1:
library_unit_renaming_declaration ::= 
   package_renaming_declaration
 | generic_renaming_declaration
 | subprogram_renaming_declaration

10.1.1:
library_unit_body ::= subprogram_body | package_body

10.1.1:
parent_unit_name ::= name

10.1.2:
context_clause ::= {context_item}

10.1.2:
context_item ::= with_clause | use_clause

10.1.2:
with_clause ::= with library_unit_name {, library_unit_name};

10.1.3:
body_stub ::= 
   subprogram_body_stub | package_body_stub | task_body_stub | protected_body_stub

10.1.3:
subprogram_body_stub ::= subprogram_specification is separate;

10.1.3:
package_body_stub ::= package body defining_identifier is separate;

10.1.3:
task_body_stub ::= task body defining_identifier is separate;

10.1.3:
protected_body_stub ::= protected body defining_identifier is separate;

10.1.3:
subunit ::= separate (parent_unit_name) proper_body

11.1:
exception_declaration ::= defining_identifier_list : exception;

11.2:
handled_sequence_of_statements ::= 
     sequence_of_statements
  [exception
     exception_handler
    {exception_handler}]

11.2:
exception_handler ::= 
  when [choice_parameter_specification:] exception_choice {| exception_choice} =&gt
     sequence_of_statements

11.2:
choice_parameter_specification ::= defining_identifier

11.2:
exception_choice ::= exception_name | others

11.3:
raise_statement ::= raise [exception_name];

12.1:
generic_declaration ::= generic_subprogram_declaration | generic_package_declaration

12.1:
generic_subprogram_declaration ::= 
     generic_formal_part  subprogram_specification;

12.1:
generic_package_declaration ::= 
     generic_formal_part  package_specification;

12.1:
generic_formal_part ::= generic {generic_formal_parameter_declaration | use_clause}

12.1:
generic_formal_parameter_declaration ::= 
      formal_object_declaration
    | formal_type_declaration
    | formal_subprogram_declaration
    | formal_package_declaration

12.3:
generic_instantiation ::= 
     package defining_program_unit_name is
         new generic_package_name [generic_actual_part];
   | procedure defining_program_unit_name is
         new generic_procedure_name [generic_actual_part];
   | function defining_designator is
         new generic_function_name [generic_actual_part];

12.3:
generic_actual_part ::= 
   (generic_association {, generic_association})

12.3:
generic_association ::= 
   [generic_formal_parameter_selector_name =&gt] explicit_generic_actual_parameter

12.3:
explicit_generic_actual_parameter ::= expression | variable_name
   | subprogram_name | entry_name | subtype_mark
   | package_instance_name

12.4:
formal_object_declaration ::= 
    defining_identifier_list : mode subtype_mark [:= default_expression];

12.5:
formal_type_declaration ::= 
    type defining_identifier[discriminant_part] is formal_type_definition;

12.5:
formal_type_definition ::= 
      formal_private_type_definition
    | formal_derived_type_definition
    | formal_discrete_type_definition
    | formal_signed_integer_type_definition
    | formal_modular_type_definition
    | formal_floating_point_definition
    | formal_ordinary_fixed_point_definition
    | formal_decimal_fixed_point_definition
    | formal_array_type_definition
    | formal_access_type_definition

12.5.1:
formal_private_type_definition ::= [[abstract] tagged] [limited] private

12.5.1:
formal_derived_type_definition ::= [abstract] new subtype_mark [with private]

12.5.2:
formal_discrete_type_definition ::= (&lt&gt)

12.5.2:
formal_signed_integer_type_definition ::= range &lt&gt

12.5.2:
formal_modular_type_definition ::= mod &lt&gt

12.5.2:
formal_floating_point_definition ::= digits &lt&gt

12.5.2:
formal_ordinary_fixed_point_definition ::= delta &lt&gt

12.5.2:
formal_decimal_fixed_point_definition ::= delta &lt&gt digits &lt&gt

12.5.3:
formal_array_type_definition ::= array_type_definition

12.5.4:
formal_access_type_definition ::= access_type_definition

12.6:
formal_subprogram_declaration ::= with subprogram_specification [is subprogram_default];

12.6:
subprogram_default ::= default_name | &lt&gt

12.6:
default_name ::= name

12.7:
formal_package_declaration ::= 
    with package defining_identifier is new generic_package_name  formal_package_actual_part;

12.7:
formal_package_actual_part ::= 
    (&lt&gt) | [generic_actual_part]

13.1:
representation_clause ::= attribute_definition_clause
      | enumeration_representation_clause
      | record_representation_clause
      | at_clause

13.1:
local_name ::= direct_name
      | direct_name'attribute_designator
      | library_unit_name

13.3:
attribute_definition_clause ::= 
      for local_name'attribute_designator use expression;
    | for local_name'attribute_designator use name;

13.4:
enumeration_representation_clause ::= 
    for first_subtype_local_name use enumeration_aggregate;

13.4:
enumeration_aggregate ::= array_aggregate

13.5.1:
record_representation_clause ::= 
    for first_subtype_local_name use
       record [mod_clause]
          {component_clause}
       end record;

13.5.1:
component_clause ::= 
    component_local_name at position range first_bit .. last_bit;

13.5.1:
position ::= static_expression

13.5.1:
first_bit ::= static_simple_expression

13.5.1:
last_bit ::= static_simple_expression

13.8:
code_statement ::= qualified_expression;

13.12:
restriction ::= restriction_identifier
    | restriction_parameter_identifier =&gt expression

J.3:
delta_constraint ::= delta static_expression [range_constraint]

J.7:
at_clause ::= for direct_name use at expression;

J.8:
mod_clause ::= at mod static_expression;


## P.2  Syntax Cross Reference



abort_statement	9.8
	simple_statement	5.1

abortable_part	9.7.4
	asynchronous_select	9.7.4

abstract_subprogram_declaration	6.1
	basic_declaration	3.1

accept_alternative	9.7.1
	select_alternative	9.7.1

accept_statement	9.5.2
	accept_alternative	9.7.1
	compound_statement	5.1

access_definition	3.10
	discriminant_specification	3.7
	parameter_specification	6.1

access_to_object_definition	3.10
	access_type_definition	3.10

access_to_subprogram_definition	3.10
	access_type_definition	3.10

access_type_definition	3.10
	formal_access_type_definition	12.5.4
	type_definition	3.2.1

actual_parameter_part	6.4
	entry_call_statement	9.5.3
	function_call	6.4
	procedure_call_statement	6.4

aggregate	4.3
	primary	4.4
	qualified_expression	4.7

allocator	4.8
	primary	4.4

ancestor_part	4.3.2
	extension_aggregate	4.3.2

array_aggregate	4.3.3
	aggregate	4.3
	enumeration_aggregate	13.4

array_component_association	4.3.3
	named_array_aggregate	4.3.3

array_type_definition	3.6
	formal_array_type_definition	12.5.3
	object_declaration	3.3.1
	type_definition	3.2.1

assignment_statement	5.2
	simple_statement	5.1

asynchronous_select	9.7.4
	select_statement	9.7

at_clause	J.7
	representation_clause	13.1

attribute_definition_clause	13.3
	representation_clause	13.1

attribute_designator	4.1.4
	attribute_definition_clause	13.3
	attribute_reference	4.1.4
	local_name	13.1

attribute_reference	4.1.4
	name	4.1

base	2.4.2
	based_literal	2.4.2

based_literal	2.4.2
	numeric_literal	2.4

based_numeral	2.4.2
	based_literal	2.4.2

basic_declaration	3.1
	basic_declarative_item	3.11

basic_declarative_item	3.11
	declarative_item	3.11
	package_specification	7.1

binary_adding_operator	4.5
	simple_expression	4.4

block_statement	5.6
	compound_statement	5.1

body	3.11
	declarative_item	3.11

body_stub	10.1.3
	body	3.11

case_statement	5.4
	compound_statement	5.1

case_statement_alternative	5.4
	case_statement	5.4

character	2.1
	comment	2.7

character_literal	2.5
	defining_character_literal	3.5.1
	name	4.1
	selector_name	4.1.3

choice_parameter_specification	11.2
	exception_handler	11.2

code_statement	13.8
	simple_statement	5.1

compilation_unit	10.1.1
	compilation	10.1.1

component_choice_list	4.3.1
	record_component_association	4.3.1

component_clause	13.5.1
	record_representation_clause	13.5.1

component_declaration	3.8
	component_item	3.8
	protected_element_declaration	9.4

component_definition	3.6
	component_declaration	3.8
	constrained_array_definition	3.6
	unconstrained_array_definition	3.6

component_item	3.8
	component_list	3.8

component_list	3.8
	record_definition	3.8
	variant	3.8.1

composite_constraint	3.2.2
	constraint	3.2.2

compound_statement	5.1
	statement	5.1

condition	5.3
	entry_barrier	9.5.2
	exit_statement	5.7
	guard	9.7.1
	if_statement	5.3
	iteration_scheme	5.5

conditional_entry_call	9.7.3
	select_statement	9.7

constrained_array_definition	3.6
	array_type_definition	3.6

constraint	3.2.2
	subtype_indication	3.2.2

context_clause	10.1.2
	compilation_unit	10.1.1

context_item	10.1.2
	context_clause	10.1.2

decimal_fixed_point_definition	3.5.9
	fixed_point_definition	3.5.9

decimal_literal	2.4.1
	numeric_literal	2.4

declarative_item	3.11
	declarative_part	3.11

declarative_part	3.11
	block_statement	5.6
	entry_body	9.5.2
	package_body	7.2
	subprogram_body	6.3
	task_body	9.1

default_expression	3.7
	component_declaration	3.8
	discriminant_specification	3.7
	formal_object_declaration	12.4
	parameter_specification	6.1

default_name	12.6
	subprogram_default	12.6

defining_character_literal	3.5.1
	enumeration_literal_specification	3.5.1

defining_designator	6.1
	generic_instantiation	12.3
	subprogram_specification	6.1

defining_identifier	3.1
	choice_parameter_specification	11.2
	defining_identifier_list	3.3.1
	defining_program_unit_name	6.1
	entry_body	9.5.2
	entry_declaration	9.5.2
	entry_index_specification	9.5.2
	enumeration_literal_specification	3.5.1
	exception_renaming_declaration	8.5.2
	formal_package_declaration	12.7
	formal_type_declaration	12.5
	full_type_declaration	3.2.1
	incomplete_type_declaration	3.10.1
	loop_parameter_specification	5.5
	object_renaming_declaration	8.5.1
	package_body_stub	10.1.3
	private_extension_declaration	7.3
	private_type_declaration	7.3
	protected_body	9.4
	protected_body_stub	10.1.3
	protected_type_declaration	9.4
	single_protected_declaration	9.4
	single_task_declaration	9.1
	subtype_declaration	3.2.2
	task_body	9.1
	task_body_stub	10.1.3
	task_type_declaration	9.1

defining_identifier_list	3.3.1
	component_declaration	3.8
	discriminant_specification	3.7
	exception_declaration	11.1
	formal_object_declaration	12.4
	number_declaration	3.3.2
	object_declaration	3.3.1
	parameter_specification	6.1

defining_operator_symbol	6.1
	defining_designator	6.1

defining_program_unit_name	6.1
	defining_designator	6.1
	generic_instantiation	12.3
	generic_renaming_declaration	8.5.5
	package_body	7.2
	package_renaming_declaration	8.5.3
	package_specification	7.1
	subprogram_specification	6.1

delay_alternative	9.7.1
	select_alternative	9.7.1
	timed_entry_call	9.7.2

delay_relative_statement	9.6
	delay_statement	9.6

delay_statement	9.6
	delay_alternative	9.7.1
	simple_statement	5.1
	triggering_statement	9.7.4

delay_until_statement	9.6
	delay_statement	9.6

delta_constraint	J.3
	scalar_constraint	3.2.2

derived_type_definition	3.4
	type_definition	3.2.1

designator	6.1
	subprogram_body	6.3

digit	...
	extended_digit	2.4.2
	graphic_character	2.1
	letter_or_digit	2.3
	numeral	2.4.1

digits_constraint	3.5.9
	scalar_constraint	3.2.2

direct_name	4.1
	accept_statement	9.5.2
	at_clause	J.7
	local_name	13.1
	name	4.1
	statement_identifier	5.1
	variant_part	3.8.1

discrete_choice	3.8.1
	discrete_choice_list	3.8.1

discrete_choice_list	3.8.1
	array_component_association	4.3.3
	case_statement_alternative	5.4
	variant	3.8.1

discrete_range	3.6.1
	discrete_choice	3.8.1
	index_constraint	3.6.1
	slice	4.1.2

discrete_subtype_definition	3.6
	constrained_array_definition	3.6
	entry_declaration	9.5.2
	entry_index_specification	9.5.2
	loop_parameter_specification	5.5

discriminant_association	3.7.1
	discriminant_constraint	3.7.1

discriminant_constraint	3.7.1
	composite_constraint	3.2.2

discriminant_part	3.7
	formal_type_declaration	12.5
	incomplete_type_declaration	3.10.1
	private_extension_declaration	7.3
	private_type_declaration	7.3

discriminant_specification	3.7
	known_discriminant_part	3.7

entry_barrier	9.5.2
	entry_body	9.5.2

entry_body	9.5.2
	protected_operation_item	9.4

entry_body_formal_part	9.5.2
	entry_body	9.5.2

entry_call_alternative	9.7.2
	conditional_entry_call	9.7.3
	timed_entry_call	9.7.2

entry_call_statement	9.5.3
	entry_call_alternative	9.7.2
	simple_statement	5.1
	triggering_statement	9.7.4

entry_declaration	9.5.2
	protected_operation_declaration	9.4
	task_item	9.1

entry_index	9.5.2
	accept_statement	9.5.2

entry_index_specification	9.5.2
	entry_body_formal_part	9.5.2

enumeration_aggregate	13.4
	enumeration_representation_clause	13.4

enumeration_literal_specification	3.5.1
	enumeration_type_definition	3.5.1

enumeration_representation_clause	13.4
	representation_clause	13.1

enumeration_type_definition	3.5.1
	type_definition	3.2.1

exception_choice	11.2
	exception_handler	11.2

exception_declaration	11.1
	basic_declaration	3.1

exception_handler	11.2
	handled_sequence_of_statements	11.2

exception_renaming_declaration	8.5.2
	renaming_declaration	8.5

exit_statement	5.7
	simple_statement	5.1

explicit_actual_parameter	6.4
	parameter_association	6.4

explicit_dereference	4.1
	name	4.1

explicit_generic_actual_parameter	12.3
	generic_association	12.3

exponent	2.4.1
	based_literal	2.4.2
	decimal_literal	2.4.1

expression	4.4
	ancestor_part	4.3.2
	array_component_association	4.3.3
	assignment_statement	5.2
	at_clause	J.7
	attribute_definition_clause	13.3
	attribute_designator	4.1.4
	case_statement	5.4
	condition	5.3
	decimal_fixed_point_definition	3.5.9
	default_expression	3.7
	delay_relative_statement	9.6
	delay_until_statement	9.6
	delta_constraint	J.3
	digits_constraint	3.5.9
	discrete_choice	3.8.1
	discriminant_association	3.7.1
	entry_index	9.5.2
	explicit_actual_parameter	6.4
	explicit_generic_actual_parameter	12.3
	floating_point_definition	3.5.7
	indexed_component	4.1.1
	mod_clause	J.8
	modular_type_definition	3.5.4
	number_declaration	3.3.2
	object_declaration	3.3.1
	ordinary_fixed_point_definition	3.5.9
	position	13.5.1
	positional_array_aggregate	4.3.3
	pragma_argument_association	2.8
	primary	4.4
	qualified_expression	4.7
	range_attribute_designator	4.1.4
	record_component_association	4.3.1
	restriction	13.12
	return_statement	6.5
	type_conversion	4.6

extended_digit	2.4.2
	based_numeral	2.4.2

extension_aggregate	4.3.2
	aggregate	4.3

factor	4.4
	term	4.4

first_bit	13.5.1
	component_clause	13.5.1

fixed_point_definition	3.5.9
	real_type_definition	3.5.6

floating_point_definition	3.5.7
	real_type_definition	3.5.6

formal_access_type_definition	12.5.4
	formal_type_definition	12.5

formal_array_type_definition	12.5.3
	formal_type_definition	12.5

formal_decimal_fixed_point_definition	12.5.2
	formal_type_definition	12.5

formal_derived_type_definition	12.5.1
	formal_type_definition	12.5

formal_discrete_type_definition	12.5.2
	formal_type_definition	12.5

formal_floating_point_definition	12.5.2
	formal_type_definition	12.5

formal_modular_type_definition	12.5.2
	formal_type_definition	12.5

formal_object_declaration	12.4
	generic_formal_parameter_declaration	12.1

formal_ordinary_fixed_point_definition	12.5.2
	formal_type_definition	12.5

formal_package_actual_part	12.7
	formal_package_declaration	12.7

formal_package_declaration	12.7
	generic_formal_parameter_declaration	12.1

formal_part	6.1
	parameter_and_result_profile	6.1
	parameter_profile	6.1

formal_private_type_definition	12.5.1
	formal_type_definition	12.5

formal_signed_integer_type_definition	12.5.2
	formal_type_definition	12.5

formal_subprogram_declaration	12.6
	generic_formal_parameter_declaration	12.1

formal_type_declaration	12.5
	generic_formal_parameter_declaration	12.1

formal_type_definition	12.5
	formal_type_declaration	12.5

format_effector	...
	character	2.1

full_type_declaration	3.2.1
	type_declaration	3.2.1

function_call	6.4
	name	4.1

general_access_modifier	3.10
	access_to_object_definition	3.10

generic_actual_part	12.3
	formal_package_actual_part	12.7
	generic_instantiation	12.3

generic_association	12.3
	generic_actual_part	12.3

generic_declaration	12.1
	basic_declaration	3.1
	library_unit_declaration	10.1.1

generic_formal_parameter_declaration	12.1
	generic_formal_part	12.1

generic_formal_part	12.1
	generic_package_declaration	12.1
	generic_subprogram_declaration	12.1

generic_instantiation	12.3
	basic_declaration	3.1
	library_unit_declaration	10.1.1

generic_package_declaration	12.1
	generic_declaration	12.1

generic_renaming_declaration	8.5.5
	library_unit_renaming_declaration	10.1.1
	renaming_declaration	8.5

generic_subprogram_declaration	12.1
	generic_declaration	12.1

goto_statement	5.8
	simple_statement	5.1

graphic_character	2.1
	character	2.1
	character_literal	2.5
	string_element	2.6

guard	9.7.1
	selective_accept	9.7.1

handled_sequence_of_statements	11.2
	accept_statement	9.5.2
	block_statement	5.6
	entry_body	9.5.2
	package_body	7.2
	subprogram_body	6.3
	task_body	9.1

identifier	2.3
	accept_statement	9.5.2
	attribute_designator	4.1.4
	block_statement	5.6
	defining_identifier	3.1
	designator	6.1
	direct_name	4.1
	entry_body	9.5.2
	loop_statement	5.5
	package_body	7.2
	package_specification	7.1
	pragma	2.8
	pragma_argument_association	2.8
	protected_body	9.4
	protected_definition	9.4
	restriction	13.12
	selector_name	4.1.3
	task_body	9.1
	task_definition	9.1

identifier_letter	...
	graphic_character	2.1
	identifier	2.3
	letter_or_digit	2.3

if_statement	5.3
	compound_statement	5.1

implicit_dereference	4.1
	prefix	4.1

incomplete_type_declaration	3.10.1
	type_declaration	3.2.1

index_constraint	3.6.1
	composite_constraint	3.2.2

index_subtype_definition	3.6
	unconstrained_array_definition	3.6

indexed_component	4.1.1
	name	4.1

integer_type_definition	3.5.4
	type_definition	3.2.1

iteration_scheme	5.5
	loop_statement	5.5

known_discriminant_part	3.7
	discriminant_part	3.7
	full_type_declaration	3.2.1
	protected_type_declaration	9.4
	task_type_declaration	9.1

label	5.1
	statement	5.1

last_bit	13.5.1
	component_clause	13.5.1

letter_or_digit	2.3
	identifier	2.3

library_item	10.1.1
	compilation_unit	10.1.1

library_unit_body	10.1.1
	library_item	10.1.1

library_unit_declaration	10.1.1
	library_item	10.1.1

library_unit_renaming_declaration	10.1.1
	library_item	10.1.1

local_name	13.1
	attribute_definition_clause	13.3
	component_clause	13.5.1
	enumeration_representation_clause	13.4
	record_representation_clause	13.5.1

loop_parameter_specification	5.5
	iteration_scheme	5.5

loop_statement	5.5
	compound_statement	5.1

mod_clause	J.8
	record_representation_clause	13.5.1

mode	6.1
	formal_object_declaration	12.4
	parameter_specification	6.1

modular_type_definition	3.5.4
	integer_type_definition	3.5.4

multiplying_operator	4.5
	term	4.4

name	4.1
	abort_statement	9.8
	assignment_statement	5.2
	attribute_definition_clause	13.3
	default_name	12.6
	entry_call_statement	9.5.3
	exception_choice	11.2
	exception_renaming_declaration	8.5.2
	exit_statement	5.7
	explicit_actual_parameter	6.4
	explicit_dereference	4.1
	explicit_generic_actual_parameter	12.3
	formal_package_declaration	12.7
	function_call	6.4
	generic_instantiation	12.3
	generic_renaming_declaration	8.5.5
	goto_statement	5.8
	implicit_dereference	4.1
	local_name	13.1
	object_renaming_declaration	8.5.1
	package_renaming_declaration	8.5.3
	parent_unit_name	10.1.1
	pragma_argument_association	2.8
	prefix	4.1
	primary	4.4
	procedure_call_statement	6.4
	raise_statement	11.3
	requeue_statement	9.5.4
	subprogram_renaming_declaration	8.5.4
	subtype_mark	3.2.2
	type_conversion	4.6
	use_package_clause	8.4
	with_clause	10.1.2

named_array_aggregate	4.3.3
	array_aggregate	4.3.3

null_statement	5.1
	simple_statement	5.1

number_declaration	3.3.2
	basic_declaration	3.1

numeral	2.4.1
	base	2.4.2
	decimal_literal	2.4.1
	exponent	2.4.1

numeric_literal	2.4
	primary	4.4

object_declaration	3.3.1
	basic_declaration	3.1

object_renaming_declaration	8.5.1
	renaming_declaration	8.5

operator_symbol	6.1
	defining_operator_symbol	6.1
	designator	6.1
	direct_name	4.1
	selector_name	4.1.3

ordinary_fixed_point_definition	3.5.9
	fixed_point_definition	3.5.9

other_control_function	...
	character	2.1

package_body	7.2
	library_unit_body	10.1.1
	proper_body	3.11

package_body_stub	10.1.3
	body_stub	10.1.3

package_declaration	7.1
	basic_declaration	3.1
	library_unit_declaration	10.1.1

package_renaming_declaration	8.5.3
	library_unit_renaming_declaration	10.1.1
	renaming_declaration	8.5

package_specification	7.1
	generic_package_declaration	12.1
	package_declaration	7.1

parameter_and_result_profile	6.1
	access_to_subprogram_definition	3.10
	subprogram_specification	6.1

parameter_association	6.4
	actual_parameter_part	6.4

parameter_profile	6.1
	accept_statement	9.5.2
	access_to_subprogram_definition	3.10
	entry_body_formal_part	9.5.2
	entry_declaration	9.5.2
	subprogram_specification	6.1

parameter_specification	6.1
	formal_part	6.1

parent_unit_name	10.1.1
	defining_program_unit_name	6.1
	designator	6.1
	package_body	7.2
	package_specification	7.1
	subunit	10.1.3

position	13.5.1
	component_clause	13.5.1

positional_array_aggregate	4.3.3
	array_aggregate	4.3.3

pragma_argument_association	2.8
	pragma	2.8

prefix	4.1
	attribute_reference	4.1.4
	function_call	6.4
	indexed_component	4.1.1
	procedure_call_statement	6.4
	range_attribute_reference	4.1.4
	selected_component	4.1.3
	slice	4.1.2

primary	4.4
	factor	4.4

private_extension_declaration	7.3
	type_declaration	3.2.1

private_type_declaration	7.3
	type_declaration	3.2.1

procedure_call_statement	6.4
	simple_statement	5.1

proper_body	3.11
	body	3.11
	subunit	10.1.3

protected_body	9.4
	proper_body	3.11

protected_body_stub	10.1.3
	body_stub	10.1.3

protected_definition	9.4
	protected_type_declaration	9.4
	single_protected_declaration	9.4

protected_element_declaration	9.4
	protected_definition	9.4

protected_operation_declaration	9.4
	protected_definition	9.4
	protected_element_declaration	9.4

protected_operation_item	9.4
	protected_body	9.4

protected_type_declaration	9.4
	full_type_declaration	3.2.1

qualified_expression	4.7
	allocator	4.8
	code_statement	13.8
	primary	4.4

raise_statement	11.3
	simple_statement	5.1

range	3.5
	discrete_range	3.6.1
	discrete_subtype_definition	3.6
	range_constraint	3.5
	relation	4.4

range_attribute_designator	4.1.4
	range_attribute_reference	4.1.4

range_attribute_reference	4.1.4
	range	3.5

range_constraint	3.5
	delta_constraint	J.3
	digits_constraint	3.5.9
	scalar_constraint	3.2.2

real_range_specification	3.5.7
	decimal_fixed_point_definition	3.5.9
	floating_point_definition	3.5.7
	ordinary_fixed_point_definition	3.5.9

real_type_definition	3.5.6
	type_definition	3.2.1

record_aggregate	4.3.1
	aggregate	4.3

record_component_association	4.3.1
	record_component_association_list	4.3.1

record_component_association_list	4.3.1
	extension_aggregate	4.3.2
	record_aggregate	4.3.1

record_definition	3.8
	record_extension_part	3.9.1
	record_type_definition	3.8

record_extension_part	3.9.1
	derived_type_definition	3.4

record_representation_clause	13.5.1
	representation_clause	13.1

record_type_definition	3.8
	type_definition	3.2.1

relation	4.4
	expression	4.4

relational_operator	4.5
	relation	4.4

renaming_declaration	8.5
	basic_declaration	3.1

representation_clause	13.1
	basic_declarative_item	3.11
	component_item	3.8
	protected_operation_declaration	9.4
	protected_operation_item	9.4
	task_item	9.1

requeue_statement	9.5.4
	simple_statement	5.1

return_statement	6.5
	simple_statement	5.1

scalar_constraint	3.2.2
	constraint	3.2.2

select_alternative	9.7.1
	selective_accept	9.7.1

select_statement	9.7
	compound_statement	5.1

selected_component	4.1.3
	name	4.1

selective_accept	9.7.1
	select_statement	9.7

selector_name	4.1.3
	component_choice_list	4.3.1
	discriminant_association	3.7.1
	generic_association	12.3
	parameter_association	6.4
	selected_component	4.1.3

sequence_of_statements	5.1
	abortable_part	9.7.4
	accept_alternative	9.7.1
	case_statement_alternative	5.4
	conditional_entry_call	9.7.3
	delay_alternative	9.7.1
	entry_call_alternative	9.7.2
	exception_handler	11.2
	handled_sequence_of_statements	11.2
	if_statement	5.3
	loop_statement	5.5
	selective_accept	9.7.1
	triggering_alternative	9.7.4

signed_integer_type_definition	3.5.4
	integer_type_definition	3.5.4

simple_expression	4.4
	first_bit	13.5.1
	last_bit	13.5.1
	range	3.5
	real_range_specification	3.5.7
	relation	4.4
	signed_integer_type_definition	3.5.4

simple_statement	5.1
	statement	5.1

single_protected_declaration	9.4
	object_declaration	3.3.1

single_task_declaration	9.1
	object_declaration	3.3.1

slice	4.1.2
	name	4.1

space_character	...
	graphic_character	2.1

special_character	...
	graphic_character	2.1

statement	5.1
	sequence_of_statements	5.1

statement_identifier	5.1
	block_statement	5.6
	label	5.1
	loop_statement	5.5

string_element	2.6
	string_literal	2.6

string_literal	2.6
	operator_symbol	6.1
	primary	4.4

subprogram_body	6.3
	library_unit_body	10.1.1
	proper_body	3.11
	protected_operation_item	9.4

subprogram_body_stub	10.1.3
	body_stub	10.1.3

subprogram_declaration	6.1
	basic_declaration	3.1
	library_unit_declaration	10.1.1
	protected_operation_declaration	9.4
	protected_operation_item	9.4

subprogram_default	12.6
	formal_subprogram_declaration	12.6

subprogram_renaming_declaration	8.5.4
	library_unit_renaming_declaration	10.1.1
	renaming_declaration	8.5

subprogram_specification	6.1
	abstract_subprogram_declaration	6.1
	formal_subprogram_declaration	12.6
	generic_subprogram_declaration	12.1
	subprogram_body	6.3
	subprogram_body_stub	10.1.3
	subprogram_declaration	6.1
	subprogram_renaming_declaration	8.5.4

subtype_declaration	3.2.2
	basic_declaration	3.1

subtype_indication	3.2.2
	access_to_object_definition	3.10
	allocator	4.8
	component_definition	3.6
	derived_type_definition	3.4
	discrete_range	3.6.1
	discrete_subtype_definition	3.6
	object_declaration	3.3.1
	private_extension_declaration	7.3
	subtype_declaration	3.2.2

subtype_mark	3.2.2
	access_definition	3.10
	ancestor_part	4.3.2
	discriminant_specification	3.7
	explicit_generic_actual_parameter	12.3
	formal_derived_type_definition	12.5.1
	formal_object_declaration	12.4
	index_subtype_definition	3.6
	object_renaming_declaration	8.5.1
	parameter_and_result_profile	6.1
	parameter_specification	6.1
	qualified_expression	4.7
	relation	4.4
	subtype_indication	3.2.2
	type_conversion	4.6
	use_type_clause	8.4

subunit	10.1.3
	compilation_unit	10.1.1

task_body	9.1
	proper_body	3.11

task_body_stub	10.1.3
	body_stub	10.1.3

task_definition	9.1
	single_task_declaration	9.1
	task_type_declaration	9.1

task_item	9.1
	task_definition	9.1

task_type_declaration	9.1
	full_type_declaration	3.2.1

term	4.4
	simple_expression	4.4

terminate_alternative	9.7.1
	select_alternative	9.7.1

timed_entry_call	9.7.2
	select_statement	9.7

triggering_alternative	9.7.4
	asynchronous_select	9.7.4

triggering_statement	9.7.4
	triggering_alternative	9.7.4

type_conversion	4.6
	name	4.1

type_declaration	3.2.1
	basic_declaration	3.1

type_definition	3.2.1
	full_type_declaration	3.2.1

unary_adding_operator	4.5
	simple_expression	4.4

unconstrained_array_definition	3.6
	array_type_definition	3.6

underline	...
	based_numeral	2.4.2
	identifier	2.3
	numeral	2.4.1

unknown_discriminant_part	3.7
	discriminant_part	3.7

use_clause	8.4
	basic_declarative_item	3.11
	context_item	10.1.2
	generic_formal_part	12.1

use_package_clause	8.4
	use_clause	8.4

use_type_clause	8.4
	use_clause	8.4

variant	3.8.1
	variant_part	3.8.1

variant_part	3.8.1
	component_list	3.8

with_clause	10.1.2
	context_item	10.1.2

 

Ada   A.2(2)

Address_To_Access_Conversions   child of System   13.7.2(2)

ASCII   in Standard   A.1

Asynchronous_Task_Control   child of Ada   D.11(3)

Bounded   child of Ada.Strings   A.4.4(3)

C   child of Interfaces   B.3(4)

Calendar   child of Ada   9.6(10)

Characters   child of Ada   A.3.1(2)

COBOL   child of Interfaces   B.4(7)

Command_Line   child of Ada   A.15(3)

Complex_Elementary_Functions   child of Ada.Numerics   G.1.2(9)

Complex_Types   child of Ada.Numerics   G.1.1(25)

Complex_IO   child of Ada.Text_IO   G.1.3(3)   child of Ada.Wide_Text_IO   G.1.4(1)

Constants   child of Ada.Strings.Maps   A.4.6(3)

Decimal   child of Ada   F.2(2)

Decimal_Conversions   in Interfaces.COBOL   B.4(31)

Decimal_IO   in Ada.Text_IO   A.10.1(73)

Decimal_Output   in Ada.Text_IO.Editing   F.3.3(11)

Direct_IO   child of Ada   A.8.4(2)

Discrete_Random   child of Ada.Numerics   A.5.2(17)

Dynamic_Priorities   child of Ada   D.5(3)

Editing   child of Ada.Text_IO   F.3.3(3)   child of Ada.Wide_Text_IO   F.3.4(1)

Elementary_Functions   child of Ada.Numerics   A.5.1(9)

Enumeration_IO   in Ada.Text_IO   A.10.1(79)

Exceptions   child of Ada   11.4.1(2)

Finalization   child of Ada   7.6(5)

Fixed   child of Ada.Strings   A.4.3(5)

Fixed_IO   in Ada.Text_IO   A.10.1(68)

Float_Random   child of Ada.Numerics   A.5.2(5)

Float_Text_IO   child of Ada   A.10.9(33)

Float_Wide_Text_IO   child of Ada   A.11(3)

Float_IO   in Ada.Text_IO   A.10.1(63)

Fortran   child of Interfaces   B.5(4)

Generic_Complex_Elementary_Functions   child of Ada.Numerics   G.1.2(2)

Generic_Complex_Types   child of Ada.Numerics   G.1.1(2)

Generic_Elementary_Functions   child of Ada.Numerics   A.5.1(3)

Generic_Bounded_Length   in Ada.Strings.Bounded   A.4.4(4)

Handling   child of Ada.Characters   A.3.2(2)

Integer_Text_IO   child of Ada   A.10.8(21)

Integer_Wide_Text_IO   child of Ada   A.11(3)

Integer_IO   in Ada.Text_IO   A.10.1(52)

Interfaces   B.2(3)

Interrupts   child of Ada   C.3.2(2)

IO_Exceptions   child of Ada   A.13(3)

Latin_1   child of Ada.Characters   A.3.3(3)

Machine_Code   child of System   13.8(7)

Maps   child of Ada.Strings   A.4.2(3)

Modular_IO   in Ada.Text_IO   A.10.1(57)

Names   child of Ada.Interrupts   C.3.2(12)

Numerics   child of Ada   A.5(3)

Pointers   child of Interfaces.C   B.3.2(4)

Real_Time   child of Ada   D.8(3)

RPC   child of System   E.5(3)

Sequential_IO   child of Ada   A.8.1(2)

Single_Precision_Complex_Types   in Interfaces.Fortran   B.5(8)

Standard   A.1(4)

Storage_Elements   child of System   13.7.1(2)

Storage_IO   child of Ada   A.9(3)

Storage_Pools   child of System   13.11(6)

Stream_IO   child of Ada.Streams   A.12.1(3)

Streams   child of Ada   13.13.1(2)

Strings   child of Ada   A.4.1(3)   child of Interfaces.C   B.3.1(3)

Synchronous_Task_Control   child of Ada   D.10(3)

System   13.7(3)

Tags   child of Ada   3.9(7)

Task_Attributes   child of Ada   C.7.2(2)

Task_Identification   child of Ada   C.7.1(2)

Text_Streams   child of Ada.Text_IO   A.12.2(3)   child of Ada.Wide_Text_IO   A.12.3(3)

Text_IO   child of Ada   A.10.1(2)

Unbounded   child of Ada.Strings   A.4.5(3)

Wide_Bounded   child of Ada.Strings   A.4.7(1)

Wide_Constants   child of Ada.Strings.Wide_Maps   A.4.7(1)

Wide_Fixed   child of Ada.Strings   A.4.7(1)

Wide_Hash   child of Ada.Strings   A.4.7(1)

Wide_Maps   child of Ada.Strings   A.4.7(3)

Wide_Text_IO   child of Ada   A.11(2)

Wide_Unbounded   child of Ada.Strings   A.4.7(1)

 

Address   in System   13.7(12)

Alignment   in Ada.Strings   A.4.1(6)

Alphanumeric   in Interfaces.COBOL   B.4(16)

Any_Priority subtype of Integer   in System   13.7(16)

Attribute_Handle   in Ada.Task_Attributes   C.7.2(3)

Binary   in Interfaces.COBOL   B.4(10)

Binary_Format   in Interfaces.COBOL   B.4(24)

Bit_Order   in System   13.7(15)

Boolean   in Standard   A.1(5)

Bounded_String   in Ada.Strings.Bounded   A.4.4(6)

Buffer_Type subtype of Storage_Array   in Ada.Storage_IO   A.9(4)

Byte   in Interfaces.COBOL   B.4(29)

Byte_Array   in Interfaces.COBOL   B.4(29)

C_float   in Interfaces.C   B.3(15)

char   in Interfaces.C   B.3(19)

char_array   in Interfaces.C   B.3(23)

char_array_access   in Interfaces.C.Strings   B.3.1(4)

Character   in Standard   A.1(35)

Character_Mapping   in Ada.Strings.Maps   A.4.2(20)

Character_Mapping_Function   in Ada.Strings.Maps   A.4.2(25)

Character_Range   in Ada.Strings.Maps   A.4.2(6)

Character_Ranges   in Ada.Strings.Maps   A.4.2(7)

Character_Sequence subtype of String   in Ada.Strings.Maps   A.4.2(16)

Character_Set   in Ada.Strings.Maps   A.4.2(4)   in Interfaces.Fortran   B.5(11)

chars_ptr   in Interfaces.C.Strings   B.3.1(5)

chars_ptr_array   in Interfaces.C.Strings   B.3.1(6)

COBOL_Character   in Interfaces.COBOL   B.4(13)

Complex   in Ada.Numerics.Generic_Complex_Types   G.1.1(3)   in Interfaces.Fortran   B.5(9)

Controlled   in Ada.Finalization   7.6(6)

Count   in Ada.Direct_IO   A.8.4(4)   in Ada.Streams.Stream_IO   A.12.1(7)   in Ada.Text_IO   A.10.1(5)

Day_Duration subtype of Duration   in Ada.Calendar   9.6(11)

Day_Number subtype of Integer   in Ada.Calendar   9.6(11)

Decimal_Element   in Interfaces.COBOL   B.4(12)

Direction   in Ada.Strings   A.4.1(6)

Display_Format   in Interfaces.COBOL   B.4(22)

double   in Interfaces.C   B.3(16)

Double_Precision   in Interfaces.Fortran   B.5(6)

Duration   in Standard   A.1(43)

Exception_Id   in Ada.Exceptions   11.4.1(2)

Exception_Occurrence   in Ada.Exceptions   11.4.1(3)

Exception_Occurrence_Access   in Ada.Exceptions   11.4.1(3)

Exit_Status   in Ada.Command_Line   A.15(7)

Field subtype of Integer   in Ada.Text_IO   A.10.1(6)

File_Access   in Ada.Text_IO   A.10.1(18)

File_Mode   in Ada.Direct_IO   A.8.4(4)   in Ada.Sequential_IO   A.8.1(4)   in Ada.Streams.Stream_IO   A.12.1(6)   in Ada.Text_IO   A.10.1(4)

File_Type   in Ada.Direct_IO   A.8.4(3)   in Ada.Sequential_IO   A.8.1(3)   in Ada.Streams.Stream_IO   A.12.1(5)   in Ada.Text_IO   A.10.1(3)

Float   in Standard   A.1(21)

Floating   in Interfaces.COBOL   B.4(9)

Fortran_Character   in Interfaces.Fortran   B.5(12)

Fortran_Integer   in Interfaces.Fortran   B.5(5)

Generator   in Ada.Numerics.Discrete_Random   A.5.2(19)   in Ada.Numerics.Float_Random   A.5.2(7)

Imaginary   in Ada.Numerics.Generic_Complex_Types   G.1.1(4)

Imaginary subtype of Imaginary   in Interfaces.Fortran   B.5(10)

int   in Interfaces.C   B.3(7)

Integer   in Standard   A.1(12)

Integer_Address   in System.Storage_Elements   13.7.1(10)

Interrupt_Id   in Ada.Interrupts   C.3.2(2)

Interrupt_Priority subtype of Any_Priority   in System   13.7(16)

ISO_646 subtype of Character   in Ada.Characters.Handling   A.3.2(9)

Length_Range subtype of Natural   in Ada.Strings.Bounded   A.4.4(8)

Limited_Controlled   in Ada.Finalization   7.6(8)

Logical   in Interfaces.Fortran   B.5(7)

long   in Interfaces.C   B.3(7)

Long_Binary   in Interfaces.COBOL   B.4(10)

long_double   in Interfaces.C   B.3(17)

Long_Floating   in Interfaces.COBOL   B.4(9)

Membership   in Ada.Strings   A.4.1(6)

Month_Number subtype of Integer   in Ada.Calendar   9.6(11)

Name   in System   13.7(4)

Natural subtype of Integer   in Standard   A.1(13)

Number_Base subtype of Integer   in Ada.Text_IO   A.10.1(6)

Numeric   in Interfaces.COBOL   B.4(20)

Packed_Decimal   in Interfaces.COBOL   B.4(12)

Packed_Format   in Interfaces.COBOL   B.4(26)

Parameterless_Handler   in Ada.Interrupts   C.3.2(2)

Params_Stream_Type   in System.RPC   E.5(6)

Partition_Id   in System.RPC   E.5(4)

Picture   in Ada.Text_IO.Editing   F.3.3(4)

plain_char   in Interfaces.C   B.3(11)

Pointer   in Interfaces.C.Pointers   B.3.2(5)

Positive subtype of Integer   in Standard   A.1(13)

Positive_Count subtype of Count   in Ada.Direct_IO   A.8.4(4)   in Ada.Streams.Stream_IO   A.12.1(7)   in Ada.Text_IO   A.10.1(5)

Priority subtype of Any_Priority   in System   13.7(16)

ptrdiff_t   in Interfaces.C   B.3(12)

Real   in Interfaces.Fortran   B.5(6)

Root_Storage_Pool   in System.Storage_Pools   13.11(7)

Root_Stream_Type   in Ada.Streams   13.13.1(3)

RPC_Receiver   in System.RPC   E.5(11)

Seconds_Count   in Ada.Real_Time   D.8(15)

short   in Interfaces.C   B.3(7)

signed_char   in Interfaces.C   B.3(8)

size_t   in Interfaces.C   B.3(13)

State   in Ada.Numerics.Discrete_Random   A.5.2(23)   in Ada.Numerics.Float_Random   A.5.2(11)

Storage_Array   in System.Storage_Elements   13.7.1(5)

Storage_Count subtype of Storage_Offset   in System.Storage_Elements   13.7.1(4)

Storage_Element   in System.Storage_Elements   13.7.1(5)

Storage_Offset   in System.Storage_Elements   13.7.1(3)

Stream_Access   in Ada.Streams.Stream_IO   A.12.1(4)   in Ada.Text_IO.Text_Streams   A.12.2(3)   in Ada.Wide_Text_IO.Text_Streams   A.12.3(3)

Stream_Element   in Ada.Streams   13.13.1(4)

Stream_Element_Array   in Ada.Streams   13.13.1(4)

Stream_Element_Count subtype of Stream_Element_Offset   in Ada.Streams   13.13.1(4)

Stream_Element_Offset   in Ada.Streams   13.13.1(4)

String   in Standard   A.1(37)

String_Access   in Ada.Strings.Unbounded   A.4.5(7)

Suspension_Object   in Ada.Synchronous_Task_Control   D.10(4)

Tag   in Ada.Tags   3.9(7)

Task_Id   in Ada.Task_Identification   C.7.1(2)

Time   in Ada.Calendar   9.6(10)   in Ada.Real_Time   D.8(4)

Time_Span   in Ada.Real_Time   D.8(5)

Trim_End   in Ada.Strings   A.4.1(6)

Truncation   in Ada.Strings   A.4.1(6)

Type_Set   in Ada.Text_IO   A.10.1(7)

Unbounded_String   in Ada.Strings.Unbounded   A.4.5(4)

Uniformly_Distributed subtype of Float   in Ada.Numerics.Float_Random   A.5.2(8)

unsigned   in Interfaces.C   B.3(9)

unsigned_char   in Interfaces.C   B.3(10)

unsigned_long   in Interfaces.C   B.3(9)

unsigned_short   in Interfaces.C   B.3(9)

wchar_array   in Interfaces.C   B.3(33)

wchar_t   in Interfaces.C   B.3(30)

Wide_Character   in Standard   A.1

Wide_Character_Mapping   in Ada.Strings.Wide_Maps   A.4.7(20)

Wide_Character_Mapping_Function   in Ada.Strings.Wide_Maps   A.4.7(26)

Wide_Character_Range   in Ada.Strings.Wide_Maps   A.4.7(6)

Wide_Character_Ranges   in Ada.Strings.Wide_Maps   A.4.7(7)

Wide_Character_Sequence subtype of Wide_String   in Ada.Strings.Wide_Maps   A.4.7(16)

Wide_Character_Set   in Ada.Strings.Wide_Maps   A.4.7(4)

Wide_String   in Standard   A.1(41)

Year_Number subtype of Integer   in Ada.Calendar   9.6(11)

 

Abort_Task in Ada.Task_Identification   C.7.1(3)

Adjust in Ada.Finalization   7.6(7)

Allocate in System.Storage_Pools   13.11(8)

Append   in Ada.Strings.Bounded   A.4.4(13), A.4.4(14), A.4.4(15), A.4.4(16), A.4.4(17), A.4.4(18), A.4.4(19), A.4.4(20)   in Ada.Strings.Unbounded   A.4.5(12), A.4.5(13), A.4.5(14)

Arccos   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arccosh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Arccot   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arccoth   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Arcsin   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arcsinh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Arctan   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arctanh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Argument   in Ada.Command_Line   A.15(5)   in Ada.Numerics.Generic_Complex_Types   G.1.1(10)

Argument_Count in Ada.Command_Line   A.15(4)

Attach_Handler in Ada.Interrupts   C.3.2(7)

Blank_When_Zero   in Ada.Text_IO.Editing   F.3.3(7)

Clock   in Ada.Calendar   9.6(12)   in Ada.Real_Time   D.8(6)

Close   in Ada.Direct_IO   A.8.4(8)   in Ada.Sequential_IO   A.8.1(8)   in Ada.Streams.Stream_IO   A.12.1(10)   in Ada.Text_IO   A.10.1(11)

Col in Ada.Text_IO   A.10.1(37)

Command_Name in Ada.Command_Line   A.15(6)

Compose_From_Cartesian   in Ada.Numerics.Generic_Complex_Types   G.1.1(8)

Compose_From_Polar   in Ada.Numerics.Generic_Complex_Types   G.1.1(11)

Conjugate   in Ada.Numerics.Generic_Complex_Types   G.1.1(12), G.1.1(15)

Continue   in Ada.Asynchronous_Task_Control   D.11(3)

Copy_Array in Interfaces.C.Pointers   B.3.2(15)

Copy_Terminated_Array   in Interfaces.C.Pointers   B.3.2(14)

Cos   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

Cosh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Cot   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

Coth   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Count   in Ada.Strings.Bounded   A.4.4(48), A.4.4(49), A.4.4(50)   in Ada.Strings.Fixed   A.4.3(13), A.4.3(14), A.4.3(15)   in Ada.Strings.Unbounded   A.4.5(43), A.4.5(44), A.4.5(45)

Create   in Ada.Direct_IO   A.8.4(6)   in Ada.Sequential_IO   A.8.1(6)   in Ada.Streams.Stream_IO   A.12.1(8)   in Ada.Text_IO   A.10.1(9)

Current_Error in Ada.Text_IO   A.10.1(17), A.10.1(20)

Current_Handler in Ada.Interrupts   C.3.2(6)

Current_Input in Ada.Text_IO   A.10.1(17), A.10.1(20)

Current_Output in Ada.Text_IO   A.10.1(17), A.10.1(20)

Current_State   in Ada.Synchronous_Task_Control   D.10(4)

Current_Task   in Ada.Task_Identification   C.7.1(3)

Day in Ada.Calendar   9.6(13)

Deallocate in System.Storage_Pools   13.11(9)

Decrement in Interfaces.C.Pointers   B.3.2(11)

Delete   in Ada.Direct_IO   A.8.4(8)   in Ada.Sequential_IO   A.8.1(8)   in Ada.Streams.Stream_IO   A.12.1(10)   in Ada.Strings.Bounded   A.4.4(64), A.4.4(65)   in Ada.Strings.Fixed   A.4.3(29), A.4.3(30)   in Ada.Strings.Unbounded   A.4.5(59), A.4.5(60)   in Ada.Text_IO   A.10.1(11)

Dereference_Error   in Interfaces.C.Strings   B.3.1(12)

Detach_Handler in Ada.Interrupts   C.3.2(9)

Divide in Ada.Decimal   F.2(6)

Do_APC in System.RPC   E.5(10)

Do_RPC in System.RPC   E.5(9)

Element   in Ada.Strings.Bounded   A.4.4(26)   in Ada.Strings.Unbounded   A.4.5(20)

End_Of_File   in Ada.Direct_IO   A.8.4(16)   in Ada.Sequential_IO   A.8.1(13)   in Ada.Streams.Stream_IO   A.12.1(12)   in Ada.Text_IO   A.10.1(34)

End_Of_Line in Ada.Text_IO   A.10.1(30)

End_Of_Page in Ada.Text_IO   A.10.1(33)

Establish_RPC_Receiver in System.RPC   E.5(12)

Exception_Identity in Ada.Exceptions   11.4.1(5)

Exception_Information   in Ada.Exceptions   11.4.1(5)

Exception_Message in Ada.Exceptions   11.4.1(4)

Exception_Name in Ada.Exceptions   11.4.1(2), 11.4.1(5)

Exchange_Handler in Ada.Interrupts   C.3.2(8)

Exp   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)

Expanded_Name in Ada.Tags   3.9(8)

External_Tag in Ada.Tags   3.9(8)

Finalize in Ada.Finalization   7.6(7), 7.6(9)

Find_Token   in Ada.Strings.Bounded   A.4.4(51)   in Ada.Strings.Fixed   A.4.3(16)   in Ada.Strings.Unbounded   A.4.5(46)

Flush   in Ada.Streams.Stream_IO   A.12.1(25)   in Ada.Text_IO   A.10.1(21)

Form   in Ada.Direct_IO   A.8.4(9)   in Ada.Sequential_IO   A.8.1(9)   in Ada.Streams.Stream_IO   A.12.1(11)   in Ada.Text_IO   A.10.1(12)

Free   in Ada.Strings.Unbounded   A.4.5(7)   in Interfaces.C.Strings   B.3.1(11)

Get   in Ada.Text_IO   A.10.1(41), A.10.1(47), A.10.1(54), A.10.1(55), A.10.1(59), A.10.1(60), A.10.1(65), A.10.1(67), A.10.1(70), A.10.1(72), A.10.1(75), A.10.1(77), A.10.1(81), A.10.1(83)   in Ada.Text_IO.Complex_IO   G.1.3(6), G.1.3(8)

Get_Immediate in Ada.Text_IO   A.10.1(44), A.10.1(45)

Get_Line in Ada.Text_IO   A.10.1(49)

Get_Priority   in Ada.Dynamic_Priorities   D.5(5)

Head   in Ada.Strings.Bounded   A.4.4(70), A.4.4(71)   in Ada.Strings.Fixed   A.4.3(35), A.4.3(36)   in Ada.Strings.Unbounded   A.4.5(65), A.4.5(66)

Hold in Ada.Asynchronous_Task_Control   D.11(3)

Im   in Ada.Numerics.Generic_Complex_Types   G.1.1(6)

Image   in Ada.Numerics.Discrete_Random   A.5.2(26)   in Ada.Numerics.Float_Random   A.5.2(14)   in Ada.Task_Identification   C.7.1(3)   in Ada.Text_IO.Editing   F.3.3(13)

Increment in Interfaces.C.Pointers   B.3.2(11)

Index   in Ada.Direct_IO   A.8.4(15)   in Ada.Streams.Stream_IO   A.12.1(23)   in Ada.Strings.Bounded   A.4.4(44), A.4.4(45), A.4.4(46)   in Ada.Strings.Fixed   A.4.3(9), A.4.3(10), A.4.3(11)   in Ada.Strings.Unbounded   A.4.5(39), A.4.5(40), A.4.5(41)

Index_Non_Blank   in Ada.Strings.Bounded   A.4.4(47)   in Ada.Strings.Fixed   A.4.3(12)   in Ada.Strings.Unbounded   A.4.5(42)

Initialize in Ada.Finalization   7.6(7), 7.6(9)

Insert   in Ada.Strings.Bounded   A.4.4(60), A.4.4(61)   in Ada.Strings.Fixed   A.4.3(25), A.4.3(26)   in Ada.Strings.Unbounded   A.4.5(55), A.4.5(56)

Internal_Tag in Ada.Tags   3.9(8)

Is_Alphanumeric   in Ada.Characters.Handling   A.3.2(4)

Is_Attached in Ada.Interrupts   C.3.2(5)

Is_Basic in Ada.Characters.Handling   A.3.2(4)

Is_Callable   in Ada.Task_Identification   C.7.1(4)

Is_Character   in Ada.Characters.Handling   A.3.2(14)

Is_Control in Ada.Characters.Handling   A.3.2(4)

Is_Decimal_Digit   in Ada.Characters.Handling   A.3.2(4)

Is_Digit in Ada.Characters.Handling   A.3.2(4)

Is_Graphic in Ada.Characters.Handling   A.3.2(4)

Is_Held   in Ada.Asynchronous_Task_Control   D.11(3)

Is_Hexadecimal_Digit   in Ada.Characters.Handling   A.3.2(4)

Is_In   in Ada.Strings.Maps   A.4.2(13)   in Ada.Strings.Wide_Maps   A.4.7(13)

Is_ISO_646 in Ada.Characters.Handling   A.3.2(10)

Is_Letter in Ada.Characters.Handling   A.3.2(4)

Is_Lower in Ada.Characters.Handling   A.3.2(4)

Is_Nul_Terminated in Interfaces.C   B.3(24), B.3(35)

Is_Open   in Ada.Direct_IO   A.8.4(10)   in Ada.Sequential_IO   A.8.1(10)   in Ada.Streams.Stream_IO   A.12.1(12)   in Ada.Text_IO   A.10.1(13)

Is_Reserved in Ada.Interrupts   C.3.2(4)

Is_Special in Ada.Characters.Handling   A.3.2(4)

Is_String in Ada.Characters.Handling   A.3.2(14)

Is_Subset   in Ada.Strings.Maps   A.4.2(14)   in Ada.Strings.Wide_Maps   A.4.7(14)

Is_Terminated   in Ada.Task_Identification   C.7.1(4)

Is_Upper in Ada.Characters.Handling   A.3.2(4)

Length   in Ada.Strings.Bounded   A.4.4(9)   in Ada.Strings.Unbounded   A.4.5(6)   in Ada.Text_IO.Editing   F.3.3(11)   in Interfaces.COBOL   B.4(34), B.4(39), B.4(44)

Line in Ada.Text_IO   A.10.1(38)

Line_Length in Ada.Text_IO   A.10.1(25)

Log   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)

Look_Ahead in Ada.Text_IO   A.10.1(43)

Microseconds in Ada.Real_Time   D.8(14)

Milliseconds in Ada.Real_Time   D.8(14)

Mode   in Ada.Direct_IO   A.8.4(9)   in Ada.Sequential_IO   A.8.1(9)   in Ada.Streams.Stream_IO   A.12.1(11)   in Ada.Text_IO   A.10.1(12)

Modulus   in Ada.Numerics.Generic_Complex_Types   G.1.1(9)

Month in Ada.Calendar   9.6(13)

Move in Ada.Strings.Fixed   A.4.3(7)

Name   in Ada.Direct_IO   A.8.4(9)   in Ada.Sequential_IO   A.8.1(9)   in Ada.Streams.Stream_IO   A.12.1(11)   in Ada.Text_IO   A.10.1(12)

Nanoseconds in Ada.Real_Time   D.8(14)

New_Char_Array   in Interfaces.C.Strings   B.3.1(9)

New_Line in Ada.Text_IO   A.10.1(28)

New_Page in Ada.Text_IO   A.10.1(31)

New_String in Interfaces.C.Strings   B.3.1(10)

Null_Task_Id   in Ada.Task_Identification   C.7.1(2)

Open   in Ada.Direct_IO   A.8.4(7)   in Ada.Sequential_IO   A.8.1(7)   in Ada.Streams.Stream_IO   A.12.1(9)   in Ada.Text_IO   A.10.1(10)

Overwrite   in Ada.Strings.Bounded   A.4.4(62), A.4.4(63)   in Ada.Strings.Fixed   A.4.3(27), A.4.3(28)   in Ada.Strings.Unbounded   A.4.5(57), A.4.5(58)

Page in Ada.Text_IO   A.10.1(39)

Page_Length in Ada.Text_IO   A.10.1(26)

Pic_String in Ada.Text_IO.Editing   F.3.3(7)

Put   in Ada.Text_IO   A.10.1(42), A.10.1(48), A.10.1(55), A.10.1(60), A.10.1(66), A.10.1(67), A.10.1(71), A.10.1(72), A.10.1(76), A.10.1(77), A.10.1(82), A.10.1(83)   in Ada.Text_IO.Complex_IO   G.1.3(7), G.1.3(8)   in Ada.Text_IO.Editing   F.3.3(14), F.3.3(15), F.3.3(16)

Put_Line in Ada.Text_IO   A.10.1(50)

Raise_Exception in Ada.Exceptions   11.4.1(4)

Random   in Ada.Numerics.Discrete_Random   A.5.2(20)   in Ada.Numerics.Float_Random   A.5.2(8)

Re   in Ada.Numerics.Generic_Complex_Types   G.1.1(6)

Read   in Ada.Direct_IO   A.8.4(12)   in Ada.Sequential_IO   A.8.1(12)   in Ada.Storage_IO   A.9(6)   in Ada.Streams   13.13.1(5)   in Ada.Streams.Stream_IO   A.12.1(15), A.12.1(16)   in System.RPC   E.5(7)

Reference   in Ada.Interrupts   C.3.2(10)   in Ada.Task_Attributes   C.7.2(5)

Reinitialize in Ada.Task_Attributes   C.7.2(6)

Replace_Element   in Ada.Strings.Bounded   A.4.4(27)   in Ada.Strings.Unbounded   A.4.5(21)

Replace_Slice   in Ada.Strings.Bounded   A.4.4(58), A.4.4(59)   in Ada.Strings.Fixed   A.4.3(23), A.4.3(24)   in Ada.Strings.Unbounded   A.4.5(53), A.4.5(54)

Replicate in Ada.Strings.Bounded   A.4.4(78), A.4.4(79), A.4.4(80)

Reraise_Occurrence in Ada.Exceptions   11.4.1(4)

Reset   in Ada.Direct_IO   A.8.4(8)   in Ada.Numerics.Discrete_Random   A.5.2(21), A.5.2(24)   in Ada.Numerics.Float_Random   A.5.2(9), A.5.2(12)   in Ada.Sequential_IO   A.8.1(8)   in Ada.Streams.Stream_IO   A.12.1(10)   in Ada.Text_IO   A.10.1(11)

Save   in Ada.Numerics.Discrete_Random   A.5.2(24)   in Ada.Numerics.Float_Random   A.5.2(12)

Save_Occurrence in Ada.Exceptions   11.4.1(6)

Seconds in Ada.Calendar   9.6(13)

Set_Col in Ada.Text_IO   A.10.1(35)

Set_Error in Ada.Text_IO   A.10.1(15)

Set_Exit_Status in Ada.Command_Line   A.15(9)

Set_False   in Ada.Synchronous_Task_Control   D.10(4)

Set_Im   in Ada.Numerics.Generic_Complex_Types   G.1.1(7)

Set_Index   in Ada.Direct_IO   A.8.4(14)   in Ada.Streams.Stream_IO   A.12.1(22)

Set_Input in Ada.Text_IO   A.10.1(15)

Set_Line in Ada.Text_IO   A.10.1(36)

Set_Line_Length in Ada.Text_IO   A.10.1(23)

Set_Mode in Ada.Streams.Stream_IO   A.12.1(24)

Set_Output in Ada.Text_IO   A.10.1(15)

Set_Page_Length in Ada.Text_IO   A.10.1(24)

Set_Priority   in Ada.Dynamic_Priorities   D.5(4)

Set_Re   in Ada.Numerics.Generic_Complex_Types   G.1.1(7)

Set_True   in Ada.Synchronous_Task_Control   D.10(4)

Set_Value in Ada.Task_Attributes   C.7.2(6)

Sin   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

Sinh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Size   in Ada.Direct_IO   A.8.4(15)   in Ada.Streams.Stream_IO   A.12.1(23)

Skip_Line in Ada.Text_IO   A.10.1(29)

Skip_Page in Ada.Text_IO   A.10.1(32)

Slice   in Ada.Strings.Bounded   A.4.4(28)   in Ada.Strings.Unbounded   A.4.5(22)

Split   in Ada.Calendar   9.6(14)   in Ada.Real_Time   D.8(16)

Sqrt   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)

Standard_Error in Ada.Text_IO   A.10.1(16), A.10.1(19)

Standard_Input in Ada.Text_IO   A.10.1(16), A.10.1(19)

Standard_Output in Ada.Text_IO   A.10.1(16), A.10.1(19)

Storage_Size in System.Storage_Pools   13.11(10)

Stream   in Ada.Streams.Stream_IO   A.12.1(13)   in Ada.Text_IO.Text_Streams   A.12.2(4)   in Ada.Wide_Text_IO.Text_Streams   A.12.3(4)

Strlen in Interfaces.C.Strings   B.3.1(17)

Suspend_Until_True   in Ada.Synchronous_Task_Control   D.10(4)

Tail   in Ada.Strings.Bounded   A.4.4(72), A.4.4(73)   in Ada.Strings.Fixed   A.4.3(37), A.4.3(38)   in Ada.Strings.Unbounded   A.4.5(67), A.4.5(68)

Tan   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

Tanh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Time_Of   in Ada.Calendar   9.6(15)   in Ada.Real_Time   D.8(16)

To_Ada   in Interfaces.C   B.3(22), B.3(26), B.3(28), B.3(32), B.3(37), B.3(39)   in Interfaces.COBOL   B.4(17), B.4(19)   in Interfaces.Fortran   B.5(13), B.5(14), B.5(16)

To_Address   in System.Address_To_Access_Conversions   13.7.2(3)   in System.Storage_Elements   13.7.1(10)

To_Basic in Ada.Characters.Handling   A.3.2(6), A.3.2(7)

To_Binary in Interfaces.COBOL   B.4(45), B.4(48)

To_Bounded_String   in Ada.Strings.Bounded   A.4.4(11)

To_C in Interfaces.C   B.3(21), B.3(25), B.3(27), B.3(32), B.3(36), B.3(38)

To_Character   in Ada.Characters.Handling   A.3.2(15)

To_Chars_Ptr in Interfaces.C.Strings   B.3.1(8)

To_COBOL in Interfaces.COBOL   B.4(17), B.4(18)

To_Decimal in Interfaces.COBOL   B.4(35), B.4(40), B.4(44), B.4(47)

To_Display in Interfaces.COBOL   B.4(36)

To_Domain   in Ada.Strings.Maps   A.4.2(24)   in Ada.Strings.Wide_Maps   A.4.7(24)

To_Duration in Ada.Real_Time   D.8(13)

To_Fortran in Interfaces.Fortran   B.5(13), B.5(14), B.5(15)

To_Integer in System.Storage_Elements   13.7.1(10)

To_ISO_646 in Ada.Characters.Handling   A.3.2(11), A.3.2(12)

To_Long_Binary in Interfaces.COBOL   B.4(48)

To_Lower in Ada.Characters.Handling   A.3.2(6), A.3.2(7)

To_Mapping   in Ada.Strings.Maps   A.4.2(23)   in Ada.Strings.Wide_Maps   A.4.7(23)

To_Packed in Interfaces.COBOL   B.4(41)

To_Picture in Ada.Text_IO.Editing   F.3.3(6)

To_Pointer   in System.Address_To_Access_Conversions   13.7.2(3)

To_Range   in Ada.Strings.Maps   A.4.2(24)   in Ada.Strings.Wide_Maps   A.4.7(25)

To_Ranges   in Ada.Strings.Maps   A.4.2(10)   in Ada.Strings.Wide_Maps   A.4.7(10)

To_Sequence   in Ada.Strings.Maps   A.4.2(19)   in Ada.Strings.Wide_Maps   A.4.7(19)

To_Set   in Ada.Strings.Maps   A.4.2(8), A.4.2(9), A.4.2(17), A.4.2(18)   in Ada.Strings.Wide_Maps   A.4.7(8), A.4.7(9), A.4.7(17), A.4.7(18)

To_String   in Ada.Characters.Handling   A.3.2(16)   in Ada.Strings.Bounded   A.4.4(12)   in Ada.Strings.Unbounded   A.4.5(11)

To_Time_Span in Ada.Real_Time   D.8(13)

To_Unbounded_String   in Ada.Strings.Unbounded   A.4.5(9), A.4.5(10)

To_Upper in Ada.Characters.Handling   A.3.2(6), A.3.2(7)

To_Wide_Character   in Ada.Characters.Handling   A.3.2(17)

To_Wide_String   in Ada.Characters.Handling   A.3.2(18)

Translate   in Ada.Strings.Bounded   A.4.4(53), A.4.4(54), A.4.4(55), A.4.4(56)   in Ada.Strings.Fixed   A.4.3(18), A.4.3(19), A.4.3(20), A.4.3(21)   in Ada.Strings.Unbounded   A.4.5(48), A.4.5(49), A.4.5(50), A.4.5(51)

Trim   in Ada.Strings.Bounded   A.4.4(67), A.4.4(68), A.4.4(69)   in Ada.Strings.Fixed   A.4.3(31), A.4.3(32), A.4.3(33), A.4.3(34)   in Ada.Strings.Unbounded   A.4.5(61), A.4.5(62), A.4.5(63), A.4.5(64)

Unchecked_Conversion   child of Ada   13.9(3)

Unchecked_Deallocation   child of Ada   13.11.2(3)

Update in Interfaces.C.Strings   B.3.1(18), B.3.1(19)

Update_Error in Interfaces.C.Strings   B.3.1(20)

Valid   in Ada.Text_IO.Editing   F.3.3(5), F.3.3(12)   in Interfaces.COBOL   B.4(33), B.4(38), B.4(43)

Value   in Ada.Numerics.Discrete_Random   A.5.2(26)   in Ada.Numerics.Float_Random   A.5.2(14)   in Ada.Strings.Maps   A.4.2(21)   in Ada.Strings.Wide_Maps   A.4.7(21)   in Ada.Task_Attributes   C.7.2(4)   in Interfaces.C.Pointers   B.3.2(6), B.3.2(7)   in Interfaces.C.Strings   B.3.1(13), B.3.1(14), B.3.1(15), B.3.1(16)

Virtual_Length   in Interfaces.C.Pointers   B.3.2(13)

Wide_Hash   child of Ada.Strings.Wide_Bounded   A.4.7(1)   child of Ada.Strings.Wide_Fixed   A.4.7(1)   child of Ada.Strings.Wide_Unbounded   A.4.7(1)

Write   in Ada.Direct_IO   A.8.4(13)   in Ada.Sequential_IO   A.8.1(12)   in Ada.Storage_IO   A.9(7)   in Ada.Streams   13.13.1(6)   in Ada.Streams.Stream_IO   A.12.1(18), A.12.1(19)   in System.RPC   E.5(8)

Year in Ada.Calendar   9.6(13)

 

Argument_Error   in Ada.Numerics   A.5(3)

Communication_Error   in System.RPC   E.5(5)

Constraint_Error   in Standard   A.1(46)

Conversion_Error   in Interfaces.COBOL   B.4(30)

Data_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Storage_IO   A.9(9)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Device_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

End_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Index_Error   in Ada.Strings   A.4.1(5)

Layout_Error   in Ada.IO_Exceptions   A.13(4)   in Ada.Text_IO   A.10.1(85)

Length_Error   in Ada.Strings   A.4.1(5)

Mode_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Name_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Pattern_Error   in Ada.Strings   A.4.1(5)

Picture_Error   in Ada.Text_IO.Editing   F.3.3(9)

Pointer_Error   in Interfaces.C.Pointers   B.3.2(8)

Program_Error   in Standard   A.1(46)

Status_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Storage_Error   in Standard   A.1(46)

Tag_Error   in Ada.Tags   3.9(9)

Tasking_Error   in Standard   A.1(46)

Terminator_Error   in Interfaces.C   B.3(40)

Time_Error   in Ada.Calendar   9.6(18)

Translation_Error   in Ada.Strings   A.4.1(5)

Use_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

 

ACK in Ada.Characters.Latin_1   A.3.3(5)

Acute in Ada.Characters.Latin_1   A.3.3(22)

Ada_To_COBOL in Interfaces.COBOL   B.4(14)

Alphanumeric_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Ampersand in Ada.Characters.Latin_1   A.3.3(8)

APC in Ada.Characters.Latin_1   A.3.3(19)

Apostrophe in Ada.Characters.Latin_1   A.3.3(8)

Asterisk in Ada.Characters.Latin_1   A.3.3(8)

Basic_Map   in Ada.Strings.Maps.Constants   A.4.6(5)

Basic_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

BEL in Ada.Characters.Latin_1   A.3.3(5)

BPH in Ada.Characters.Latin_1   A.3.3(17)

Broken_Bar in Ada.Characters.Latin_1   A.3.3(21)

BS in Ada.Characters.Latin_1   A.3.3(5)

Buffer_Size in Ada.Storage_IO   A.9(4)

CAN in Ada.Characters.Latin_1   A.3.3(6)

CCH in Ada.Characters.Latin_1   A.3.3(18)

Cedilla in Ada.Characters.Latin_1   A.3.3(22)

Cent_Sign in Ada.Characters.Latin_1   A.3.3(21)

CHAR_BIT in Interfaces.C   B.3(6)

Character_Set   in Ada.Strings.Wide_Maps   A.4.7(46)

Circumflex in Ada.Characters.Latin_1   A.3.3(12)

COBOL_To_Ada in Interfaces.COBOL   B.4(15)

Colon in Ada.Characters.Latin_1   A.3.3(10)

Comma in Ada.Characters.Latin_1   A.3.3(8)

Commercial_At   in Ada.Characters.Latin_1   A.3.3(10)

Control_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Copyright_Sign   in Ada.Characters.Latin_1   A.3.3(21)

CR in Ada.Characters.Latin_1   A.3.3(5)

CSI in Ada.Characters.Latin_1   A.3.3(19)

Currency_Sign   in Ada.Characters.Latin_1   A.3.3(21)

DC1 in Ada.Characters.Latin_1   A.3.3(6)

DC2 in Ada.Characters.Latin_1   A.3.3(6)

DC3 in Ada.Characters.Latin_1   A.3.3(6)

DC4 in Ada.Characters.Latin_1   A.3.3(6)

DCS in Ada.Characters.Latin_1   A.3.3(18)

Decimal_Digit_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Default_Aft   in Ada.Text_IO   A.10.1(64), A.10.1(69), A.10.1(74)   in Ada.Text_IO.Complex_IO   G.1.3(5)

Default_Base in Ada.Text_IO   A.10.1(53), A.10.1(58)

Default_Bit_Order in System   13.7(15)

Default_Currency   in Ada.Text_IO.Editing   F.3.3(10)

Default_Exp   in Ada.Text_IO   A.10.1(64), A.10.1(69), A.10.1(74)   in Ada.Text_IO.Complex_IO   G.1.3(5)

Default_Fill in Ada.Text_IO.Editing   F.3.3(10)

Default_Fore   in Ada.Text_IO   A.10.1(64), A.10.1(69), A.10.1(74)   in Ada.Text_IO.Complex_IO   G.1.3(5)

Default_Priority in System   13.7(17)

Default_Radix_Mark   in Ada.Text_IO.Editing   F.3.3(10)

Default_Separator   in Ada.Text_IO.Editing   F.3.3(10)

Default_Setting in Ada.Text_IO   A.10.1(80)

Default_Width in Ada.Text_IO   A.10.1(53), A.10.1(58), A.10.1(80)

Degree_Sign in Ada.Characters.Latin_1   A.3.3(22)

DEL in Ada.Characters.Latin_1   A.3.3(14)

Diaeresis in Ada.Characters.Latin_1   A.3.3(21)

Division_Sign   in Ada.Characters.Latin_1   A.3.3(26)

DLE in Ada.Characters.Latin_1   A.3.3(6)

Dollar_Sign in Ada.Characters.Latin_1   A.3.3(8)

e in Ada.Numerics   A.5(3)

EM in Ada.Characters.Latin_1   A.3.3(6)

ENQ in Ada.Characters.Latin_1   A.3.3(5)

EOT in Ada.Characters.Latin_1   A.3.3(5)

EPA in Ada.Characters.Latin_1   A.3.3(18)

Equals_Sign in Ada.Characters.Latin_1   A.3.3(10)

ESA in Ada.Characters.Latin_1   A.3.3(17)

ESC in Ada.Characters.Latin_1   A.3.3(6)

ETB in Ada.Characters.Latin_1   A.3.3(6)

ETX in Ada.Characters.Latin_1   A.3.3(5)

Exclamation in Ada.Characters.Latin_1   A.3.3(8)

Failure in Ada.Command_Line   A.15(8)

Feminine_Ordinal_Indicator   in Ada.Characters.Latin_1   A.3.3(21)

FF in Ada.Characters.Latin_1   A.3.3(5)

Fine_Delta in System   13.7(9)

Fraction_One_Half   in Ada.Characters.Latin_1   A.3.3(22)

Fraction_One_Quarter   in Ada.Characters.Latin_1   A.3.3(22)

Fraction_Three_Quarters   in Ada.Characters.Latin_1   A.3.3(22)

FS in Ada.Characters.Latin_1   A.3.3(6)

Full_Stop in Ada.Characters.Latin_1   A.3.3(8)

Graphic_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Grave in Ada.Characters.Latin_1   A.3.3(13)

Greater_Than_Sign   in Ada.Characters.Latin_1   A.3.3(10)

GS in Ada.Characters.Latin_1   A.3.3(6)

Hexadecimal_Digit_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

High_Order_First   in Interfaces.COBOL   B.4(25)   in System   13.7(15)

HT in Ada.Characters.Latin_1   A.3.3(5)

HTJ in Ada.Characters.Latin_1   A.3.3(17)

HTS in Ada.Characters.Latin_1   A.3.3(17)

Hyphen in Ada.Characters.Latin_1   A.3.3(8)

i   in Ada.Numerics.Generic_Complex_Types   G.1.1(5)   in Interfaces.Fortran   B.5(10)

Identity   in Ada.Strings.Maps   A.4.2(22)   in Ada.Strings.Wide_Maps   A.4.7(22)

Inverted_Exclamation   in Ada.Characters.Latin_1   A.3.3(21)

Inverted_Question   in Ada.Characters.Latin_1   A.3.3(22)

IS1 in Ada.Characters.Latin_1   A.3.3(16)

IS2 in Ada.Characters.Latin_1   A.3.3(16)

IS3 in Ada.Characters.Latin_1   A.3.3(16)

IS4 in Ada.Characters.Latin_1   A.3.3(16)

ISO_646_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

j   in Ada.Numerics.Generic_Complex_Types   G.1.1(5)   in Interfaces.Fortran   B.5(10)

LC_A in Ada.Characters.Latin_1   A.3.3(13)

LC_A_Acute in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Circumflex   in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Diaeresis   in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Grave in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Ring in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Tilde in Ada.Characters.Latin_1   A.3.3(25)

LC_AE_Diphthong   in Ada.Characters.Latin_1   A.3.3(25)

LC_B in Ada.Characters.Latin_1   A.3.3(13)

LC_C in Ada.Characters.Latin_1   A.3.3(13)

LC_C_Cedilla   in Ada.Characters.Latin_1   A.3.3(25)

LC_D in Ada.Characters.Latin_1   A.3.3(13)

LC_E in Ada.Characters.Latin_1   A.3.3(13)

LC_E_Acute in Ada.Characters.Latin_1   A.3.3(25)

LC_E_Circumflex   in Ada.Characters.Latin_1   A.3.3(25)

LC_E_Diaeresis   in Ada.Characters.Latin_1   A.3.3(25)

LC_E_Grave in Ada.Characters.Latin_1   A.3.3(25)

LC_F in Ada.Characters.Latin_1   A.3.3(13)

LC_G in Ada.Characters.Latin_1   A.3.3(13)

LC_German_Sharp_S   in Ada.Characters.Latin_1   A.3.3(24)

LC_H in Ada.Characters.Latin_1   A.3.3(13)

LC_I in Ada.Characters.Latin_1   A.3.3(13)

LC_I_Acute in Ada.Characters.Latin_1   A.3.3(25)

LC_I_Circumflex   in Ada.Characters.Latin_1   A.3.3(25)

LC_I_Diaeresis   in Ada.Characters.Latin_1   A.3.3(25)

LC_I_Grave in Ada.Characters.Latin_1   A.3.3(25)

LC_Icelandic_Eth   in Ada.Characters.Latin_1   A.3.3(26)

LC_Icelandic_Thorn   in Ada.Characters.Latin_1   A.3.3(26)

LC_J in Ada.Characters.Latin_1   A.3.3(13)

LC_K in Ada.Characters.Latin_1   A.3.3(13)

LC_L in Ada.Characters.Latin_1   A.3.3(13)

LC_M in Ada.Characters.Latin_1   A.3.3(13)

LC_N in Ada.Characters.Latin_1   A.3.3(13)

LC_N_Tilde in Ada.Characters.Latin_1   A.3.3(26)

LC_O in Ada.Characters.Latin_1   A.3.3(13)

LC_O_Acute in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Circumflex   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Diaeresis   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Grave in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Oblique_Stroke   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Tilde in Ada.Characters.Latin_1   A.3.3(26)

LC_P in Ada.Characters.Latin_1   A.3.3(14)

LC_Q in Ada.Characters.Latin_1   A.3.3(14)

LC_R in Ada.Characters.Latin_1   A.3.3(14)

LC_S in Ada.Characters.Latin_1   A.3.3(14)

LC_T in Ada.Characters.Latin_1   A.3.3(14)

LC_U in Ada.Characters.Latin_1   A.3.3(14)

LC_U_Acute in Ada.Characters.Latin_1   A.3.3(26)

LC_U_Circumflex   in Ada.Characters.Latin_1   A.3.3(26)

LC_U_Diaeresis   in Ada.Characters.Latin_1   A.3.3(26)

LC_U_Grave in Ada.Characters.Latin_1   A.3.3(26)

LC_V in Ada.Characters.Latin_1   A.3.3(14)

LC_W in Ada.Characters.Latin_1   A.3.3(14)

LC_X in Ada.Characters.Latin_1   A.3.3(14)

LC_Y in Ada.Characters.Latin_1   A.3.3(14)

LC_Y_Acute in Ada.Characters.Latin_1   A.3.3(26)

LC_Y_Diaeresis   in Ada.Characters.Latin_1   A.3.3(26)

LC_Z in Ada.Characters.Latin_1   A.3.3(14)

Leading_Nonseparate   in Interfaces.COBOL   B.4(23)

Leading_Separate in Interfaces.COBOL   B.4(23)

Left_Angle_Quotation   in Ada.Characters.Latin_1   A.3.3(21)

Left_Curly_Bracket   in Ada.Characters.Latin_1   A.3.3(14)

Left_Parenthesis   in Ada.Characters.Latin_1   A.3.3(8)

Left_Square_Bracket   in Ada.Characters.Latin_1   A.3.3(12)

Less_Than_Sign   in Ada.Characters.Latin_1   A.3.3(10)

Letter_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

LF in Ada.Characters.Latin_1   A.3.3(5)

Low_Line in Ada.Characters.Latin_1   A.3.3(12)

Low_Order_First   in Interfaces.COBOL   B.4(25)   in System   13.7(15)

Lower_Case_Map   in Ada.Strings.Maps.Constants   A.4.6(5)

Lower_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Macron in Ada.Characters.Latin_1   A.3.3(21)

Masculine_Ordinal_Indicator   in Ada.Characters.Latin_1   A.3.3(22)

Max_Base_Digits in System   13.7(8)

Max_Binary_Modulus in System   13.7(7)

Max_Decimal_Digits in Ada.Decimal   F.2(5)

Max_Delta in Ada.Decimal   F.2(4)

Max_Digits in System   13.7(8)

Max_Digits_Binary in Interfaces.COBOL   B.4(11)

Max_Digits_Long_Binary   in Interfaces.COBOL   B.4(11)

Max_Image_Width   in Ada.Numerics.Discrete_Random   A.5.2(25)   in Ada.Numerics.Float_Random   A.5.2(13)

Max_Int in System   13.7(6)

Max_Length in Ada.Strings.Bounded   A.4.4(5)

Max_Mantissa in System   13.7(9)

Max_Nonbinary_Modulus in System   13.7(7)

Max_Picture_Length   in Ada.Text_IO.Editing   F.3.3(8)

Max_Scale in Ada.Decimal   F.2(3)

Memory_Size in System   13.7(13)

Micro_Sign in Ada.Characters.Latin_1   A.3.3(22)

Middle_Dot in Ada.Characters.Latin_1   A.3.3(22)

Min_Delta in Ada.Decimal   F.2(4)

Min_Int in System   13.7(6)

Min_Scale in Ada.Decimal   F.2(3)

Minus_Sign in Ada.Characters.Latin_1   A.3.3(8)

Multiplication_Sign   in Ada.Characters.Latin_1   A.3.3(24)

MW in Ada.Characters.Latin_1   A.3.3(18)

NAK in Ada.Characters.Latin_1   A.3.3(6)

Native_Binary in Interfaces.COBOL   B.4(25)

NBH in Ada.Characters.Latin_1   A.3.3(17)

NBSP in Ada.Characters.Latin_1   A.3.3(21)

NEL in Ada.Characters.Latin_1   A.3.3(17)

No_Break_Space   in Ada.Characters.Latin_1   A.3.3(21)

Not_Sign in Ada.Characters.Latin_1   A.3.3(21)

NUL   in Ada.Characters.Latin_1   A.3.3(5)   in Interfaces.C   B.3(20)

Null_Address in System   13.7(12)

Null_Bounded_String   in Ada.Strings.Bounded   A.4.4(7)

Null_Id in Ada.Exceptions   11.4.1(2)

Null_Occurrence in Ada.Exceptions   11.4.1(3)

Null_Ptr in Interfaces.C.Strings   B.3.1(7)

Null_Set   in Ada.Strings.Maps   A.4.2(5)   in Ada.Strings.Wide_Maps   A.4.7(5)

Null_Unbounded_String   in Ada.Strings.Unbounded   A.4.5(5)

Number_Sign in Ada.Characters.Latin_1   A.3.3(8)

OSC in Ada.Characters.Latin_1   A.3.3(19)

Packed_Signed in Interfaces.COBOL   B.4(27)

Packed_Unsigned in Interfaces.COBOL   B.4(27)

Paragraph_Sign   in Ada.Characters.Latin_1   A.3.3(22)

Percent_Sign   in Ada.Characters.Latin_1   A.3.3(8)

Pi in Ada.Numerics   A.5(3)

Pilcrow_Sign   in Ada.Characters.Latin_1   A.3.3(22)

PLD in Ada.Characters.Latin_1   A.3.3(17)

PLU in Ada.Characters.Latin_1   A.3.3(17)

Plus_Minus_Sign   in Ada.Characters.Latin_1   A.3.3(22)

Plus_Sign in Ada.Characters.Latin_1   A.3.3(8)

PM in Ada.Characters.Latin_1   A.3.3(19)

Pound_Sign in Ada.Characters.Latin_1   A.3.3(21)

PU1 in Ada.Characters.Latin_1   A.3.3(18)

PU2 in Ada.Characters.Latin_1   A.3.3(18)

Question in Ada.Characters.Latin_1   A.3.3(10)

Quotation in Ada.Characters.Latin_1   A.3.3(8)

Registered_Trade_Mark_Sign   in Ada.Characters.Latin_1   A.3.3(21)

Reserved_128   in Ada.Characters.Latin_1   A.3.3(17)

Reserved_129   in Ada.Characters.Latin_1   A.3.3(17)

Reserved_132   in Ada.Characters.Latin_1   A.3.3(17)

Reserved_153   in Ada.Characters.Latin_1   A.3.3(19)

Reverse_Solidus   in Ada.Characters.Latin_1   A.3.3(12)

RI in Ada.Characters.Latin_1   A.3.3(17)

Right_Angle_Quotation   in Ada.Characters.Latin_1   A.3.3(22)

Right_Curly_Bracket   in Ada.Characters.Latin_1   A.3.3(14)

Right_Parenthesis   in Ada.Characters.Latin_1   A.3.3(8)

Right_Square_Bracket   in Ada.Characters.Latin_1   A.3.3(12)

Ring_Above in Ada.Characters.Latin_1   A.3.3(22)

RS in Ada.Characters.Latin_1   A.3.3(6)

SCHAR_MAX in Interfaces.C   B.3(6)

SCHAR_MIN in Interfaces.C   B.3(6)

SCI in Ada.Characters.Latin_1   A.3.3(19)

Section_Sign   in Ada.Characters.Latin_1   A.3.3(21)

Semicolon in Ada.Characters.Latin_1   A.3.3(10)

SI in Ada.Characters.Latin_1   A.3.3(5)

SO in Ada.Characters.Latin_1   A.3.3(5)

Soft_Hyphen in Ada.Characters.Latin_1   A.3.3(21)

SOH in Ada.Characters.Latin_1   A.3.3(5)

Solidus in Ada.Characters.Latin_1   A.3.3(8)

SOS in Ada.Characters.Latin_1   A.3.3(19)

SPA in Ada.Characters.Latin_1   A.3.3(18)

Space   in Ada.Characters.Latin_1   A.3.3(8)   in Ada.Strings   A.4.1(4)

Special_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

SS2 in Ada.Characters.Latin_1   A.3.3(17)

SS3 in Ada.Characters.Latin_1   A.3.3(17)

SSA in Ada.Characters.Latin_1   A.3.3(17)

ST in Ada.Characters.Latin_1   A.3.3(19)

Storage_Unit in System   13.7(13)

STS in Ada.Characters.Latin_1   A.3.3(18)

STX in Ada.Characters.Latin_1   A.3.3(5)

SUB in Ada.Characters.Latin_1   A.3.3(6)

Success in Ada.Command_Line   A.15(8)

Superscript_One   in Ada.Characters.Latin_1   A.3.3(22)

Superscript_Three   in Ada.Characters.Latin_1   A.3.3(22)

Superscript_Two   in Ada.Characters.Latin_1   A.3.3(22)

SYN in Ada.Characters.Latin_1   A.3.3(6)

System_Name in System   13.7(4)

Tick   in Ada.Real_Time   D.8(6)   in System   13.7(10)

Tilde in Ada.Characters.Latin_1   A.3.3(14)

Time_First in Ada.Real_Time   D.8(4)

Time_Last in Ada.Real_Time   D.8(4)

Time_Span_First in Ada.Real_Time   D.8(5)

Time_Span_Last in Ada.Real_Time   D.8(5)

Time_Span_Unit in Ada.Real_Time   D.8(5)

Time_Span_Zero in Ada.Real_Time   D.8(5)

Time_Unit in Ada.Real_Time   D.8(4)

Trailing_Nonseparate   in Interfaces.COBOL   B.4(23)

Trailing_Separate in Interfaces.COBOL   B.4(23)

UC_A_Acute in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Circumflex   in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Diaeresis   in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Grave in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Ring in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Tilde in Ada.Characters.Latin_1   A.3.3(23)

UC_AE_Diphthong   in Ada.Characters.Latin_1   A.3.3(23)

UC_C_Cedilla   in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Acute in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Circumflex   in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Diaeresis   in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Grave in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Acute in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Circumflex   in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Diaeresis   in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Grave in Ada.Characters.Latin_1   A.3.3(23)

UC_Icelandic_Eth   in Ada.Characters.Latin_1   A.3.3(24)

UC_Icelandic_Thorn   in Ada.Characters.Latin_1   A.3.3(24)

UC_N_Tilde in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Acute in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Circumflex   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Diaeresis   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Grave in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Oblique_Stroke   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Tilde in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Acute in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Circumflex   in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Diaeresis   in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Grave in Ada.Characters.Latin_1   A.3.3(24)

UC_Y_Acute in Ada.Characters.Latin_1   A.3.3(24)

UCHAR_MAX in Interfaces.C   B.3(6)

Unbounded in Ada.Text_IO   A.10.1(5)

Unsigned in Interfaces.COBOL   B.4(23)

Upper_Case_Map   in Ada.Strings.Maps.Constants   A.4.6(5)

Upper_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

US in Ada.Characters.Latin_1   A.3.3(6)

Vertical_Line   in Ada.Characters.Latin_1   A.3.3(14)

VT in Ada.Characters.Latin_1   A.3.3(5)

VTS in Ada.Characters.Latin_1   A.3.3(17)

wide_nul in Interfaces.C   B.3(31)

Wide_Space in Ada.Strings   A.4.1(4)

Word_Size in System   13.7(13)

Yen_Sign in Ada.Characters.Latin_1   A.3.3(21)

Index entries are given by paragraph number. A list of all language-defined library units may be found under Language-Defined Library Units. A list of all language-defined types may be found under Language-Defined Types. 

 


   3.9.3(18)

 

& operator   4.5(1), 4.5.3(3)

 

* operator   4.5(1), 4.5.5(1)

** operator   4.5(1), 4.5.6(7)

 

+ operator   4.5(1), 4.5.3(1), 4.5.4(1)

 

- operator   4.5(1), 4.5.3(1), 4.5.4(1)

 

/ operator   4.5(1), 4.5.5(1)

/= operator   4.5(1), 4.5.2(1), 6.6(6)

 

< operator   4.5(1), 4.5.2(1)

<= operator   4.5(1), 4.5.2(1)

 

= operator   4.5(1), 4.5.2(1)

 

> operator   4.5(1), 4.5.2(1)

>= operator   4.5(1), 4.5.2(1)

A

abnormal completion   7.6.1(2)

abnormal state of an object   13.9.1(4)   [partial]   9.8(21), 11.6(6), A.13(17)

abnormal task   9.8(4)

abnormal termination   of a partition   10.2(26.c)

abort   of a partition   E.1(7)   of a task   9.8(4)   of the execution of a construct   9.8(5)

abort completion point   9.8(15)

abort-deferred operation   9.8(5)

abort_statement   9.8(2)   used   5.1(5), P.1

Abort_Task   in Ada.Task_Identification   C.7.1(3)

abortable_part   9.7.4(5)   used   9.7.4(2), P.1

abs operator   4.5(1), 4.5.6(1)

absolute value   4.5(1), 4.5.6(1)

abstract data type (ADT)   See private types and private extensions   7.3(1)   See also abstract type   3.9.3(1)

abstract subprogram   3.9.3(1), 3.9.3(3)

abstract type   3.9.3(1), 3.9.3(2)

abstract_subprogram_declaration   6.1(3)   used   3.1(3), P.1

accept_alternative   9.7.1(5)   used   9.7.1(4), P.1

accept_statement   9.5.2(3)   used   5.1(6), 9.7.1(5), P.1

acceptable interpretation   8.6(14)

Access attribute   See also Unchecked_Access attribute   13.10(3)

access discriminant   3.7(10)

access parameter   6.1(24)

access paths   distinct   6.2(12)

access result type   6.1(24)

access type   3.10(1)

access types   input-output unspecified   A.7(6)

access value   3.10(1)

access-to-constant type   3.10(11)

access-to-object type   3.10(7)

access-to-subprogram type   3.10(7), 3.10(12)

access-to-variable type   3.10(11)

Access_Check   11.5(14)   [partial]   4.1(13), 4.6(49)

access_definition   3.10(6)   used   3.7(6), 6.1(15), P.1

access_to_object_definition   3.10(3)   used   3.10(2), P.1

access_to_subprogram_definition   3.10(5)   used   3.10(2), P.1

access_type_definition   3.10(2)   used   3.2.1(4), 12.5.4(2), P.1

accessibility   from shared passive library units   E.2.1(8)

accessibility level   3.10.2(3)

accessibility rule   Access attribute   3.10.2(28), 3.10.2(32)   checking in generic units   12.3(11.s)   not part of generic contract   3.9.1(4.k)   record extension   3.9.1(3)   requeue statement   9.5.4(6)   type conversion   4.6(17), 4.6(20)

Accessibility_Check   11.5(24)   [partial]   3.10.2(29), 4.6(48), 6.5(17), E.4(18)

accessible partition   E.1(7)

accuracy   4.6(32), G.2(1)

ACID   1.3(1.c)

ACK   in Ada.Characters.Latin_1   A.3.3(5)

acquire   execution resource associated with protected object   9.5.1(5)

actions   sequential   9.10(11)

activation   of a task   9.2(1)

activation failure   9.2(1)

activator   of a task   9.2(5)

active partition   10.2(29), E.1(2)

active priority   D.1(15)

actual   12.3(7)

actual duration   D.9(12)

actual parameter   for a formal parameter   6.4.1(3)

actual subtype   3.3(24), 12.5(4)   of an object   3.3.1(9)

actual type   12.5(4)

actual_parameter_part   6.4(4)   used   6.4(2), 6.4(3), 9.5.3(2), P.1

Acute   in Ada.Characters.Latin_1   A.3.3(22)

ACVC   Ada Compiler Validation Capability   1.1.1(54.b)

Ada   A.2(2)

Ada calling convention   6.3.1(3)

Ada Commentary Integration Document (ACID)   1.3(1.c)

Ada Compiler Validation Capability   ACVC   1.1.1(54.b)

Ada Issue (AI)   1.3(1.c)

Ada Rapporteur Group (ARG)   1.3(1.c)

Ada.Asynchronous_Task_Control   D.11(3)

Ada.Calendar   9.6(10)

Ada.Characters   A.3.1(2)

Ada.Characters.Handling   A.3.2(2)

Ada.Characters.Latin_1   A.3.3(3)

Ada.Command_Line   A.15(3)

Ada.Decimal   F.2(2)

Ada.Direct_IO   A.8.4(2)

Ada.Dynamic_Priorities   D.5(3)

Ada.Exceptions   11.4.1(2)

Ada.Finalization   7.6(5)

Ada.Float_Text_IO   A.10.9(33)

Ada.Float_Wide_Text_IO   A.11(3)

Ada.Integer_Text_IO   A.10.8(21)

Ada.Integer_Wide_Text_IO   A.11(3)

Ada.Interrupts   C.3.2(2)

Ada.Interrupts.Names   C.3.2(12)

Ada.IO_Exceptions   A.13(3)

Ada.Numerics   A.5(3)

Ada.Numerics.Complex_Elementary_Functions   G.1.2(9)

Ada.Numerics.Complex_Types   G.1.1(25)

Ada.Numerics.Discrete_Random   A.5.2(17)

Ada.Numerics.Elementary_Functions   A.5.1(9)

Ada.Numerics.Float_Random   A.5.2(5)

Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(2)

Ada.Numerics.Generic_Complex_Types   G.1.1(2)

Ada.Numerics.Generic_Elementary_Functions   A.5.1(3)

Ada.Real_Time   D.8(3)

Ada.Sequential_IO   A.8.1(2)

Ada.Storage_IO   A.9(3)

Ada.Streams   13.13.1(2)

Ada.Streams.Stream_IO   A.12.1(3)

Ada.Strings   A.4.1(3)

Ada.Strings.Bounded   A.4.4(3)

Ada.Strings.Fixed   A.4.3(5)

Ada.Strings.Maps   A.4.2(3)

Ada.Strings.Maps.Constants   A.4.6(3)

Ada.Strings.Unbounded   A.4.5(3)

Ada.Strings.Wide_Bounded   A.4.7(1)

Ada.Strings.Wide_Bounded.Wide_Hash   A.4.7(1)

Ada.Strings.Wide_Fixed   A.4.7(1)

Ada.Strings.Wide_Fixed.Wide_Hash   A.4.7(1)

Ada.Strings.Wide_Hash   A.4.7(1)

Ada.Strings.Wide_Maps   A.4.7(3)

Ada.Strings.Wide_Maps.Wide_Constants   A.4.7(1)

Ada.Strings.Wide_Unbounded   A.4.7(1)

Ada.Strings.Wide_Unbounded.Wide_Hash   A.4.7(1)

Ada.Synchronous_Task_Control   D.10(3)

Ada.Tags   3.9(7)

Ada.Task_Attributes   C.7.2(2)

Ada.Task_Identification   C.7.1(2)

Ada.Text_IO   A.10.1(2)

Ada.Text_IO.Complex_IO   G.1.3(3)

Ada.Text_IO.Editing   F.3.3(3)

Ada.Text_IO.Text_Streams   A.12.2(3)

Ada.Unchecked_Conversion   13.9(3)

Ada.Unchecked_Deallocation   13.11.2(3)

Ada.Wide_Text_IO   A.11(2)

Ada.Wide_Text_IO.Complex_IO   G.1.4(1)

Ada.Wide_Text_IO.Editing   F.3.4(1)

Ada.Wide_Text_IO.Text_Streams   A.12.3(3)

Ada_To_COBOL   in Interfaces.COBOL   B.4(14)

address   arithmetic   13.7.1(6)   comparison   13.7(14)   in System   13.7(12)

Address attribute   13.3(11), J.7.1(5)

Address clause   13.3(7), 13.3(12)

Address_To_Access_Conversions   child of System   13.7.2(2)

Adjacent attribute   A.5.3(49)

Adjust   7.6(2)   in Ada.Finalization   7.6(7)

adjusting the value of an object   7.6(16), 7.6(17)

adjustment   7.6(16), 7.6(17)   as part of assignment   5.2(14)

ADT (abstract data type)   See private types and private extensions   7.3(1)   See also abstract type   3.9.3(1)

advice   1.1.1(54)

Aft attribute   3.5.10(5)

aggregate   4.3(1), 4.3(3)   used   4.4(7), 4.7(2), P.1   See also composite type   3.2(3)

AI   1.3(1.c)

aliased   3.10(9)

aliasing   See distinct access paths   6.2(12)

Alignment   in Ada.Strings   A.4.1(6)

Alignment attribute   13.3(23)

Alignment clause   13.3(7), 13.3(25)

All_Calls_Remote pragma   E.2.3(5), L(2)

All_Checks   11.5(28)

Allocate   in System.Storage_Pools   13.11(8)

allocator   4.8(2)   used   4.4(7), P.1

Alphanumeric   in Interfaces.COBOL   B.4(16)

alphanumeric character   a category of Character   A.3.2(31)

Alphanumeric_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

ambiguous   8.6(30)

ambiguous grammar   1.1.1(94.a)

ampersand   2.1(15)   in Ada.Characters.Latin_1   A.3.3(8)

ampersand operator   4.5(1), 4.5.3(3)

ancestor   of a library unit   10.1.1(11)   of a type   3.4.1(10)   ultimate   3.4.1(10)

ancestor subtype   of a formal derived type   12.5.1(5)   of a private_extension_declaration   7.3(8)

ancestor_part   4.3.2(3)   used   4.3.2(2), P.1

and operator   4.5(1), 4.5.1(2)

and then (short-circuit control form)   4.5(1), 4.5.1(1)

angle threshold   G.2.4(10)

Annex   informative   1.1.1(36)   normative   1.1.1(32)   Specialized Needs   1.1.1(24)

anonymous access type   3.10(13)

anonymous array type   3.3.1(1)

anonymous protected type   3.3.1(1)

anonymous task type   3.3.1(1)

anonymous type   3.2.1(7)

Any_Priority subtype of Integer   in System   13.7(16)

APC   in Ada.Characters.Latin_1   A.3.3(19)

apostrophe   2.1(15)   in Ada.Characters.Latin_1   A.3.3(8)

Append   in Ada.Strings.Bounded   A.4.4(13), A.4.4(14), A.4.4(15), A.4.4(16), A.4.4(17), A.4.4(18), A.4.4(19), A.4.4(20)   in Ada.Strings.Unbounded   A.4.5(12), A.4.5(13), A.4.5(14)

applicable index constraint   4.3.3(11)

application areas   1.1.1(24)

apply   to a callable construct by a return_statement   6.5(4)   to a loop_statement by an exit_statement   5.7(4)   to a program unit by a program unit pragma   10.1.5(2)

arbitrary order   1.1.1(98)   allowed   2.8(12), 3.3.1(20), 3.5(9), 3.6(22), 3.11(10), 3.11(11), 3.11(13), 4.1.1(7), 4.1.2(7), 4.3(6), 4.3.1(20), 4.3.2(7), 4.3.3(23), 4.3.3(24), 4.5.2(27), 4.8(10), 5.2(7), 6.4(10), 6.4.1(17), 7.6(13), 7.6(17), 7.6.1(9), 7.6.1(11), 9.7.1(15), 9.8(4), 12.3(20)

Arccos   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arccosh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Arccot   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arccoth   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Arcsin   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arcsinh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Arctan   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)

Arctanh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

ARG   1.3(1.c)

Argument   in Ada.Command_Line   A.15(5)   in Ada.Numerics.Generic_Complex_Types   G.1.1(10)

argument of a pragma   2.8(9)

Argument_Count   in Ada.Command_Line   A.15(4)

Argument_Error   in Ada.Numerics   A.5(3)

array   3.6(1)

array component expression   4.3.3(7)

array indexing   See indexed_component   4.1.1(1)

array slice   4.1.2(1)

array type   3.6(1)

array_aggregate   4.3.3(2)   used   4.3(3), 13.4(3), P.1

array_component_association   4.3.3(6)   used   4.3.3(5), P.1

array_type_definition   3.6(2)   used   3.2.1(4), 3.3.1(2), 12.5.3(2), P.1

ASCII   package physically nested within the declaration of Standard   A.1   in Standard   A.1

aspect of representation   13.1(10)   coding   13.4(7)   controlled   13.11.3(5)   convention, calling convention   B.1(28)   exported   B.1(28)   imported   B.1(28)   layout   13.5(1)   packing   13.2(5)   record layout   13.5(1)   specifiable attributes   13.3(5)   storage place   13.5(1)

assembly language   C.1(4)

assign   See assignment operation   5.2(3)

assigning back of parameters   6.4.1(17)

assignment   user-defined   7.6(1)

assignment operation   5.2(3), 5.2(12), 7.6(14)   during elaboration of an object_declaration   3.3.1(19)   during evaluation of a generic_association for a formal object of mode in   12.4(11)   during evaluation of a parameter_association   6.4.1(11)   during evaluation of an aggregate   4.3(6)   during evaluation of an initialized allocator   4.8(7)   during evaluation of an uninitialized allocator   4.8(9), 4.8(10)   during evaluation of concatenation   4.5.3(10)   during execution of a for loop   5.5(10)   during execution of a return_statement   6.5(21)   during execution of an assignment_statement   5.2(12)   during parameter copy back   6.4.1(17)   list of uses   7.6.1(24.d)

assignment_statement   5.2(2)   used   5.1(5), P.1

associated components   of a record_component_association   4.3.1(11)

associated discriminants   of a named discriminant_association   3.7.1(5)   of a positional discriminant_association   3.7.1(5)

associated object   of a value of a by-reference type   6.2(10)   of a value of a limited type   6.2(10.f)

asterisk   2.1(15)   in Ada.Characters.Latin_1   A.3.3(8)

asynchronous   remote procedure call   E.4.1(9)

Asynchronous pragma   E.4.1(3), L(3)

asynchronous remote procedure call   E.4(1)

asynchronous_select   9.7.4(2)   used   9.7(2), P.1

Asynchronous_Task_Control   child of Ada   D.11(3)

at-most-once execution   E.4(11)

at_clause   J.7(1)   used   13.1(4), P.1

atomic   C.6(7)

Atomic pragma   C.6(3), L(4)

Atomic_Components pragma   C.6(5), L(5)

Attach_Handler   in Ada.Interrupts   C.3.2(7)

Attach_Handler pragma   C.3.1(4), L(6)

attaching   to an interrupt   C.3(2)

attribute   4.1.4(1), K(4)   representation   13.3(1)   specifiable   13.3(5)   specifying   13.3(1)   stream-oriented   13.13.2(1)

attribute_definition_clause   13.3(2)   used   13.1(4), P.1

attribute_designator   4.1.4(3)   used   4.1.4(2), 13.1(5), 13.3(2), P.1

Attribute_Handle   in Ada.Task_Attributes   C.7.2(3)

attribute_reference   4.1.4(2)   used   4.1(2), P.1

attributes   Address   13.3(11), J.7.1(5)   Adjacent   A.5.3(49)   Aft   3.5.10(5)   Alignment   13.3(23)   Base   3.5(15)   Bit_Order   13.5.3(4)   Body_Version   E.3(4)   Callable   9.9(2)   Caller   C.7.1(14)   Ceiling   A.5.3(33)   Class   3.9(16), 7.3.1(9)   Component_Size   13.3(71)   Compose   A.5.3(24)   Constrained   3.7.2(3), J.4(2)   Copy_Sign   A.5.3(52)   Count   9.9(5)   Definite   12.5.1(23)   Delta   3.5.10(3)   Denorm   A.5.3(9)   Digits   3.5.10(7)   Exponent   A.5.3(18)   External_Tag   13.3(79)   First   3.5(12), 3.6.2(3)   First(N)   3.6.2(4)   First_Bit   13.5.2(3)   Floor   A.5.3(30)   Fore   3.5.10(4)   Fraction   A.5.3(21)   Identity   11.4.1(9), C.7.1(12)   Image   3.5(36), 4.9.1(8)   Input   13.13.2(23), 13.13.2(33)   Last   3.5(13), 3.6.2(5)   Last(N)   3.6.2(6)   Last_Bit   13.5.2(4)   Leading_Part   A.5.3(55)   Length   3.6.2(9)   Length(N)   3.6.2(10)   Machine   A.5.3(61)   Machine_Emax   A.5.3(8)   Machine_Emin   A.5.3(7)   Machine_Mantissa   A.5.3(6)   Machine_Overflows   A.5.3(12), A.5.4(4)   Machine_Radix   A.5.3(2), A.5.4(2)   Machine_Rounds   A.5.3(11), A.5.4(3)   Max   3.5(19)   Max_Size_In_Storage_Elements   13.11.1(3)   Min   3.5(16)   Model   A.5.3(69), G.2.2(7)   Model_Emin   A.5.3(66), G.2.2(4)   Model_Epsilon   A.5.3(67)   Model_Mantissa   A.5.3(65), G.2.2(3)   Model_Small   A.5.3(68)   Modulus   3.5.4(18)   Output   13.13.2(20), 13.13.2(30)   Partition_Id   E.1(9)   Pos   3.5.5(2)   Position   13.5.2(2)   Pred   3.5(25)   Range   3.5(14), 3.6.2(7)   Range(N)   3.6.2(8)   Read   13.13.2(7), 13.13.2(15)   Remainder   A.5.3(46)   Round   3.5.10(12)   Rounding   A.5.3(36)   Safe_First   A.5.3(72), G.2.2(5)   Safe_Last   A.5.3(73), G.2.2(6)   Scale   3.5.10(11)   Scaling   A.5.3(27)   Signed_Zeros   A.5.3(13)   Size   13.3(41), 13.3(46)   Storage_Pool   13.11(14)   Storage_Size   13.11(15), J.9(2)   Succ   3.5(22)   Tag   3.9(18), 3.9(20)   Terminated   9.9(3)   Truncation   A.5.3(43)   Unbiased_Rounding   A.5.3(39)   Unchecked_Access   13.10(3), H.4(18)   Val   3.5.5(5)   Valid   13.9.2(3), H(6)   Value   3.5(55)   Version   E.3(3)   Wide_Image   3.5(29), 4.9.1(7)   Wide_Value   3.5(43)   Wide_Width   3.5(40)   Width   3.5(41)   Write   13.13.2(4), 13.13.2(12)

avoid overspecifying environmental issues   10(4.a)

B

Backus-Naur Form (BNF)   complete listing   P.1(1)   cross reference   P.2(1)   notation   1.1.1(83)   under Syntax heading   1.1.1(42)

base   2.4.2(3), 2.4.2(6)   used   2.4.2(2), P.1

base 16 literal   2.4.2(1)

base 2 literal   2.4.2(1)

base 8 literal   2.4.2(1)

Base attribute   3.5(15)

base decimal precision   of a floating point type   3.5.7(9)   of a floating point type   3.5.7(10)

base priority   D.1(15)

base range   of a decimal fixed point type   3.5.9(16)   of a fixed point type   3.5.9(12)   of a floating point type   3.5.7(8), 3.5.7(10)   of a modular type   3.5.4(10)   of a scalar type   3.5(6)   of a signed integer type   3.5.4(9)   of an enumeration type   3.5(6.b)   of an ordinary fixed point type   3.5.9(13)

base subtype   of a type   3.5(15)

based_literal   2.4.2(2)   used   2.4(2), P.1

based_numeral   2.4.2(4)   used   2.4.2(2), P.1

basic letter   a category of Character   A.3.2(27)

basic_declaration   3.1(3)   used   3.11(4), P.1

basic_declarative_item   3.11(4)   used   3.11(3), 7.1(3), P.1

Basic_Map   in Ada.Strings.Maps.Constants   A.4.6(5)

Basic_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Beaujolais effect   8.4(1.b)   [partial]   3.6(18.b), 8.6(22.a), 8.6(34.a), 8.6(34.k)

become nonlimited   7.3.1(5), 7.5(17)

BEL   in Ada.Characters.Latin_1   A.3.3(5)

belong   to a range   3.5(4)   to a subtype   3.2(10)

bibliography   1.2(1)

big endian   13.5.3(2)

binary   literal   2.4.2(1)   in Interfaces.COBOL   B.4(10)

binary adding operator   4.5.3(1)

binary literal   2.4.2(1)

binary operator   4.5(9)

binary_adding_operator   4.5(4)   used   4.4(4), P.1

Binary_Format   in Interfaces.COBOL   B.4(24)

bit field   See record_representation_clause   13.5.1(1)

bit ordering   13.5.3(2)

bit string   See logical operators on boolean arrays   4.5.1(2)

Bit_Order   in System   13.7(15)

Bit_Order attribute   13.5.3(4)

Bit_Order clause   13.3(7), 13.5.3(4)

blank   in text input for enumeration and numeric types   A.10.6(5)

Blank_When_Zero   in Ada.Text_IO.Editing   F.3.3(7)

block_statement   5.6(2)   used   5.1(6), P.1

blocked   [partial]   D.2.1(11)   a task state   9(11)   during an entry call   9.5.3(19)   execution of a selective_accept   9.7.1(16)   on a delay_statement   9.6(21)   on an accept_statement   9.5.2(24)   waiting for activations to complete   9.2(5)   waiting for dependents to terminate   9.3(5)

blocked interrupt   C.3(2)

blocking, potentially   9.5.1(8)   Abort_Task   C.7.1(16)   delay_statement   9.6(34), D.9(5)   remote subprogram call   E.4(17)   RPC operations   E.5(23)   Suspend_Until_True   D.10(10)

BMP   3.5.2(2), 3.5.2(3)

BNF (Backus-Naur Form)   complete listing   P.1(1)   cross reference   P.2(1)   notation   1.1.1(83)   under Syntax heading   1.1.1(42)

body   3.11(5)   used   3.11(3), P.1

body_stub   10.1.3(2)   used   3.11(5), P.1

Body_Version attribute   E.3(4)

Boolean   3.5.3(1)   in Standard   A.1(5)

boolean type   3.5.3(1)

Bounded   child of Ada.Strings   A.4.4(3)

bounded error   1.1.1(110), 1.1.1(48)   cause   6.2(12), 7.6.1(14), 9.5.1(8), 9.8(20), 10.2(27), 13.9.1(9), 13.11.2(11), C.7.1(17), D.5(11), E.1(10), E.3(6), J.7.1(11)

Bounded_String   in Ada.Strings.Bounded   A.4.4(6)

bounds   of a discrete_range   3.6.1(6)   of an array   3.6(13)   of the index range of an array_aggregate   4.3.3(25)

box   compound delimiter   3.6(15)

BPH   in Ada.Characters.Latin_1   A.3.3(17)

broadcast signal   See protected object   9.4(1)   See requeue   9.5.4(1)

Broken_Bar   in Ada.Characters.Latin_1   A.3.3(21)

BS   in Ada.Characters.Latin_1   A.3.3(5)

Buffer_Size   in Ada.Storage_IO   A.9(4)

Buffer_Type subtype of Storage_Array   in Ada.Storage_IO   A.9(4)

by copy parameter passing   6.2(2)

by reference parameter passing   6.2(2)

by-copy type   6.2(3)

by-reference type   6.2(4)   atomic or volatile   C.6(18)

Byte   in Interfaces.COBOL   B.4(29)   See storage element   13.3(8)

byte sex   See ordering of storage elements in a word   13.5.3(5)

Byte_Array   in Interfaces.COBOL   B.4(29)

C

C   child of Interfaces   B.3(4)

C interface   B.3(1)

C_float   in Interfaces.C   B.3(15)

Calendar   child of Ada   9.6(10)

call   6(3)

call on a dispatching operation   3.9.2(2)

callable   9.9(2)

Callable attribute   9.9(2)

callable construct   6(3)

callable entity   6(3)

called partition   E.4(1)

Caller attribute   C.7.1(14)

calling convention   6.3.1(2), B.1(11)   Ada   6.3.1(3)   associated with a designated profile   3.10(12)   entry   6.3.1(13)   Intrinsic   6.3.1(4)   protected   6.3.1(12)

calling partition   E.4(1)

calling stub   E.4(10)

CAN   in Ada.Characters.Latin_1   A.3.3(6)

cancellation   of a delay_statement   9.6(22)   of an entry call   9.5.3(20)

cancellation of a remote subprogram call   E.4(13)

canonical form   A.5.3(3)

canonical semantics   11.6(2)

canonical-form representation   A.5.3(10)

case insensitive   2.3(5)

case_statement   5.4(2)   used   5.1(6), P.1

case_statement_alternative   5.4(3)   used   5.4(2), P.1

cast   See type conversion   4.6(1)   See unchecked type conversion   13.9(1)

catch (an exception)   See handle   11(1)

categorization pragma   E.2(2)   Remote_Call_Interface   E.2.3(2)   Remote_Types   E.2.2(2)   Shared_Passive   E.2.1(2)

categorized library unit   E.2(2)

catenation operator   See concatenation operator   4.5(1)   See concatenation operator   4.5.3(3)

CCH   in Ada.Characters.Latin_1   A.3.3(18)

cease to exist   object   13.11.2(10)

Cedilla   in Ada.Characters.Latin_1   A.3.3(22)

Ceiling attribute   A.5.3(33)

ceiling priority   of a protected object   D.3(8)

Ceiling_Check   [partial]   C.3.1(11), D.3(13)

Cent_Sign   in Ada.Characters.Latin_1   A.3.3(21)

change of representation   13.6(1)

char   in Interfaces.C   B.3(19)

char_array   in Interfaces.C   B.3(23)

char_array_access   in Interfaces.C.Strings   B.3.1(4)

CHAR_BIT   in Interfaces.C   B.3(6)

character   2.1(2), 3.5.2(2)   used   2.7(2), P.1   in Standard   A.1(35)

character set   2.1(1)

character type   3.5.2(1)

character_literal   2.5(2)   used   3.5.1(4), 4.1(2), 4.1.3(3), P.1

Character_Mapping   in Ada.Strings.Maps   A.4.2(20)

Character_Mapping_Function   in Ada.Strings.Maps   A.4.2(25)

Character_Range   in Ada.Strings.Maps   A.4.2(6)

Character_Ranges   in Ada.Strings.Maps   A.4.2(7)

Character_Sequence subtype of String   in Ada.Strings.Maps   A.4.2(16)

Character_Set   in Ada.Strings.Maps   A.4.2(4)   in Ada.Strings.Wide_Maps   A.4.7(46)   in Interfaces.Fortran   B.5(11)

characteristics   [partial]   7.3(15)

Characters   child of Ada   A.3.1(2)

chars_ptr   in Interfaces.C.Strings   B.3.1(5)

chars_ptr_array   in Interfaces.C.Strings   B.3.1(6)

check   language-defined   11.5(3), 11.6(1)

check, language-defined   Access_Check   4.1(13), 4.6(49)   Accessibility_Check   3.10.2(29), 4.6(48), 6.5(17), E.4(18)   Ceiling_Check   C.3.1(11), D.3(13)   Discriminant_Check   4.1.3(15), 4.3(7), 4.3.2(8), 4.6(43), 4.6(45), 4.6(51), 4.6(52), 4.7(4), 4.8(10)   Division_Check   3.5.4(21), 4.5.5(22), A.5.3(48), G.1.1(40), K(203)   Elaboration_Check   3.11(9)   Index_Check   4.1.1(7), 4.1.2(7), 4.3.3(30), 4.3.3(31), 4.5.3(8), 4.6(51), 4.7(4), 4.8(10)   Length_Check   4.5.1(8), 4.6(37), 4.6(52)   Overflow_Check   3.5.4(21), 4.4(11), G.2.1(11), G.2.2(7), G.2.3(25), G.2.4(2), G.2.6(3)   Partition_Check   E.4(19)   Range_Check   3.2.2(11), 3.5(24), 3.5(27), 3.5(46), 3.5(47), 3.5(54), 3.5(58), 3.5.5(7), 3.5.9(19), 4.2(11), 4.3.3(29), 4.5.1(8), 4.5.6(6), 4.5.6(13), 4.6(28), 4.6(38), 4.6(46), 4.6(51), 4.7(4), 13.13.2(36), A.5.3(26), A.5.3(29), A.5.3(51), A.5.3(54), A.5.3(60), A.5.3(63), K(115), K(123), K(14), K(185), K(221), K(242), K(44), K(50)   Reserved_Check   C.3.1(10)   Storage_Check   11.1(6), 13.3(69), 13.11(18), D.7(15)   Tag_Check   3.9.2(16), 4.6(42), 4.6(52), 5.2(10), 6.5(9)   Tasking_Check   9.2(5), 9.5.3(21)

child   of a library unit   10.1.1(1)

choice   of an exception_handler   11.2(5.b)

choice parameter   11.2(9)

choice_parameter_specification   11.2(4)   used   11.2(3), P.1

Circumflex   in Ada.Characters.Latin_1   A.3.3(12)

class   of types   3.2(3)   See also package   7(1)   See also tag   3.9(4)

Class attribute   3.9(16), 7.3.1(9)

class determined for a formal type   12.5(6)

class-wide type   3.4.1(4), 3.7(27)

cleanup   See finalization   7.6.1(1)

clock   9.6(6)   in Ada.Calendar   9.6(12)   in Ada.Real_Time   D.8(6)

clock jump   D.8(32)

clock tick   D.8(23)

Close   in Ada.Direct_IO   A.8.4(8)   in Ada.Sequential_IO   A.8.1(8)   in Ada.Streams.Stream_IO   A.12.1(10)   in Ada.Text_IO   A.10.1(11)

close result set   G.2.3(5)

closed entry   9.5.3(5)   of a protected object   9.5.3(7)   of a task   9.5.3(6)

closed under derivation   3.2(1.a/0), 3.2(3.a/0), 3.2(4), 3.4(30)

closure   downward   3.10.2(37)

COBOL   child of Interfaces   B.4(7)

COBOL interface   B.4(1)

COBOL_Character   in Interfaces.COBOL   B.4(13)

COBOL_To_Ada   in Interfaces.COBOL   B.4(15)

code_statement   13.8(2)   used   5.1(5), P.1

coding   aspect of representation   13.4(7)

Col   in Ada.Text_IO   A.10.1(37)

colon   2.1(15)   in Ada.Characters.Latin_1   A.3.3(10)

column number   A.10(9)

comma   2.1(15)   in Ada.Characters.Latin_1   A.3.3(8)

Command_Line   child of Ada   A.15(3)

Command_Name   in Ada.Command_Line   A.15(6)

comment   2.7(2)

comments, instructions for submission   0.2(59)

Commercial_At   in Ada.Characters.Latin_1   A.3.3(10)

Communication_Error   in System.RPC   E.5(5)

comparison operator   See relational operator   4.5.2(1)

compatibility   composite_constraint with an access subtype   3.10(16)   constraint with a subtype   3.2.2(12)   delta_constraint with an ordinary fixed point subtype   J.3(9)   digits_constraint with a decimal fixed point subtype   3.5.9(18)   digits_constraint with a floating point subtype   J.3(10)   discriminant constraint with a subtype   3.7.1(10)   index constraint with a subtype   3.6.1(7)   range with a scalar subtype   3.5(8)   range_constraint with a scalar subtype   3.5(8)

compatible   a type, with a convention   B.1(12)

compilation   10.1.1(2)   separate   10.1(1)

Compilation unit   10.1(2), 10.1.1(9)

compilation units needed   by a compilation unit   10.2(2)   remote call interface   E.2.3(18)   shared passive library unit   E.2.1(11)

compilation_unit   10.1.1(3)   used   10.1.1(2), P.1

compile-time error   1.1.1(106), 1.1.1(44)

compile-time semantics   1.1.1(45)

complete context   8.6(4)

completely defined   3.11.1(8)

completion   abnormal   7.6.1(2)   compile-time concept   3.11.1(1)   normal   7.6.1(2)   run-time concept   7.6.1(2)

completion and leaving (completed and left)   7.6.1(2)

completion legality   [partial]   3.10.1(13)   entry_body   9.5.2(16)

Complex   in Ada.Numerics.Generic_Complex_Types   G.1.1(3)   in Interfaces.Fortran   B.5(9)

Complex_Elementary_Functions   child of Ada.Numerics   G.1.2(9)

Complex_IO   child of Ada.Text_IO   G.1.3(3)   child of Ada.Wide_Text_IO   G.1.4(1)

Complex_Types   child of Ada.Numerics   G.1.1(25)

component   3.2(3)

component subtype   3.6(10)

component_choice_list   4.3.1(5)   used   4.3.1(4), P.1

component_clause   13.5.1(3)   used   13.5.1(2), P.1

component_declaration   3.8(6)   used   3.8(5), 9.4(6), P.1

component_definition   3.6(7)   used   3.6(3), 3.6(5), 3.8(6), P.1

component_item   3.8(5)   used   3.8(4), P.1

component_list   3.8(4)   used   3.8(3), 3.8.1(3), P.1

Component_Size attribute   13.3(71)

Component_Size clause   13.3(7), 13.3(72)

components   of a record type   3.8(9)

Compose attribute   A.5.3(24)

Compose_From_Cartesian   in Ada.Numerics.Generic_Complex_Types   G.1.1(8)

Compose_From_Polar   in Ada.Numerics.Generic_Complex_Types   G.1.1(11)

composite type   3.2(3)

composite_constraint   3.2.2(7)   used   3.2.2(5), P.1

compound delimiter   2.2(10)

compound_statement   5.1(6)   used   5.1(4), P.1

concatenation operator   4.5(1), 4.5.3(3)

concrete subprogram   See nonabstract subprogram   3.9.3(1)

concrete type   See nonabstract type   3.9.3(1)

concurrent processing   See task   9(1)

condition   5.3(3)   used   5.3(2), 5.5(3), 5.7(2), 9.5.2(7), 9.7.1(3), P.1   See also exception   11(1)

conditional_entry_call   9.7.3(2)   used   9.7(2), P.1

configuration   of the partitions of a program   E(4)

configuration pragma   10.1.5(8)   Discard_Names   C.5(4)   Locking_Policy   D.3(5)   Normalize_Scalars   H.1(4)   Queuing_Policy   D.4(5)   Restrictions   13.12(8)   Reviewable   H.3.1(4)   Suppress   11.5(8)   Task_Dispatching_Policy   D.2.2(5)

conformance   6.3.1(1)   of an implementation   1.1.1(58)   See also full conformance, mode conformance, subtype conformance, type conformance

Conjugate   in Ada.Numerics.Generic_Complex_Types   G.1.1(12), G.1.1(15)

consistency   among compilation units   10.1.4(5)

constant   3.3(14)   result of a function_call   6.4(12)   See also literal   4.2(1)   See also static   4.9(1)

constant object   3.3(14)

constant view   3.3(14)

Constants   child of Ada.Strings.Maps   A.4.6(3)

constituent   of a construct   1.1.1(97)

constrained   3.2(11)   object   3.3.1(9), 3.10(9)   object   6.4.1(16)   subtype   3.2(11), 3.4(8), 3.5(7), 3.5.1(10), 3.5.4(9), 3.5.4(10), 3.5.7(11), 3.5.9(13), 3.5.9(16), 3.6(15), 3.6(16), 3.7(27), 3.9(17)   subtype   3.10(15)   subtype   K(36)

Constrained attribute   3.7.2(3), J.4(2)

constrained by its initial value   3.3.1(9), 3.10(9)   [partial]   4.8(6)

constrained_array_definition   3.6(5)   used   3.6(2), P.1

constraint   3.2.2(5)   [partial]   3.2(9)   null   3.2(9)   of a first array subtype   3.6(16)   of an object   3.3.1(9)   used   3.2.2(3), P.1

Constraint_Error   raised by case statement   5.4(13)   raised by detection of a bounded error   13.9.1(9)   raised by failure of runtime check   3.2.2(12), 3.5(24), 3.5(27), 3.5(46), 3.5(47), 3.5(54), 3.5(58), 3.5.4(21), 3.5.5(7), 3.5.9(19), 3.9.2(16), 4.1(13), 4.1.1(7), 4.1.2(7), 4.1.3(15), 4.2(11), 4.3(7), 4.3.2(8), 4.3.3(32), 4.4(11), 4.5(10), 4.5(11), 4.5(12), 4.5.1(8), 4.5.3(8), 4.5.5(22), 4.5.6(6), 4.5.6(13), 4.6(28), 4.6(57), 4.6(60), 4.7(4), 4.8(10), 5.2(10), 6.5(9), 11.1(4), 11.5(13), 13.13.2(36), A.5.3(26), A.5.3(29), A.5.3(48), A.5.3(51), A.5.3(54), A.5.3(60), A.5.3(63), E.4(19), E.4(20.u), E.4(20.v), G.1.1(40), G.2.1(12), G.2.2(7), G.2.3(26), G.2.4(3), G.2.6(4), K(115), K(123), K(14), K(185), K(203), K(221), K(242), K(262), K(44), K(50)   in Standard   A.1(46)

Construct   1.1.1(96)

constructor   See initialization   3.3.1(19)   See initialization   7.6(1)   See initialization expression   3.3.1(4)   See Initialize   7.6(1)   See initialized allocator   4.8(4)

context free grammar   complete listing   P.1(1)   cross reference   P.2(1)   notation   1.1.1(83)   under Syntax heading   1.1.1(42)

context_clause   10.1.2(2)   used   10.1.1(3), P.1

context_item   10.1.2(3)   used   10.1.2(2), P.1

contiguous representation   [partial]   13.1(9.a), 13.5.2(5), 13.7.1(12), 13.9(9), 13.9(17), 13.11(17), 13.11(18.d)

Continue   in Ada.Asynchronous_Task_Control   D.11(3)

contract model of generics   12.3(1.a)

control character   a category of Character   A.3.2(22)   a category of Character   A.3.3(4), A.3.3(15)   See also format_effector   2.1(13)   See also other_control_function   2.1(14)

Control_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

controlled   aspect of representation   13.11.3(5)   in Ada.Finalization   7.6(6)

Controlled pragma   13.11.3(3), L(7)

controlled type   7.6(2), 7.6(10)

controlling formal parameter   3.9.2(2)

controlling operand   3.9.2(2)

controlling result   3.9.2(2)

controlling tag   for a call on a dispatching operation   3.9.2(1)

controlling tag value   3.9.2(14)   for the expression in an assignment_statement   5.2(9)

convention   6.3.1(2), B.1(11)   aspect of representation   B.1(28)

Convention pragma   B.1(7), L(8)

conversion   4.6(1), 4.6(28)   access   4.6(13), 4.6(18), 4.6(47)   arbitrary order   1.1.1(98)   array   4.6(9), 4.6(36)   composite (non-array)   4.6(21), 4.6(40)   enumeration   4.6(21), 4.6(34)   numeric   4.6(8), 4.6(29)   unchecked   13.9(1)   value   4.6(5)   view   4.6(5)

Conversion_Error   in Interfaces.COBOL   B.4(30)

convertible   4.6(4)   required   3.7(17), 3.7.1(9), 4.6(11), 4.6(15), 6.4.1(6)

copy back of parameters   6.4.1(17)

copy parameter passing   6.2(2)

Copy_Array   in Interfaces.C.Pointers   B.3.2(15)

Copy_Sign attribute   A.5.3(52)

Copy_Terminated_Array   in Interfaces.C.Pointers   B.3.2(14)

Copyright_Sign   in Ada.Characters.Latin_1   A.3.3(21)

core language   1.1.1(18)

corresponding constraint   3.4(8)

corresponding discriminants   3.7(19)

corresponding index   for an array_aggregate   4.3.3(9)

corresponding subtype   3.4(20)

corresponding value   of the target type of a conversion   4.6(28)

Cos   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

Cosh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Cot   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

Coth   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

Count   in Ada.Direct_IO   A.8.4(4)   in Ada.Streams.Stream_IO   A.12.1(7)   in Ada.Strings.Bounded   A.4.4(48), A.4.4(49), A.4.4(50)   in Ada.Strings.Fixed   A.4.3(13), A.4.3(14), A.4.3(15)   in Ada.Strings.Unbounded   A.4.5(43), A.4.5(44), A.4.5(45)   in Ada.Text_IO   A.10.1(5)

Count attribute   9.9(5)

cover   a type   3.4.1(9)   of a choice and an exception   11.2(6)

cover a value   3.8.1(1.a)   by a discrete_choice   3.8.1(9)   by a discrete_choice_list   3.8.1(13)

CR   in Ada.Characters.Latin_1   A.3.3(5)

create   3.1(16)   in Ada.Direct_IO   A.8.4(6)   in Ada.Sequential_IO   A.8.1(6)   in Ada.Streams.Stream_IO   A.12.1(8)   in Ada.Text_IO   A.10.1(9)

creation   of a protected object   C.3.1(10)   of a task object   D.1(17)   of an object   3.3(1)

critical section   See intertask communication   9.5(1)

CSI   in Ada.Characters.Latin_1   A.3.3(19)

Currency_Sign   in Ada.Characters.Latin_1   A.3.3(21)

current column number   A.10(9)

current index   of an open direct file   A.8(4)

current instance   of a generic unit   8.6(18)   of a type   8.6(17)

current line number   A.10(9)

current mode   of an open file   A.7(7)

current page number   A.10(9)

current size   of an external file   A.8(3)

Current_Error   in Ada.Text_IO   A.10.1(17), A.10.1(20)

Current_Handler   in Ada.Interrupts   C.3.2(6)

Current_Input   in Ada.Text_IO   A.10.1(17), A.10.1(20)

Current_Output   in Ada.Text_IO   A.10.1(17), A.10.1(20)

Current_State   in Ada.Synchronous_Task_Control   D.10(4)

Current_Task   in Ada.Task_Identification   C.7.1(3)

D

dangling references   prevention via accessibility rules   3.10.2(3)

Data_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Storage_IO   A.9(9)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Day   in Ada.Calendar   9.6(13)

Day_Duration subtype of Duration   in Ada.Calendar   9.6(11)

Day_Number subtype of Integer   in Ada.Calendar   9.6(11)

DC1   in Ada.Characters.Latin_1   A.3.3(6)

DC2   in Ada.Characters.Latin_1   A.3.3(6)

DC3   in Ada.Characters.Latin_1   A.3.3(6)

DC4   in Ada.Characters.Latin_1   A.3.3(6)

DCS   in Ada.Characters.Latin_1   A.3.3(18)

Deallocate   in System.Storage_Pools   13.11(9)

deallocation of storage   13.11.2(1)

Decimal   child of Ada   F.2(2)

decimal digit   a category of Character   A.3.2(28)

decimal fixed point type   3.5.9(1), 3.5.9(6)

Decimal_Conversions   in Interfaces.COBOL   B.4(31)

Decimal_Digit_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Decimal_Element   in Interfaces.COBOL   B.4(12)

decimal_fixed_point_definition   3.5.9(4)   used   3.5.9(2), P.1

Decimal_IO   in Ada.Text_IO   A.10.1(73)

decimal_literal   2.4.1(2)   used   2.4(2), P.1

Decimal_Output   in Ada.Text_IO.Editing   F.3.3(11)

Declaration   3.1(5), 3.1(7)

declarative region   of a construct   8.1(1)

declarative_item   3.11(3)   used   3.11(2), P.1

declarative_part   3.11(2)   used   5.6(2), 6.3(2), 7.2(2), 9.1(6), 9.5.2(5), P.1

declare   3.1(10), 3.1(16)

declared pure   10.2.1(19)

Decrement   in Interfaces.C.Pointers   B.3.2(11)

deeper   accessibility level   3.10.2(3)   statically   3.10.2(4), 3.10.2(17)

default entry queuing policy   9.5.3(17)

default treatment   C.3(5)

Default_Aft   in Ada.Text_IO   A.10.1(64), A.10.1(69), A.10.1(74)   in Ada.Text_IO.Complex_IO   G.1.3(5)

Default_Base   in Ada.Text_IO   A.10.1(53), A.10.1(58)

Default_Bit_Order   in System   13.7(15)

Default_Currency   in Ada.Text_IO.Editing   F.3.3(10)

Default_Exp   in Ada.Text_IO   A.10.1(64), A.10.1(69), A.10.1(74)   in Ada.Text_IO.Complex_IO   G.1.3(5)

default_expression   3.7(7)   used   3.7(6), 3.8(6), 6.1(15), 12.4(2), P.1

Default_Fill   in Ada.Text_IO.Editing   F.3.3(10)

Default_Fore   in Ada.Text_IO   A.10.1(64), A.10.1(69), A.10.1(74)   in Ada.Text_IO.Complex_IO   G.1.3(5)

default_name   12.6(4)   used   12.6(3), P.1

Default_Priority   in System   13.7(17)

Default_Radix_Mark   in Ada.Text_IO.Editing   F.3.3(10)

Default_Separator   in Ada.Text_IO.Editing   F.3.3(10)

Default_Setting   in Ada.Text_IO   A.10.1(80)

Default_Width   in Ada.Text_IO   A.10.1(53), A.10.1(58), A.10.1(80)

deferred constant   7.4(2)

deferred constant declaration   3.3.1(6), 7.4(2)

defining name   3.1(12)

defining_character_literal   3.5.1(4)   used   3.5.1(3), P.1

defining_designator   6.1(6)   used   6.1(4), 12.3(2), P.1

defining_identifier   3.1(4)   used   3.2.1(3), 3.2.2(2), 3.3.1(3), 3.5.1(3), 3.10.1(2), 5.5(4), 6.1(7), 7.3(2), 7.3(3), 8.5.1(2), 8.5.2(2), 9.1(2), 9.1(3), 9.1(6), 9.4(2), 9.4(3), 9.4(7), 9.5.2(2), 9.5.2(5), 9.5.2(8), 10.1.3(4), 10.1.3(5), 10.1.3(6), 11.2(4), 12.5(2), 12.7(2), P.1

defining_identifier_list   3.3.1(3)   used   3.3.1(2), 3.3.2(2), 3.7(6), 3.8(6), 6.1(15), 11.1(2), 12.4(2), P.1

defining_operator_symbol   6.1(11)   used   6.1(6), P.1

defining_program_unit_name   6.1(7)   used   6.1(4), 6.1(6), 7.1(3), 7.2(2), 8.5.3(2), 8.5.5(2), 12.3(2), P.1

Definite attribute   12.5.1(23)

definite subtype   3.3(24)

definition   3.1(8)

Degree_Sign   in Ada.Characters.Latin_1   A.3.3(22)

DEL   in Ada.Characters.Latin_1   A.3.3(14)

delay_alternative   9.7.1(6)   used   9.7.1(4), 9.7.2(2), P.1

delay_relative_statement   9.6(4)   used   9.6(2), P.1

delay_statement   9.6(2)   used   5.1(5), 9.7.1(6), 9.7.4(4), P.1

delay_until_statement   9.6(3)   used   9.6(2), P.1

Delete   in Ada.Direct_IO   A.8.4(8)   in Ada.Sequential_IO   A.8.1(8)   in Ada.Streams.Stream_IO   A.12.1(10)   in Ada.Strings.Bounded   A.4.4(64), A.4.4(65)   in Ada.Strings.Fixed   A.4.3(29), A.4.3(30)   in Ada.Strings.Unbounded   A.4.5(59), A.4.5(60)   in Ada.Text_IO   A.10.1(11)

delimiter   2.2(8)

delivery   of an interrupt   C.3(2)

delta   of a fixed point type   3.5.9(1)

Delta attribute   3.5.10(3)

delta_constraint   J.3(2)   used   3.2.2(6), P.1

Denorm attribute   A.5.3(9)

denormalized number   A.5.3(10)

denote   8.6(16)   informal definition   3.1(10)   name used as a pragma argument   8.6(32)

depend on a discriminant   for a component   3.7(21)   for a constraint or component_definition   3.7(20)

dependence   elaboration   10.2(10)   of a task on a master   9.3(1)   of a task on another task   9.3(4)   semantic   10.1.1(26)

depth   accessibility level   3.10.2(3)

dereference   4.1(8)

Dereference_Error   in Interfaces.C.Strings   B.3.1(12)

derivation class   for a type   3.4.1(2)

derived from   directly or indirectly   3.4.1(2)

derived type   3.4(1)   [partial]   3.4(26)

derived_type_definition   3.4(3)   used   3.2.1(4), P.1

descendant   10.1.1(11)   of a type   3.4.1(10)   relationship with scope   8.2(4)

designate   3.10(1)

designated profile   of an access-to-subprogram type   3.10(12)

designated subtype   of a named access type   3.10(11)   of an anonymous access type   3.10(13)

designated type   of a named access type   3.10(11)   of an anonymous access type   3.10(13)

designator   6.1(5)   used   6.3(2), P.1

destructor   See finalization   7.6(1)   See finalization   7.6.1(1)

Detach_Handler   in Ada.Interrupts   C.3.2(9)

determined class for a formal type   12.5(6)

determines   a type by a subtype_mark   3.2.2(8)

Device_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Diaeresis   in Ada.Characters.Latin_1   A.3.3(21)

digit   2.1(10)   used   2.1(3), 2.3(3), 2.4.1(3), 2.4.2(5), P.1

digits   of a decimal fixed point subtype   3.5.9(6), 3.5.10(7)

Digits attribute   3.5.10(7)

digits_constraint   3.5.9(5)   used   3.2.2(6), P.1

dimensionality   of an array   3.6(12)

direct access   A.8(3)

direct file   A.8(1)

Direct_IO   child of Ada   A.8.4(2)

direct_name   4.1(3)   used   3.8.1(2), 4.1(2), 5.1(9), 9.5.2(3), 13.1(5), J.7(1), P.1

Direction   in Ada.Strings   A.4.1(6)

directly specified   of an aspect of representation of an entity   13.1(10)

directly visible   8.3(2), 8.3(22)   within a pragma in a context_clause   10.1.6(3)   within a pragma that appears at the place of a compilation unit   10.1.6(5)   within a use_clause in a context_clause   10.1.6(3)   within a with_clause   10.1.6(2)   within the parent_unit_name of a library unit   10.1.6(2)   within the parent_unit_name of a subunit   10.1.6(4)

Discard_Names pragma   C.5(3), L(9)

discontiguous representation   [partial]   13.1(9.a), 13.5.2(5), 13.7.1(12), 13.9(9), 13.9(17), 13.11(17), 13.11(18.d)

discrete array type   4.5.2(1)

discrete type   3.2(5), 3.5(1)

discrete_choice   3.8.1(5)   used   3.8.1(4), P.1

discrete_choice_list   3.8.1(4)   used   3.8.1(3), 4.3.3(6), 5.4(3), P.1

Discrete_Random   child of Ada.Numerics   A.5.2(17)

discrete_range   3.6.1(3)   used   3.6.1(2), 3.8.1(5), 4.1.2(2), 4.3.3(48), P.1

discrete_subtype_definition   3.6(6)   used   3.6(5), 5.5(4), 9.5.2(2), 9.5.2(8), P.1

discriminant   3.2(7), 3.7(1)   of a variant_part   3.8.1(6)   use in a record definition   3.8(12)

discriminant_association   3.7.1(3)   used   3.7.1(2), P.1

Discriminant_Check   11.5(15)   [partial]   4.1.3(15), 4.3(7), 4.3.2(8), 4.6(43), 4.6(45), 4.6(51), 4.6(52), 4.7(4), 4.8(10)

discriminant_constraint   3.7.1(2)   used   3.2.2(7), P.1

discriminant_part   3.7(3)   used   3.10.1(2), 7.3(2), 7.3(3), 12.5(2), P.1

discriminant_specification   3.7(6)   used   3.7(5), P.1

discriminants   known   3.7(27)   unknown   3.7(2.a)   unknown   3.7(27)

discriminated type   3.7(9)

dispatching   3.9(4)

dispatching call   on a dispatching operation   3.9.2(1)

dispatching operation   3.9.2(1), 3.9.2(2)   [partial]   3.9(1)

dispatching point   D.2.1(4)   [partial]   D.2.1(8), D.2.2(13)

dispatching policy for tasks   9(11.a)   [partial]   D.2.1(5)

dispatching, task   D.2.1(4)

Display_Format   in Interfaces.COBOL   B.4(22)

displayed magnitude (of a decimal value)   F.3.2(14)

disruption of an assignment   9.8(21), 13.9.1(5)   [partial]   11.6(6)

distinct access paths   6.2(12)

distributed program   E(3)

distributed system   E(2)

distributed systems   C(1)

divide   2.1(15)   in Ada.Decimal   F.2(6)

divide operator   4.5(1), 4.5.5(1)

Division_Check   11.5(16)   [partial]   3.5.4(21), 4.5.5(22), A.5.3(48), G.1.1(40), K(203)

Division_Sign   in Ada.Characters.Latin_1   A.3.3(26)

DLE   in Ada.Characters.Latin_1   A.3.3(6)

Do_APC   in System.RPC   E.5(10)

Do_RPC   in System.RPC   E.5(9)

documentation (required of an implementation)   1.1.1(75), M(3)

documentation requirements   1.1.1(51)

Dollar_Sign   in Ada.Characters.Latin_1   A.3.3(8)

dope   13.5.1(15.d)

dot   2.1(15)

dot selection   See selected_component   4.1.3(1)

double   in Interfaces.C   B.3(16)

Double_Precision   in Interfaces.Fortran   B.5(6)

downward closure   3.10.2(37)

drift rate   D.8(41)

Duration   in Standard   A.1(43)

dynamic binding   See dispatching operation   3.9(1)

dynamic semantics   1.1.1(47)

Dynamic_Priorities   child of Ada   D.5(3)

dynamically determined tag   3.9.2(1)

dynamically enclosing   of one execution by another   11.4(2)

dynamically tagged   3.9.2(5)

E

e   in Ada.Numerics   A.5(3)

edited output   F.3(1)

Editing   child of Ada.Text_IO   F.3.3(3)   child of Ada.Wide_Text_IO   F.3.4(1)

effect   external   1.1.1(65)

efficiency   11.5(32), 11.6(1)

elaborable   3.1(15.a)

Elaborate pragma   10.2.1(22), L(10)

Elaborate_All pragma   10.2.1(23), L(11)

Elaborate_Body pragma   10.2.1(24), L(12)

elaborated   3.11(8)

elaboration   3.1(13), 3.1(13.a)   abstract_subprogram_declaration   6.1(31)   access_definition   3.10(18)   access_type_definition   3.10(17)   array_type_definition   3.6(21)   choice_parameter_specification   11.4(7)   component_declaration   3.8(17)   component_definition   3.6(22), 3.8(18)   component_list   3.8(17)   declaration named by a pragma Import   B.1(38)   declarative_part   3.11(7)   deferred constant declaration   7.4(10)   delta_constraint   J.3(11)   derived_type_definition   3.4(28)   digits_constraint   3.5.9(19)   discrete_subtype_definition   3.6(22)   discriminant_constraint   3.7.1(12)   entry_declaration   9.5.2(22)   enumeration_type_definition   3.5.1(10)   exception_declaration   11.1(5)   fixed_point_definition   3.5.9(17)   floating_point_definition   3.5.7(13)   full type definition   3.2.1(11)   full_type_declaration   3.2.1(11)   generic body   12.2(2)   generic_declaration   12.1(10)   generic_instantiation   12.3(20)   incomplete_type_declaration   3.10.1(12)   index_constraint   3.6.1(8)   integer_type_definition   3.5.4(19)   loop_parameter_specification   5.5(10)   nongeneric package_body   7.2(6)   nongeneric subprogram_body   6.3(6)   number_declaration   3.3.2(7)   object_declaration   3.3.1(15), 7.6(11)   package_body of Standard   A.1(50)   package_declaration   7.1(8)   partition   E.1(6)   partition   E.5(21)   pragma   2.8(12)   private_extension_declaration   7.3(17)   private_type_declaration   7.3(17)   protected declaration   9.4(12)   protected_body   9.4(15)   protected_definition   9.4(13)   range_constraint   3.5(9)   real_type_definition   3.5.6(5)   record_definition   3.8(16)   record_extension_part   3.9.1(5)   record_type_definition   3.8(16)   renaming_declaration   8.5(4)   representation_clause   13.1(21)   single_protected_declaration   9.4(12)   single_task_declaration   9.1(10)   Storage_Size pragma   13.3(68)   subprogram_declaration   6.1(31)   subtype_declaration   3.2.2(9)   subtype_indication   3.2.2(9)   task declaration   9.1(10)   task_body   9.1(13)   task_definition   9.1(11)   use_clause   8.4(12)   variant_part   3.8.1(22)

elaboration control   10.2.1(1)

elaboration dependence   library_item on another   10.2(10)

Elaboration_Check   11.5(23)   [partial]   3.11(9)

element   of a storage pool   13.11(12)   in Ada.Strings.Bounded   A.4.4(26)   in Ada.Strings.Unbounded   A.4.5(20)

elementary type   3.2(3)

Elementary_Functions   child of Ada.Numerics   A.5.1(9)

eligible   a type, for a convention   B.1(14)

else part   of a selective_accept   9.7.1(11)

EM   in Ada.Characters.Latin_1   A.3.3(6)

embedded systems   C(1), D(1)

encapsulation   See package   7(1)

enclosing   immediately   8.1(13)

end of a line   2.2(2)

End_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

End_Of_File   in Ada.Direct_IO   A.8.4(16)   in Ada.Sequential_IO   A.8.1(13)   in Ada.Streams.Stream_IO   A.12.1(12)   in Ada.Text_IO   A.10.1(34)

End_Of_Line   in Ada.Text_IO   A.10.1(30)

End_Of_Page   in Ada.Text_IO   A.10.1(33)

endian   big   13.5.3(2)   little   13.5.3(2)

ENQ   in Ada.Characters.Latin_1   A.3.3(5)

entity   3.1(16.b)   [partial]   3.1(1)

entry   closed   9.5.3(5)   open   9.5.3(5)   single   9.5.2(20)

entry call   9.5.3(1)   simple   9.5.3(1)

entry calling convention   6.3.1(13)

entry family   9.5.2(20)

entry index subtype   3.8(18), 9.5.2(20)

entry queue   9.5.3(12)

entry queuing policy   9.5.3(17)   default policy   9.5.3(17)

entry_barrier   9.5.2(7)   used   9.5.2(5), P.1

entry_body   9.5.2(5)   used   9.4(8), P.1

entry_body_formal_part   9.5.2(6)   used   9.5.2(5), P.1

entry_call_alternative   9.7.2(3)   used   9.7.2(2), 9.7.3(2), P.1

entry_call_statement   9.5.3(2)   used   5.1(5), 9.7.2(3), 9.7.4(4), P.1

entry_declaration   9.5.2(2)   used   9.1(5), 9.4(5), P.1

entry_index   9.5.2(4)   used   9.5.2(3), P.1

entry_index_specification   9.5.2(8)   used   9.5.2(6), P.1

enumeration literal   3.5.1(6)

enumeration type   3.2(5), 3.5.1(1)

enumeration_aggregate   13.4(3)   used   13.4(2), P.1

Enumeration_IO   in Ada.Text_IO   A.10.1(79)

enumeration_literal_specification   3.5.1(3)   used   3.5.1(2), P.1

enumeration_representation_clause   13.4(2)   used   13.1(4), P.1

enumeration_type_definition   3.5.1(2)   used   3.2.1(4), P.1

environment   10.1.4(1)

environment declarative_part   10.1.4(1)   for the environment task of a partition   10.2(14)

environment task   10.2(9)

EOT   in Ada.Characters.Latin_1   A.3.3(5)

EPA   in Ada.Characters.Latin_1   A.3.3(18)

epoch   D.8(19)

equal operator   4.5(1), 4.5.2(1)

equality operator   4.5.2(1)   special inheritance rule for tagged types   3.4(19), 4.5.2(14)

equals sign   2.1(15)

Equals_Sign   in Ada.Characters.Latin_1   A.3.3(10)

equivalence of use_clauses and selected_components   8.4(1.a)

erroneous execution   1.1.1(112), 1.1.1(49)   cause   3.7.2(4), 9.8(21), 9.10(11), 11.5(29), 13.3(13), 13.3(28), 13.3(29), 13.9.1(8), 13.9.1(12), 13.9.1(13), 13.11(22), 13.11.2(16), 13.11.3(9), A.10.3(22), A.13(17), B.3.1(51), B.3.1(55), B.3.1(56), B.3.1(57), B.3.2(35), B.3.2(36), B.3.2(37), B.3.2(38), B.3.2(39), B.3.2(42), C.3.1(14), C.7.1(18), C.7.2(14), C.7.2(15), D.5(12), D.11(9), H.4(26), H.4(27)

error   compile-time   1.1.1(106), 1.1.1(44)   link-time   1.1.1(106), 1.1.1(46)   run-time   1.1.1(108), 1.1.1(47), 11.5(3), 11.6(1)   See also bounded error, erroneous execution

ESA   in Ada.Characters.Latin_1   A.3.3(17)

ESC   in Ada.Characters.Latin_1   A.3.3(6)

Establish_RPC_Receiver   in System.RPC   E.5(12)

ETB   in Ada.Characters.Latin_1   A.3.3(6)

ETX   in Ada.Characters.Latin_1   A.3.3(5)

evaluable   3.1(15.a)

evaluation   3.1(13), 3.1(13.a)   aggregate   4.3(6)   allocator   4.8(7)   array_aggregate   4.3.3(22)   attribute_reference   4.1.4(11)   concatenation   4.5.3(5)   dereference   4.1(13)   discrete_range   3.6.1(8)   extension_aggregate   4.3.2(7)   generic_association   12.3(21)   generic_association for a formal object of mode in   12.4(11)   indexed_component   4.1.1(7)   initialized allocator   4.8(7)   membership test   4.5.2(27)   name   4.1(11)   name that has a prefix   4.1(12)   null literal   4.2(9)   numeric literal   4.2(9)   parameter_association   6.4.1(7)   prefix   4.1(12)   primary that is a name   4.4(10)   qualified_expression   4.7(4)   range   3.5(9)   range_attribute_reference   4.1.4(11)   record_aggregate   4.3.1(19)   record_component_association_list   4.3.1(20)   selected_component   4.1.3(14)   short-circuit control form   4.5.1(7)   slice   4.1.2(7)   string_literal   4.2(10)   uninitialized allocator   4.8(8)   Val   3.5.5(7), K(262)   Value   3.5(58)   value conversion   4.6(28)   view conversion   4.6(52)   Wide_Value   3.5(46)

Exception   11(1), 11.1(1)

exception occurrence   11(1)

exception_choice   11.2(5)   used   11.2(3), P.1

exception_declaration   11.1(2)   used   3.1(3), P.1

exception_handler   11.2(3)   used   11.2(2), P.1

Exception_Id   in Ada.Exceptions   11.4.1(2)

Exception_Identity   in Ada.Exceptions   11.4.1(5)

Exception_Information   in Ada.Exceptions   11.4.1(5)

Exception_Message   in Ada.Exceptions   11.4.1(4)

Exception_Name   in Ada.Exceptions   11.4.1(2), 11.4.1(5)

Exception_Occurrence   in Ada.Exceptions   11.4.1(3)

Exception_Occurrence_Access   in Ada.Exceptions   11.4.1(3)

exception_renaming_declaration   8.5.2(2)   used   8.5(3), P.1

Exceptions   child of Ada   11.4.1(2)

Exchange_Handler   in Ada.Interrupts   C.3.2(8)

Exclamation   in Ada.Characters.Latin_1   A.3.3(8)

executable   3.1(15.a)

execution   3.1(13)   abort_statement   9.8(4)   aborting the execution of a construct   9.8(5)   accept_statement   9.5.2(24)   Ada program   9(1)   assignment_statement   5.2(7), 7.6(18), 7.6.1(12)   asynchronous_select with a delay_statement trigger   9.7.4(7)   asynchronous_select with an entry call trigger   9.7.4(6)   block_statement   5.6(5)   call on a dispatching operation   3.9.2(14)   call on an inherited subprogram   3.4(29)   case_statement   5.4(11)   conditional_entry_call   9.7.3(3)   delay_statement   9.6(20)   dynamically enclosing   11.4(2)   entry_body   9.5.2(26)   entry_call_statement   9.5.3(8)   exit_statement   5.7(5)   goto_statement   5.8(5)   handled_sequence_of_statements   11.2(10)   handler   11.4(7)   if_statement   5.3(5)   included by another execution   11.4(2.a)   instance of Unchecked_Deallocation   7.6.1(10)   loop_statement   5.5(8)   loop_statement with a for iteration_scheme   5.5(10)   loop_statement with a while iteration_scheme   5.5(9)   null_statement   5.1(14)   partition   10.2(26)   pragma   2.8(12)   program   10.2(26)   protected subprogram call   9.5.1(3)   raise_statement with an exception_name   11.3(4)   re-raise statement   11.3(4)   remote subprogram call   E.4(9)   requeue protected entry   9.5.4(9)   requeue task entry   9.5.4(8)   requeue_statement   9.5.4(7)   return_statement   6.5(6)   selective_accept   9.7.1(15)   sequence_of_statements   5.1(16)   subprogram call   6.4(10)   subprogram_body   6.3(7)   task   9.2(1)   task_body   9.2(1)   timed_entry_call   9.7.2(4)

execution resource   associated with a protected object   9.4(18)   required for a task to run   9(11)

exist   cease to   13.11.2(10)

exit_statement   5.7(2)   used   5.1(5), P.1

Exit_Status   in Ada.Command_Line   A.15(7)

Exp   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)

expanded name   4.1.3(4)

Expanded_Name   in Ada.Tags   3.9(8)

expected profile   8.6(26)   accept_statement entry_direct_name   9.5.2(11)   Access attribute_reference prefix   3.10.2(2)   attribute_definition_clause name   13.3(4)   character_literal   4.2(3)   formal subprogram actual   12.6(6)   formal subprogram default_name   12.6(5)   subprogram_renaming_declaration   8.5.4(3)

expected type   8.6(20)   abort_statement task_name   9.8(3)   access attribute_reference   3.10.2(2)   actual parameter   6.4.1(3)   aggregate   4.3(4)   allocator   4.8(3)   array_aggregate   4.3.3(8)   array_aggregate component expression   4.3.3(8)   array_aggregate discrete_choice   4.3.3(9)   assignment_statement expression   5.2(4)   assignment_statement variable_name   5.2(4)   attribute_definition_clause expression or name   13.3(4)   attribute_designator expression   4.1.4(7)   case expression   5.4(4)   case_statement_alternative discrete_choice   5.4(4)   character_literal   4.2(3)   code_statement   13.8(4)   component_clause expressions   13.5.1(7)   component_declaration default_expression   3.8(7)   condition   5.3(4)   decimal fixed point type digits   3.5.9(6)   delay_relative_statement expression   9.6(5)   delay_until_statement expression   9.6(5)   delta_constraint expression   J.3(3)   dereference name   4.1(8)   discrete_subtype_definition range   3.6(8)   discriminant default_expression   3.7(8)   discriminant_association expression   3.7.1(6)   entry_index   9.5.2(11)   enumeration_representation_clause expressions   13.4(4)   extension_aggregate   4.3.2(4)   extension_aggregate ancestor expression   4.3.2(4)   first_bit   13.5.1(7)   fixed point type delta   3.5.9(6)   generic formal in object actual   12.4(4)   generic formal object default_expression   12.4(3)   index_constraint discrete_range   3.6.1(4)   indexed_component expression   4.1.1(4)   Interrupt_Priority pragma argument   D.1(6)   last_bit   13.5.1(7)   link name   B.1(10)   membership test simple_expression   4.5.2(3)   modular_type_definition expression   3.5.4(5)   null literal   4.2(2)   number_declaration expression   3.3.2(3)   object_declaration initialization expression   3.3.1(4)   parameter default_expression   6.1(17)   position   13.5.1(7)   Priority pragma argument   D.1(6)   range simple_expressions   3.5(5)   range_attribute_designator expression   4.1.4(7)   range_constraint range   3.5(5)   real_range_specification bounds   3.5.7(5)   record_aggregate   4.3.1(8)   record_component_association expression   4.3.1(11)   requested decimal precision   3.5.7(4)   restriction parameter expression   13.12(5)   return expression   6.5(3)   short-circuit control form relation   4.5.1(1)   signed_integer_type_definition simple_expression   3.5.4(5)   slice discrete_range   4.1.2(4)   Storage_Size pragma argument   13.3(67)   string_literal   4.2(4)   type_conversion operand   4.6(6)   Unchecked_Access attribute   13.10(4.a)   variant_part discrete_choice   3.8.1(6)

expiration time   [partial]   9.6(1)   for a delay_relative_statement   9.6(20)   for a delay_until_statement   9.6(20)

explicit declaration   3.1(5)

explicit initial value   3.3.1(1)

explicit_actual_parameter   6.4(6)   used   6.4(5), P.1

explicit_dereference   4.1(5)   used   4.1(2), P.1

explicit_generic_actual_parameter   12.3(5)   used   12.3(4), P.1

explicitly assign   10.2(2)

exponent   2.4.1(4), 4.5.6(11)   used   2.4.1(2), 2.4.2(2), P.1

Exponent attribute   A.5.3(18)

exponentiation operator   4.5(1), 4.5.6(7)

Export pragma   B.1(6), L(13)

exported   aspect of representation   B.1(28)

exported entity   B.1(23)

expression   4.4(1), 4.4(2)   used   2.8(3), 3.3.1(2), 3.3.2(2), 3.5.4(4), 3.5.7(2), 3.5.9(3), 3.5.9(4), 3.5.9(5), 3.7(7), 3.7.1(3), 3.8.1(5), 4.1.1(2), 4.1.4(3), 4.1.4(5), 4.3.1(4), 4.3.2(3), 4.3.3(3), 4.3.3(6), 4.3.3(46), 4.3.3(48), 4.3.3(49), 4.4(7), 4.6(2), 4.7(2), 5.2(2), 5.3(3), 5.4(2), 6.4(6), 6.5(2), 9.5.2(4), 9.6(3), 9.6(4), 12.3(5), 13.3(2), 13.3(65), 13.5.1(4), 13.12(4), B.1(5), B.1(6), B.1(8), B.1(10), C.3.1(4), D.1(3), D.1(5), J.3(2), J.7(1), J.8(1), L(6), L(13), L(14), L(18), L(19), L(29), L(39), P.1

extended_digit   2.4.2(5)   used   2.4.2(4), P.1

extension   of a private type   3.9(2), 3.9.1(1)   of a record type   3.9(2), 3.9.1(1)   of a type   3.9(2), 3.9.1(1)

extension_aggregate   4.3.2(2)   used   4.3(3), P.1

extensions to Ada 83   1.1.1(56.g), 2.1(18.b), 2.8(19.d), 2.8(29.a), 3.2.3(9.b), 3.3(28.a), 3.3.1(33.a), 3.3.2(10.a), 3.4(40.d), 3.5(69.b), 3.5.2(9.f), 3.5.4(37.a), 3.5.5(19.a), 3.5.9(28.b), 3.6(30.a), 3.6.1(18.a), 3.6.3(8.e), 3.7(38.a), 3.7.2(4.b), 3.8(31.a), 3.8.1(29.a), 3.9(35.a), 3.9.1(17.a), 3.9.2(24.a), 3.10(27.a), 3.10.1(23.a), 3.10.2(41.a), 3.11(14.a), 4.1(17.a), 4.1.3(19.a), 4.1.4(16.a), 4.2(14.b), 4.3(7.b), 4.3.1(32.a), 4.3.2(13.a), 4.3.3(44.a), 4.4(15.a), 4.5.2(39.a), 4.5.3(14.d), 4.5.5(35.a), 4.6(71.d), 4.8(20.b), 4.9(44.a), 5.1(20.a), 5.2(28.a), 5.4(18.a), 6.1(42.a), 6.2(13.a), 6.3(11.a), 6.3.1(25.a), 6.3.2(7.a), 6.4.1(17.e), 6.6(9.a), 7.3(24.a), 7.4(14.a), 7.5(24.a), 7.6(22.c), 8.2(12.b), 8.3(30.p), 8.4(16.e), 8.5.5(7.a), 8.6(34.b), 9.1(32.a), 9.4(35.a), 9.5.2(37.a), 9.5.4(20.a), 9.6(40.b), 9.7(4.a), 9.7.4(13.a), 10.1.1(35.n), 10.1.2(9.c), 10.1.3(25.a), 10.2(35.d), 10.2.1(30.c), 11.2(12.a), 11.4.1(19.x), 11.5(34.a), 12.1(24.a), 12.3(29.c), 12.4(12.b), 12.5.4(13.b), 12.7(10.d), 13.1(26.e), 13.3(89.a), 13.4(16.a), 13.5.3(8.b), 13.8(14.a), 13.9.2(12.d), 13.11(44.a), 13.12(12.b), 13.13(2.a), 13.14(19.q), A.1(56.d), A.2(4.e), A.3(1.a), A.4(1.a), A.5(5.b), A.5.3(73.g), A.5.4(4.c), A.6(1.b), A.10(11.a), A.10.1(85.b), A.11(3.a), A.15(22.b), B(1.c), B.1(51.a), C(1.a), D(6.a), D.1(29.b), E(1.a), F(7.a), G(7.a), G.2(3.a), G.2.1(16.g), H(6.b), J.7(2.c)

external call   9.5(4)

external effect   of the execution of an Ada program   1.1.1(65)   volatile/atomic objects   C.6(20)

external file   A.7(1)

external interaction   1.1.1(65)

external name   B.1(34)

external requeue   9.5(7)

External_Tag   in Ada.Tags   3.9(8)

External_Tag attribute   13.3(79)

External_Tag clause   13.3(7), 13.3(79), K(68)

extra permission to avoid raising exceptions   11.6(5)

extra permission to reorder actions   11.6(6)

F

factor   4.4(6)   used   4.4(5), P.1

failure   of a language-defined check   11.5(3)   in Ada.Command_Line   A.15(8)

False   3.5.3(1)

family   entry   9.5.2(20)

Feminine_Ordinal_Indicator   in Ada.Characters.Latin_1   A.3.3(21)

FF   in Ada.Characters.Latin_1   A.3.3(5)

Field subtype of Integer   in Ada.Text_IO   A.10.1(6)

file   as file object   A.7(2)

file terminator   A.10(7)

File_Access   in Ada.Text_IO   A.10.1(18)

File_Mode   in Ada.Direct_IO   A.8.4(4)   in Ada.Sequential_IO   A.8.1(4)   in Ada.Streams.Stream_IO   A.12.1(6)   in Ada.Text_IO   A.10.1(4)

File_Type   in Ada.Direct_IO   A.8.4(3)   in Ada.Sequential_IO   A.8.1(3)   in Ada.Streams.Stream_IO   A.12.1(5)   in Ada.Text_IO   A.10.1(3)

finalization   of a master   7.6.1(4)   of a protected object   9.4(20)   of a protected object   C.3.1(12)   of a task object   J.7.1(8)   of an object   7.6.1(5)   child of Ada   7.6(5)

Finalize   7.6(2)   in Ada.Finalization   7.6(7), 7.6(9)

Find_Token   in Ada.Strings.Bounded   A.4.4(51)   in Ada.Strings.Fixed   A.4.3(16)   in Ada.Strings.Unbounded   A.4.5(46)

Fine_Delta   in System   13.7(9)

First attribute   3.5(12), 3.6.2(3)

first subtype   3.2.1(6), 3.4.1(5)

First(N) attribute   3.6.2(4)

first_bit   13.5.1(5)   used   13.5.1(3), P.1

First_Bit attribute   13.5.2(3)

Fixed   child of Ada.Strings   A.4.3(5)

fixed point type   3.5.9(1)

Fixed_IO   in Ada.Text_IO   A.10.1(68)

fixed_point_definition   3.5.9(2)   used   3.5.6(2), P.1

Float   3.5.7(12), 3.5.7(14)   in Standard   A.1(21)

Float_IO   in Ada.Text_IO   A.10.1(63)

Float_Random   child of Ada.Numerics   A.5.2(5)

Float_Text_IO   child of Ada   A.10.9(33)

Float_Wide_Text_IO   child of Ada   A.11(3)

Floating   in Interfaces.COBOL   B.4(9)

floating point type   3.5.7(1)

floating_point_definition   3.5.7(2)   used   3.5.6(2), P.1

Floor attribute   A.5.3(30)

Flush   in Ada.Streams.Stream_IO   A.12.1(25)   in Ada.Text_IO   A.10.1(21)

Fore attribute   3.5.10(4)

form   of an external file   A.7(1)   in Ada.Direct_IO   A.8.4(9)   in Ada.Sequential_IO   A.8.1(9)   in Ada.Streams.Stream_IO   A.12.1(11)   in Ada.Text_IO   A.10.1(12)

formal object, generic   12.4(1)

formal package, generic   12.7(1)

formal parameter   of a subprogram   6.1(17)

formal subprogram, generic   12.6(1)

formal subtype   12.5(5)

formal type   12.5(5)

formal_access_type_definition   12.5.4(2)   used   12.5(3), P.1

formal_array_type_definition   12.5.3(2)   used   12.5(3), P.1

formal_decimal_fixed_point_definition   12.5.2(7)   used   12.5(3), P.1

formal_derived_type_definition   12.5.1(3)   used   12.5(3), P.1

formal_discrete_type_definition   12.5.2(2)   used   12.5(3), P.1

formal_floating_point_definition   12.5.2(5)   used   12.5(3), P.1

formal_modular_type_definition   12.5.2(4)   used   12.5(3), P.1

formal_object_declaration   12.4(2)   used   12.1(6), P.1

formal_ordinary_fixed_point_definition   12.5.2(6)   used   12.5(3), P.1

formal_package_actual_part   12.7(3)   used   12.7(2), P.1

formal_package_declaration   12.7(2)   used   12.1(6), P.1

formal_part   6.1(14)   used   6.1(12), 6.1(13), P.1

formal_private_type_definition   12.5.1(2)   used   12.5(3), P.1

formal_signed_integer_type_definition   12.5.2(3)   used   12.5(3), P.1

formal_subprogram_declaration   12.6(2)   used   12.1(6), P.1

formal_type_declaration   12.5(2)   used   12.1(6), P.1

formal_type_definition   12.5(3)   used   12.5(2), P.1

format_effector   2.1(13)   used   2.1(2), P.1

Fortran   child of Interfaces   B.5(4)

Fortran interface   B.5(1)

Fortran_Character   in Interfaces.Fortran   B.5(12)

Fortran_Integer   in Interfaces.Fortran   B.5(5)

Fraction attribute   A.5.3(21)

Fraction_One_Half   in Ada.Characters.Latin_1   A.3.3(22)

Fraction_One_Quarter   in Ada.Characters.Latin_1   A.3.3(22)

Fraction_Three_Quarters   in Ada.Characters.Latin_1   A.3.3(22)

Free   in Ada.Strings.Unbounded   A.4.5(7)   in Interfaces.C.Strings   B.3.1(11)

freed   See nonexistent   13.11.2(10)

freeing storage   13.11.2(1)

freezing   by a constituent of a construct   13.14(4)   by an expression   13.14(8)   class-wide type caused by the freezing of the specific type   13.14(15)   constituents of a full type definition   13.14(15)   designated subtype caused by an allocator   13.14(13)   entity   13.14(2)   entity caused by a body   13.14(3)   entity caused by a construct   13.14(4)   entity caused by a name   13.14(11)   entity caused by the end of an enclosing construct   13.14(3)   first subtype caused by the freezing of the type   13.14(15)   function call   13.14(14)   generic_instantiation   13.14(5)   nominal subtype caused by a name   13.14(11)   object_declaration   13.14(6)   specific type caused by the freezing of the class-wide type   13.14(15)   subtype caused by a record extension   13.14(7)   subtypes of the profile of a callable entity   13.14(14)   type caused by a range   13.14(12)   type caused by an expression   13.14(10)   type caused by the freezing of a subtype   13.14(15)

freezing points   entity   13.14(2)

FS   in Ada.Characters.Latin_1   A.3.3(6)

full conformance   for discrete_subtype_definitions   6.3.1(24)   for expressions   6.3.1(19)   for known_discriminant_parts   6.3.1(23)   for profiles   6.3.1(18)   required   3.10.1(4), 6.3(4), 7.3(9), 8.5.4(5), 9.5.2(14), 9.5.2(16), 9.5.2(17), 10.1.3(12), 10.1.3(13)

full constant declaration   3.3.1(6)

full declaration   7.4(2)

full stop   2.1(15)

full type   3.2.1(8)

full type definition   3.2.1(8)

full view   of a type   7.3(4)

Full_Stop   in Ada.Characters.Latin_1   A.3.3(8)

full_type_declaration   3.2.1(3)   used   3.2.1(2), P.1

function   6(1)

function instance   12.3(13)

function_call   6.4(3)   used   4.1(2), P.1

G

gaps   13.1(9), 13.3(53.d)

garbage collection   13.11.3(6)

general access type   3.10(7), 3.10(8)

general_access_modifier   3.10(4)   used   3.10(3), P.1

generation   of an interrupt   C.3(2)

Generator   in Ada.Numerics.Discrete_Random   A.5.2(19)   in Ada.Numerics.Float_Random   A.5.2(7)

generic actual   12.3(7)

generic actual parameter   12.3(7)

generic actual subtype   12.5(4)

generic actual type   12.5(4)

generic body   12.2(1)

generic contract issue   10.2.1(11), 12.3(11.y)   [partial]   3.7(11), 3.9.1(3), 3.10.2(28), 3.10.2(32), 4.6(17), 4.6(20), 6.3.1(17.a), 6.5(20.c), 7.3(8), 8.3(27), 10.2.1(12)

generic contract model   12.3(1.a)

generic contract/private type contract analogy   7.3(19.a)

generic formal   12.1(9)

generic formal object   12.4(1)

generic formal package   12.7(1)

generic formal subprogram   12.6(1)

generic formal subtype   12.5(5)

generic formal type   12.5(5)

generic function   12.1(8)

generic package   12.1(8)

generic procedure   12.1(8)

generic subprogram   12.1(8)

generic unit   12(1)   See also dispatching operation   3.9(1)

generic_actual_part   12.3(3)   used   12.3(2), 12.7(3), P.1

generic_association   12.3(4)   used   12.3(3), P.1

Generic_Bounded_Length   in Ada.Strings.Bounded   A.4.4(4)

Generic_Complex_Elementary_Functions   child of Ada.Numerics   G.1.2(2)

Generic_Complex_Types   child of Ada.Numerics   G.1.1(2)

generic_declaration   12.1(2)   used   3.1(3), 10.1.1(5), P.1

Generic_Elementary_Functions   child of Ada.Numerics   A.5.1(3)

generic_formal_parameter_declaration   12.1(6)   used   12.1(5), P.1

generic_formal_part   12.1(5)   used   12.1(3), 12.1(4), P.1

generic_instantiation   12.3(2)   used   3.1(3), 10.1.1(5), P.1

generic_package_declaration   12.1(4)   used   12.1(2), P.1

generic_renaming_declaration   8.5.5(2)   used   8.5(3), 10.1.1(6), P.1

generic_subprogram_declaration   12.1(3)   used   12.1(2), P.1

Get   in Ada.Text_IO   A.10.1(41), A.10.1(47), A.10.1(54), A.10.1(55), A.10.1(59), A.10.1(60), A.10.1(65), A.10.1(67), A.10.1(70), A.10.1(72), A.10.1(75), A.10.1(77), A.10.1(81), A.10.1(83)   in Ada.Text_IO.Complex_IO   G.1.3(6), G.1.3(8)

Get_Immediate   in Ada.Text_IO   A.10.1(44), A.10.1(45)

Get_Line   in Ada.Text_IO   A.10.1(49)

Get_Priority   in Ada.Dynamic_Priorities   D.5(5)

global to   8.1(15)

Glossary   N(1)

goto_statement   5.8(2)   used   5.1(5), P.1

govern a variant   3.8.1(20)

govern a variant_part   3.8.1(20)

grammar   ambiguous   1.1.1(94.a)   complete listing   P.1(1)   cross reference   P.2(1)   notation   1.1.1(83)   resolution of ambiguity   1.1.1(94.a), 8.6(3)   under Syntax heading   1.1.1(42)

graphic character   a category of Character   A.3.2(23)

graphic_character   2.1(3)   used   2.1(2), 2.5(2), 2.6(3), P.1

Graphic_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Grave   in Ada.Characters.Latin_1   A.3.3(13)

greater than operator   4.5(1), 4.5.2(1)

greater than or equal operator   4.5(1), 4.5.2(1)

greater-than sign   2.1(15)

Greater_Than_Sign   in Ada.Characters.Latin_1   A.3.3(10)

GS   in Ada.Characters.Latin_1   A.3.3(6)

guard   9.7.1(3)   used   9.7.1(2), P.1

H

handle   an exception   11(1)   an exception occurrence   11(1.a)   an exception occurrence   11.4(1), 11.4(7)

handled_sequence_of_statements   11.2(2)   used   5.6(2), 6.3(2), 7.2(2), 9.1(6), 9.5.2(3), 9.5.2(5), P.1

handler   11.2(5.a)   interrupt   C.3(2)

Handling   child of Ada.Characters   A.3.2(2)

Head   in Ada.Strings.Bounded   A.4.4(70), A.4.4(71)   in Ada.Strings.Fixed   A.4.3(35), A.4.3(36)   in Ada.Strings.Unbounded   A.4.5(65), A.4.5(66)

head (of a queue)   D.2.1(5)

heap management   user-defined   13.11(1)   See also allocator   4.8(1)

held priority   D.11(4)

heterogeneous input-output   A.12.1(1)

hexadecimal   literal   2.4.2(1)

hexadecimal digit   a category of Character   A.3.2(30)

hexadecimal literal   2.4.2(1)

Hexadecimal_Digit_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

hidden from all visibility   8.3(5), 8.3(15)   by lack of a with_clause   8.3(21)   for a declaration completed by a subsequent declaration   8.3(20)   for overridden declaration   8.3(16)   within the declaration itself   8.3(17)

hidden from direct visibility   8.3(5), 8.3(22)   by an inner homograph   8.3(23)   where hidden from all visibility   8.3(24)

hiding   8.3(5)

High_Order_First   13.5.3(2)   in Interfaces.COBOL   B.4(25)   in System   13.7(15)

highest precedence operator   4.5.6(1)

highest_precedence_operator   4.5(7)

Hold   in Ada.Asynchronous_Task_Control   D.11(3)

homograph   8.3(8)

HT   in Ada.Characters.Latin_1   A.3.3(5)

HTJ   in Ada.Characters.Latin_1   A.3.3(17)

HTS   in Ada.Characters.Latin_1   A.3.3(17)

Hyphen   in Ada.Characters.Latin_1   A.3.3(8)

hyphen-minus   2.1(15)

I

i   in Ada.Numerics.Generic_Complex_Types   G.1.1(5)   in Interfaces.Fortran   B.5(10)

identifier   2.3(2)   used   2.8(2), 2.8(3), 2.8(21), 2.8(23), 3.1(4), 4.1(3), 4.1.3(3), 4.1.4(3), 5.5(2), 5.6(2), 6.1(5), 7.1(3), 7.2(2), 9.1(4), 9.1(6), 9.4(4), 9.4(7), 9.5.2(3), 9.5.2(5), 11.5(6), 13.12(4), B.1(5), B.1(6), B.1(7), D.2.2(2), D.2.2(4), D.2.2(4.a/0), D.3(3), D.3(4), D.3(4.a), D.4(3), D.4(4), L(8), L(13), L(14), L(20), L(21), L(24), L(32), L(40), L(41), M(101), M(104), P.1

identifier specific to a pragma   2.8(10)

identifier_letter   2.1(7)   used   2.1(3), 2.3(2), 2.3(3), P.1

Identity   in Ada.Strings.Maps   A.4.2(22)   in Ada.Strings.Wide_Maps   A.4.7(22)

Identity attribute   11.4.1(9), C.7.1(12)

idle task   D.11(4)

IEC 559:1989   G.2.2(11.a)

IEEE floating point arithmetic   B.2(10.a), G.2.2(11.a)

IEEE_Float_32   B.2(10.a)

IEEE_Float_64   B.2(10.a)

if_statement   5.3(2)   used   5.1(6), P.1

illegal   construct   1.1.1(44)   partition   1.1.1(46)

Im   in Ada.Numerics.Generic_Complex_Types   G.1.1(6)

image   of a value   3.5(31)   in Ada.Numerics.Discrete_Random   A.5.2(26)   in Ada.Numerics.Float_Random   A.5.2(14)   in Ada.Task_Identification   C.7.1(3)   in Ada.Text_IO.Editing   F.3.3(13)

Image attribute   3.5(36), 4.9.1(8)

Imaginary   in Ada.Numerics.Generic_Complex_Types   G.1.1(4)

Imaginary subtype of Imaginary   in Interfaces.Fortran   B.5(10)

immediate scope   of (a view of) an entity   8.2(11)   of a declaration   8.2(2)

immediately enclosing   8.1(13)

immediately visible   8.3(4), 8.3(22)

immediately within   8.1(13)

implementation   1.1.1(58.a)

implementation advice   1.1.1(54)

implementation defined   1.1.1(75)   summary of characteristics   M(3)

implementation permissions   1.1.1(53)

implementation requirements   1.1.1(50)

implementation-dependent   See unspecified   1.1.1(75)

implicit declaration   3.1(5)

implicit initial values   for a subtype   3.3.1(10)

implicit subtype conversion   4.6(59), 4.6(60)   Access attribute   3.10.2(30)   access discriminant   3.7(28)   array bounds   4.6(38)   array index   4.1.1(7)   assignment to view conversion   4.6(55)   assignment_statement   5.2(11)   bounds of a decimal fixed point type   3.5.9(16)   bounds of a fixed point type   3.5.9(14)   bounds of a range   3.5(9), 3.6(18)   choices of aggregate   4.3.3(23)   component defaults   3.3.1(13)   delay expression   9.6(20)   derived type discriminants   3.4(23)   discriminant values   3.7.1(12)   entry index   9.5.2(24)   expressions in aggregate   4.3.1(20)   expressions of aggregate   4.3.3(24)   function return   6.5(6)   generic formal object of mode in   12.4(11)   inherited enumeration literal   3.4(31)   initialization expression   3.3.1(17)   initialization expression of allocator   4.8(7)   named number value   3.3.2(6)   operand of concatenation   4.5.3(9)   parameter passing   6.4.1(10), 6.4.1(11), 6.4.1(17)   pragma Interrupt_Priority   D.1(17), D.3(9)   pragma Priority   D.1(17), D.3(9)   qualified_expression   4.7(4)   reading a view conversion   4.6(56)   result of inherited function   3.4(29)

implicit_dereference   4.1(6)   used   4.1(4), P.1

Import pragma   B.1(5), L(14)

imported   aspect of representation   B.1(28)

imported entity   B.1(23)

in (membership test)   4.5(1), 4.5.2(2)

inaccessible partition   E.1(7)

inactive   a task state   9(11)

included   one execution by another   11.4(2.a)   one range in another   3.5(4)

incompatibilities with Ada 83   1.1.1(56.e), 2.8(19.c), 2.9(3.a), 3.2.2(15.a), 3.2.3(9.a), 3.4(40.b), 3.5(69.a), 3.5.2(9.b), 3.6.3(8.b), 4.2(14.a), 4.6(71.a), 4.8(20.a), 4.9(44.m), 6.5(25.a), 7.1(17.a), 8.6(34.a), 12.3(29.b), 12.5.1(28.a), 12.5.3(16.a), 12.5.4(13.a), 13.1(26.d), 13.14(19.m), A.5.3(73.d), A.5.4(4.a), A.8.1(16.a), A.10.1(85.a), C.6(22.a)

incomplete type   3.10.1(11)

incomplete_type_declaration   3.10.1(2)   used   3.2.1(2), P.1

inconsistencies with Ada 83   1.1.1(56.b), 3.4(40.a), 3.5.2(9.a), 3.5.7(22.a), 3.5.9(28.a), 3.6.3(8.a), 3.7.1(15.a), 4.5.3(14.a), 9.6(40.a), 11.1(8.a), 12.3(29.a), A.6(1.a), G.2.1(16.c), G.2.3(27.b)

Increment   in Interfaces.C.Pointers   B.3.2(11)

indefinite subtype   3.3(24), 3.7(27)

independent subprogram   11.6(6)

independently addressable   9.10(1)

index   of an array   3.6(9.a)   of an element of an open direct file   A.8(3)   in Ada.Direct_IO   A.8.4(15)   in Ada.Streams.Stream_IO   A.12.1(23)   in Ada.Strings.Bounded   A.4.4(44), A.4.4(45), A.4.4(46)   in Ada.Strings.Fixed   A.4.3(9), A.4.3(10), A.4.3(11)   in Ada.Strings.Unbounded   A.4.5(39), A.4.5(40), A.4.5(41)

index range   3.6(13)

index subtype   3.6(9)

index type   3.6(9)

Index_Check   11.5(17)   [partial]   4.1.1(7), 4.1.2(7), 4.3.3(30), 4.3.3(31), 4.5.3(8), 4.6(51), 4.7(4), 4.8(10)

index_constraint   3.6.1(2)   used   3.2.2(7), P.1

Index_Error   in Ada.Strings   A.4.1(5)

Index_Non_Blank   in Ada.Strings.Bounded   A.4.4(47)   in Ada.Strings.Fixed   A.4.3(12)   in Ada.Strings.Unbounded   A.4.5(42)

index_subtype_definition   3.6(4)   used   3.6(3), P.1

indexed_component   4.1.1(2)   used   4.1(2), P.1

indivisible   C.6(10)

information hiding   See package   7(1)   See private types and private extensions   7.3(1)

information systems   C(1), F(1)

informative   1.1.1(36)

inheritance   See derived types and classes   3.4(1)   See also tagged types and type extension   3.9(1)

inherited   from an ancestor type   3.4.1(13)

inherited component   3.4(13), 3.4(14)

inherited discriminant   3.4(13)

inherited entry   3.4(14)

inherited protected subprogram   3.4(14)

inherited subprogram   3.4(19)

initialization   of a protected object   9.4(14)   of a protected object   C.3.1(10), C.3.1(11)   of a task object   9.1(12), J.7.1(7)   of an object   3.3.1(19)

initialization expression   3.3.1(1), 3.3.1(4)

Initialize   7.6(2)   in Ada.Finalization   7.6(7), 7.6(9)

initialized allocator   4.8(4)

Inline pragma   6.3.2(3), L(15)

innermost dynamically enclosing   11.4(2)

input   A.6(1)

Input attribute   13.13.2(23), 13.13.2(33)

Input clause   13.3(7), 13.13.2(37)

input-output   unspecified for access types   A.7(6)

Insert   in Ada.Strings.Bounded   A.4.4(60), A.4.4(61)   in Ada.Strings.Fixed   A.4.3(25), A.4.3(26)   in Ada.Strings.Unbounded   A.4.5(55), A.4.5(56)

inspectable object   H.3.2(5)

inspection point   H.3.2(5)

Inspection_Point pragma   H.3.2(3), L(16)

instance   of a generic function   12.3(13)   of a generic package   12.3(13)   of a generic procedure   12.3(13)   of a generic subprogram   12.3(13)   of a generic unit   12.3(1)

instructions for comment submission   0.2(59)

int   in Interfaces.C   B.3(7)

Integer   3.5.4(11), 3.5.4(22)   in Standard   A.1(12)

integer literal   2.4(1)

integer literals   3.5.4(14), 3.5.4(31)

integer type   3.5.4(1)

Integer_Address   in System.Storage_Elements   13.7.1(10)

Integer_IO   in Ada.Text_IO   A.10.1(52)

Integer_N   B.2(8)

Integer_Text_IO   child of Ada   A.10.8(21)

integer_type_definition   3.5.4(2)   used   3.2.1(4), P.1

Integer_Wide_Text_IO   child of Ada   A.11(3)

interaction   between tasks   9(1)

interface to assembly language   C.1(4)

interface to C   B.3(1)

interface to COBOL   B.4(1)

interface to Fortran   B.5(1)

interface to other languages   B(1)

Interfaces   B.2(3)

Interfaces.C   B.3(4)

Interfaces.C.Pointers   B.3.2(4)

Interfaces.C.Strings   B.3.1(3)

Interfaces.COBOL   B.4(7)

Interfaces.Fortran   B.5(4)

interfacing pragma   B.1(4)   Convention   B.1(4)   Export   B.1(4)   Import   B.1(4)

internal call   9.5(3)

internal code   13.4(7)

internal requeue   9.5(7)

Internal_Tag   in Ada.Tags   3.9(8)

interpretation   of a complete context   8.6(10)   of a constituent of a complete context   8.6(15)   overload resolution   8.6(14)

interrupt   C.3(2)   example using asynchronous_select   9.7.4(10), 9.7.4(12)

interrupt entry   J.7.1(5)

interrupt handler   C.3(2)

Interrupt_Handler pragma   C.3.1(2), L(17)

Interrupt_Id   in Ada.Interrupts   C.3.2(2)

Interrupt_Priority pragma   D.1(5), L(18)

Interrupt_Priority subtype of Any_Priority   in System   13.7(16)

Interrupts   child of Ada   C.3.2(2)

intertask communication   9.5(1)   See also task   9(1)

Intrinsic calling convention   6.3.1(4)

invalid representation   13.9.1(9)

Inverted_Exclamation   in Ada.Characters.Latin_1   A.3.3(21)

Inverted_Question   in Ada.Characters.Latin_1   A.3.3(22)

IO_Exceptions   child of Ada   A.13(3)

IS1   in Ada.Characters.Latin_1   A.3.3(16)

IS2   in Ada.Characters.Latin_1   A.3.3(16)

IS3   in Ada.Characters.Latin_1   A.3.3(16)

IS4   in Ada.Characters.Latin_1   A.3.3(16)

Is_Alphanumeric   in Ada.Characters.Handling   A.3.2(4)

Is_Attached   in Ada.Interrupts   C.3.2(5)

Is_Basic   in Ada.Characters.Handling   A.3.2(4)

Is_Callable   in Ada.Task_Identification   C.7.1(4)

Is_Character   in Ada.Characters.Handling   A.3.2(14)

Is_Control   in Ada.Characters.Handling   A.3.2(4)

Is_Decimal_Digit   in Ada.Characters.Handling   A.3.2(4)

Is_Digit   in Ada.Characters.Handling   A.3.2(4)

Is_Graphic   in Ada.Characters.Handling   A.3.2(4)

Is_Held   in Ada.Asynchronous_Task_Control   D.11(3)

Is_Hexadecimal_Digit   in Ada.Characters.Handling   A.3.2(4)

Is_In   in Ada.Strings.Maps   A.4.2(13)   in Ada.Strings.Wide_Maps   A.4.7(13)

Is_ISO_646   in Ada.Characters.Handling   A.3.2(10)

Is_Letter   in Ada.Characters.Handling   A.3.2(4)

Is_Lower   in Ada.Characters.Handling   A.3.2(4)

Is_Nul_Terminated   in Interfaces.C   B.3(24), B.3(35)

Is_Open   in Ada.Direct_IO   A.8.4(10)   in Ada.Sequential_IO   A.8.1(10)   in Ada.Streams.Stream_IO   A.12.1(12)   in Ada.Text_IO   A.10.1(13)

Is_Reserved   in Ada.Interrupts   C.3.2(4)

Is_Special   in Ada.Characters.Handling   A.3.2(4)

Is_String   in Ada.Characters.Handling   A.3.2(14)

Is_Subset   in Ada.Strings.Maps   A.4.2(14)   in Ada.Strings.Wide_Maps   A.4.7(14)

Is_Terminated   in Ada.Task_Identification   C.7.1(4)

Is_Upper   in Ada.Characters.Handling   A.3.2(4)

ISO 10646   3.5.2(2), 3.5.2(3)

ISO_646 subtype of Character   in Ada.Characters.Handling   A.3.2(9)

ISO_646_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

issue   an entry call   9.5.3(8)

italics   formal parameters of attribute functions   3.5(18.a)   implementation-defined   1.1.1(62.c)   nongraphic characters   3.5.2(2)   pseudo-names of anonymous types   3.2.1(7), A.1(2)   syntax rules   1.1.1(94)   terms introduced or defined   1.3(1)

italics, like this   1(2.mm)

iterated_element_association   used   4.3.3(46)

iteration_scheme   5.5(3)   used   5.5(2), P.1

iterator_specification   used   4.3.3(49)

J

j   in Ada.Numerics.Generic_Complex_Types   G.1.1(5)   in Interfaces.Fortran   B.5(10)

K

key_choice   used   4.3.3(47)

key_choice_list   used   4.3.3(46)

known discriminants   3.7(27)

known_discriminant_part   3.7(5)   used   3.2.1(3), 3.7(3), 9.1(2), 9.4(2), P.1

L

label   5.1(8)   used   5.1(4), P.1

language   interface to assembly   C.1(4)   interface to non-Ada   B(1)

language-defined check   11.5(3), 11.6(1)

language-defined class   [partial]   3.2(12)   of types   3.2(3)

Language-Defined Library Units   A(1)   Ada   A.2(2)   Ada.Asynchronous_Task_Control   D.11(3)   Ada.Calendar   9.6(10)   Ada.Characters   A.3.1(2)   Ada.Characters.Handling   A.3.2(2)   Ada.Characters.Latin_1   A.3.3(3)   Ada.Command_Line   A.15(3)   Ada.Decimal   F.2(2)   Ada.Direct_IO   A.8.4(2)   Ada.Dynamic_Priorities   D.5(3)   Ada.Exceptions   11.4.1(2)   Ada.Finalization   7.6(5)   Ada.Float_Text_IO   A.10.9(33)   Ada.Float_Wide_Text_IO   A.11(3)   Ada.Integer_Text_IO   A.10.8(21)   Ada.Integer_Wide_Text_IO   A.11(3)   Ada.Interrupts   C.3.2(2)   Ada.Interrupts.Names   C.3.2(12)   Ada.IO_Exceptions   A.13(3)   Ada.Numerics   A.5(3)   Ada.Numerics.Complex_Elementary_Functions   G.1.2(9)   Ada.Numerics.Complex_Types   G.1.1(25)   Ada.Numerics.Discrete_Random   A.5.2(17)   Ada.Numerics.Elementary_Functions   A.5.1(9)   Ada.Numerics.Float_Random   A.5.2(5)   Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(2)   Ada.Numerics.Generic_Complex_Types   G.1.1(2)   Ada.Numerics.Generic_Elementary_Functions   A.5.1(3)   Ada.Real_Time   D.8(3)   Ada.Sequential_IO   A.8.1(2)   Ada.Storage_IO   A.9(3)   Ada.Streams   13.13.1(2)   Ada.Streams.Stream_IO   A.12.1(3)   Ada.Strings   A.4.1(3)   Ada.Strings.Bounded   A.4.4(3)   Ada.Strings.Fixed   A.4.3(5)   Ada.Strings.Maps   A.4.2(3)   Ada.Strings.Maps.Constants   A.4.6(3)   Ada.Strings.Unbounded   A.4.5(3)   Ada.Strings.Wide_Bounded   A.4.7(1)   Ada.Strings.Wide_Bounded.Wide_Hash   A.4.7(1)   Ada.Strings.Wide_Fixed   A.4.7(1)   Ada.Strings.Wide_Fixed.Wide_Hash   A.4.7(1)   Ada.Strings.Wide_Hash   A.4.7(1)   Ada.Strings.Wide_Maps   A.4.7(3)   Ada.Strings.Wide_Maps.Wide_Constants   A.4.7(1)   Ada.Strings.Wide_Unbounded   A.4.7(1)   Ada.Strings.Wide_Unbounded.Wide_Hash   A.4.7(1)   Ada.Synchronous_Task_Control   D.10(3)   Ada.Tags   3.9(7)   Ada.Task_Attributes   C.7.2(2)   Ada.Task_Identification   C.7.1(2)   Ada.Text_IO   A.10.1(2)   Ada.Text_IO.Complex_IO   G.1.3(3)   Ada.Text_IO.Editing   F.3.3(3)   Ada.Text_IO.Text_Streams   A.12.2(3)   Ada.Unchecked_Conversion   13.9(3)   Ada.Unchecked_Deallocation   13.11.2(3)   Ada.Wide_Text_IO   A.11(2)   Ada.Wide_Text_IO.Complex_IO   G.1.4(1)   Ada.Wide_Text_IO.Editing   F.3.4(1)   Ada.Wide_Text_IO.Text_Streams   A.12.3(3)   Interfaces   B.2(3)   Interfaces.C   B.3(4)   Interfaces.C.Pointers   B.3.2(4)   Interfaces.C.Strings   B.3.1(3)   Interfaces.COBOL   B.4(7)   Interfaces.Fortran   B.5(4)   Standard   A.1(4)   System   13.7(3)   System.Address_To_Access_Conversions   13.7.2(2)   System.Machine_Code   13.8(7)   System.RPC   E.5(3)   System.Storage_Elements   13.7.1(2)   System.Storage_Pools   13.11(6)

Language-Defined Subprogram   Abort_Task in Ada.Task_Identification   C.7.1(3)   Adjust in Ada.Finalization   7.6(7)   Allocate in System.Storage_Pools   13.11(8)   Append in Ada.Strings.Bounded   A.4.4(13), A.4.4(14), A.4.4(15), A.4.4(16), A.4.4(17), A.4.4(18), A.4.4(19), A.4.4(20)   Append in Ada.Strings.Unbounded   A.4.5(12), A.4.5(13), A.4.5(14)   Arccos in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   Arccos in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)   Arccosh in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   Arccosh in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Arccot in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   Arccot in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)   Arccoth in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   Arccoth in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Arcsin in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   Arcsin in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)   Arcsinh in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   Arcsinh in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Arctan in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(5)   Arctan in Ada.Numerics.Generic_Elementary_Functions   A.5.1(6)   Arctanh in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(7)   Arctanh in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Argument in Ada.Command_Line   A.15(5)   Argument in Ada.Numerics.Generic_Complex_Types   G.1.1(10)   Argument_Count in Ada.Command_Line   A.15(4)   Attach_Handler in Ada.Interrupts   C.3.2(7)   Blank_When_Zero in Ada.Text_IO.Editing   F.3.3(7)   Clock in Ada.Calendar   9.6(12)   Clock in Ada.Real_Time   D.8(6)   Close in Ada.Direct_IO   A.8.4(8)   Close in Ada.Sequential_IO   A.8.1(8)   Close in Ada.Streams.Stream_IO   A.12.1(10)   Close in Ada.Text_IO   A.10.1(11)   Col in Ada.Text_IO   A.10.1(37)   Command_Name in Ada.Command_Line   A.15(6)   Compose_From_Cartesian in Ada.Numerics.Generic_Complex_Types   G.1.1(8)   Compose_From_Polar in Ada.Numerics.Generic_Complex_Types   G.1.1(11)   Conjugate in Ada.Numerics.Generic_Complex_Types   G.1.1(12), G.1.1(15)   Continue in Ada.Asynchronous_Task_Control   D.11(3)   Copy_Array in Interfaces.C.Pointers   B.3.2(15)   Copy_Terminated_Array in Interfaces.C.Pointers   B.3.2(14)   Cos in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   Cos in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)   Cosh in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   Cosh in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Cot in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   Cot in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)   Coth in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   Coth in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Count in Ada.Strings.Bounded   A.4.4(48), A.4.4(49), A.4.4(50)   Count in Ada.Strings.Fixed   A.4.3(13), A.4.3(14), A.4.3(15)   Count in Ada.Strings.Unbounded   A.4.5(43), A.4.5(44), A.4.5(45)   Create in Ada.Direct_IO   A.8.4(6)   Create in Ada.Sequential_IO   A.8.1(6)   Create in Ada.Streams.Stream_IO   A.12.1(8)   Create in Ada.Text_IO   A.10.1(9)   Current_Error in Ada.Text_IO   A.10.1(17), A.10.1(20)   Current_Handler in Ada.Interrupts   C.3.2(6)   Current_Input in Ada.Text_IO   A.10.1(17), A.10.1(20)   Current_Output in Ada.Text_IO   A.10.1(17), A.10.1(20)   Current_State in Ada.Synchronous_Task_Control   D.10(4)   Current_Task in Ada.Task_Identification   C.7.1(3)   Day in Ada.Calendar   9.6(13)   Deallocate in System.Storage_Pools   13.11(9)   Decrement in Interfaces.C.Pointers   B.3.2(11)   Delete in Ada.Direct_IO   A.8.4(8)   Delete in Ada.Sequential_IO   A.8.1(8)   Delete in Ada.Streams.Stream_IO   A.12.1(10)   Delete in Ada.Strings.Bounded   A.4.4(64), A.4.4(65)   Delete in Ada.Strings.Fixed   A.4.3(29), A.4.3(30)   Delete in Ada.Strings.Unbounded   A.4.5(59), A.4.5(60)   Delete in Ada.Text_IO   A.10.1(11)   Dereference_Error in Interfaces.C.Strings   B.3.1(12)   Detach_Handler in Ada.Interrupts   C.3.2(9)   Divide in Ada.Decimal   F.2(6)   Do_APC in System.RPC   E.5(10)   Do_RPC in System.RPC   E.5(9)   Element in Ada.Strings.Bounded   A.4.4(26)   Element in Ada.Strings.Unbounded   A.4.5(20)   End_Of_File in Ada.Direct_IO   A.8.4(16)   End_Of_File in Ada.Sequential_IO   A.8.1(13)   End_Of_File in Ada.Streams.Stream_IO   A.12.1(12)   End_Of_File in Ada.Text_IO   A.10.1(34)   End_Of_Line in Ada.Text_IO   A.10.1(30)   End_Of_Page in Ada.Text_IO   A.10.1(33)   Establish_RPC_Receiver in System.RPC   E.5(12)   Exception_Identity in Ada.Exceptions   11.4.1(5)   Exception_Information in Ada.Exceptions   11.4.1(5)   Exception_Message in Ada.Exceptions   11.4.1(4)   Exception_Name in Ada.Exceptions   11.4.1(2), 11.4.1(5)   Exchange_Handler in Ada.Interrupts   C.3.2(8)   Exp in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   Exp in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)   Expanded_Name in Ada.Tags   3.9(8)   External_Tag in Ada.Tags   3.9(8)   Finalize in Ada.Finalization   7.6(7), 7.6(9)   Find_Token in Ada.Strings.Bounded   A.4.4(51)   Find_Token in Ada.Strings.Fixed   A.4.3(16)   Find_Token in Ada.Strings.Unbounded   A.4.5(46)   Flush in Ada.Streams.Stream_IO   A.12.1(25)   Flush in Ada.Text_IO   A.10.1(21)   Form in Ada.Direct_IO   A.8.4(9)   Form in Ada.Sequential_IO   A.8.1(9)   Form in Ada.Streams.Stream_IO   A.12.1(11)   Form in Ada.Text_IO   A.10.1(12)   Free in Ada.Strings.Unbounded   A.4.5(7)   Free in Interfaces.C.Strings   B.3.1(11)   Get in Ada.Text_IO   A.10.1(41), A.10.1(47), A.10.1(54), A.10.1(55), A.10.1(59), A.10.1(60), A.10.1(65), A.10.1(67), A.10.1(70), A.10.1(72), A.10.1(75), A.10.1(77), A.10.1(81), A.10.1(83)   Get in Ada.Text_IO.Complex_IO   G.1.3(6), G.1.3(8)   Get_Immediate in Ada.Text_IO   A.10.1(44), A.10.1(45)   Get_Line in Ada.Text_IO   A.10.1(49)   Get_Priority in Ada.Dynamic_Priorities   D.5(5)   Head in Ada.Strings.Bounded   A.4.4(70), A.4.4(71)   Head in Ada.Strings.Fixed   A.4.3(35), A.4.3(36)   Head in Ada.Strings.Unbounded   A.4.5(65), A.4.5(66)   Hold in Ada.Asynchronous_Task_Control   D.11(3)   Im in Ada.Numerics.Generic_Complex_Types   G.1.1(6)   Image in Ada.Numerics.Discrete_Random   A.5.2(26)   Image in Ada.Numerics.Float_Random   A.5.2(14)   Image in Ada.Task_Identification   C.7.1(3)   Image in Ada.Text_IO.Editing   F.3.3(13)   Increment in Interfaces.C.Pointers   B.3.2(11)   Index in Ada.Direct_IO   A.8.4(15)   Index in Ada.Streams.Stream_IO   A.12.1(23)   Index in Ada.Strings.Bounded   A.4.4(44), A.4.4(45), A.4.4(46)   Index in Ada.Strings.Fixed   A.4.3(9), A.4.3(10), A.4.3(11)   Index in Ada.Strings.Unbounded   A.4.5(39), A.4.5(40), A.4.5(41)   Index_Non_Blank in Ada.Strings.Bounded   A.4.4(47)   Index_Non_Blank in Ada.Strings.Fixed   A.4.3(12)   Index_Non_Blank in Ada.Strings.Unbounded   A.4.5(42)   Initialize in Ada.Finalization   7.6(7), 7.6(9)   Insert in Ada.Strings.Bounded   A.4.4(60), A.4.4(61)   Insert in Ada.Strings.Fixed   A.4.3(25), A.4.3(26)   Insert in Ada.Strings.Unbounded   A.4.5(55), A.4.5(56)   Internal_Tag in Ada.Tags   3.9(8)   Is_Alphanumeric in Ada.Characters.Handling   A.3.2(4)   Is_Attached in Ada.Interrupts   C.3.2(5)   Is_Basic in Ada.Characters.Handling   A.3.2(4)   Is_Callable in Ada.Task_Identification   C.7.1(4)   Is_Character in Ada.Characters.Handling   A.3.2(14)   Is_Control in Ada.Characters.Handling   A.3.2(4)   Is_Decimal_Digit in Ada.Characters.Handling   A.3.2(4)   Is_Digit in Ada.Characters.Handling   A.3.2(4)   Is_Graphic in Ada.Characters.Handling   A.3.2(4)   Is_Held in Ada.Asynchronous_Task_Control   D.11(3)   Is_Hexadecimal_Digit in Ada.Characters.Handling   A.3.2(4)   Is_In in Ada.Strings.Maps   A.4.2(13)   Is_In in Ada.Strings.Wide_Maps   A.4.7(13)   Is_ISO_646 in Ada.Characters.Handling   A.3.2(10)   Is_Letter in Ada.Characters.Handling   A.3.2(4)   Is_Lower in Ada.Characters.Handling   A.3.2(4)   Is_Nul_Terminated in Interfaces.C   B.3(24), B.3(35)   Is_Open in Ada.Direct_IO   A.8.4(10)   Is_Open in Ada.Sequential_IO   A.8.1(10)   Is_Open in Ada.Streams.Stream_IO   A.12.1(12)   Is_Open in Ada.Text_IO   A.10.1(13)   Is_Reserved in Ada.Interrupts   C.3.2(4)   Is_Special in Ada.Characters.Handling   A.3.2(4)   Is_String in Ada.Characters.Handling   A.3.2(14)   Is_Subset in Ada.Strings.Maps   A.4.2(14)   Is_Subset in Ada.Strings.Wide_Maps   A.4.7(14)   Is_Terminated in Ada.Task_Identification   C.7.1(4)   Is_Upper in Ada.Characters.Handling   A.3.2(4)   Length in Ada.Strings.Bounded   A.4.4(9)   Length in Ada.Strings.Unbounded   A.4.5(6)   Length in Ada.Text_IO.Editing   F.3.3(11)   Length in Interfaces.COBOL   B.4(34), B.4(39), B.4(44)   Line in Ada.Text_IO   A.10.1(38)   Line_Length in Ada.Text_IO   A.10.1(25)   Log in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   Log in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)   Look_Ahead in Ada.Text_IO   A.10.1(43)   Microseconds in Ada.Real_Time   D.8(14)   Milliseconds in Ada.Real_Time   D.8(14)   Mode in Ada.Direct_IO   A.8.4(9)   Mode in Ada.Sequential_IO   A.8.1(9)   Mode in Ada.Streams.Stream_IO   A.12.1(11)   Mode in Ada.Text_IO   A.10.1(12)   Modulus in Ada.Numerics.Generic_Complex_Types   G.1.1(9)   Month in Ada.Calendar   9.6(13)   Move in Ada.Strings.Fixed   A.4.3(7)   Name in Ada.Direct_IO   A.8.4(9)   Name in Ada.Sequential_IO   A.8.1(9)   Name in Ada.Streams.Stream_IO   A.12.1(11)   Name in Ada.Text_IO   A.10.1(12)   Nanoseconds in Ada.Real_Time   D.8(14)   New_Char_Array in Interfaces.C.Strings   B.3.1(9)   New_Line in Ada.Text_IO   A.10.1(28)   New_Page in Ada.Text_IO   A.10.1(31)   New_String in Interfaces.C.Strings   B.3.1(10)   Null_Task_Id in Ada.Task_Identification   C.7.1(2)   Open in Ada.Direct_IO   A.8.4(7)   Open in Ada.Sequential_IO   A.8.1(7)   Open in Ada.Streams.Stream_IO   A.12.1(9)   Open in Ada.Text_IO   A.10.1(10)   Overwrite in Ada.Strings.Bounded   A.4.4(62), A.4.4(63)   Overwrite in Ada.Strings.Fixed   A.4.3(27), A.4.3(28)   Overwrite in Ada.Strings.Unbounded   A.4.5(57), A.4.5(58)   Page in Ada.Text_IO   A.10.1(39)   Page_Length in Ada.Text_IO   A.10.1(26)   Pic_String in Ada.Text_IO.Editing   F.3.3(7)   Put in Ada.Text_IO   A.10.1(42), A.10.1(48), A.10.1(55), A.10.1(60), A.10.1(66), A.10.1(67), A.10.1(71), A.10.1(72), A.10.1(76), A.10.1(77), A.10.1(82), A.10.1(83)   Put in Ada.Text_IO.Complex_IO   G.1.3(7), G.1.3(8)   Put in Ada.Text_IO.Editing   F.3.3(14), F.3.3(15), F.3.3(16)   Put_Line in Ada.Text_IO   A.10.1(50)   Raise_Exception in Ada.Exceptions   11.4.1(4)   Random in Ada.Numerics.Discrete_Random   A.5.2(20)   Random in Ada.Numerics.Float_Random   A.5.2(8)   Re in Ada.Numerics.Generic_Complex_Types   G.1.1(6)   Read in Ada.Direct_IO   A.8.4(12)   Read in Ada.Sequential_IO   A.8.1(12)   Read in Ada.Storage_IO   A.9(6)   Read in Ada.Streams   13.13.1(5)   Read in Ada.Streams.Stream_IO   A.12.1(15), A.12.1(16)   Read in System.RPC   E.5(7)   Reference in Ada.Interrupts   C.3.2(10)   Reference in Ada.Task_Attributes   C.7.2(5)   Reinitialize in Ada.Task_Attributes   C.7.2(6)   Replace_Element in Ada.Strings.Bounded   A.4.4(27)   Replace_Element in Ada.Strings.Unbounded   A.4.5(21)   Replace_Slice in Ada.Strings.Bounded   A.4.4(58), A.4.4(59)   Replace_Slice in Ada.Strings.Fixed   A.4.3(23), A.4.3(24)   Replace_Slice in Ada.Strings.Unbounded   A.4.5(53), A.4.5(54)   Replicate in Ada.Strings.Bounded   A.4.4(78), A.4.4(79), A.4.4(80)   Reraise_Occurrence in Ada.Exceptions   11.4.1(4)   Reset in Ada.Direct_IO   A.8.4(8)   Reset in Ada.Numerics.Discrete_Random   A.5.2(21), A.5.2(24)   Reset in Ada.Numerics.Float_Random   A.5.2(9), A.5.2(12)   Reset in Ada.Sequential_IO   A.8.1(8)   Reset in Ada.Streams.Stream_IO   A.12.1(10)   Reset in Ada.Text_IO   A.10.1(11)   Save in Ada.Numerics.Discrete_Random   A.5.2(24)   Save in Ada.Numerics.Float_Random   A.5.2(12)   Save_Occurrence in Ada.Exceptions   11.4.1(6)   Seconds in Ada.Calendar   9.6(13)   Set_Col in Ada.Text_IO   A.10.1(35)   Set_Error in Ada.Text_IO   A.10.1(15)   Set_Exit_Status in Ada.Command_Line   A.15(9)   Set_False in Ada.Synchronous_Task_Control   D.10(4)   Set_Im in Ada.Numerics.Generic_Complex_Types   G.1.1(7)   Set_Index in Ada.Direct_IO   A.8.4(14)   Set_Index in Ada.Streams.Stream_IO   A.12.1(22)   Set_Input in Ada.Text_IO   A.10.1(15)   Set_Line in Ada.Text_IO   A.10.1(36)   Set_Line_Length in Ada.Text_IO   A.10.1(23)   Set_Mode in Ada.Streams.Stream_IO   A.12.1(24)   Set_Output in Ada.Text_IO   A.10.1(15)   Set_Page_Length in Ada.Text_IO   A.10.1(24)   Set_Priority in Ada.Dynamic_Priorities   D.5(4)   Set_Re in Ada.Numerics.Generic_Complex_Types   G.1.1(7)   Set_True in Ada.Synchronous_Task_Control   D.10(4)   Set_Value in Ada.Task_Attributes   C.7.2(6)   Sin in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   Sin in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)   Sinh in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   Sinh in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Size in Ada.Direct_IO   A.8.4(15)   Size in Ada.Streams.Stream_IO   A.12.1(23)   Skip_Line in Ada.Text_IO   A.10.1(29)   Skip_Page in Ada.Text_IO   A.10.1(32)   Slice in Ada.Strings.Bounded   A.4.4(28)   Slice in Ada.Strings.Unbounded   A.4.5(22)   Split in Ada.Calendar   9.6(14)   Split in Ada.Real_Time   D.8(16)   Sqrt in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   Sqrt in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)   Standard_Error in Ada.Text_IO   A.10.1(16), A.10.1(19)   Standard_Input in Ada.Text_IO   A.10.1(16), A.10.1(19)   Standard_Output in Ada.Text_IO   A.10.1(16), A.10.1(19)   Storage_Size in System.Storage_Pools   13.11(10)   Stream in Ada.Streams.Stream_IO   A.12.1(13)   Stream in Ada.Text_IO.Text_Streams   A.12.2(4)   Stream in Ada.Wide_Text_IO.Text_Streams   A.12.3(4)   Strlen in Interfaces.C.Strings   B.3.1(17)   Suspend_Until_True in Ada.Synchronous_Task_Control   D.10(4)   Tail in Ada.Strings.Bounded   A.4.4(72), A.4.4(73)   Tail in Ada.Strings.Fixed   A.4.3(37), A.4.3(38)   Tail in Ada.Strings.Unbounded   A.4.5(67), A.4.5(68)   Tan in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   Tan in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)   Tanh in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   Tanh in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)   Time_Of in Ada.Calendar   9.6(15)   Time_Of in Ada.Real_Time   D.8(16)   To_Ada in Interfaces.C   B.3(22), B.3(26), B.3(28), B.3(32), B.3(37), B.3(39)   To_Ada in Interfaces.COBOL   B.4(17), B.4(19)   To_Ada in Interfaces.Fortran   B.5(13), B.5(14), B.5(16)   To_Address in System.Address_To_Access_Conversions   13.7.2(3)   To_Address in System.Storage_Elements   13.7.1(10)   To_Basic in Ada.Characters.Handling   A.3.2(6), A.3.2(7)   To_Binary in Interfaces.COBOL   B.4(45), B.4(48)   To_Bounded_String in Ada.Strings.Bounded   A.4.4(11)   To_C in Interfaces.C   B.3(21), B.3(25), B.3(27), B.3(32), B.3(36), B.3(38)   To_Character in Ada.Characters.Handling   A.3.2(15)   To_Chars_Ptr in Interfaces.C.Strings   B.3.1(8)   To_COBOL in Interfaces.COBOL   B.4(17), B.4(18)   To_Decimal in Interfaces.COBOL   B.4(35), B.4(40), B.4(44), B.4(47)   To_Display in Interfaces.COBOL   B.4(36)   To_Domain in Ada.Strings.Maps   A.4.2(24)   To_Domain in Ada.Strings.Wide_Maps   A.4.7(24)   To_Duration in Ada.Real_Time   D.8(13)   To_Fortran in Interfaces.Fortran   B.5(13), B.5(14), B.5(15)   To_Integer in System.Storage_Elements   13.7.1(10)   To_ISO_646 in Ada.Characters.Handling   A.3.2(11), A.3.2(12)   To_Long_Binary in Interfaces.COBOL   B.4(48)   To_Lower in Ada.Characters.Handling   A.3.2(6), A.3.2(7)   To_Mapping in Ada.Strings.Maps   A.4.2(23)   To_Mapping in Ada.Strings.Wide_Maps   A.4.7(23)   To_Packed in Interfaces.COBOL   B.4(41)   To_Picture in Ada.Text_IO.Editing   F.3.3(6)   To_Pointer in System.Address_To_Access_Conversions   13.7.2(3)   To_Range in Ada.Strings.Maps   A.4.2(24)   To_Range in Ada.Strings.Wide_Maps   A.4.7(25)   To_Ranges in Ada.Strings.Maps   A.4.2(10)   To_Ranges in Ada.Strings.Wide_Maps   A.4.7(10)   To_Sequence in Ada.Strings.Maps   A.4.2(19)   To_Sequence in Ada.Strings.Wide_Maps   A.4.7(19)   To_Set in Ada.Strings.Maps   A.4.2(8), A.4.2(9), A.4.2(17), A.4.2(18)   To_Set in Ada.Strings.Wide_Maps   A.4.7(8), A.4.7(9), A.4.7(17), A.4.7(18)   To_String in Ada.Characters.Handling   A.3.2(16)   To_String in Ada.Strings.Bounded   A.4.4(12)   To_String in Ada.Strings.Unbounded   A.4.5(11)   To_Time_Span in Ada.Real_Time   D.8(13)   To_Unbounded_String in Ada.Strings.Unbounded   A.4.5(9), A.4.5(10)   To_Upper in Ada.Characters.Handling   A.3.2(6), A.3.2(7)   To_Wide_Character in Ada.Characters.Handling   A.3.2(17)   To_Wide_String in Ada.Characters.Handling   A.3.2(18)   Translate in Ada.Strings.Bounded   A.4.4(53), A.4.4(54), A.4.4(55), A.4.4(56)   Translate in Ada.Strings.Fixed   A.4.3(18), A.4.3(19), A.4.3(20), A.4.3(21)   Translate in Ada.Strings.Unbounded   A.4.5(48), A.4.5(49), A.4.5(50), A.4.5(51)   Trim in Ada.Strings.Bounded   A.4.4(67), A.4.4(68), A.4.4(69)   Trim in Ada.Strings.Fixed   A.4.3(31), A.4.3(32), A.4.3(33), A.4.3(34)   Trim in Ada.Strings.Unbounded   A.4.5(61), A.4.5(62), A.4.5(63), A.4.5(64)   Update in Interfaces.C.Strings   B.3.1(18), B.3.1(19)   Update_Error in Interfaces.C.Strings   B.3.1(20)   Valid in Ada.Text_IO.Editing   F.3.3(5), F.3.3(12)   Valid in Interfaces.COBOL   B.4(33), B.4(38), B.4(43)   Value in Ada.Numerics.Discrete_Random   A.5.2(26)   Value in Ada.Numerics.Float_Random   A.5.2(14)   Value in Ada.Strings.Maps   A.4.2(21)   Value in Ada.Strings.Wide_Maps   A.4.7(21)   Value in Ada.Task_Attributes   C.7.2(4)   Value in Interfaces.C.Pointers   B.3.2(6), B.3.2(7)   Value in Interfaces.C.Strings   B.3.1(13), B.3.1(14), B.3.1(15), B.3.1(16)   Virtual_Length in Interfaces.C.Pointers   B.3.2(13)   Write in Ada.Direct_IO   A.8.4(13)   Write in Ada.Sequential_IO   A.8.1(12)   Write in Ada.Storage_IO   A.9(7)   Write in Ada.Streams   13.13.1(6)   Write in Ada.Streams.Stream_IO   A.12.1(18), A.12.1(19)   Write in System.RPC   E.5(8)   Year in Ada.Calendar   9.6(13)

Language-Defined Type   Address in System   13.7(12)   Alignment in Ada.Strings   A.4.1(6)   Alphanumeric in Interfaces.COBOL   B.4(16)   Attribute_Handle in Ada.Task_Attributes   C.7.2(3)   Binary in Interfaces.COBOL   B.4(10)   Binary_Format in Interfaces.COBOL   B.4(24)   Bit_Order in System   13.7(15)   Boolean in Standard   A.1(5)   Bounded_String in Ada.Strings.Bounded   A.4.4(6)   Byte in Interfaces.COBOL   B.4(29)   Byte_Array in Interfaces.COBOL   B.4(29)   C_float in Interfaces.C   B.3(15)   char in Interfaces.C   B.3(19)   char_array in Interfaces.C   B.3(23)   char_array_access in Interfaces.C.Strings   B.3.1(4)   Character in Standard   A.1(35)   Character_Mapping in Ada.Strings.Maps   A.4.2(20)   Character_Mapping_Function in Ada.Strings.Maps   A.4.2(25)   Character_Range in Ada.Strings.Maps   A.4.2(6)   Character_Ranges in Ada.Strings.Maps   A.4.2(7)   Character_Set in Ada.Strings.Maps   A.4.2(4)   Character_Set in Interfaces.Fortran   B.5(11)   chars_ptr in Interfaces.C.Strings   B.3.1(5)   chars_ptr_array in Interfaces.C.Strings   B.3.1(6)   COBOL_Character in Interfaces.COBOL   B.4(13)   Complex in Ada.Numerics.Generic_Complex_Types   G.1.1(3)   Complex in Interfaces.Fortran   B.5(9)   Controlled in Ada.Finalization   7.6(6)   Count in Ada.Direct_IO   A.8.4(4)   Count in Ada.Streams.Stream_IO   A.12.1(7)   Count in Ada.Text_IO   A.10.1(5)   Decimal_Element in Interfaces.COBOL   B.4(12)   Direction in Ada.Strings   A.4.1(6)   Display_Format in Interfaces.COBOL   B.4(22)   double in Interfaces.C   B.3(16)   Double_Precision in Interfaces.Fortran   B.5(6)   Duration in Standard   A.1(43)   Exception_Id in Ada.Exceptions   11.4.1(2)   Exception_Occurrence in Ada.Exceptions   11.4.1(3)   Exception_Occurrence_Access in Ada.Exceptions   11.4.1(3)   Exit_Status in Ada.Command_Line   A.15(7)   File_Access in Ada.Text_IO   A.10.1(18)   File_Mode in Ada.Direct_IO   A.8.4(4)   File_Mode in Ada.Sequential_IO   A.8.1(4)   File_Mode in Ada.Streams.Stream_IO   A.12.1(6)   File_Mode in Ada.Text_IO   A.10.1(4)   File_Type in Ada.Direct_IO   A.8.4(3)   File_Type in Ada.Sequential_IO   A.8.1(3)   File_Type in Ada.Streams.Stream_IO   A.12.1(5)   File_Type in Ada.Text_IO   A.10.1(3)   Float in Standard   A.1(21)   Floating in Interfaces.COBOL   B.4(9)   Fortran_Character in Interfaces.Fortran   B.5(12)   Fortran_Integer in Interfaces.Fortran   B.5(5)   Generator in Ada.Numerics.Discrete_Random   A.5.2(19)   Generator in Ada.Numerics.Float_Random   A.5.2(7)   Imaginary in Ada.Numerics.Generic_Complex_Types   G.1.1(4)   int in Interfaces.C   B.3(7)   Integer in Standard   A.1(12)   Integer_Address in System.Storage_Elements   13.7.1(10)   Interrupt_Id in Ada.Interrupts   C.3.2(2)   Limited_Controlled in Ada.Finalization   7.6(8)   Logical in Interfaces.Fortran   B.5(7)   long in Interfaces.C   B.3(7)   Long_Binary in Interfaces.COBOL   B.4(10)   long_double in Interfaces.C   B.3(17)   Long_Floating in Interfaces.COBOL   B.4(9)   Membership in Ada.Strings   A.4.1(6)   Name in System   13.7(4)   Numeric in Interfaces.COBOL   B.4(20)   Packed_Decimal in Interfaces.COBOL   B.4(12)   Packed_Format in Interfaces.COBOL   B.4(26)   Parameterless_Handler in Ada.Interrupts   C.3.2(2)   Params_Stream_Type in System.RPC   E.5(6)   Partition_Id in System.RPC   E.5(4)   Picture in Ada.Text_IO.Editing   F.3.3(4)   plain_char in Interfaces.C   B.3(11)   Pointer in Interfaces.C.Pointers   B.3.2(5)   ptrdiff_t in Interfaces.C   B.3(12)   Real in Interfaces.Fortran   B.5(6)   Root_Storage_Pool in System.Storage_Pools   13.11(7)   Root_Stream_Type in Ada.Streams   13.13.1(3)   RPC_Receiver in System.RPC   E.5(11)   Seconds_Count in Ada.Real_Time   D.8(15)   short in Interfaces.C   B.3(7)   signed_char in Interfaces.C   B.3(8)   size_t in Interfaces.C   B.3(13)   State in Ada.Numerics.Discrete_Random   A.5.2(23)   State in Ada.Numerics.Float_Random   A.5.2(11)   Storage_Array in System.Storage_Elements   13.7.1(5)   Storage_Element in System.Storage_Elements   13.7.1(5)   Storage_Offset in System.Storage_Elements   13.7.1(3)   Stream_Access in Ada.Streams.Stream_IO   A.12.1(4)   Stream_Access in Ada.Text_IO.Text_Streams   A.12.2(3)   Stream_Access in Ada.Wide_Text_IO.Text_Streams   A.12.3(3)   Stream_Element in Ada.Streams   13.13.1(4)   Stream_Element_Array in Ada.Streams   13.13.1(4)   Stream_Element_Offset in Ada.Streams   13.13.1(4)   String in Standard   A.1(37)   String_Access in Ada.Strings.Unbounded   A.4.5(7)   Suspension_Object in Ada.Synchronous_Task_Control   D.10(4)   Tag in Ada.Tags   3.9(7)   Task_Id in Ada.Task_Identification   C.7.1(2)   Time in Ada.Calendar   9.6(10)   Time in Ada.Real_Time   D.8(4)   Time_Span in Ada.Real_Time   D.8(5)   Trim_End in Ada.Strings   A.4.1(6)   Truncation in Ada.Strings   A.4.1(6)   Type_Set in Ada.Text_IO   A.10.1(7)   Unbounded_String in Ada.Strings.Unbounded   A.4.5(4)   unsigned in Interfaces.C   B.3(9)   unsigned_char in Interfaces.C   B.3(10)   unsigned_long in Interfaces.C   B.3(9)   unsigned_short in Interfaces.C   B.3(9)   wchar_array in Interfaces.C   B.3(33)   wchar_t in Interfaces.C   B.3(30)   Wide_Character in Standard   A.1   Wide_Character_Mapping in Ada.Strings.Wide_Maps   A.4.7(20)   Wide_Character_Mapping_Function in Ada.Strings.Wide_Maps   A.4.7(26)   Wide_Character_Range in Ada.Strings.Wide_Maps   A.4.7(6)   Wide_Character_Ranges in Ada.Strings.Wide_Maps   A.4.7(7)   Wide_Character_Set in Ada.Strings.Wide_Maps   A.4.7(4)   Wide_String in Standard   A.1(41)

Last attribute   3.5(13), 3.6.2(5)

Last(N) attribute   3.6.2(6)

last_bit   13.5.1(6)   used   13.5.1(3), P.1

Last_Bit attribute   13.5.2(4)

lateness   D.9(12)

Latin-1   3.5.2(2)

Latin_1   child of Ada.Characters   A.3.3(3)

layout   aspect of representation   13.5(1)

Layout_Error   in Ada.IO_Exceptions   A.13(4)   in Ada.Text_IO   A.10.1(85)

LC_A   in Ada.Characters.Latin_1   A.3.3(13)

LC_A_Acute   in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Circumflex   in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Diaeresis   in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Grave   in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Ring   in Ada.Characters.Latin_1   A.3.3(25)

LC_A_Tilde   in Ada.Characters.Latin_1   A.3.3(25)

LC_AE_Diphthong   in Ada.Characters.Latin_1   A.3.3(25)

LC_B   in Ada.Characters.Latin_1   A.3.3(13)

LC_C   in Ada.Characters.Latin_1   A.3.3(13)

LC_C_Cedilla   in Ada.Characters.Latin_1   A.3.3(25)

LC_D   in Ada.Characters.Latin_1   A.3.3(13)

LC_E   in Ada.Characters.Latin_1   A.3.3(13)

LC_E_Acute   in Ada.Characters.Latin_1   A.3.3(25)

LC_E_Circumflex   in Ada.Characters.Latin_1   A.3.3(25)

LC_E_Diaeresis   in Ada.Characters.Latin_1   A.3.3(25)

LC_E_Grave   in Ada.Characters.Latin_1   A.3.3(25)

LC_F   in Ada.Characters.Latin_1   A.3.3(13)

LC_G   in Ada.Characters.Latin_1   A.3.3(13)

LC_German_Sharp_S   in Ada.Characters.Latin_1   A.3.3(24)

LC_H   in Ada.Characters.Latin_1   A.3.3(13)

LC_I   in Ada.Characters.Latin_1   A.3.3(13)

LC_I_Acute   in Ada.Characters.Latin_1   A.3.3(25)

LC_I_Circumflex   in Ada.Characters.Latin_1   A.3.3(25)

LC_I_Diaeresis   in Ada.Characters.Latin_1   A.3.3(25)

LC_I_Grave   in Ada.Characters.Latin_1   A.3.3(25)

LC_Icelandic_Eth   in Ada.Characters.Latin_1   A.3.3(26)

LC_Icelandic_Thorn   in Ada.Characters.Latin_1   A.3.3(26)

LC_J   in Ada.Characters.Latin_1   A.3.3(13)

LC_K   in Ada.Characters.Latin_1   A.3.3(13)

LC_L   in Ada.Characters.Latin_1   A.3.3(13)

LC_M   in Ada.Characters.Latin_1   A.3.3(13)

LC_N   in Ada.Characters.Latin_1   A.3.3(13)

LC_N_Tilde   in Ada.Characters.Latin_1   A.3.3(26)

LC_O   in Ada.Characters.Latin_1   A.3.3(13)

LC_O_Acute   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Circumflex   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Diaeresis   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Grave   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Oblique_Stroke   in Ada.Characters.Latin_1   A.3.3(26)

LC_O_Tilde   in Ada.Characters.Latin_1   A.3.3(26)

LC_P   in Ada.Characters.Latin_1   A.3.3(14)

LC_Q   in Ada.Characters.Latin_1   A.3.3(14)

LC_R   in Ada.Characters.Latin_1   A.3.3(14)

LC_S   in Ada.Characters.Latin_1   A.3.3(14)

LC_T   in Ada.Characters.Latin_1   A.3.3(14)

LC_U   in Ada.Characters.Latin_1   A.3.3(14)

LC_U_Acute   in Ada.Characters.Latin_1   A.3.3(26)

LC_U_Circumflex   in Ada.Characters.Latin_1   A.3.3(26)

LC_U_Diaeresis   in Ada.Characters.Latin_1   A.3.3(26)

LC_U_Grave   in Ada.Characters.Latin_1   A.3.3(26)

LC_V   in Ada.Characters.Latin_1   A.3.3(14)

LC_W   in Ada.Characters.Latin_1   A.3.3(14)

LC_X   in Ada.Characters.Latin_1   A.3.3(14)

LC_Y   in Ada.Characters.Latin_1   A.3.3(14)

LC_Y_Acute   in Ada.Characters.Latin_1   A.3.3(26)

LC_Y_Diaeresis   in Ada.Characters.Latin_1   A.3.3(26)

LC_Z   in Ada.Characters.Latin_1   A.3.3(14)

Leading_Nonseparate   in Interfaces.COBOL   B.4(23)

Leading_Part attribute   A.5.3(55)

Leading_Separate   in Interfaces.COBOL   B.4(23)

leaving   7.6.1(3)

left   7.6.1(3)

left curly bracket   2.1(15)

left parenthesis   2.1(15)

left square bracket   2.1(15)

Left_Angle_Quotation   in Ada.Characters.Latin_1   A.3.3(21)

Left_Curly_Bracket   in Ada.Characters.Latin_1   A.3.3(14)

Left_Parenthesis   in Ada.Characters.Latin_1   A.3.3(8)

Left_Square_Bracket   in Ada.Characters.Latin_1   A.3.3(12)

legal   construct   1.1.1(44)   partition   1.1.1(46)

legality determinable via semantic dependences   10(4.c)

legality rules   1.1.1(44)

length   of a dimension of an array   3.6(13)   of a one-dimensional array   3.6(13)   in Ada.Strings.Bounded   A.4.4(9)   in Ada.Strings.Unbounded   A.4.5(6)   in Ada.Text_IO.Editing   F.3.3(11)   in Interfaces.COBOL   B.4(34), B.4(39), B.4(44)

Length attribute   3.6.2(9)

Length(N) attribute   3.6.2(10)

Length_Check   11.5(18)   [partial]   4.5.1(8), 4.6(37), 4.6(52)

Length_Error   in Ada.Strings   A.4.1(5)

Length_Range subtype of Natural   in Ada.Strings.Bounded   A.4.4(8)

less than operator   4.5(1), 4.5.2(1)

less than or equal operator   4.5(1), 4.5.2(1)

less-than sign   2.1(15)

Less_Than_Sign   in Ada.Characters.Latin_1   A.3.3(10)

letter   a category of Character   A.3.2(24)

letter_or_digit   2.3(3)   used   2.3(2), P.1

Letter_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

level   accessibility   3.10.2(3)   library   3.10.2(22)

lexical element   2.2(1)

lexicographic order   4.5.2(26)

LF   in Ada.Characters.Latin_1   A.3.3(5)

library   10.1.4(9)   [partial]   10.1.1(9)   informal introduction   10(3)   See also library level, library unit, library_item

library level   3.10.2(22)

Library unit   10.1(3), 10.1.1(9)   informal introduction   10(3)   See also language-defined library units

library unit pragma   10.1.5(7)   All_Calls_Remote   E.2.3(6)   categorization pragmas   E.2(2)   Elaborate_Body   10.2.1(26)   Preelaborate   10.2.1(4)   Pure   10.2.1(17)

library_item   10.1.1(4)   informal introduction   10(3)   used   10.1.1(3), P.1

library_unit_body   10.1.1(7)   used   10.1.1(4), P.1

library_unit_declaration   10.1.1(5)   used   10.1.1(4), P.1

library_unit_renaming_declaration   10.1.1(6)   used   10.1.1(4), P.1

lifetime   3.10.2(3)

limited type   7.5(4)   becoming nonlimited   7.3.1(5), 7.5(17)

Limited_Controlled   in Ada.Finalization   7.6(8)

line   2.2(2)   in Ada.Text_IO   A.10.1(38)

line terminator   A.10(7)

Line_Length   in Ada.Text_IO   A.10.1(25)

link name   B.1(35)

link-time error   See post-compilation error   1.1.1(106)   See post-compilation error   1.1.1(46)

Linker_Options pragma   B.1(8), L(19)

linking   See partition building   10.2(2)

List pragma   2.8(21), L(20)

literal   4.2(1)   based   2.4.2(1)   decimal   2.4.1(1)   numeric   2.4(1)   See also aggregate   4.3(1)

little endian   13.5.3(2)

load time   C.4(3)

local to   8.1(14)

local_name   13.1(5)   used   13.2(3), 13.3(2), 13.4(2), 13.5.1(2), 13.5.1(3), 13.11.3(3), B.1(5), B.1(6), B.1(7), C.5(3), C.6(3), C.6(4), C.6(5), C.6(6), E.4.1(3), L(3), L(4), L(5), L(7), L(8), L(9), L(13), L(14), L(25), L(43), L(44), P.1

localization   3.5.2(4), 3.5.2(5)

locking policy   D.3(6)

Locking_Policy pragma   D.3(3), L(21)

Log   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)

Logical   in Interfaces.Fortran   B.5(7)

logical operator   4.5.1(2)   See also not operator   4.5.6(3)

logical_operator   4.5(2)

long   in Interfaces.C   B.3(7)

Long_Binary   in Interfaces.COBOL   B.4(10)

long_double   in Interfaces.C   B.3(17)

Long_Float   3.5.7(15), 3.5.7(16), 3.5.7(17)

Long_Floating   in Interfaces.COBOL   B.4(9)

Long_Integer   3.5.4(23), 3.5.4(26), 3.5.4(29)

Look_Ahead   in Ada.Text_IO   A.10.1(43)

loop parameter   5.5(6)

loop_parameter_specification   5.5(4)   used   4.3.3(49), 5.5(3), P.1

loop_statement   5.5(2)   used   5.1(6), P.1

low line   2.1(15)

low-level programming   C(1)

Low_Line   in Ada.Characters.Latin_1   A.3.3(12)

Low_Order_First   13.5.3(2)   in Interfaces.COBOL   B.4(25)   in System   13.7(15)

lower bound   of a range   3.5(4)

lower-case letter   a category of Character   A.3.2(25)

lower_case_identifier_letter   2.1(9)

Lower_Case_Map   in Ada.Strings.Maps.Constants   A.4.6(5)

Lower_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

LR(1)   1.1.1(94.a)

M

Machine attribute   A.5.3(61)

machine code insertion   13.8(1), C.1(2)

machine numbers   of a floating point type   3.5.7(8)

Machine_Code   child of System   13.8(7)

Machine_Emax attribute   A.5.3(8)

Machine_Emin attribute   A.5.3(7)

Machine_Mantissa attribute   A.5.3(6)

Machine_Overflows attribute   A.5.3(12), A.5.4(4)

Machine_Radix attribute   A.5.3(2), A.5.4(2)

Machine_Radix clause   13.3(7), F.1(1)

Machine_Rounds attribute   A.5.3(11), A.5.4(3)

macro   See generic unit   12(1)

Macron   in Ada.Characters.Latin_1   A.3.3(21)

main subprogram   for a partition   10.2(8)

malloc   See allocator   4.8(1)

Maps   child of Ada.Strings   A.4.2(3)

marshalling   E.4(9)

Masculine_Ordinal_Indicator   in Ada.Characters.Latin_1   A.3.3(22)

master   7.6.1(3)

match   a character to a pattern character   A.4.2(54)   a character to a pattern character, with respect to a character mapping function   A.4.2(64)   a string to a pattern string   A.4.2(54)

matching components   4.5.2(16)

Max attribute   3.5(19)

Max_Base_Digits   3.5.7(6)   in System   13.7(8)

Max_Binary_Modulus   3.5.4(7)   in System   13.7(7)

Max_Decimal_Digits   in Ada.Decimal   F.2(5)

Max_Delta   in Ada.Decimal   F.2(4)

Max_Digits   3.5.7(6)   in System   13.7(8)

Max_Digits_Binary   in Interfaces.COBOL   B.4(11)

Max_Digits_Long_Binary   in Interfaces.COBOL   B.4(11)

Max_Image_Width   in Ada.Numerics.Discrete_Random   A.5.2(25)   in Ada.Numerics.Float_Random   A.5.2(13)

Max_Int   3.5.4(14)   in System   13.7(6)

Max_Length   in Ada.Strings.Bounded   A.4.4(5)

Max_Mantissa   in System   13.7(9)

Max_Nonbinary_Modulus   3.5.4(7)   in System   13.7(7)

Max_Picture_Length   in Ada.Text_IO.Editing   F.3.3(8)

Max_Scale   in Ada.Decimal   F.2(3)

Max_Size_In_Storage_Elements attribute   13.11.1(3)

maximum box error   for a component of the result of evaluating a complex function   G.2.6(3)

maximum line length   A.10(11)

maximum page length   A.10(11)

maximum relative error   for a component of the result of evaluating a complex function   G.2.6(3)   for the evaluation of an elementary function   G.2.4(2)

Membership   in Ada.Strings   A.4.1(6)

membership test   4.5.2(2)

Memory_Size   in System   13.7(13)

mentioned   in a with_clause   10.1.2(6)

message   See dispatching call   3.9.2(1)

method   See dispatching subprogram   3.9.2(1)

methodological restriction   10.1.3(14.a)

metrics   1.1.1(52)

Micro_Sign   in Ada.Characters.Latin_1   A.3.3(22)

Microseconds   in Ada.Real_Time   D.8(14)

Middle_Dot   in Ada.Characters.Latin_1   A.3.3(22)

Milliseconds   in Ada.Real_Time   D.8(14)

Min attribute   3.5(16)

Min_Delta   in Ada.Decimal   F.2(4)

Min_Int   3.5.4(14)   in System   13.7(6)

Min_Scale   in Ada.Decimal   F.2(3)

minus   2.1(15)

minus operator   4.5(1), 4.5.3(1), 4.5.4(1)

Minus_Sign   in Ada.Characters.Latin_1   A.3.3(8)

mixed-language programs   B(1), C.1(4)

mod operator   4.5(1), 4.5.5(1)

mod_clause   J.8(1)   used   13.5.1(2), P.1

mode   6.1(16)   used   6.1(15), 12.4(2), P.1   in Ada.Direct_IO   A.8.4(9)   in Ada.Sequential_IO   A.8.1(9)   in Ada.Streams.Stream_IO   A.12.1(11)   in Ada.Text_IO   A.10.1(12)

mode conformance   6.3.1(16)   required   8.5.4(4), 12.5.4(5), 12.6(7), 12.6(8), 13.3(6)

mode of operation   nonstandard   1.1.1(113)   standard   1.1.1(113)

Mode_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

Model attribute   A.5.3(69), G.2.2(7)

model interval   G.2.1(4)   associated with a value   G.2.1(4)

model number   G.2.1(3)

model-oriented attributes   of a floating point subtype   A.5.3(64)

Model_Emin attribute   A.5.3(66), G.2.2(4)

Model_Epsilon attribute   A.5.3(67)

Model_Mantissa attribute   A.5.3(65), G.2.2(3)

Model_Small attribute   A.5.3(68)

modular type   3.5.4(1)

Modular_IO   in Ada.Text_IO   A.10.1(57)

modular_type_definition   3.5.4(4)   used   3.5.4(2), P.1

module   See package   7(1)

modulus   of a modular type   3.5.4(7)   in Ada.Numerics.Generic_Complex_Types   G.1.1(9)

Modulus attribute   3.5.4(18)

Month   in Ada.Calendar   9.6(13)

Month_Number subtype of Integer   in Ada.Calendar   9.6(11)

Move   in Ada.Strings.Fixed   A.4.3(7)

multi-dimensional array   3.6(12)

Multiplication_Sign   in Ada.Characters.Latin_1   A.3.3(24)

multiply   2.1(15)

multiply operator   4.5(1), 4.5.5(1)

multiplying operator   4.5.5(1)

multiplying_operator   4.5(6)   used   4.4(5), P.1

mutable   3.7(29.b)

MW   in Ada.Characters.Latin_1   A.3.3(18)

N

n-dimensional array_aggregate   4.3.3(7)

NAK   in Ada.Characters.Latin_1   A.3.3(6)

name   4.1(2)   [partial]   3.1(1)   of (a view of) an entity   3.1(10)   of a pragma   2.8(9)   of an external file   A.7(1)   used   2.8(3), 3.2.2(4), 4.1(4), 4.1(5), 4.1(6), 4.4(7), 4.6(2), 5.2(2), 5.7(2), 5.8(2), 6.3.2(3), 6.4(2), 6.4(3), 6.4(6), 8.4(3), 8.5.1(2), 8.5.2(2), 8.5.3(2), 8.5.4(2), 8.5.5(2), 9.5.3(2), 9.5.4(2), 9.8(2), 10.1.1(8), 10.1.2(4), 10.2.1(3), 10.2.1(16), 10.2.1(22), 10.2.1(23), 10.2.1(24), 11.2(5), 11.3(2), 11.5(6), 12.3(2), 12.3(5), 12.6(4), 12.7(2), 13.1(5), 13.3(2), C.3.1(2), C.3.1(4), E.2.1(3), E.2.2(3), E.2.3(3), E.2.3(5), H.3.2(3), L(2), L(6), L(10), L(11), L(12), L(15), L(16), L(17), L(28), L(31), L(34), L(35), L(38), L(40), P.1   in Ada.Direct_IO   A.8.4(9)   in Ada.Sequential_IO   A.8.1(9)   in Ada.Streams.Stream_IO   A.12.1(11)   in Ada.Text_IO   A.10.1(12)   in System   13.7(4)

name resolution rules   1.1.1(43)

Name_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

named association   6.4(7), 12.3(6)

named component association   4.3.1(6)

named discriminant association   3.7.1(4)

named entry index   9.5.2(21)

named number   3.3(26)

named type   3.2.1(7)

named_array_aggregate   4.3.3(5)   used   4.3.3(2), P.1

Names   child of Ada.Interrupts   C.3.2(12)

names of special_characters   2.1(15)

Nanoseconds   in Ada.Real_Time   D.8(14)

Native_Binary   in Interfaces.COBOL   B.4(25)

Natural   3.5.4(12)

Natural subtype of Integer   in Standard   A.1(13)

NBH   in Ada.Characters.Latin_1   A.3.3(17)

NBSP   in Ada.Characters.Latin_1   A.3.3(21)

needed   of a compilation unit by another   10.2(2)   remote call interface   E.2.3(18)   shared passive library unit   E.2.1(11)

needed component   extension_aggregate record_component_association_list   4.3.2(6)   record_aggregate record_component_association_list   4.3.1(9)

NEL   in Ada.Characters.Latin_1   A.3.3(17)

new   See allocator   4.8(1)

New_Char_Array   in Interfaces.C.Strings   B.3.1(9)

New_Line   in Ada.Text_IO   A.10.1(28)

New_Page   in Ada.Text_IO   A.10.1(31)

New_String   in Interfaces.C.Strings   B.3.1(10)

No_Break_Space   in Ada.Characters.Latin_1   A.3.3(21)

nominal subtype   3.3(24), 3.3.1(8)   associated with a dereference   4.1(9)   associated with a type_conversion   4.6(27)   associated with an indexed_component   4.1.1(5)   of a component   3.6(20)   of a formal parameter   6.1(23)   of a generic formal object   12.4(9)   of a name   4.1(9.b)   of a record component   3.8(14)   of the result of a function_call   6.4(12)

nondispatching call   on a dispatching operation   3.9.2(1)

nonexistent   13.11.2(10), 13.11.2(16)

nongraphic character   3.5(33)

nonlimited type   7.5(8)   becoming nonlimited   7.3.1(5), 7.5(17)

nonnormative   See informative   1.1.1(36)

nonstandard integer type   3.5.4(27)

nonstandard mode   1.1.1(113)

nonstandard real type   3.5.6(8)

normal completion   7.6.1(2)

normal library unit   E.2(4)

normal state of an object   11.6(6), 13.9.1(4)   [partial]   9.8(21), A.13(17)

normal termination   of a partition   10.2(26.c)

Normalize_Scalars pragma   H.1(3), L(23)

normalized exponent   A.5.3(14)

normalized number   A.5.3(10)

normative   1.1.1(32)

not equal operator   4.5(1), 4.5.2(1)

not in (membership test)   4.5(1), 4.5.2(2)

not operator   4.5(1), 4.5.6(3)

Not_Sign   in Ada.Characters.Latin_1   A.3.3(21)

notes   1.1.1(55)

notwithstanding   10.1.6(2), 10.2(19.c), B.1(22), B.1(38), C.3.1(19), E.2.1(8), E.2.1(11), E.2.3(18), J.3(6)

NUL   in Ada.Characters.Latin_1   A.3.3(5)   in Interfaces.C   B.3(20)

null access value   4.2(9)

null array   3.6.1(7)

null constraint   3.2(9)

null pointer   See null access value   4.2(9)

null range   3.5(4)

null record   3.8(15)

null slice   4.1.2(7)

null string literal   2.6(6)

null value   of an access type   3.10(14)

Null_Address   in System   13.7(12)

Null_Bounded_String   in Ada.Strings.Bounded   A.4.4(7)

Null_Id   in Ada.Exceptions   11.4.1(2)

Null_Occurrence   in Ada.Exceptions   11.4.1(3)

Null_Ptr   in Interfaces.C.Strings   B.3.1(7)

Null_Set   in Ada.Strings.Maps   A.4.2(5)   in Ada.Strings.Wide_Maps   A.4.7(5)

null_statement   5.1(7)   used   5.1(5), P.1

Null_Task_Id   in Ada.Task_Identification   C.7.1(2)

Null_Unbounded_String   in Ada.Strings.Unbounded   A.4.5(5)

number sign   2.1(15)

Number_Base subtype of Integer   in Ada.Text_IO   A.10.1(6)

number_declaration   3.3.2(2)   used   3.1(3), P.1

Number_Sign   in Ada.Characters.Latin_1   A.3.3(8)

numeral   2.4.1(3)   used   2.4.1(2), 2.4.1(4), 2.4.2(3), P.1

Numeric   in Interfaces.COBOL   B.4(20)

numeric type   3.5(1)

numeric_literal   2.4(2)   used   4.4(7), P.1

numerics   G(1)   child of Ada   A.5(3)

O

object   3.3(2)   [partial]   3.2(1)   required   5.2(5), 6.4.1(5), 8.5.1(4), 12.4(7)

object-oriented programming (OOP)   See dispatching operations of tagged types   3.9.2(1)   See tagged types and type extensions   3.9(1)

object_declaration   3.3.1(2)   used   3.1(3), P.1

object_renaming_declaration   8.5.1(2)   used   8.5(3), P.1

obsolescent feature   J(1)

occur immediately within   8.1(13)

occurrence   of an interrupt   C.3(2)

occurrence (of an exception)   11(1.c)

octal   literal   2.4.2(1)

octal literal   2.4.2(1)

one's complement   modular types   3.5.4(28)

one-dimensional array   3.6(12)

one-pass context_clauses   10.1.2(1.a)

only as a completion   entry_body   9.5.2(16)

OOP (object-oriented programming)   See dispatching operations of tagged types   3.9.2(1)   See tagged types and type extensions   3.9(1)

opaque type   See private types and private extensions   7.3(1)

Open   in Ada.Direct_IO   A.8.4(7)   in Ada.Sequential_IO   A.8.1(7)   in Ada.Streams.Stream_IO   A.12.1(9)   in Ada.Text_IO   A.10.1(10)

open alternative   9.7.1(14)

open entry   9.5.3(5)   of a protected object   9.5.3(7)   of a task   9.5.3(6)

operand   of a qualified_expression   4.7(3)   of a type_conversion   4.6(3)

operand interval   G.2.1(6)

operand type   of a type_conversion   4.6(3)

operates on a type   3.2.3(1)

operation   3.2(12.a)

operator   6.6(1)   &   4.5(1), 4.5.3(3)   *   4.5(1), 4.5.5(1)   **   4.5(1), 4.5.6(7)   +   4.5(1), 4.5.3(1), 4.5.4(1)   -   4.5(1), 4.5.3(1), 4.5.4(1)   /   4.5(1), 4.5.5(1)   /=   4.5(1), 4.5.2(1)   <   4.5(1), 4.5.2(1)   <=   4.5(1), 4.5.2(1)   =   4.5(1), 4.5.2(1)   >   4.5(1), 4.5.2(1)   >=   4.5(1), 4.5.2(1)   abs   4.5(1), 4.5.6(1)   ampersand   4.5(1), 4.5.3(3)   and   4.5(1), 4.5.1(2)   binary   4.5(9)   binary adding   4.5.3(1)   concatenation   4.5(1), 4.5.3(3)   divide   4.5(1), 4.5.5(1)   equal   4.5(1), 4.5.2(1)   equality   4.5.2(1)   exponentiation   4.5(1), 4.5.6(7)   greater than   4.5(1), 4.5.2(1)   greater than or equal   4.5(1), 4.5.2(1)   highest precedence   4.5.6(1)   less than   4.5(1), 4.5.2(1)   less than or equal   4.5(1), 4.5.2(1)   logical   4.5.1(2)   minus   4.5(1), 4.5.3(1), 4.5.4(1)   mod   4.5(1), 4.5.5(1)   multiply   4.5(1), 4.5.5(1)   multiplying   4.5.5(1)   not   4.5(1), 4.5.6(3)   not equal   4.5(1), 4.5.2(1)   or   4.5(1), 4.5.1(2)   ordering   4.5.2(1)   plus   4.5(1), 4.5.3(1), 4.5.4(1)   predefined   4.5(9)   relational   4.5.2(1)   rem   4.5(1), 4.5.5(1)   times   4.5(1), 4.5.5(1)   unary   4.5(9)   unary adding   4.5.4(1)   user-defined   6.6(1)   xor   4.5(1), 4.5.1(2)

operator precedence   4.5(1)

operator_symbol   6.1(9)   used   4.1(3), 4.1.3(3), 6.1(5), 6.1(11), P.1

optimization   11.5(32), 11.6(1)

Optimize pragma   2.8(23), L(24)

or else (short-circuit control form)   4.5(1), 4.5.1(1)

or operator   4.5(1), 4.5.1(2)

ordering operator   4.5.2(1)

ordinary fixed point type   3.5.9(1), 3.5.9(8)

ordinary_fixed_point_definition   3.5.9(3)   used   3.5.9(2), P.1

OSC   in Ada.Characters.Latin_1   A.3.3(19)

other_control_function   2.1(14)   used   2.1(2), P.1

others choice   4.3.3(7.b)

output   A.6(1)

Output attribute   13.13.2(20), 13.13.2(30)

Output clause   13.3(7), 13.13.2(37)

overall interpretation   of a complete context   8.6(10)

Overflow_Check   11.5(19)   [partial]   3.5.4(21), 4.4(11), G.2.1(11), G.2.2(7), G.2.3(25), G.2.4(2), G.2.6(3)

overload resolution   8.6(1)

overloadable   8.3(7)

overloaded   8.3(6)   enumeration literal   3.5.1(9)

overloading rules   1.1.1(43), 8.6(2)

override   8.3(10), 12.3(17)   a primitive subprogram   3.2.3(8)

Overwrite   in Ada.Strings.Bounded   A.4.4(62), A.4.4(63)   in Ada.Strings.Fixed   A.4.3(27), A.4.3(28)   in Ada.Strings.Unbounded   A.4.5(57), A.4.5(58)

P

Pack pragma   13.2(3), L(25)

Package   7(1)

package instance   12.3(13)

package-private extension   7.3(14.a)

package-private type   7.3(14.a)

package_body   7.2(2)   used   3.11(6), 10.1.1(7), P.1

package_body_stub   10.1.3(4)   used   10.1.3(2), P.1

package_declaration   7.1(2)   used   3.1(3), 10.1.1(5), P.1

package_renaming_declaration   8.5.3(2)   used   8.5(3), 10.1.1(6), P.1

package_specification   7.1(3)   used   7.1(2), 12.1(4), P.1

packed   13.2(5)

Packed_Decimal   in Interfaces.COBOL   B.4(12)

Packed_Format   in Interfaces.COBOL   B.4(26)

Packed_Signed   in Interfaces.COBOL   B.4(27)

Packed_Unsigned   in Interfaces.COBOL   B.4(27)

packing   aspect of representation   13.2(5)

padding bits   13.1(9)

Page   in Ada.Text_IO   A.10.1(39)

Page pragma   2.8(22), L(26)

page terminator   A.10(7)

Page_Length   in Ada.Text_IO   A.10.1(26)

Paragraph_Sign   in Ada.Characters.Latin_1   A.3.3(22)

parallel processing   See task   9(1)

parameter   See formal parameter   6.1(17)   See generic formal parameter   12(1)   See also discriminant   3.7(1)   See also loop parameter   5.5(6)

parameter assigning back   6.4.1(17)

parameter copy back   6.4.1(17)

parameter mode   6.1(18)

parameter passing   6.4.1(1)

parameter_and_result_profile   6.1(13)   used   3.10(5), 6.1(4), P.1

parameter_association   6.4(5)   used   6.4(4), P.1

parameter_profile   6.1(12)   used   3.10(5), 6.1(4), 9.5.2(2), 9.5.2(3), 9.5.2(6), P.1

parameter_specification   6.1(15)   used   6.1(14), P.1

Parameterless_Handler   in Ada.Interrupts   C.3.2(2)

Params_Stream_Type   in System.RPC   E.5(6)

parent body   of a subunit   10.1.3(8)

parent declaration   of a library unit   10.1.1(10)   of a library_item   10.1.1(10)

parent subtype   3.4(4)

parent type   3.4(4)

parent unit   of a library unit   10.1.1(10)

parent_unit_name   10.1.1(8)   used   6.1(5), 6.1(7), 7.1(3), 7.2(2), 10.1.3(7), P.1

part   of an object or value   3.2(8)

partial view   of a type   7.3(4)

partition   10.2(2)

partition building   10.2(2)

partition communication subsystem (PCS)   E.5(1)

Partition_Check   [partial]   E.4(19)

Partition_Id   in System.RPC   E.5(4)

Partition_Id attribute   E.1(9)

pass by copy   6.2(2)

pass by reference   6.2(2)

passive partition   E.1(2)

Pattern_Error   in Ada.Strings   A.4.1(5)

PC-map approach to finalization   7.6.1(24.s)

PCS (partition communication subsystem)   E.5(1)

pending interrupt occurrence   C.3(2)

per-object constraint   3.8(18)

per-object expression   3.8(18)

Percent_Sign   in Ada.Characters.Latin_1   A.3.3(8)

perfect result set   G.2.3(5)

periodic task   example   9.6(39)   See delay_until_statement   9.6(39)

Pi   in Ada.Numerics   A.5(3)

Pic_String   in Ada.Text_IO.Editing   F.3.3(7)

Picture   in Ada.Text_IO.Editing   F.3.3(4)

picture String   for edited output   F.3.1(1)

Picture_Error   in Ada.Text_IO.Editing   F.3.3(9)

Pilcrow_Sign   in Ada.Characters.Latin_1   A.3.3(22)

plain_char   in Interfaces.C   B.3(11)

PLD   in Ada.Characters.Latin_1   A.3.3(17)

PLU   in Ada.Characters.Latin_1   A.3.3(17)

plus operator   4.5(1), 4.5.3(1), 4.5.4(1)

plus sign   2.1(15)

Plus_Minus_Sign   in Ada.Characters.Latin_1   A.3.3(22)

Plus_Sign   in Ada.Characters.Latin_1   A.3.3(8)

PM   in Ada.Characters.Latin_1   A.3.3(19)

point   2.1(15)

Pointer   in Interfaces.C.Pointers   B.3.2(5)   See access value   3.10(1)   See type System.Address   13.7(34)

pointer type   See access type   3.10(1)

Pointer_Error   in Interfaces.C.Pointers   B.3.2(8)

Pointers   child of Interfaces.C   B.3.2(4)

polymorphism   3.9(1), 3.9.2(1)

pool element   3.10(7), 13.11(12)

pool type   13.11(12)

pool-specific access type   3.10(7), 3.10(8)

Pos attribute   3.5.5(2)

position   13.5.1(4)   used   13.5.1(3), P.1

Position attribute   13.5.2(2)

position number   3.5(1)   of an enumeration value   3.5.1(7)   of an integer value   3.5.4(15)

positional association   6.4(7), 12.3(6)

positional component association   4.3.1(6)

positional discriminant association   3.7.1(4)

positional_array_aggregate   4.3.3(3)   used   4.3.3(2), P.1

Positive   3.5.4(12)

Positive subtype of Integer   in Standard   A.1(13)

Positive_Count subtype of Count   in Ada.Direct_IO   A.8.4(4)   in Ada.Streams.Stream_IO   A.12.1(7)   in Ada.Text_IO   A.10.1(5)

possible interpretation   8.6(14)   for direct_names   8.3(25)   for selector_names   8.3(25)

post-compilation error   1.1.1(46)

post-compilation rules   1.1.1(46)

potentially blocking operation   9.5.1(8)   Abort_Task   C.7.1(16)   delay_statement   9.6(34), D.9(5)   remote subprogram call   E.4(17)   RPC operations   E.5(23)   Suspend_Until_True   D.10(10)

potentially use-visible   8.4(8)

Pound_Sign   in Ada.Characters.Latin_1   A.3.3(21)

Pragma   2.8(1), 2.8(2), L(1)

pragma argument   2.8(9)

pragma name   2.8(9)

pragma, categorization   E.2(2)   Remote_Call_Interface   E.2.3(2)   Remote_Types   E.2.2(2)   Shared_Passive   E.2.1(2)

pragma, configuration   10.1.5(8)   Discard_Names   C.5(4)   Locking_Policy   D.3(5)   Normalize_Scalars   H.1(4)   Queuing_Policy   D.4(5)   Restrictions   13.12(8)   Reviewable   H.3.1(4)   Suppress   11.5(8)   Task_Dispatching_Policy   D.2.2(5)

pragma, identifier specific to   2.8(10)

pragma, interfacing   Convention   B.1(4)   Export   B.1(4)   Import   B.1(4)   Linker_Options   B.1(4)

pragma, library unit   10.1.5(7)   All_Calls_Remote   E.2.3(6)   categorization pragmas   E.2(2)   Elaborate_Body   10.2.1(26)   Preelaborate   10.2.1(4)   Pure   10.2.1(17)

pragma, program unit   10.1.5(2)   Convention   B.1(29)   Export   B.1(29)   Import   B.1(29)   Inline   6.3.2(2)   library unit pragmas   10.1.5(7)

pragma, representation   13.1(2)   Asynchronous   E.4.1(8)   Atomic   C.6(14)   Atomic_Components   C.6(14)   Controlled   13.11.3(5)   Convention   B.1(28)   Discard_Names   C.5(6)   Export   B.1(28)   Import   B.1(28)   Pack   13.2(5)   Volatile   C.6(14)   Volatile_Components   C.6(14)

pragma_argument_association   2.8(3)   used   2.8(2), P.1

pragmas   All_Calls_Remote   E.2.3(5), L(2)   Asynchronous   E.4.1(3), L(3)   Atomic   C.6(3), L(4)   Atomic_Components   C.6(5), L(5)   Attach_Handler   C.3.1(4), L(6)   Controlled   13.11.3(3), L(7)   Convention   B.1(7), L(8)   Discard_Names   C.5(3), L(9)   Elaborate   10.2.1(22), L(10)   Elaborate_All   10.2.1(23), L(11)   Elaborate_Body   10.2.1(24), L(12)   Export   B.1(6), L(13)   Import   B.1(5), L(14)   Inline   6.3.2(3), L(15)   Inspection_Point   H.3.2(3), L(16)   Interrupt_Handler   C.3.1(2), L(17)   Interrupt_Priority   D.1(5), L(18)   Linker_Options   B.1(8), L(19)   List   2.8(21), L(20)   Locking_Policy   D.3(3), L(21)   Normalize_Scalars   H.1(3), L(23)   Optimize   2.8(23), L(24)   Pack   13.2(3), L(25)   Page   2.8(22), L(26)   Preelaborate   10.2.1(3), L(28)   Priority   D.1(3), L(29)   Pure   10.2.1(16), L(31)   Queuing_Policy   D.4(3), L(32)   Remote_Call_Interface   E.2.3(3), L(34)   Remote_Types   E.2.2(3), L(35)   Restrictions   13.12(3), L(36)   Reviewable   H.3.1(3), L(37)   Shared_Passive   E.2.1(3), L(38)   Storage_Size   13.3(65), L(39)   Suppress   11.5(6), L(40)   Task_Dispatching_Policy   D.2.2(2), L(41)   Volatile   C.6(4), L(43)   Volatile_Components   C.6(6), L(44)

precedence of operators   4.5(1)

Pred attribute   3.5(25)

predefined environment   A(1)

predefined exception   11.1(4)

predefined library unit   See language-defined library units

predefined operation   of a type   3.2.3(1)

predefined operations   of a discrete type   3.5.5(12)   of a fixed point type   3.5.10(17)   of a floating point type   3.5.8(3)   of a record type   3.8(24)   of an access type   3.10.2(34)   of an array type   3.6.2(15)

predefined operator   4.5(9)   [partial]   3.2.1(9)

predefined type   3.2.1(10)   See language-defined types

preelaborable   of an elaborable construct   10.2.1(6)

Preelaborate pragma   10.2.1(3), L(28)

preelaborated   10.2.1(12)   [partial]   10.2.1(12), E.2.1(9)   implementation requirements   C.4(2)

preempted task   D.2.1(7)

preemptible resource   D.2.1(7)

preference   for root numeric operators and ranges   8.6(29)

preference control   See requeue   9.5.4(1)

prefix   4.1(4)   used   4.1.1(2), 4.1.2(2), 4.1.3(2), 4.1.4(2), 4.1.4(4), 6.4(2), 6.4(3), P.1

prescribed result   for the evaluation of a complex arithmetic operation   G.1.1(42)   for the evaluation of a complex elementary function   G.1.2(35)   for the evaluation of an elementary function   A.5.1(37)

primary   4.4(7)   used   4.4(6), P.1

primitive function   A.5.3(17)

primitive operation   [partial]   3.2(1)

primitive operations   of a type   3.2.3(1)

primitive operator   of a type   3.2.3(9)

primitive subprograms   of a type   3.2.3(3)

priority   D.1(15)

priority inheritance   D.1(15)

priority inversion   D.2.2(15)

priority of an entry call   D.4(9)

Priority pragma   D.1(3), L(29)

Priority subtype of Any_Priority   in System   13.7(16)

private declaration of a library unit   10.1.1(12)

private descendant   of a library unit   10.1.1(12)

private extension   3.2(6), 3.9(2), 3.9.1(1)   [partial]   7.3(14), 12.5.1(5)

private library unit   10.1.1(12)

private operations   7.3.1(1)

private part   8.2(5)   of a package   7.1(6), 12.3(12.b)   of a protected unit   9.4(11)   of a task unit   9.1(9)

private type   3.2(6)   [partial]   7.3(14)

private types and private extensions   7.3(1)

private_extension_declaration   7.3(3)   used   3.2.1(2), P.1

private_type_declaration   7.3(2)   used   3.2.1(2), P.1

procedure   6(1)

procedure instance   12.3(13)

procedure_call_statement   6.4(2)   used   5.1(5), P.1

processing node   E(2)

profile   6.1(22)   associated with a dereference   4.1(10)   fully conformant   6.3.1(18)   mode conformant   6.3.1(16)   subtype conformant   6.3.1(17)   type conformant   6.3.1(15)

profile resolution rule   name with a given expected profile   8.6(26)

program   10.2(1)

program execution   10.2(1)

program library   See library   10(3)   See library   10.1.4(9)

Program unit   10.1(1)

program unit pragma   10.1.5(2)   Convention   B.1(29)   Export   B.1(29)   Import   B.1(29)   Inline   6.3.2(2)   library unit pragmas   10.1.5(7)

program-counter-map approach to finalization   7.6.1(24.s)

Program_Error   raised by barrier failure   9.5.3(7)   raised by closed alternatives   9.7.1(21)   raised by detection of a bounded error   1.1.1(110), 6.2(12), 9.5.1(17), 9.8(20), 10.2(27), 13.9.1(9), 13.11.2(13), C.7.1(17), D.5(11)   raised by failed finalization   7.6.1(15), 7.6.1(16), 7.6.1(17), 7.6.1(18)   raised by failure of runtime check   3.10.2(29), 3.11(14), 4.6(57), 6.5(20), 7.6.1(20.b), 11.1(4), 11.5(11.a), 11.5(22), C.3.1(10), C.3.1(11), C.3.2(20), D.3(13), E.4(18), J.7.1(7)   raised by finalization of a protected object   9.4(20)   raised for missing return statement   6.4(11)   in Standard   A.1(46)

propagate   11.4(1)   an exception by a construct   11.4(6.a)   an exception by an execution   11.4(6.a)   an exception occurrence by an execution, to a dynamically enclosing execution   11.4(6)

proper_body   3.11(6)   used   3.11(5), 10.1.3(7), P.1

protected action   9.5.1(4)   complete   9.5.1(6)   start   9.5.1(5)

protected calling convention   6.3.1(12)

protected declaration   9.4(1)

protected entry   9.4(1)

protected function   9.5.1(1)

protected object   9(4), 9.4(1)

protected operation   9.4(1)

protected procedure   9.5.1(1)

protected subprogram   9.4(1), 9.5.1(1)

protected unit   9.4(1)

protected_body   9.4(7)   used   3.11(6), P.1

protected_body_stub   10.1.3(6)   used   10.1.3(2), P.1

protected_definition   9.4(4)   used   9.4(2), 9.4(3), P.1

protected_element_declaration   9.4(6)   used   9.4(4), P.1

protected_operation_declaration   9.4(5)   used   9.4(4), 9.4(6), P.1

protected_operation_item   9.4(8)   used   9.4(7), P.1

protected_type_declaration   9.4(2)   used   3.2.1(3), P.1

ptrdiff_t   in Interfaces.C   B.3(12)

PU1   in Ada.Characters.Latin_1   A.3.3(18)

PU2   in Ada.Characters.Latin_1   A.3.3(18)

public declaration of a library unit   10.1.1(12)

public descendant   of a library unit   10.1.1(12)

public library unit   10.1.1(12)

pure   10.2.1(18)

Pure pragma   10.2.1(16), L(31)

Put   in Ada.Text_IO   A.10.1(42), A.10.1(48), A.10.1(55), A.10.1(60), A.10.1(66), A.10.1(67), A.10.1(71), A.10.1(72), A.10.1(76), A.10.1(77), A.10.1(82), A.10.1(83)   in Ada.Text_IO.Complex_IO   G.1.3(7), G.1.3(8)   in Ada.Text_IO.Editing   F.3.3(14), F.3.3(15), F.3.3(16)

Put_Line   in Ada.Text_IO   A.10.1(50)

Q

qualified_expression   4.7(2)   used   4.4(7), 4.8(2), 13.8(2), P.1

Question   in Ada.Characters.Latin_1   A.3.3(10)

queuing policy   D.4(1), D.4(6)

Queuing_Policy pragma   D.4(3), L(32)

Quotation   in Ada.Characters.Latin_1   A.3.3(8)

quotation mark   2.1(15)

quoted string   See string_literal   2.6(1)

R

raise   an exception   11(1)   an exception   11.3(4)   an exception occurrence   11.4(3)

Raise_Exception   in Ada.Exceptions   11.4.1(4)

raise_statement   11.3(2)   used   5.1(5), P.1

Random   in Ada.Numerics.Discrete_Random   A.5.2(20)   in Ada.Numerics.Float_Random   A.5.2(8)

random number   A.5.2(1)

range   3.5(3), 3.5(4)   of a scalar subtype   3.5(7)   used   3.5(2), 3.6(6), 3.6.1(3), 4.4(3), P.1

Range attribute   3.5(14), 3.6.2(7)

Range(N) attribute   3.6.2(8)

range_attribute_designator   4.1.4(5)   used   4.1.4(4), P.1

range_attribute_reference   4.1.4(4)   used   3.5(3), P.1

Range_Check   11.5(20)   [partial]   3.2.2(11), 3.5(24), 3.5(27), 3.5(46), 3.5(47), 3.5(54), 3.5(58), 3.5.5(7), 3.5.9(19), 4.2(11), 4.3.3(29), 4.5.1(8), 4.5.6(6), 4.5.6(13), 4.6(28), 4.6(38), 4.6(46), 4.6(51), 4.7(4), 13.13.2(36), A.5.3(26), A.5.3(29), A.5.3(51), A.5.3(54), A.5.3(60), A.5.3(63), K(115), K(123), K(14), K(185), K(221), K(242), K(44), K(50)

range_constraint   3.5(2)   used   3.2.2(6), 3.5.9(5), J.3(2), P.1

RCI   generic   E.2.3(7)   library unit   E.2.3(7)   package   E.2.3(7)

Re   in Ada.Numerics.Generic_Complex_Types   G.1.1(6)

re-raise statement   11.3(3)

read   the value of an object   3.3(15)   in Ada.Direct_IO   A.8.4(12)   in Ada.Sequential_IO   A.8.1(12)   in Ada.Storage_IO   A.9(6)   in Ada.Streams   13.13.1(5)   in Ada.Streams.Stream_IO   A.12.1(15), A.12.1(16)   in System.RPC   E.5(7)

Read attribute   13.13.2(7), 13.13.2(15)

Read clause   13.3(7), 13.13.2(37)

ready   a task state   9(11)

ready queue   D.2.1(5)

ready task   D.2.1(5)

Real   in Interfaces.Fortran   B.5(6)

real literal   2.4(1)

real literals   3.5.6(4)

real time   D.8(18)

real type   3.2(5), 3.5.6(1)

real-time systems   C(1), D(1)

real_range_specification   3.5.7(3)   used   3.5.7(2), 3.5.9(3), 3.5.9(4), P.1

Real_Time   child of Ada   D.8(3)

real_type_definition   3.5.6(2)   used   3.2.1(4), P.1

receiving stub   E.4(10)

reclamation of storage   13.11.2(1)

recommended level of support   13.1(22)   Address attribute   13.3(15)   Alignment attribute for objects   13.3(34)   Alignment attribute for subtypes   13.3(30)   bit ordering   13.5.3(7)   Component_Size attribute   13.3(73)   enumeration_representation_clause   13.4(9)   pragma Pack   13.2(7)   record_representation_clause   13.5.1(17)   required in Systems Programming Annex   C.2(2)   Size attribute   13.3(43), 13.3(55)   unchecked conversion   13.9(16)   with respect to nonstatic expressions   13.1(23)

record   3.8(1)

record extension   3.4(7), 3.9.1(1)

record layout   aspect of representation   13.5(1)

record type   3.8(1)

record_aggregate   4.3.1(2)   used   4.3(3), 13.8(14.b), P.1

record_component_association   4.3.1(4)   used   4.3.1(3), P.1

record_component_association_list   4.3.1(3)   used   4.3.1(2), 4.3.2(2), P.1

record_definition   3.8(3)   used   3.8(2), 3.9.1(2), P.1

record_extension_part   3.9.1(2)   used   3.4(3), P.1

record_representation_clause   13.5.1(2)   used   13.1(4), P.1

record_type_definition   3.8(2)   used   3.2.1(4), P.1

Reference   in Ada.Interrupts   C.3.2(10)   in Ada.Task_Attributes   C.7.2(5)

reference parameter passing   6.2(2)

references   1.2(1)

Registered_Trade_Mark_Sign   in Ada.Characters.Latin_1   A.3.3(21)

Reinitialize   in Ada.Task_Attributes   C.7.2(6)

relation   4.4(3)   used   4.4(2), P.1

relational operator   4.5.2(1)

relational_operator   4.5(3)   used   4.4(3), P.1

relaxed mode   G.2(1)

release   execution resource associated with protected object   9.5.1(6)

rem operator   4.5(1), 4.5.5(1)

Remainder attribute   A.5.3(46)

remote access   E.1(5)

remote access type   E.2.2(9)

remote access-to-class-wide type   E.2.2(9)

remote access-to-subprogram type   E.2.2(9)

remote call interface   E.2(4), E.2.3(7)

remote procedure call   asynchronous   E.4.1(9)

remote subprogram   E.2.3(7)

remote subprogram binding   E.4(1)

remote subprogram call   E.4(1)

remote types library unit   E.2(4), E.2.2(4)

Remote_Call_Interface pragma   E.2.3(3), L(34)

Remote_Types pragma   E.2.2(3), L(35)

renamed entity   8.5(4)

renamed view   8.5(4)

renaming-as-body   8.5.4(1)

renaming-as-declaration   8.5.4(1)

renaming_declaration   8.5(3)   used   3.1(3), P.1

rendezvous   9.5.2(25)

Replace_Element   in Ada.Strings.Bounded   A.4.4(27)   in Ada.Strings.Unbounded   A.4.5(21)

Replace_Slice   in Ada.Strings.Bounded   A.4.4(58), A.4.4(59)   in Ada.Strings.Fixed   A.4.3(23), A.4.3(24)   in Ada.Strings.Unbounded   A.4.5(53), A.4.5(54)

Replicate   in Ada.Strings.Bounded   A.4.4(78), A.4.4(79), A.4.4(80)

representation   change of   13.6(1)

representation aspect   13.1(10)

representation attribute   13.3(1)

representation item   13.1(2)

representation of an object   13.1(9)

representation pragma   13.1(2)   Asynchronous   E.4.1(8)   Atomic   C.6(14)   Atomic_Components   C.6(14)   Controlled   13.11.3(5)   Convention   B.1(28)   Discard_Names   C.5(6)   Export   B.1(28)   Import   B.1(28)   Pack   13.2(5)   Volatile   C.6(14)   Volatile_Components   C.6(14)

representation-oriented attributes   of a fixed point subtype   A.5.4(1)   of a floating point subtype   A.5.3(1)

representation_clause   13.1(4)   used   3.8(5), 3.11(4), 9.1(5), 9.4(5), 9.4(8), P.1

represented in canonical form   A.5.3(10)

requested decimal precision   of a floating point type   3.5.7(4)

requeue   9.5.4(1)

requeue-with-abort   9.5.4(13)

requeue_statement   9.5.4(2)   used   5.1(5), P.1

requires a completion   3.11.1(1), 3.11.1(6)   declaration of a partial view   7.3(4)   declaration to which a pragma Elaborate_Body applies   10.2.1(27)   deferred constant declaration   7.4(2)   generic_package_declaration   7.1(5)   generic_subprogram_declaration   6.1(20)   incomplete_type_declaration   3.10.1(3)   library_unit_declaration   10.2(19.c)   package_declaration   7.1(5)   protected entry_declaration   9.5.2(16)   protected_declaration   9.4(10)   subprogram_declaration   6.1(20)   task_declaration   9.1(8)

Reraise_Occurrence   in Ada.Exceptions   11.4.1(4)

reserved interrupt   C.3(2)

reserved word   2.9(2)

Reserved_128   in Ada.Characters.Latin_1   A.3.3(17)

Reserved_129   in Ada.Characters.Latin_1   A.3.3(17)

Reserved_132   in Ada.Characters.Latin_1   A.3.3(17)

Reserved_153   in Ada.Characters.Latin_1   A.3.3(19)

Reserved_Check   [partial]   C.3.1(10)

Reset   in Ada.Direct_IO   A.8.4(8)   in Ada.Numerics.Discrete_Random   A.5.2(21), A.5.2(24)   in Ada.Numerics.Float_Random   A.5.2(9), A.5.2(12)   in Ada.Sequential_IO   A.8.1(8)   in Ada.Streams.Stream_IO   A.12.1(10)   in Ada.Text_IO   A.10.1(11)

resolution rules   1.1.1(43)

resolve   overload resolution   8.6(14)

restriction   13.12(4)   used   13.12(3), L(36)

restrictions   Immediate_Reclamation   H.4(10)   Max_Asynchronous_Select_Nesting   D.7(18)   Max_Protected_Entries   D.7(14)   Max_Select_Alternatives   D.7(12)   Max_Storage_At_Blocking   D.7(17)   Max_Task_Entries   D.7(13)   Max_Tasks   D.7(19)   No_Abort_Statements   D.7(5)   No_Access_Subprograms   H.4(17)   No_Allocators   H.4(7)   No_Asynchronous_Control   D.7(10)   No_Delay   H.4(21)   No_Dispatch   H.4(19)   No_Dynamic_Priorities   D.7(9)   No_Exceptions   H.4(12)   No_Fixed_Point   H.4(15)   No_Floating_Point   H.4(14)   No_Implicit_Heap_Allocations   D.7(8)   No_IO   H.4(20)   No_Local_Allocators   H.4(8)   No_Nested_Finalization   D.7(4)   No_Protected_Types   H.4(5)   No_Recursion   H.4(22)   No_Reentrancy   H.4(23)   No_Task_Allocators   D.7(7)   No_Task_Hierarchy   D.7(3)   No_Terminate_Alternatives   D.7(6)   No_Unchecked_Access   H.4(18)   No_Unchecked_Conversion   H.4(16)   No_Unchecked_Deallocation   H.4(9)

Restrictions pragma   13.12(3), L(36)

result interval   for a component of the result of evaluating a complex function   G.2.6(3)   for the evaluation of a predefined arithmetic operation   G.2.1(8)   for the evaluation of an elementary function   G.2.4(2)

result subtype   of a function   6.5(3)

return expression   6.5(3)

return-by-reference type   6.5(11)

return_statement   6.5(2)   used   5.1(5), P.1

Reverse_Solidus   in Ada.Characters.Latin_1   A.3.3(12)

Reviewable pragma   H.3.1(3), L(37)

RI   in Ada.Characters.Latin_1   A.3.3(17)

right curly bracket   2.1(15)

right parenthesis   2.1(15)

right square bracket   2.1(15)

Right_Angle_Quotation   in Ada.Characters.Latin_1   A.3.3(22)

Right_Curly_Bracket   in Ada.Characters.Latin_1   A.3.3(14)

Right_Parenthesis   in Ada.Characters.Latin_1   A.3.3(8)

Right_Square_Bracket   in Ada.Characters.Latin_1   A.3.3(12)

Ring_Above   in Ada.Characters.Latin_1   A.3.3(22)

root library unit   10.1.1(10)

root type   of a class   3.4.1(2)

root_integer   3.5.4(14)   [partial]   3.4.1(8)

root_real   3.5.6(3)   [partial]   3.4.1(8)

Root_Storage_Pool   in System.Storage_Pools   13.11(7)

Root_Stream_Type   in Ada.Streams   13.13.1(3)

rooted at a type   3.4.1(2)

rotate   B.2(9)

Rotate_Left   B.2(6)

Rotate_Right   B.2(6)

Round attribute   3.5.10(12)

Rounding attribute   A.5.3(36)

RPC   child of System   E.5(3)

RPC-receiver   E.5(21)

RPC_Receiver   in System.RPC   E.5(11)

RS   in Ada.Characters.Latin_1   A.3.3(6)

run-time error   1.1.1(108), 1.1.1(47), 11.5(3), 11.6(1)

run-time polymorphism   3.9.2(1)

run-time semantics   1.1.1(47)

run-time type   See tag   3.9(4)

running a program   See program execution   10.2(1)

running task   D.2.1(6)

runtime check   See language-defined check   11.5(3)

S

safe range   of a floating point type   3.5.7(9)   of a floating point type   3.5.7(10)

safe separate compilation   10(4.b)

Safe_First attribute   A.5.3(72), G.2.2(5)

Safe_Last attribute   A.5.3(73), G.2.2(6)

safety-critical systems   H(1)

same value   for a limited type   6.2(10.f)

satisfies   a discriminant constraint   3.7.1(11)   a range constraint   3.5(4)   an index constraint   3.6.1(7)   for an access value   3.10(16)

Save   in Ada.Numerics.Discrete_Random   A.5.2(24)   in Ada.Numerics.Float_Random   A.5.2(12)

Save_Occurrence   in Ada.Exceptions   11.4.1(6)

scalar type   3.2(5), 3.5(1)

scalar_constraint   3.2.2(6)   used   3.2.2(5), P.1

scale   of a decimal fixed point subtype   3.5.10(11), K(217)

Scale attribute   3.5.10(11)

Scaling attribute   A.5.3(27)

SCHAR_MAX   in Interfaces.C   B.3(6)

SCHAR_MIN   in Interfaces.C   B.3(6)

SCI   in Ada.Characters.Latin_1   A.3.3(19)

scope   informal definition   3.1(10)   of (a view of) an entity   8.2(11)   of a declaration   8.2(10)   of a use_clause   8.4(6)   of a with_clause   10.1.2(5)

Seconds   in Ada.Calendar   9.6(13)

Seconds_Count   in Ada.Real_Time   D.8(15)

Section_Sign   in Ada.Characters.Latin_1   A.3.3(21)

secure systems   H(1)

select an entry call   from an entry queue   9.5.3(13), 9.5.3(16)   immediately   9.5.3(8)

select_alternative   9.7.1(4)   used   9.7.1(2), P.1

select_statement   9.7(2)   used   5.1(6), P.1

selected_component   4.1.3(2)   used   4.1(2), P.1

selection   of an entry caller   9.5.2(24)

selective_accept   9.7.1(2)   used   9.7(2), P.1

selector_name   4.1.3(3)   used   3.7.1(3), 4.1.3(2), 4.3.1(5), 6.4(5), 12.3(4), P.1

semantic dependence   of one compilation unit upon another   10.1.1(26)

semicolon   2.1(15)   in Ada.Characters.Latin_1   A.3.3(10)

separate compilation   10.1(1)   safe   10(4.b)

separator   2.2(3)

sequence of characters   of a string_literal   2.6(5)

sequence_of_statements   5.1(3)   used   5.3(2), 5.4(3), 5.5(2), 9.7.1(2), 9.7.1(5), 9.7.1(6), 9.7.2(3), 9.7.3(2), 9.7.4(3), 9.7.4(5), 11.2(2), 11.2(3), P.1

sequential   actions   9.10(11), C.6(17)

sequential access   A.8(2)

sequential file   A.8(1)

Sequential_IO   child of Ada   A.8.1(2)

service   an entry queue   9.5.3(13)

Set_Col   in Ada.Text_IO   A.10.1(35)

Set_Error   in Ada.Text_IO   A.10.1(15)

Set_Exit_Status   in Ada.Command_Line   A.15(9)

Set_False   in Ada.Synchronous_Task_Control   D.10(4)

Set_Im   in Ada.Numerics.Generic_Complex_Types   G.1.1(7)

Set_Index   in Ada.Direct_IO   A.8.4(14)   in Ada.Streams.Stream_IO   A.12.1(22)

Set_Input   in Ada.Text_IO   A.10.1(15)

Set_Line   in Ada.Text_IO   A.10.1(36)

Set_Line_Length   in Ada.Text_IO   A.10.1(23)

Set_Mode   in Ada.Streams.Stream_IO   A.12.1(24)

Set_Output   in Ada.Text_IO   A.10.1(15)

Set_Page_Length   in Ada.Text_IO   A.10.1(24)

Set_Priority   in Ada.Dynamic_Priorities   D.5(4)

Set_Re   in Ada.Numerics.Generic_Complex_Types   G.1.1(7)

Set_True   in Ada.Synchronous_Task_Control   D.10(4)

Set_Value   in Ada.Task_Attributes   C.7.2(6)

shared passive library unit   E.2(4), E.2.1(4)

shared variable   protection of   9.10(1)

Shared_Passive pragma   E.2.1(3), L(38)

shift   B.2(9)

Shift_Left   B.2(6)

Shift_Right   B.2(6)

Shift_Right_Arithmetic   B.2(6)

short   in Interfaces.C   B.3(7)

short-circuit control form   4.5.1(1)

Short_Float   3.5.7(16)

Short_Integer   3.5.4(26)

SI   in Ada.Characters.Latin_1   A.3.3(5)

signal   as defined between actions   9.10(2)   See interrupt   C.3(1)

signal (an exception)   See raise   11(1)

signal handling   example   9.7.4(10)

signed integer type   3.5.4(1)

signed_char   in Interfaces.C   B.3(8)

signed_integer_type_definition   3.5.4(3)   used   3.5.4(2), P.1

Signed_Zeros attribute   A.5.3(13)

simple entry call   9.5.3(1)

simple_expression   4.4(4)   used   3.5(3), 3.5.4(3), 3.5.7(3), 4.4(3), 13.5.1(5), 13.5.1(6), P.1

simple_statement   5.1(5)   used   5.1(4), P.1

Sin   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

single   class expected type   8.6(27)

single entry   9.5.2(20)

Single_Precision_Complex_Types   in Interfaces.Fortran   B.5(8)

single_protected_declaration   9.4(3)   used   3.3.1(2), P.1

single_task_declaration   9.1(3)   used   3.3.1(2), P.1

Sinh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

size   of an object   13.1(9)   in Ada.Direct_IO   A.8.4(15)   in Ada.Streams.Stream_IO   A.12.1(23)

Size attribute   13.3(41), 13.3(46)

Size clause   13.3(7), 13.3(42), 13.3(49)

size_t   in Interfaces.C   B.3(13)

Skip_Line   in Ada.Text_IO   A.10.1(29)

Skip_Page   in Ada.Text_IO   A.10.1(32)

slice   4.1.2(2)   used   4.1(2), P.1   in Ada.Strings.Bounded   A.4.4(28)   in Ada.Strings.Unbounded   A.4.5(22)

small   of a fixed point type   3.5.9(8)

Small clause   3.5.10(2), 13.3(7)

SO   in Ada.Characters.Latin_1   A.3.3(5)

Soft_Hyphen   in Ada.Characters.Latin_1   A.3.3(21)

SOH   in Ada.Characters.Latin_1   A.3.3(5)

solidus   2.1(15)   in Ada.Characters.Latin_1   A.3.3(8)

SOS   in Ada.Characters.Latin_1   A.3.3(19)

SPA   in Ada.Characters.Latin_1   A.3.3(18)

Space   in Ada.Characters.Latin_1   A.3.3(8)   in Ada.Strings   A.4.1(4)

space_character   2.1(11)   used   2.1(3), P.1

special graphic character   a category of Character   A.3.2(32)

special_character   2.1(12)   names   2.1(15)   used   2.1(3), P.1

Special_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

Specialized Needs Annexes   1.1.1(24)

specifiable   of Address for entries   J.7.1(6)   of Address for stand-alone objects and for program units   13.3(12)   of Alignment for first subtypes and objects   13.3(25)   of Bit_Order for record types and record extensions   13.5.3(4)   of Component_Size for array types   13.3(72)   of External_Tag for a tagged type   13.3(79), K(68)   of Input for a type   13.13.2(37)   of Machine_Radix for decimal first subtypes   F.1(1)   of Output for a type   13.13.2(37)   of Read for a type   13.13.2(37)   of Size for first subtypes   13.3(49)   of Size for stand-alone objects   13.3(42)   of Small for fixed point types   3.5.10(2)   of Storage_Pool for a nonderived access-to-object type   13.11(16)   of Storage_Size for a nonderived access-to-object type   13.11(16)   of Storage_Size for a task first subtype   J.9(3)   of Write for a type   13.13.2(37)

specifiable (of an attribute and for an entity)   13.3(5)

specific type   3.4.1(3)

specified   of an aspect of representation of an entity   13.1(19)

specified (not!)   1.1.1(75), M(3.a)

specified discriminant   3.7(19)

Split   in Ada.Calendar   9.6(14)   in Ada.Real_Time   D.8(16)

Sqrt   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(3)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(4)

squirrel away   8.5.4(8.g)

SS2   in Ada.Characters.Latin_1   A.3.3(17)

SS3   in Ada.Characters.Latin_1   A.3.3(17)

SSA   in Ada.Characters.Latin_1   A.3.3(17)

ST   in Ada.Characters.Latin_1   A.3.3(19)

stand-alone constant   3.3.1(23)   corresponding to a formal object of mode in   12.4(10)

stand-alone object   3.3.1(1)   [partial]   12.4(10)

stand-alone variable   3.3.1(23)

Standard   A.1(4)

standard error file   A.10(6)

standard input file   A.10(5)

standard mode   1.1.1(113)

standard output file   A.10(5)

standard storage pool   13.11(18)

Standard_Error   in Ada.Text_IO   A.10.1(16), A.10.1(19)

Standard_Input   in Ada.Text_IO   A.10.1(16), A.10.1(19)

Standard_Output   in Ada.Text_IO   A.10.1(16), A.10.1(19)

State   in Ada.Numerics.Discrete_Random   A.5.2(23)   in Ada.Numerics.Float_Random   A.5.2(11)

statement   5.1(4)   used   5.1(3), P.1

statement_identifier   5.1(9)   used   5.1(8), 5.5(2), 5.6(2), P.1

static   3.3.2(1.a), 4.9(1)   constant   4.9(24)   constraint   4.9(27)   delta constraint   4.9(29)   digits constraint   4.9(29)   discrete_range   4.9(25)   discriminant constraint   4.9(31)   expression   4.9(2)   function   4.9(18)   index constraint   4.9(30)   range   4.9(25)   range constraint   4.9(29)   scalar subtype   4.9(26)   string subtype   4.9(26)   subtype   4.9(26)   subtype   12.4(9)   value   4.9(13.a)

static semantics   1.1.1(45)

statically compatible   for a constraint and a scalar subtype   4.9.1(4)   for a constraint and an access or composite subtype   4.9.1(4)   for two subtypes   4.9.1(4)

statically constrained   4.9(32)

statically deeper   3.10.2(4), 3.10.2(17)

statically denote   4.9(14)

statically determined tag   3.9.2(1)   [partial]   3.9.2(15), 3.9.2(19)

statically matching   effect on subtype-specific aspects   13.1(16)   for constraints   4.9.1(1)   for ranges   4.9.1(3)   for subtypes   4.9.1(2)   required   3.9.2(10), 3.10.2(27), 4.6(12), 4.6(16), 6.3.1(16), 6.3.1(17), 6.3.1(23), 7.3(13), 12.5.1(14), 12.5.3(6), 12.5.3(7), 12.5.4(3), 12.7(7)

statically tagged   3.9.2(4)

Status_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

storage deallocation   unchecked   13.11.2(1)

storage element   13.3(8)

storage management   user-defined   13.11(1)

storage node   E(2)

storage place   of a component   13.5(1)

storage place attributes   of a component   13.5.2(1)

storage pool   3.10(7)

storage pool element   13.11(12)

storage pool type   13.11(12)

Storage_Array   in System.Storage_Elements   13.7.1(5)

Storage_Check   11.5(26)   [partial]   11.1(6), 13.3(69), 13.11(18), D.7(15)

Storage_Count subtype of Storage_Offset   in System.Storage_Elements   13.7.1(4)

Storage_Element   in System.Storage_Elements   13.7.1(5)

Storage_Elements   child of System   13.7.1(2)

Storage_Error   raised by failure of runtime check   11.1(4), 11.1(6), 11.5(26), 13.3(69), 13.11(18), 13.11(19), D.7(15)   in Standard   A.1(46)

Storage_IO   child of Ada   A.9(3)

Storage_Offset   in System.Storage_Elements   13.7.1(3)

Storage_Pool attribute   13.11(14)

Storage_Pool clause   13.3(7), 13.11(16)

Storage_Pools   child of System   13.11(6)

Storage_Size   in System.Storage_Pools   13.11(10)

Storage_Size attribute   13.11(15), J.9(2)

Storage_Size clause   13.3(7), 13.11(16)   See also pragma Storage_Size   13.3(63)

Storage_Size pragma   13.3(65), L(39)

Storage_Unit   in System   13.7(13)

stream   13.13(1)   in Ada.Streams.Stream_IO   A.12.1(13)   in Ada.Text_IO.Text_Streams   A.12.2(4)   in Ada.Wide_Text_IO.Text_Streams   A.12.3(4)

stream type   13.13(1)

stream-oriented attributes   13.13.2(1)

Stream_Access   in Ada.Streams.Stream_IO   A.12.1(4)   in Ada.Text_IO.Text_Streams   A.12.2(3)   in Ada.Wide_Text_IO.Text_Streams   A.12.3(3)

Stream_Element   in Ada.Streams   13.13.1(4)

Stream_Element_Array   in Ada.Streams   13.13.1(4)

Stream_Element_Count subtype of Stream_Element_Offset   in Ada.Streams   13.13.1(4)

Stream_Element_Offset   in Ada.Streams   13.13.1(4)

Stream_IO   child of Ada.Streams   A.12.1(3)

Streams   child of Ada   13.13.1(2)

strict mode   G.2(1)

String   in Standard   A.1(37)

string type   3.6.3(1)

String_Access   in Ada.Strings.Unbounded   A.4.5(7)

string_element   2.6(3)   used   2.6(2), P.1

string_literal   2.6(2)   used   4.4(7), 6.1(9), P.1

Strings   child of Ada   A.4.1(3)   child of Interfaces.C   B.3.1(3)

Strlen   in Interfaces.C.Strings   B.3.1(17)

structure   See record type   3.8(1)

STS   in Ada.Characters.Latin_1   A.3.3(18)

STX   in Ada.Characters.Latin_1   A.3.3(5)

SUB   in Ada.Characters.Latin_1   A.3.3(6)

subaggregate   of an array_aggregate   4.3.3(7)

subcomponent   3.2(8)

subprogram   6(1)   abstract   3.9.3(3)

subprogram call   6.4(1)

subprogram instance   12.3(13)

subprogram_body   6.3(2)   used   3.11(6), 9.4(8), 10.1.1(7), P.1

subprogram_body_stub   10.1.3(3)   used   10.1.3(2), P.1

subprogram_declaration   6.1(2)   used   3.1(3), 9.4(5), 9.4(8), 10.1.1(5), P.1

subprogram_default   12.6(3)   used   12.6(2), P.1

subprogram_renaming_declaration   8.5.4(2)   used   8.5(3), 10.1.1(6), P.1

subprogram_specification   6.1(4)   used   6.1(2), 6.1(3), 6.3(2), 8.5.4(2), 10.1.3(3), 12.1(3), 12.6(2), P.1

subsystem   10.1(3)

subtype   3.2(10)   of a generic formal object   12.4(10.c)

subtype (of an object)   See actual subtype of an object   3.3(24)   See actual subtype of an object   3.3.1(9)

subtype conformance   6.3.1(17), 12.3(11.j)   [partial]   3.10.2(34), 9.5.4(17)   required   3.9.2(10), 3.10.2(32), 4.6(19), 8.5.4(5), 9.5.4(5)

subtype conversion   See type conversion   4.6(1)   See also implicit subtype conversion   4.6(1)

subtype-specific   attribute_definition_clause   13.3(7.a)   of a representation item   13.1(10)   of an aspect   13.1(10)

subtype_declaration   3.2.2(2)   used   3.1(3), P.1

subtype_indication   3.2.2(3)   used   3.2.2(2), 3.3.1(2), 3.4(3), 3.6(6), 3.6(7), 3.6.1(3), 3.10(3), 4.8(2), 7.3(3), P.1

subtype_mark   3.2.2(4)   used   3.2.2(3), 3.6(4), 3.7(6), 3.10(6), 4.3.2(3), 4.4(3), 4.6(2), 4.7(2), 6.1(13), 6.1(15), 8.4(4), 8.5.1(2), 12.3(5), 12.4(2), 12.5.1(3), 13.8(14.b), P.1

subtypes   of a profile   6.1(25)

subunit   10.1.3(7), 10.1.3(8)   used   10.1.1(3), P.1

Succ attribute   3.5(22)

Success   in Ada.Command_Line   A.15(8)

super   See view conversion   4.6(5)

Superscript_One   in Ada.Characters.Latin_1   A.3.3(22)

Superscript_Three   in Ada.Characters.Latin_1   A.3.3(22)

Superscript_Two   in Ada.Characters.Latin_1   A.3.3(22)

Suppress pragma   11.5(6), L(40)

suppressed check   11.5(11)

Suspend_Until_True   in Ada.Synchronous_Task_Control   D.10(4)

Suspension_Object   in Ada.Synchronous_Task_Control   D.10(4)

SYN   in Ada.Characters.Latin_1   A.3.3(6)

synchronization   9(1)

Synchronous_Task_Control   child of Ada   D.10(3)

syntactic category   1.1.1(95)

syntax   complete listing   P.1(1)   cross reference   P.2(1)   notation   1.1.1(83)   under Syntax heading   1.1.1(42)

System   13.7(3)

System.Address_To_Access_Conversions   13.7.2(2)

System.Machine_Code   13.8(7)

System.RPC   E.5(3)

System.Storage_Elements   13.7.1(2)

System.Storage_Pools   13.11(6)

System_Name   in System   13.7(4)

systems programming   C(1)

T

Tag   in Ada.Tags   3.9(7)

Tag attribute   3.9(18), 3.9(20)

tag indeterminate   3.9.2(6)

tag of an object   3.9(4)   class-wide object   3.9(24)   object created by an allocator   3.9(23)   preserved by type conversion and parameter passing   3.9(27)   returned by a function   3.9(25), 3.9(26)   stand-alone object, component, or aggregate   3.9(22)

Tag_Check   11.5(21)   [partial]   3.9.2(16), 4.6(42), 4.6(52), 5.2(10), 6.5(9)

Tag_Error   in Ada.Tags   3.9(9)

tagged type   3.9(2)

Tags   child of Ada   3.9(7)

Tail   in Ada.Strings.Bounded   A.4.4(72), A.4.4(73)   in Ada.Strings.Fixed   A.4.3(37), A.4.3(38)   in Ada.Strings.Unbounded   A.4.5(67), A.4.5(68)

tail (of a queue)   D.2.1(5)

Tan   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(4)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(5)

Tanh   in Ada.Numerics.Generic_Complex_Elementary_Functions   G.1.2(6)   in Ada.Numerics.Generic_Elementary_Functions   A.5.1(7)

target   of an assignment operation   5.2(3)   of an assignment_statement   5.2(3)

target entry   of a requeue_statement   9.5.4(3)

target object   of a call on an entry or a protected subprogram   9.5(2)   of a requeue_statement   9.5(7)

target statement   of a goto_statement   5.8(3)

target subtype   of a type_conversion   4.6(3)

task   9(1)   activation   9.2(1)   completion   9.3(1)   dependence   9.3(1)   execution   9.2(1)   termination   9.3(1)

task declaration   9.1(1)

task dispatching   D.2.1(4)

task dispatching point   D.2.1(4)   [partial]   D.2.1(8), D.2.2(13)

task dispatching policy   9(11.a), D.2.2(7)   [partial]   D.2.1(5)

task priority   D.1(15)

task state   abnormal   9.8(4)   blocked   9(11)   callable   9.9(2)   held   D.11(4)   inactive   9(11)   ready   9(11)   terminated   9(11)

task unit   9(10)

Task_Attributes   child of Ada   C.7.2(2)

task_body   9.1(6)   used   3.11(6), P.1

task_body_stub   10.1.3(5)   used   10.1.3(2), P.1

task_definition   9.1(4)   used   9.1(2), 9.1(3), P.1

Task_Dispatching_Policy pragma   D.2.2(2), L(41)

Task_Id   in Ada.Task_Identification   C.7.1(2)

Task_Identification   child of Ada   C.7.1(2)

task_item   9.1(5)   used   9.1(4), P.1

task_type_declaration   9.1(2)   used   3.2.1(3), P.1

Tasking_Check   [partial]   9.2(5), 9.5.3(21)

Tasking_Error   raised by detection of a bounded error   13.11.2(13)   raised by failure of runtime check   9.2(5), 9.5.3(21), 11.1(4)   in Standard   A.1(46)

template   12(1)   for a formal package   12.7(4)   See generic unit   12(1)

term   4.4(5)   used   4.4(4), P.1

terminal interrupt   example   9.7.4(10)

terminate_alternative   9.7.1(7)   used   9.7.1(4), P.1

terminated   a task state   9(11)

Terminated attribute   9.9(3)

termination   abnormal   10.2(26.c)   normal   10.2(26.c)   of a partition   10.2(26.c)   of a partition   E.1(7)

Terminator_Error   in Interfaces.C   B.3(40)

tested type   of a membership test   4.5.2(3)

text of a program   2.2(1)

Text_IO   child of Ada   A.10.1(2)

Text_Streams   child of Ada.Text_IO   A.12.2(3)   child of Ada.Wide_Text_IO   A.12.3(3)

throw (an exception)   See raise   11(1)

thunk   13.14(19.i)

tick   2.1(15)   in Ada.Real_Time   D.8(6)   in System   13.7(10)

Tilde   in Ada.Characters.Latin_1   A.3.3(14)

Time   in Ada.Calendar   9.6(10)   in Ada.Real_Time   D.8(4)

time base   9.6(6)

time limit   example   9.7.4(12)

time type   9.6(6)

Time-dependent Reset procedure   of the random number generator   A.5.2(34)

time-out   example   9.7.4(12)   See asynchronous_select   9.7.4(12)   See selective_accept   9.7.1(1)   See timed_entry_call   9.7.2(1)

Time_Error   in Ada.Calendar   9.6(18)

Time_First   in Ada.Real_Time   D.8(4)

Time_Last   in Ada.Real_Time   D.8(4)

Time_Of   in Ada.Calendar   9.6(15)   in Ada.Real_Time   D.8(16)

Time_Span   in Ada.Real_Time   D.8(5)

Time_Span_First   in Ada.Real_Time   D.8(5)

Time_Span_Last   in Ada.Real_Time   D.8(5)

Time_Span_Unit   in Ada.Real_Time   D.8(5)

Time_Span_Zero   in Ada.Real_Time   D.8(5)

Time_Unit   in Ada.Real_Time   D.8(4)

timed_entry_call   9.7.2(2)   used   9.7(2), P.1

timer interrupt   example   9.7.4(12)

times operator   4.5(1), 4.5.5(1)

timing   See delay_statement   9.6(1)

To_Ada   in Interfaces.C   B.3(22), B.3(26), B.3(28), B.3(32), B.3(37), B.3(39)   in Interfaces.COBOL   B.4(17), B.4(19)   in Interfaces.Fortran   B.5(13), B.5(14), B.5(16)

To_Address   in System.Address_To_Access_Conversions   13.7.2(3)   in System.Storage_Elements   13.7.1(10)

To_Basic   in Ada.Characters.Handling   A.3.2(6), A.3.2(7)

To_Binary   in Interfaces.COBOL   B.4(45), B.4(48)

To_Bounded_String   in Ada.Strings.Bounded   A.4.4(11)

To_C   in Interfaces.C   B.3(21), B.3(25), B.3(27), B.3(32), B.3(36), B.3(38)

To_Character   in Ada.Characters.Handling   A.3.2(15)

To_Chars_Ptr   in Interfaces.C.Strings   B.3.1(8)

To_COBOL   in Interfaces.COBOL   B.4(17), B.4(18)

To_Decimal   in Interfaces.COBOL   B.4(35), B.4(40), B.4(44), B.4(47)

To_Display   in Interfaces.COBOL   B.4(36)

To_Domain   in Ada.Strings.Maps   A.4.2(24)   in Ada.Strings.Wide_Maps   A.4.7(24)

To_Duration   in Ada.Real_Time   D.8(13)

To_Fortran   in Interfaces.Fortran   B.5(13), B.5(14), B.5(15)

To_Integer   in System.Storage_Elements   13.7.1(10)

To_ISO_646   in Ada.Characters.Handling   A.3.2(11), A.3.2(12)

To_Long_Binary   in Interfaces.COBOL   B.4(48)

To_Lower   in Ada.Characters.Handling   A.3.2(6), A.3.2(7)

To_Mapping   in Ada.Strings.Maps   A.4.2(23)   in Ada.Strings.Wide_Maps   A.4.7(23)

To_Packed   in Interfaces.COBOL   B.4(41)

To_Picture   in Ada.Text_IO.Editing   F.3.3(6)

To_Pointer   in System.Address_To_Access_Conversions   13.7.2(3)

To_Range   in Ada.Strings.Maps   A.4.2(24)   in Ada.Strings.Wide_Maps   A.4.7(25)

To_Ranges   in Ada.Strings.Maps   A.4.2(10)   in Ada.Strings.Wide_Maps   A.4.7(10)

To_Sequence   in Ada.Strings.Maps   A.4.2(19)   in Ada.Strings.Wide_Maps   A.4.7(19)

To_Set   in Ada.Strings.Maps   A.4.2(8), A.4.2(9), A.4.2(17), A.4.2(18)   in Ada.Strings.Wide_Maps   A.4.7(8), A.4.7(9), A.4.7(17), A.4.7(18)

To_String   in Ada.Characters.Handling   A.3.2(16)   in Ada.Strings.Bounded   A.4.4(12)   in Ada.Strings.Unbounded   A.4.5(11)

To_Time_Span   in Ada.Real_Time   D.8(13)

To_Unbounded_String   in Ada.Strings.Unbounded   A.4.5(9), A.4.5(10)

To_Upper   in Ada.Characters.Handling   A.3.2(6), A.3.2(7)

To_Wide_Character   in Ada.Characters.Handling   A.3.2(17)

To_Wide_String   in Ada.Characters.Handling   A.3.2(18)

token   See lexical element   2.2(1)

Trailing_Nonseparate   in Interfaces.COBOL   B.4(23)

Trailing_Separate   in Interfaces.COBOL   B.4(23)

transfer of control   5.1(15)

Translate   in Ada.Strings.Bounded   A.4.4(53), A.4.4(54), A.4.4(55), A.4.4(56)   in Ada.Strings.Fixed   A.4.3(18), A.4.3(19), A.4.3(20), A.4.3(21)   in Ada.Strings.Unbounded   A.4.5(48), A.4.5(49), A.4.5(50), A.4.5(51)

Translation_Error   in Ada.Strings   A.4.1(5)

triggering_alternative   9.7.4(3)   used   9.7.4(2), P.1

triggering_statement   9.7.4(4)   used   9.7.4(3), P.1

Trim   in Ada.Strings.Bounded   A.4.4(67), A.4.4(68), A.4.4(69)   in Ada.Strings.Fixed   A.4.3(31), A.4.3(32), A.4.3(33), A.4.3(34)   in Ada.Strings.Unbounded   A.4.5(61), A.4.5(62), A.4.5(63), A.4.5(64)

Trim_End   in Ada.Strings   A.4.1(6)

True   3.5.3(1)

Truncation   in Ada.Strings   A.4.1(6)

Truncation attribute   A.5.3(43)

two's complement   modular types   3.5.4(30)

type   3.2(1)   abstract   3.9.3(2)   See also tag   3.9(4)   See also language-defined types

type conformance   6.3.1(15)   [partial]   3.4(19), 8.3(8), 8.3(27), 10.1.4(4)   required   3.11.1(5), 4.1.4(14), 8.6(26), 9.5.4(3)

type conversion   4.6(1)   access   4.6(13), 4.6(18), 4.6(47)   arbitrary order   1.1.1(98)   array   4.6(9), 4.6(36)   composite (non-array)   4.6(21), 4.6(40)   enumeration   4.6(21), 4.6(34)   numeric   4.6(8), 4.6(29)   unchecked   13.9(1)   See also qualified_expression   4.7(1)

type conversion, implicit   See implicit subtype conversion   4.6(1)

type extension   3.9(2), 3.9.1(1)

type of a discrete_range   3.6.1(4)

type of a range   3.5(4)

type parameter   See discriminant   3.7(1)

type profile   See profile, type conformant   6.3.1(15)

type resolution rules   8.6(20)   if any type in a specified class of types is expected   8.6(21)   if expected type is specific   8.6(22)   if expected type is universal or class-wide   8.6(21)

type tag   See tag   3.9(4)

type-related   aspect   13.1(10)   attribute_definition_clause   13.3(7.a)   representation item   13.1(10)

type_conversion   4.6(2)   used   4.1(2), P.1   See also unchecked type conversion   13.9(1)

type_declaration   3.2.1(2)   used   3.1(3), P.1

type_definition   3.2.1(4)   used   3.2.1(3), P.1

Type_Set   in Ada.Text_IO   A.10.1(7)

types   of a profile   6.1(29)

U

UC_A_Acute   in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Circumflex   in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Diaeresis   in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Grave   in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Ring   in Ada.Characters.Latin_1   A.3.3(23)

UC_A_Tilde   in Ada.Characters.Latin_1   A.3.3(23)

UC_AE_Diphthong   in Ada.Characters.Latin_1   A.3.3(23)

UC_C_Cedilla   in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Acute   in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Circumflex   in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Diaeresis   in Ada.Characters.Latin_1   A.3.3(23)

UC_E_Grave   in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Acute   in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Circumflex   in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Diaeresis   in Ada.Characters.Latin_1   A.3.3(23)

UC_I_Grave   in Ada.Characters.Latin_1   A.3.3(23)

UC_Icelandic_Eth   in Ada.Characters.Latin_1   A.3.3(24)

UC_Icelandic_Thorn   in Ada.Characters.Latin_1   A.3.3(24)

UC_N_Tilde   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Acute   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Circumflex   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Diaeresis   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Grave   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Oblique_Stroke   in Ada.Characters.Latin_1   A.3.3(24)

UC_O_Tilde   in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Acute   in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Circumflex   in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Diaeresis   in Ada.Characters.Latin_1   A.3.3(24)

UC_U_Grave   in Ada.Characters.Latin_1   A.3.3(24)

UC_Y_Acute   in Ada.Characters.Latin_1   A.3.3(24)

UCHAR_MAX   in Interfaces.C   B.3(6)

UI   1.3(1.c)

ultimate ancestor   of a type   3.4.1(10)

unary adding operator   4.5.4(1)

unary operator   4.5(9)

unary_adding_operator   4.5(5)   used   4.4(4), P.1

Unbiased_Rounding attribute   A.5.3(39)

Unbounded   child of Ada.Strings   A.4.5(3)   in Ada.Text_IO   A.10.1(5)

Unbounded_String   in Ada.Strings.Unbounded   A.4.5(4)

unchecked storage deallocation   13.11.2(1)

unchecked type conversion   13.9(1)

Unchecked_Access attribute   13.10(3), H.4(18)   See also Access attribute   3.10.2(24)

Unchecked_Conversion   child of Ada   13.9(3)

Unchecked_Deallocation   child of Ada   13.11.2(3)

unconstrained   3.2(11)   object   3.3.1(9), 3.10(9)   object   6.4.1(16)   subtype   3.2(11), 3.4(8), 3.5(7), 3.5.1(10), 3.5.4(9), 3.5.4(10), 3.5.7(11), 3.5.9(13), 3.5.9(16), 3.6(15), 3.6(16), 3.7(27), 3.9(17)   subtype   3.10(15)   subtype   K(36)

unconstrained_array_definition   3.6(3)   used   3.6(2), P.1

undefined result   11.6(5)

underline   2.1(15)   used   2.3(2), 2.4.1(3), 2.4.2(4), P.1

Uniformity Issue (UI)   1.3(1.c)

Uniformity Rapporteur Group (URG)   1.3(1.c)

Uniformly_Distributed subtype of Float   in Ada.Numerics.Float_Random   A.5.2(8)

uninitialized allocator   4.8(4)

uninitialized variables   13.9.1(2)   [partial]   3.3.1(21), 13.3(56.i)

unit consistency   E.3(6)

universal type   3.4.1(6)

universal_fixed   [partial]   3.5.6(4)

universal_integer   [partial]   3.5.4(14), 3.5.4(31)

universal_real   [partial]   3.5.6(4)

unknown discriminants   3.7(27)   [partial]   3.7(2.a)

unknown_discriminant_part   3.7(4)   used   3.7(3), P.1

unmarshalling   E.4(9)

unpolluted   13.13.1(2)

unsigned   in Interfaces.C   B.3(9)   in Interfaces.COBOL   B.4(23)

unsigned type   See modular type   3.5.4(1)

unsigned_char   in Interfaces.C   B.3(10)

unsigned_long   in Interfaces.C   B.3(9)

Unsigned_N   B.2(8)

unsigned_short   in Interfaces.C   B.3(9)

unspecified   1.1.1(75), M(3.a)   [partial]   2.1(5), 3.9(5), 3.9(14), 4.5.2(13), 4.5.5(21), 6.2(11), 7.2(5), 9.8(14), 10.2(27), 11.1(6), 11.5(30), 13.1(20), 13.7.2(5), 13.9.1(7), 13.11(21), A.1(1), A.5.1(34), A.5.2(28), A.5.2(34), A.7(6), A.10(8), A.10.7(8), A.10.7(12), A.10.7(19), A.14(1), A.15(20), D.2.2(7), D.8(19), G.1.1(40), G.1.2(33), G.1.2(48), H, H.2(1)

update   the value of an object   3.3(15)   in Interfaces.C.Strings   B.3.1(18), B.3.1(19)

Update_Error   in Interfaces.C.Strings   B.3.1(20)

upper bound   of a range   3.5(4)

upper-case letter   a category of Character   A.3.2(26)

upper_case_identifier_letter   2.1(8)

Upper_Case_Map   in Ada.Strings.Maps.Constants   A.4.6(5)

Upper_Set   in Ada.Strings.Maps.Constants   A.4.6(4)

URG   1.3(1.c)

US   in Ada.Characters.Latin_1   A.3.3(6)

usage name   3.1(12)

use-visible   8.3(4), 8.4(9)

use_clause   8.4(2)   used   3.11(4), 10.1.2(3), 12.1(5), P.1

Use_Error   in Ada.Direct_IO   A.8.4(18)   in Ada.IO_Exceptions   A.13(4)   in Ada.Sequential_IO   A.8.1(15)   in Ada.Streams.Stream_IO   A.12.1(26)   in Ada.Text_IO   A.10.1(85)

use_package_clause   8.4(3)   used   8.4(2), P.1

use_type_clause   8.4(4)   used   8.4(2), P.1

user-defined assignment   7.6(1)

user-defined heap management   13.11(1)

user-defined operator   6.6(1)

user-defined storage management   13.11(1)

V

Val attribute   3.5.5(5)

Valid   in Ada.Text_IO.Editing   F.3.3(5), F.3.3(12)   in Interfaces.COBOL   B.4(33), B.4(38), B.4(43)

Valid attribute   13.9.2(3), H(6)

value   3.2(12.a)   in Ada.Numerics.Discrete_Random   A.5.2(26)   in Ada.Numerics.Float_Random   A.5.2(14)   in Ada.Strings.Maps   A.4.2(21)   in Ada.Strings.Wide_Maps   A.4.7(21)   in Ada.Task_Attributes   C.7.2(4)   in Interfaces.C.Pointers   B.3.2(6), B.3.2(7)   in Interfaces.C.Strings   B.3.1(13), B.3.1(14), B.3.1(15), B.3.1(16)

Value attribute   3.5(55)

value conversion   4.6(5)

variable   3.3(14)   required   5.2(5), 6.4.1(5), 12.4(7)

variable object   3.3(14)

variable view   3.3(14)

variant   3.8.1(3)   used   3.8.1(2), P.1   See also tagged type   3.9(1)

variant_part   3.8.1(2)   used   3.8(4), P.1

version   of a compilation unit   E.3(5)

Version attribute   E.3(3)

vertical line   2.1(15)

Vertical_Line   in Ada.Characters.Latin_1   A.3.3(14)

view   3.1(8), 3.1(9.b/0)

view conversion   4.6(5)

virtual function   See dispatching subprogram   3.9.2(1)

Virtual_Length   in Interfaces.C.Pointers   B.3.2(13)

visibility   direct   8.3(2), 8.3(22)   immediate   8.3(4), 8.3(22)   use clause   8.3(4), 8.4(9)

visibility rules   8.3(1)

visible   8.3(2), 8.3(15)   within a pragma in a context_clause   10.1.6(3)   within a pragma that appears at the place of a compilation unit   10.1.6(5)   within a use_clause in a context_clause   10.1.6(3)   within a with_clause   10.1.6(2)   within the parent_unit_name of a library unit   10.1.6(2)   within the parent_unit_name of a subunit   10.1.6(4)

visible part   8.2(5)   of a formal package   12.7(10)   of a generic unit   8.2(8)   of a package (other than a generic formal package)   7.1(6)   of a protected unit   9.4(11)   of a task unit   9.1(9)   of a view of a callable entity   8.2(6)   of a view of a composite type   8.2(7)   of an instance   12.3(12.b)

volatile   C.6(8)

Volatile pragma   C.6(4), L(43)

Volatile_Components pragma   C.6(6), L(44)

VT   in Ada.Characters.Latin_1   A.3.3(5)

VTS   in Ada.Characters.Latin_1   A.3.3(17)

W

wchar_array   in Interfaces.C   B.3(33)

wchar_t   in Interfaces.C   B.3(30)

well-formed picture String   for edited output   F.3.1(1)

Wide_Bounded   child of Ada.Strings   A.4.7(1)

Wide_Character   3.5.2(3)   in Standard   A.1

Wide_Character_Mapping   in Ada.Strings.Wide_Maps   A.4.7(20)

Wide_Character_Mapping_Function   in Ada.Strings.Wide_Maps   A.4.7(26)

Wide_Character_Range   in Ada.Strings.Wide_Maps   A.4.7(6)

Wide_Character_Ranges   in Ada.Strings.Wide_Maps   A.4.7(7)

Wide_Character_Sequence subtype of Wide_String   in Ada.Strings.Wide_Maps   A.4.7(16)

Wide_Character_Set   in Ada.Strings.Wide_Maps   A.4.7(4)

Wide_Constants   child of Ada.Strings.Wide_Maps   A.4.7(1)

Wide_Fixed   child of Ada.Strings   A.4.7(1)

Wide_Hash   child of Ada.Strings   A.4.7(1)   child of Ada.Strings.Wide_Bounded   A.4.7(1)   child of Ada.Strings.Wide_Fixed   A.4.7(1)   child of Ada.Strings.Wide_Unbounded   A.4.7(1)

Wide_Image attribute   3.5(29), 4.9.1(7)

Wide_Maps   child of Ada.Strings   A.4.7(3)

wide_nul   in Interfaces.C   B.3(31)

Wide_Space   in Ada.Strings   A.4.1(4)

Wide_String   in Standard   A.1(41)

Wide_Text_IO   child of Ada   A.11(2)

Wide_Unbounded   child of Ada.Strings   A.4.7(1)

Wide_Value attribute   3.5(43)

Wide_Width attribute   3.5(40)

Width attribute   3.5(41)

with_clause   10.1.2(4)   mentioned in   10.1.2(6)   used   10.1.2(3), P.1

within   immediately   8.1(13)

word   13.3(8)

Word_Size   in System   13.7(13)

wording changes from Ada 83   1.1.1(56.j)

Write   in Ada.Direct_IO   A.8.4(13)   in Ada.Sequential_IO   A.8.1(12)   in Ada.Storage_IO   A.9(7)   in Ada.Streams   13.13.1(6)   in Ada.Streams.Stream_IO   A.12.1(18), A.12.1(19)   in System.RPC   E.5(8)

Write attribute   13.13.2(4), 13.13.2(12)

Write clause   13.3(7), 13.13.2(37)

X

xor operator   4.5(1), 4.5.1(2)

Y

Year   in Ada.Calendar   9.6(13)

Year_Number subtype of Integer   in Ada.Calendar   9.6(11)

Yen_Sign   in Ada.Characters.Latin_1   A.3.3(21)

