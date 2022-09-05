---
sidebar_position:  216
---

# P.1  Syntax Rules

{AI12-0426-1} This subclause lists the complete syntax of the language in the order it appears in this Reference Manual. See  for a description of the notation used. 

2.3:
identifier ::= 
   [identifier_start](./AA-2.3#S0003) {[identifier_start](./AA-2.3#S0003) | [identifier_extend](./AA-2.3#S0005)}

2.3:
identifier_start ::= 
     letter_uppercase
   | letter_lowercase
   | letter_titlecase
   | letter_modifier
   | letter_other
   | number_letter

2.3:
identifier_extend ::= 
     mark_non_spacing
   | mark_spacing_combining
   | number_decimal
   | punctuation_connector

2.4:
numeric_literal ::= [decimal_literal](./AA-2.4#S0007) | [based_literal](./AA-2.4#S0011)

2.4.1:
decimal_literal ::= [numeral](./AA-2.4#S0008) [.[numeral](./AA-2.4#S0008)] [[exponent](./AA-2.4#S0009)]

2.4.1:
numeral ::= [digit](./AA-2.4#S0010) {[underline] [digit](./AA-2.4#S0010)}

2.4.1:
exponent ::= E [+] [numeral](./AA-2.4#S0008) | E  [numeral](./AA-2.4#S0008)

2.4.1:
digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

2.4.2:
based_literal ::= 
   [base](./AA-2.4#S0012) # [based_numeral](./AA-2.4#S0013) [.[based_numeral](./AA-2.4#S0013)] # [[exponent](./AA-2.4#S0009)]

2.4.2:
base ::= [numeral](./AA-2.4#S0008)

2.4.2:
based_numeral ::= 
   [extended_digit](./AA-2.4#S0014) {[underline] [extended_digit](./AA-2.4#S0014)}

2.4.2:
extended_digit ::= [digit](./AA-2.4#S0010) | A | B | C | D | E | F

2.5:
character_literal ::= 'graphic_character'

2.6:
string_literal ::= "{[string_element](./AA-2.6#S0017)}"

2.6:
string_element ::= "" | non_quotation_mark_graphic_character

2.7:
comment ::= --{non_end_of_line_character}

2.8:
pragma ::= 
   pragma [identifier](./AA-2.3#S0002) [([pragma_argument_association](./AA-2.8#S0020) {, [pragma_argument_association](./AA-2.8#S0020)})];

2.8:
pragma_argument_association ::= 
     [pragma_argument_[identifier](./AA-2.3#S0002) =&gt] [name](./AA-4.1#S0091)
   | [pragma_argument_[identifier](./AA-2.3#S0002) =&gt] [expression](./AA-4.4#S0132)
   | pragma_argument_[aspect_mark](./AA-13.1#S0347) =&gt  [name](./AA-4.1#S0091)
   | pragma_argument_[aspect_mark](./AA-13.1#S0347) =&gt  [expression](./AA-4.4#S0132)

3.1:
basic_declaration ::= 
     [type_declaration](./AA-3.2#S0023)	| [subtype_declaration](./AA-3.2#S0026)
   | [object_declaration](./AA-3.3#S0032)	| [number_declaration](./AA-3.3#S0034)
   | [subprogram_declaration](./AA-6.1#S0195)	| [abstract_subprogram_declaration](./AA-3.9#S0076)
   | [null_procedure_declaration](./AA-6.7#S0227)	| [expression_function_declaration](./AA-6.8#S0228)
   | [package_declaration](./AA-7.1#S0229)	| [renaming_declaration](./AA-8.5#S0238)
   | [exception_declaration](./AA-11.1#S0303)	| [generic_declaration](./AA-12.1#S0310)
   | [generic_instantiation](./AA-12.3#S0315)

3.1:
defining_identifier ::= [identifier](./AA-2.3#S0002)

3.2.1:
type_declaration ::=  [full_type_declaration](./AA-3.2#S0024)
   | [incomplete_type_declaration](./AA-3.10#S0085)
   | [private_type_declaration](./AA-7.3#S0232)
   | [private_extension_declaration](./AA-7.3#S0233)

3.2.1:
full_type_declaration ::= 
     type [defining_identifier](./AA-3.1#S0022) [[known_discriminant_part](./AA-3.7#S0061)] is [type_definition](./AA-3.2#S0025)
        [[aspect_specification](./AA-13.1#S0346)];
   | [task_type_declaration](./AA-9.1#S0244)
   | [protected_type_declaration](./AA-9.4#S0249)

3.2.1:
type_definition ::= 
     [enumeration_type_definition](./AA-3.5#S0038)	| [integer_type_definition](./AA-3.5#S0041)
   | [real_type_definition](./AA-3.5#S0044)	| [array_type_definition](./AA-3.6#S0051)
   | [record_type_definition](./AA-3.8#S0066)	| [access_type_definition](./AA-3.10#S0079)
   | [derived_type_definition](./AA-3.4#S0035)	| [interface_type_definition](./AA-3.9#S0077)

3.2.2:
subtype_declaration ::= 
   subtype [defining_identifier](./AA-3.1#S0022) is [subtype_indication](./AA-3.2#S0027)
        [[aspect_specification](./AA-13.1#S0346)];

3.2.2:
subtype_indication ::=  [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028) [[constraint](./AA-3.2#S0029)]

3.2.2:
subtype_mark ::= subtype_[name](./AA-4.1#S0091)

3.2.2:
constraint ::= [scalar_constraint](./AA-3.2#S0030) | [composite_constraint](./AA-3.2#S0031)

3.2.2:
scalar_constraint ::= 
     [range_constraint](./AA-3.5#S0036) | [digits_constraint](./AA-3.5#S0050) | [delta_constraint](./AA-J.3#S0367)

3.2.2:
composite_constraint ::= 
     [index_constraint](./AA-3.6#S0057) | [discriminant_constraint](./AA-3.7#S0064)

3.3.1:
object_declaration ::= 
    [defining_identifier_list](./AA-3.3#S0033) : [aliased] [constant] [subtype_indication](./AA-3.2#S0027) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)];
  | [defining_identifier_list](./AA-3.3#S0033) : [aliased] [constant] [access_definition](./AA-3.10#S0084) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)];
  | [defining_identifier_list](./AA-3.3#S0033) : [aliased] [constant] [array_type_definition](./AA-3.6#S0051) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)];
  | [single_task_declaration](./AA-9.1#S0245)
  | [single_protected_declaration](./AA-9.4#S0250)

3.3.1:
defining_identifier_list ::= 
  [defining_identifier](./AA-3.1#S0022) {, [defining_identifier](./AA-3.1#S0022)}

3.3.2:
number_declaration ::= 
     [defining_identifier_list](./AA-3.3#S0033) : constant := static_[expression](./AA-4.4#S0132);

3.4:
derived_type_definition ::= 
    [abstract] [limited] new parent_[subtype_indication](./AA-3.2#S0027) [[and [interface_list](./AA-3.9#S0078)] [record_extension_part](./AA-3.9#S0075)]

3.5:
range_constraint ::=  range [range](./AA-3.5#S0037)

3.5:
range ::=  [range_attribute_reference](./AA-4.1#S0102)
   | [simple_expression](./AA-4.4#S0138) .. [simple_expression](./AA-4.4#S0138)

3.5.1:
enumeration_type_definition ::= 
   ([enumeration_literal_specification](./AA-3.5#S0039) {, [enumeration_literal_specification](./AA-3.5#S0039)})

3.5.1:
enumeration_literal_specification ::=  [defining_identifier](./AA-3.1#S0022) | [defining_character_literal](./AA-3.5#S0040)

3.5.1:
defining_character_literal ::= [character_literal](./AA-2.5#S0015)

3.5.4:
integer_type_definition ::= [signed_integer_type_definition](./AA-3.5#S0042) | [modular_type_definition](./AA-3.5#S0043)

3.5.4:
signed_integer_type_definition ::= range static_[simple_expression](./AA-4.4#S0138) .. static_[simple_expression](./AA-4.4#S0138)

3.5.4:
modular_type_definition ::= mod static_[expression](./AA-4.4#S0132)

3.5.6:
real_type_definition ::= 
   [floating_point_definition](./AA-3.5#S0045) | [fixed_point_definition](./AA-3.5#S0047)

3.5.7:
floating_point_definition ::= 
  digits static_[expression](./AA-4.4#S0132) [[real_range_specification](./AA-3.5#S0046)]

3.5.7:
real_range_specification ::= 
  range static_[simple_expression](./AA-4.4#S0138) .. static_[simple_expression](./AA-4.4#S0138)

3.5.9:
fixed_point_definition ::= [ordinary_fixed_point_definition](./AA-3.5#S0048) | [decimal_fixed_point_definition](./AA-3.5#S0049)

3.5.9:
ordinary_fixed_point_definition ::= 
   delta static_[expression](./AA-4.4#S0132)  [real_range_specification](./AA-3.5#S0046)

3.5.9:
decimal_fixed_point_definition ::= 
   delta static_[expression](./AA-4.4#S0132) digits static_[expression](./AA-4.4#S0132) [[real_range_specification](./AA-3.5#S0046)]

3.5.9:
digits_constraint ::= 
   digits static_[simple_expression](./AA-4.4#S0138) [[range_constraint](./AA-3.5#S0036)]

3.6:
array_type_definition ::= 
   [unconstrained_array_definition](./AA-3.6#S0052) | [constrained_array_definition](./AA-3.6#S0054)

3.6:
unconstrained_array_definition ::= 
   array([index_subtype_definition](./AA-3.6#S0053) {, [index_subtype_definition](./AA-3.6#S0053)}) of [component_definition](./AA-3.6#S0056)

3.6:
index_subtype_definition ::= [subtype_mark](./AA-3.2#S0028) range &lt&gt

3.6:
constrained_array_definition ::= 
   array ([discrete_subtype_definition](./AA-3.6#S0055) {, [discrete_subtype_definition](./AA-3.6#S0055)}) of [component_definition](./AA-3.6#S0056)

3.6:
discrete_subtype_definition ::= discrete_[subtype_indication](./AA-3.2#S0027) | [range](./AA-3.5#S0037)

3.6:
component_definition ::= 
   [aliased] [subtype_indication](./AA-3.2#S0027)
 | [aliased] [access_definition](./AA-3.10#S0084)

3.6.1:
index_constraint ::=  ([discrete_range](./AA-3.6#S0058) {, [discrete_range](./AA-3.6#S0058)})

3.6.1:
discrete_range ::= discrete_[subtype_indication](./AA-3.2#S0027) | [range](./AA-3.5#S0037)

3.7:
discriminant_part ::= [unknown_discriminant_part](./AA-3.7#S0060) | [known_discriminant_part](./AA-3.7#S0061)

3.7:
unknown_discriminant_part ::= (&lt&gt)

3.7:
known_discriminant_part ::= 
   ([discriminant_specification](./AA-3.7#S0062) {; [discriminant_specification](./AA-3.7#S0062)})

3.7:
discriminant_specification ::= 
   [defining_identifier_list](./AA-3.3#S0033) : [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028) [:= [default_expression](./AA-3.7#S0063)]
      [[aspect_specification](./AA-13.1#S0346)] 
 | [defining_identifier_list](./AA-3.3#S0033) : [access_definition](./AA-3.10#S0084) [:= [default_expression](./AA-3.7#S0063)]
      [[aspect_specification](./AA-13.1#S0346)] 

3.7:
default_expression ::= [expression](./AA-4.4#S0132)

3.7.1:
discriminant_constraint ::= 
   ([discriminant_association](./AA-3.7#S0065) {, [discriminant_association](./AA-3.7#S0065)})

3.7.1:
discriminant_association ::= 
   [discriminant_[selector_name](./AA-4.1#S0099) {'|' discriminant_[selector_name](./AA-4.1#S0099)} =&gt] [expression](./AA-4.4#S0132)

3.8:
record_type_definition ::= [[abstract] tagged] [limited] [record_definition](./AA-3.8#S0067)

3.8:
record_definition ::= 
    record
       [component_list](./AA-3.8#S0068)
    end record [record_[identifier](./AA-2.3#S0002)]
  | null record

3.8:
component_list ::= 
      [component_item](./AA-3.8#S0069) {[component_item](./AA-3.8#S0069)}
   | {[component_item](./AA-3.8#S0069)} [variant_part](./AA-3.8#S0071)
   |  null;

3.8:
component_item ::= [component_declaration](./AA-3.8#S0070) | [aspect_clause](./AA-13.1#S0343)

3.8:
component_declaration ::= 
   [defining_identifier_list](./AA-3.3#S0033) : [component_definition](./AA-3.6#S0056) [:= [default_expression](./AA-3.7#S0063)]
        [[aspect_specification](./AA-13.1#S0346)];

3.8.1:
variant_part ::= 
   case discriminant_[direct_name](./AA-4.1#S0092) is
       [variant](./AA-3.8#S0072)
      {[variant](./AA-3.8#S0072)}
   end case;

3.8.1:
variant ::= 
   when [discrete_choice_list](./AA-3.8#S0073) =&gt
      [component_list](./AA-3.8#S0068)

3.8.1:
discrete_choice_list ::= [discrete_choice](./AA-3.8#S0074) {'|' [discrete_choice](./AA-3.8#S0074)}

3.8.1:
discrete_choice ::= [choice_expression](./AA-4.4#S0133) | discrete_[subtype_indication](./AA-3.2#S0027) | [range](./AA-3.5#S0037) | others

3.9.1:
record_extension_part ::= with [record_definition](./AA-3.8#S0067)

3.9.3:
abstract_subprogram_declaration ::= 
    [[overriding_indicator](./AA-8.3#S0234)]
    [subprogram_specification](./AA-6.1#S0196) is abstract
        [[aspect_specification](./AA-13.1#S0346)];

3.9.4:
interface_type_definition ::= 
    [limited | task | protected | synchronized] interface [and [interface_list](./AA-3.9#S0078)]

3.9.4:
interface_list ::= interface_[subtype_mark](./AA-3.2#S0028) {and interface_[subtype_mark](./AA-3.2#S0028)}

3.10:
access_type_definition ::= 
    [[null_exclusion](./AA-3.10#S0083)] [access_to_object_definition](./AA-3.10#S0080)
  | [[null_exclusion](./AA-3.10#S0083)] [access_to_subprogram_definition](./AA-3.10#S0082)

3.10:
access_to_object_definition ::= 
    access [[general_access_modifier](./AA-3.10#S0081)] [subtype_indication](./AA-3.2#S0027)

3.10:
general_access_modifier ::= all | constant

3.10:
access_to_subprogram_definition ::= 
    access [protected] procedure [parameter_profile](./AA-6.1#S0204)
  | access [protected] function  [parameter_and_result_profile](./AA-6.1#S0205)

3.10:
null_exclusion ::= not null

3.10:
access_definition ::= 
    [[null_exclusion](./AA-3.10#S0083)] access [constant] [subtype_mark](./AA-3.2#S0028)
  | [[null_exclusion](./AA-3.10#S0083)] access [protected] procedure [parameter_profile](./AA-6.1#S0204)
  | [[null_exclusion](./AA-3.10#S0083)] access [protected] function [parameter_and_result_profile](./AA-6.1#S0205)

3.10.1:
incomplete_type_declaration ::= type [defining_identifier](./AA-3.1#S0022) [[discriminant_part](./AA-3.7#S0059)] [is tagged];

3.11:
declarative_part ::= {[declarative_item](./AA-3.11#S0087)}

3.11:
declarative_item ::= 
    [basic_declarative_item](./AA-3.11#S0088) | [body](./AA-3.11#S0089)

3.11:
basic_declarative_item ::= 
    [basic_declaration](./AA-3.1#S0021) | [aspect_clause](./AA-13.1#S0343) | [use_clause](./AA-8.4#S0235)

3.11:
body ::= [proper_body](./AA-3.11#S0090) | [body_stub](./AA-10.1#S0297)

3.11:
proper_body ::= 
    [subprogram_body](./AA-6.3#S0216) | [package_body](./AA-7.2#S0231) | [task_body](./AA-9.1#S0248) | [protected_body](./AA-9.4#S0254)

4.1:
name ::= 
     [direct_name](./AA-4.1#S0092)	| [explicit_dereference](./AA-4.1#S0094)
   | [indexed_component](./AA-4.1#S0096)	| [slice](./AA-4.1#S0097)
   | [selected_component](./AA-4.1#S0098)	| [attribute_reference](./AA-4.1#S0100)
   | [type_conversion](./AA-4.6#S0162)	| [function_call](./AA-6.4#S0218)
   | [character_literal](./AA-2.5#S0015)	| [qualified_expression](./AA-4.7#S0163)
   | [generalized_reference](./AA-4.1#S0104)	| [generalized_indexing](./AA-4.1#S0105)
   | [target_name](./AA-5.2#S0174)

4.1:
direct_name ::= [identifier](./AA-2.3#S0002) | [operator_symbol](./AA-6.1#S0202)

4.1:
prefix ::= [name](./AA-4.1#S0091) | [implicit_dereference](./AA-4.1#S0095)

4.1:
explicit_dereference ::= [name](./AA-4.1#S0091).all

4.1:
implicit_dereference ::= [name](./AA-4.1#S0091)

4.1.1:
indexed_component ::= [prefix](./AA-4.1#S0093)([expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)})

4.1.2:
slice ::= [prefix](./AA-4.1#S0093)([discrete_range](./AA-3.6#S0058))

4.1.3:
selected_component ::= [prefix](./AA-4.1#S0093) . [selector_name](./AA-4.1#S0099)

4.1.3:
selector_name ::= [identifier](./AA-2.3#S0002) | [character_literal](./AA-2.5#S0015) | [operator_symbol](./AA-6.1#S0202)

4.1.4:
attribute_reference ::= 
    [prefix](./AA-4.1#S0093)'[attribute_designator](./AA-4.1#S0101)
  | [reduction_attribute_reference](./AA-4.5#S0158)

4.1.4:
attribute_designator ::= 
    [identifier](./AA-2.3#S0002)[(static_[expression](./AA-4.4#S0132))]
  | Access | Delta | Digits | Mod

4.1.4:
range_attribute_reference ::= [prefix](./AA-4.1#S0093)'[range_attribute_designator](./AA-4.1#S0103)

4.1.4:
range_attribute_designator ::= Range[(static_[expression](./AA-4.4#S0132))]

4.1.5:
generalized_reference ::= reference_object_[name](./AA-4.1#S0091)

4.1.6:
generalized_indexing ::= indexable_container_object_[prefix](./AA-4.1#S0093) [actual_parameter_part](./AA-6.4#S0219)

4.3:
aggregate ::= 
    [record_aggregate](./AA-4.3#S0107) | [extension_aggregate](./AA-4.3#S0111) | [array_aggregate](./AA-4.3#S0113)
  | [delta_aggregate](./AA-4.3#S0120) | [container_aggregate](./AA-4.3#S0123)

4.3.1:
record_aggregate ::= ([record_component_association_list](./AA-4.3#S0108))

4.3.1:
record_component_association_list ::= 
    [record_component_association](./AA-4.3#S0109) {, [record_component_association](./AA-4.3#S0109)}
  | null record

4.3.1:
record_component_association ::= 
    [[component_choice_list](./AA-4.3#S0110) =&gt] [expression](./AA-4.4#S0132)
   | [component_choice_list](./AA-4.3#S0110) =&gt &lt&gt

4.3.1:
component_choice_list ::= 
     component_[selector_name](./AA-4.1#S0099) {'|' component_[selector_name](./AA-4.1#S0099)}
   | others

4.3.2:
extension_aggregate ::= 
    ([ancestor_part](./AA-4.3#S0112) with [record_component_association_list](./AA-4.3#S0108))

4.3.2:
ancestor_part ::= [expression](./AA-4.4#S0132) | [subtype_mark](./AA-3.2#S0028)

4.3.3:
array_aggregate ::= 
    [positional_array_aggregate](./AA-4.3#S0114) | [null_array_aggregate](./AA-4.3#S0115) | [named_array_aggregate](./AA-4.3#S0116)

4.3.3:
positional_array_aggregate ::= 
    ([expression](./AA-4.4#S0132), [expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)})
  | ([expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}, others =&gt [expression](./AA-4.4#S0132))
  | ([expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}, others =&gt &lt&gt)
  | '[' [expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}[, others =&gt [expression](./AA-4.4#S0132)] ']'
  | '[' [expression](./AA-4.4#S0132) {, [expression](./AA-4.4#S0132)}, others =&gt &lt&gt ']'

4.3.3:
null_array_aggregate ::= '[' ']'

4.3.3:
named_array_aggregate ::= 
    ([array_component_association_list](./AA-4.3#S0117))
  | '[' [array_component_association_list](./AA-4.3#S0117) ']'

4.3.3:
array_component_association_list ::= 
    [array_component_association](./AA-4.3#S0118) {, [array_component_association](./AA-4.3#S0118)}

4.3.3:
array_component_association ::= 
    [discrete_choice_list](./AA-3.8#S0073) =&gt [expression](./AA-4.4#S0132)
  | [discrete_choice_list](./AA-3.8#S0073) =&gt &lt&gt
  | [iterated_component_association](./AA-4.3#S0119)

4.3.3:
iterated_component_association ::= 
    for [defining_identifier](./AA-3.1#S0022) in [discrete_choice_list](./AA-3.8#S0073) =&gt [expression](./AA-4.4#S0132)
  | for [iterator_specification](./AA-5.5#S0183) =&gt [expression](./AA-4.4#S0132)

4.3.4:
delta_aggregate ::= [record_delta_aggregate](./AA-4.3#S0121) | [array_delta_aggregate](./AA-4.3#S0122)

4.3.4:
record_delta_aggregate ::= 
    (base_[expression](./AA-4.4#S0132) with delta [record_component_association_list](./AA-4.3#S0108))

4.3.4:
array_delta_aggregate ::= 
    (base_[expression](./AA-4.4#S0132) with delta [array_component_association_list](./AA-4.3#S0117))
  | '[' base_[expression](./AA-4.4#S0132) with delta [array_component_association_list](./AA-4.3#S0117) ']'

4.3.5:
container_aggregate ::= 
    [null_container_aggregate](./AA-4.3#S0124)
  | [positional_container_aggregate](./AA-4.3#S0125)
  | [named_container_aggregate](./AA-4.3#S0126)

4.3.5:
null_container_aggregate ::= '[' ']'

4.3.5:
positional_container_aggregate ::= '[' [expression](./AA-4.4#S0132){, [expression](./AA-4.4#S0132)} ']'

4.3.5:
named_container_aggregate ::= '[' [container_element_association_list](./AA-4.3#S0127) ']'

4.3.5:
container_element_association_list ::= 
    [container_element_association](./AA-4.3#S0128) {, [container_element_association](./AA-4.3#S0128)}

4.3.5:
container_element_association ::= 
    [key_choice_list](./AA-4.3#S0129) =&gt [expression](./AA-4.4#S0132)
  | [key_choice_list](./AA-4.3#S0129) =&gt &lt&gt
  | [iterated_element_association](./AA-4.3#S0131)

4.3.5:
key_choice_list ::= [key_choice](./AA-4.3#S0130) {'|' [key_choice](./AA-4.3#S0130)}

4.3.5:
key_choice ::= key_[expression](./AA-4.4#S0132) | [discrete_range](./AA-3.6#S0058)

4.3.5:
iterated_element_association ::= 
    for [loop_parameter_specification](./AA-5.5#S0181)[ use key_[expression](./AA-4.4#S0132)] =&gt [expression](./AA-4.4#S0132)
  | for [iterator_specification](./AA-5.5#S0183)[ use key_[expression](./AA-4.4#S0132)] =&gt [expression](./AA-4.4#S0132)

4.4:
expression ::= 
     [relation](./AA-4.4#S0135) {and [relation](./AA-4.4#S0135)} 	| [relation](./AA-4.4#S0135) {and then [relation](./AA-4.4#S0135)}
   | [relation](./AA-4.4#S0135) {or [relation](./AA-4.4#S0135)} 	| [relation](./AA-4.4#S0135) {or else [relation](./AA-4.4#S0135)}
   | [relation](./AA-4.4#S0135) {xor [relation](./AA-4.4#S0135)}

4.4:
choice_expression ::= 
     [choice_relation](./AA-4.4#S0134) {and [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {or [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {xor [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {and then [choice_relation](./AA-4.4#S0134)}
   | [choice_relation](./AA-4.4#S0134) {or else [choice_relation](./AA-4.4#S0134)}

4.4:
choice_relation ::= 
     [simple_expression](./AA-4.4#S0138) [[relational_operator](./AA-4.5#S0143) [simple_expression](./AA-4.4#S0138)]

4.4:
relation ::= 
     [simple_expression](./AA-4.4#S0138) [[relational_operator](./AA-4.5#S0143) [simple_expression](./AA-4.4#S0138)]
   | tested_[simple_expression](./AA-4.4#S0138) [not] in [membership_choice_list](./AA-4.4#S0136)
   | [raise_expression](./AA-11.3#S0309)

4.4:
membership_choice_list ::= [membership_choice](./AA-4.4#S0137) {'|' [membership_choice](./AA-4.4#S0137)}

4.4:
membership_choice ::= choice_[simple_expression](./AA-4.4#S0138) | [range](./AA-3.5#S0037) | [subtype_mark](./AA-3.2#S0028)

4.4:
simple_expression ::= [[unary_adding_operator](./AA-4.5#S0145)] [term](./AA-4.4#S0139) {[binary_adding_operator](./AA-4.5#S0144) [term](./AA-4.4#S0139)}

4.4:
term ::= [factor](./AA-4.4#S0140) {[multiplying_operator](./AA-4.5#S0146) [factor](./AA-4.4#S0140)}

4.4:
factor ::= [primary](./AA-4.4#S0141) [** [primary](./AA-4.4#S0141)] | abs [primary](./AA-4.4#S0141) | not [primary](./AA-4.4#S0141)

4.4:
primary ::= 
    [numeric_literal](./AA-2.4#S0006) | null | [string_literal](./AA-2.6#S0016) | [aggregate](./AA-4.3#S0106)
  | [name](./AA-4.1#S0091) | [allocator](./AA-4.8#S0164) | ([expression](./AA-4.4#S0132))
  | ([conditional_expression](./AA-4.5#S0148)) | ([quantified_expression](./AA-4.5#S0153))
  | ([declare_expression](./AA-4.5#S0156))

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

4.5.7:
conditional_expression ::= [if_expression](./AA-4.5#S0149) | [case_expression](./AA-4.5#S0151)

4.5.7:
if_expression ::= 
   if [condition](./AA-4.5#S0150) then dependent_[expression](./AA-4.4#S0132)
   {elsif [condition](./AA-4.5#S0150) then dependent_[expression](./AA-4.4#S0132)}
   [else dependent_[expression](./AA-4.4#S0132)]

4.5.7:
condition ::= boolean_[expression](./AA-4.4#S0132)

4.5.7:
case_expression ::= 
    case selecting_[expression](./AA-4.4#S0132) is
    [case_expression_alternative](./AA-4.5#S0152) {,
    [case_expression_alternative](./AA-4.5#S0152)}

4.5.7:
case_expression_alternative ::= 
    when [discrete_choice_list](./AA-3.8#S0073) =&gt
        dependent_[expression](./AA-4.4#S0132)

4.5.8:
quantified_expression ::= for [quantifier](./AA-4.5#S0154) [loop_parameter_specification](./AA-5.5#S0181) =&gt [predicate](./AA-4.5#S0155)
  | for [quantifier](./AA-4.5#S0154) [iterator_specification](./AA-5.5#S0183) =&gt [predicate](./AA-4.5#S0155)

4.5.8:
quantifier ::= all | some

4.5.8:
predicate ::= boolean_[expression](./AA-4.4#S0132)

4.5.9:
declare_expression ::= 
    declare {[declare_item](./AA-4.5#S0157)}
    begin body_[expression](./AA-4.4#S0132)

4.5.9:
declare_item ::= [object_declaration](./AA-3.3#S0032) | [object_renaming_declaration](./AA-8.5#S0239)

4.5.10:
reduction_attribute_reference ::= 
    [value_sequence](./AA-4.5#S0159)'[reduction_attribute_designator](./AA-4.5#S0160)
  | [prefix](./AA-4.1#S0093)'[reduction_attribute_designator](./AA-4.5#S0160)

4.5.10:
value_sequence ::= 
    '[' [parallel[([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)]]
        [iterated_element_association](./AA-4.3#S0131) ']'

4.5.10:
reduction_attribute_designator ::= reduction_[identifier](./AA-2.3#S0002)([reduction_specification](./AA-4.5#S0161))

4.5.10:
reduction_specification ::= reducer_[name](./AA-4.1#S0091), initial_value_[expression](./AA-4.4#S0132)

4.6:
type_conversion ::= 
    [subtype_mark](./AA-3.2#S0028)([expression](./AA-4.4#S0132))
  | [subtype_mark](./AA-3.2#S0028)([name](./AA-4.1#S0091))

4.7:
qualified_expression ::= 
   [subtype_mark](./AA-3.2#S0028)'([expression](./AA-4.4#S0132)) | [subtype_mark](./AA-3.2#S0028)'[aggregate](./AA-4.3#S0106)

4.8:
allocator ::= 
   new [[subpool_specification](./AA-4.8#S0165)] [subtype_indication](./AA-3.2#S0027)
 | new [[subpool_specification](./AA-4.8#S0165)] [qualified_expression](./AA-4.7#S0163)

4.8:
subpool_specification ::= (subpool_handle_[name](./AA-4.1#S0091))

5.1:
sequence_of_statements ::= [statement](./AA-5.1#S0167) {[statement](./AA-5.1#S0167)} {[label](./AA-5.1#S0171)}

5.1:
statement ::= 
   {[label](./AA-5.1#S0171)} [simple_statement](./AA-5.1#S0168) | {[label](./AA-5.1#S0171)} [compound_statement](./AA-5.1#S0169)

5.1:
simple_statement ::= [null_statement](./AA-5.1#S0170)
   | [assignment_statement](./AA-5.2#S0173)	| [exit_statement](./AA-5.7#S0193)
   | [goto_statement](./AA-5.8#S0194)	| [procedure_call_statement](./AA-6.4#S0217)
   | [simple_return_statement](./AA-6.5#S0222)	| [entry_call_statement](./AA-9.5#S0264)
   | [requeue_statement](./AA-9.5#S0265)	| [delay_statement](./AA-9.6#S0266)
   | [abort_statement](./AA-9.8#S0284)	| [raise_statement](./AA-11.3#S0308)
   | [code_statement](./AA-13.8#S0357)

5.1:
compound_statement ::= 
     [if_statement](./AA-5.3#S0175)	| [case_statement](./AA-5.4#S0176)
   | [loop_statement](./AA-5.5#S0178)	| [block_statement](./AA-5.6#S0191)
   | [extended_return_statement](./AA-6.5#S0225)
   | [parallel_block_statement](./AA-5.6#S0192)
   | [accept_statement](./AA-9.5#S0258)	| [select_statement](./AA-9.7#S0269)

5.1:
null_statement ::= null;

5.1:
label ::= &lt&ltlabel_[statement_identifier](./AA-5.1#S0172)&gt&gt

5.1:
statement_identifier ::= [direct_name](./AA-4.1#S0092)

5.2:
assignment_statement ::= 
   variable_[name](./AA-4.1#S0091) := [expression](./AA-4.4#S0132);

5.2.1:
target_name ::= @

5.3:
if_statement ::= 
    if [condition](./AA-4.5#S0150) then
      [sequence_of_statements](./AA-5.1#S0166)
   {elsif [condition](./AA-4.5#S0150) then
      [sequence_of_statements](./AA-5.1#S0166)}
   [else
      [sequence_of_statements](./AA-5.1#S0166)]
    end if;

5.4:
case_statement ::= 
   case selecting_[expression](./AA-4.4#S0132) is
       [case_statement_alternative](./AA-5.4#S0177)
      {[case_statement_alternative](./AA-5.4#S0177)}
   end case;

5.4:
case_statement_alternative ::= 
   when [discrete_choice_list](./AA-3.8#S0073) =&gt
      [sequence_of_statements](./AA-5.1#S0166)

5.5:
loop_statement ::= 
   [loop_[statement_identifier](./AA-5.1#S0172):]
      [[iteration_scheme](./AA-5.5#S0179)] loop
         [sequence_of_statements](./AA-5.1#S0166)
       end loop [loop_[identifier](./AA-2.3#S0002)];

5.5:
iteration_scheme ::= while [condition](./AA-4.5#S0150)
   | for [loop_parameter_specification](./AA-5.5#S0181)
   | for [iterator_specification](./AA-5.5#S0183)
   | [parallel [[aspect_specification](./AA-13.1#S0346)]]
     for [procedural_iterator](./AA-5.5#S0185)
   | parallel [([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)]
     for [loop_parameter_specification](./AA-5.5#S0181)
   | parallel [([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)]
     for [iterator_specification](./AA-5.5#S0183)

5.5:
chunk_specification ::= 
     integer_[simple_expression](./AA-4.4#S0138)
   | [defining_identifier](./AA-3.1#S0022) in [discrete_subtype_definition](./AA-3.6#S0055)

5.5:
loop_parameter_specification ::= 
   [defining_identifier](./AA-3.1#S0022) in [reverse] [discrete_subtype_definition](./AA-3.6#S0055)
     [[iterator_filter](./AA-5.5#S0182)]

5.5:
iterator_filter ::= when [condition](./AA-4.5#S0150)

5.5.2:
iterator_specification ::= 
    [defining_identifier](./AA-3.1#S0022) [: [loop_parameter_subtype_indication](./AA-5.5#S0184)] in [reverse] iterator_[name](./AA-4.1#S0091)
      [[iterator_filter](./AA-5.5#S0182)]
  | [defining_identifier](./AA-3.1#S0022) [: [loop_parameter_subtype_indication](./AA-5.5#S0184)] of [reverse] iterable_[name](./AA-4.1#S0091)
      [[iterator_filter](./AA-5.5#S0182)]

5.5.2:
loop_parameter_subtype_indication ::= [subtype_indication](./AA-3.2#S0027) | [access_definition](./AA-3.10#S0084)

5.5.3:
procedural_iterator ::= 
     [iterator_parameter_specification](./AA-5.5#S0186) of [iterator_procedure_call](./AA-5.5#S0187)
       [[iterator_filter](./AA-5.5#S0182)]

5.5.3:
iterator_parameter_specification ::= 
     [formal_part](./AA-6.1#S0206)
   | ([defining_identifier](./AA-3.1#S0022){, [defining_identifier](./AA-3.1#S0022)})

5.5.3:
iterator_procedure_call ::= 
     procedure_[name](./AA-4.1#S0091)
   | procedure_[prefix](./AA-4.1#S0093) [iterator_actual_parameter_part](./AA-5.5#S0188)

5.5.3:
iterator_actual_parameter_part ::= 
     ([iterator_parameter_association](./AA-5.5#S0189) {, [iterator_parameter_association](./AA-5.5#S0189)})

5.5.3:
iterator_parameter_association ::= 
     [parameter_association](./AA-6.4#S0220)
   | [parameter_association_with_box](./AA-5.5#S0190)

5.5.3:
parameter_association_with_box ::= 
   [ formal_parameter_[selector_name](./AA-4.1#S0099) =&gt ] &lt&gt

5.6:
block_statement ::= 
   [block_[statement_identifier](./AA-5.1#S0172):]
       [declare
            [declarative_part](./AA-3.11#S0086)]
        begin
            [handled_sequence_of_statements](./AA-11.2#S0304)
        end [block_[identifier](./AA-2.3#S0002)];

5.6.1:
parallel_block_statement ::= 
    parallel [([chunk_specification](./AA-5.5#S0180))] [[aspect_specification](./AA-13.1#S0346)] do
       [sequence_of_statements](./AA-5.1#S0166)
    and
       [sequence_of_statements](./AA-5.1#S0166)
   {and
       [sequence_of_statements](./AA-5.1#S0166)}
    end do;

5.7:
exit_statement ::= 
   exit [loop_[name](./AA-4.1#S0091)] [when [condition](./AA-4.5#S0150)];

5.8:
goto_statement ::= goto label_[name](./AA-4.1#S0091);

6.1:
subprogram_declaration ::= 
    [[overriding_indicator](./AA-8.3#S0234)]
    [subprogram_specification](./AA-6.1#S0196)
        [[aspect_specification](./AA-13.1#S0346)];

6.1:
subprogram_specification ::= 
    [procedure_specification](./AA-6.1#S0197)
  | [function_specification](./AA-6.1#S0198)

6.1:
procedure_specification ::= procedure [defining_program_unit_name](./AA-6.1#S0201) [parameter_profile](./AA-6.1#S0204)

6.1:
function_specification ::= function [defining_designator](./AA-6.1#S0200) [parameter_and_result_profile](./AA-6.1#S0205)

6.1:
designator ::= [[parent_unit_name](./AA-10.1#S0291) . ][identifier](./AA-2.3#S0002) | [operator_symbol](./AA-6.1#S0202)

6.1:
defining_designator ::= [defining_program_unit_name](./AA-6.1#S0201) | [defining_operator_symbol](./AA-6.1#S0203)

6.1:
defining_program_unit_name ::= [[parent_unit_name](./AA-10.1#S0291) . ][defining_identifier](./AA-3.1#S0022)

6.1:
operator_symbol ::= [string_literal](./AA-2.6#S0016)

6.1:
defining_operator_symbol ::= [operator_symbol](./AA-6.1#S0202)

6.1:
parameter_profile ::= [[formal_part](./AA-6.1#S0206)]

6.1:
parameter_and_result_profile ::= 
    [[formal_part](./AA-6.1#S0206)] return [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028)
  | [[formal_part](./AA-6.1#S0206)] return [access_definition](./AA-3.10#S0084)

6.1:
formal_part ::= 
   ([parameter_specification](./AA-6.1#S0207) {; [parameter_specification](./AA-6.1#S0207)})

6.1:
parameter_specification ::= 
    [defining_identifier_list](./AA-3.3#S0033) : [aliased] [mode](./AA-6.1#S0208) [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028) [:= [default_expression](./AA-3.7#S0063)]
        [[aspect_specification](./AA-13.1#S0346)]
  | [defining_identifier_list](./AA-3.3#S0033) : [access_definition](./AA-3.10#S0084) [:= [default_expression](./AA-3.7#S0063)]
        [[aspect_specification](./AA-13.1#S0346)]

6.1:
mode ::= [in] | in out | out

6.1.2:
global_aspect_definition ::= 
    null
  | Unspecified
  | [global_mode](./AA-6.1#S0211) [global_designator](./AA-6.1#S0214)
  | ([global_aspect_element](./AA-6.1#S0210){; [global_aspect_element](./AA-6.1#S0210)})

6.1.2:
global_aspect_element ::= 
    [global_mode](./AA-6.1#S0211) [global_set](./AA-6.1#S0213)
  | [global_mode](./AA-6.1#S0211) all
  | [global_mode](./AA-6.1#S0211) synchronized

6.1.2:
global_mode ::= 
    [basic_global_mode](./AA-6.1#S0212)
  | [extended_global_mode](./AA-H.7#S0361)

6.1.2:
basic_global_mode ::= in | in out | out

6.1.2:
global_set ::= [global_name](./AA-6.1#S0215) {, [global_name](./AA-6.1#S0215)}

6.1.2:
global_designator ::= all | synchronized | [global_name](./AA-6.1#S0215)

6.1.2:
global_name ::= object_[name](./AA-4.1#S0091) | package_[name](./AA-4.1#S0091)

6.3:
subprogram_body ::= 
    [[overriding_indicator](./AA-8.3#S0234)]
    [subprogram_specification](./AA-6.1#S0196)
       [[aspect_specification](./AA-13.1#S0346)] is
       [declarative_part](./AA-3.11#S0086)
    begin
        [handled_sequence_of_statements](./AA-11.2#S0304)
    end [[designator](./AA-6.1#S0199)];

6.4:
procedure_call_statement ::= 
    procedure_[name](./AA-4.1#S0091);
  | procedure_[prefix](./AA-4.1#S0093) [actual_parameter_part](./AA-6.4#S0219);

6.4:
function_call ::= 
    function_[name](./AA-4.1#S0091)
  | function_[prefix](./AA-4.1#S0093) [actual_parameter_part](./AA-6.4#S0219)

6.4:
actual_parameter_part ::= 
    ([parameter_association](./AA-6.4#S0220) {, [parameter_association](./AA-6.4#S0220)})

6.4:
parameter_association ::= 
   [formal_parameter_[selector_name](./AA-4.1#S0099) =&gt] [explicit_actual_parameter](./AA-6.4#S0221)

6.4:
explicit_actual_parameter ::= [expression](./AA-4.4#S0132) | variable_[name](./AA-4.1#S0091)

6.5:
simple_return_statement ::= return [[expression](./AA-4.4#S0132)];

6.5:
extended_return_object_declaration ::= 
    [defining_identifier](./AA-3.1#S0022) : [aliased][constant] [return_subtype_indication](./AA-6.5#S0226) [:= [expression](./AA-4.4#S0132)]
        [[aspect_specification](./AA-13.1#S0346)] 

6.5:
extended_return_statement ::= 
    return [extended_return_object_declaration](./AA-6.5#S0224) [do
        [handled_sequence_of_statements](./AA-11.2#S0304)
    end return];

6.5:
return_subtype_indication ::= [subtype_indication](./AA-3.2#S0027) | [access_definition](./AA-3.10#S0084)

6.7:
null_procedure_declaration ::= 
   [[overriding_indicator](./AA-8.3#S0234)]
   [procedure_specification](./AA-6.1#S0197) is null
       [[aspect_specification](./AA-13.1#S0346)];

6.8:
expression_function_declaration ::= 
   [[overriding_indicator](./AA-8.3#S0234)]
   [function_specification](./AA-6.1#S0198) is
       ([expression](./AA-4.4#S0132))
       [[aspect_specification](./AA-13.1#S0346)];
 | [[overriding_indicator](./AA-8.3#S0234)]
   [function_specification](./AA-6.1#S0198) is
       [aggregate](./AA-4.3#S0106)
       [[aspect_specification](./AA-13.1#S0346)];

7.1:
package_declaration ::= [package_specification](./AA-7.1#S0230);

7.1:
package_specification ::= 
    package [defining_program_unit_name](./AA-6.1#S0201)
        [[aspect_specification](./AA-13.1#S0346)] is
      {[basic_declarative_item](./AA-3.11#S0088)}
   [private
      {[basic_declarative_item](./AA-3.11#S0088)}]
    end [[[parent_unit_name](./AA-10.1#S0291).][identifier](./AA-2.3#S0002)]

7.2:
package_body ::= 
    package body [defining_program_unit_name](./AA-6.1#S0201)
        [[aspect_specification](./AA-13.1#S0346)] is
       [declarative_part](./AA-3.11#S0086)
   [begin
        [handled_sequence_of_statements](./AA-11.2#S0304)]
    end [[[parent_unit_name](./AA-10.1#S0291).][identifier](./AA-2.3#S0002)];

7.3:
private_type_declaration ::= 
   type [defining_identifier](./AA-3.1#S0022) [[discriminant_part](./AA-3.7#S0059)] is [[abstract] tagged] [limited] private
      [[aspect_specification](./AA-13.1#S0346)];

7.3:
private_extension_declaration ::= 
   type [defining_identifier](./AA-3.1#S0022) [[discriminant_part](./AA-3.7#S0059)] is
     [abstract] [limited | synchronized] new ancestor_[subtype_indication](./AA-3.2#S0027)
     [and [interface_list](./AA-3.9#S0078)] with private
       [[aspect_specification](./AA-13.1#S0346)];

8.3.1:
overriding_indicator ::= [not] overriding

8.4:
use_clause ::= [use_package_clause](./AA-8.4#S0236) | [use_type_clause](./AA-8.4#S0237)

8.4:
use_package_clause ::= use package_[name](./AA-4.1#S0091) {, package_[name](./AA-4.1#S0091)};

8.4:
use_type_clause ::= use [all] type [subtype_mark](./AA-3.2#S0028) {, [subtype_mark](./AA-3.2#S0028)};

8.5:
renaming_declaration ::= 
      [object_renaming_declaration](./AA-8.5#S0239)
    | [exception_renaming_declaration](./AA-8.5#S0240)
    | [package_renaming_declaration](./AA-8.5#S0241)
    | [subprogram_renaming_declaration](./AA-8.5#S0242)
    | [generic_renaming_declaration](./AA-8.5#S0243)

8.5.1:
object_renaming_declaration ::= 
    [defining_identifier](./AA-3.1#S0022) [: [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028)] renames object_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];
  | [defining_identifier](./AA-3.1#S0022) : [access_definition](./AA-3.10#S0084) renames object_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];

8.5.2:
exception_renaming_declaration ::= [defining_identifier](./AA-3.1#S0022) : exception renames exception_[name](./AA-4.1#S0091)
   [[aspect_specification](./AA-13.1#S0346)];

8.5.3:
package_renaming_declaration ::= package [defining_program_unit_name](./AA-6.1#S0201) renames package_[name](./AA-4.1#S0091)
   [[aspect_specification](./AA-13.1#S0346)];

8.5.4:
subprogram_renaming_declaration ::= 
    [[overriding_indicator](./AA-8.3#S0234)]
    [subprogram_specification](./AA-6.1#S0196) renames callable_entity_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];

8.5.5:
generic_renaming_declaration ::= 
    generic package	[defining_program_unit_name](./AA-6.1#S0201) renames generic_package_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];
  | generic procedure	[defining_program_unit_name](./AA-6.1#S0201) renames generic_procedure_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];
  | generic function	[defining_program_unit_name](./AA-6.1#S0201) renames generic_function_[name](./AA-4.1#S0091)
        [[aspect_specification](./AA-13.1#S0346)];

9.1:
task_type_declaration ::= 
   task type [defining_identifier](./AA-3.1#S0022) [[known_discriminant_part](./AA-3.7#S0061)]
        [[aspect_specification](./AA-13.1#S0346)] [is
     [new [interface_list](./AA-3.9#S0078) with]
     [task_definition](./AA-9.1#S0246)];

9.1:
single_task_declaration ::= 
   task [defining_identifier](./AA-3.1#S0022) 
        [[aspect_specification](./AA-13.1#S0346)][is
     [new [interface_list](./AA-3.9#S0078) with]
     [task_definition](./AA-9.1#S0246)];

9.1:
task_definition ::= 
     {[task_item](./AA-9.1#S0247)}
  [ private
     {[task_item](./AA-9.1#S0247)}]
  end [task_[identifier](./AA-2.3#S0002)]

9.1:
task_item ::= [entry_declaration](./AA-9.5#S0257) | [aspect_clause](./AA-13.1#S0343)

9.1:
task_body ::= 
   task body [defining_identifier](./AA-3.1#S0022)
        [[aspect_specification](./AA-13.1#S0346)] is
     [declarative_part](./AA-3.11#S0086)
   begin
     [handled_sequence_of_statements](./AA-11.2#S0304)
   end [task_[identifier](./AA-2.3#S0002)];

9.4:
protected_type_declaration ::= 
  protected type [defining_identifier](./AA-3.1#S0022) [[known_discriminant_part](./AA-3.7#S0061)]
        [[aspect_specification](./AA-13.1#S0346)] is
     [new [interface_list](./AA-3.9#S0078) with]
     [protected_definition](./AA-9.4#S0251);

9.4:
single_protected_declaration ::= 
  protected [defining_identifier](./AA-3.1#S0022)
        [[aspect_specification](./AA-13.1#S0346)] is
     [new [interface_list](./AA-3.9#S0078) with]
     [protected_definition](./AA-9.4#S0251);

9.4:
protected_definition ::= 
    { [protected_operation_declaration](./AA-9.4#S0252) }
[ private
    { [protected_element_declaration](./AA-9.4#S0253) } ]
  end [protected_[identifier](./AA-2.3#S0002)]

9.4:
protected_operation_declaration ::= [subprogram_declaration](./AA-6.1#S0195)
     | [entry_declaration](./AA-9.5#S0257)
     | [aspect_clause](./AA-13.1#S0343)

9.4:
protected_element_declaration ::= [protected_operation_declaration](./AA-9.4#S0252)
     | [component_declaration](./AA-3.8#S0070)

9.4:
protected_body ::= 
  protected body [defining_identifier](./AA-3.1#S0022)
        [[aspect_specification](./AA-13.1#S0346)] is
   { [protected_operation_item](./AA-9.4#S0255) }
  end [protected_[identifier](./AA-2.3#S0002)];

9.4:
protected_operation_item ::= [subprogram_declaration](./AA-6.1#S0195)
     | [subprogram_body](./AA-6.3#S0216)
     | [null_procedure_declaration](./AA-6.7#S0227)
     | [expression_function_declaration](./AA-6.8#S0228)
     | [entry_body](./AA-9.5#S0260)
     | [aspect_clause](./AA-13.1#S0343)

9.5:
synchronization_kind ::= By_Entry | By_Protected_Procedure | Optional

9.5.2:
entry_declaration ::= 
   [[overriding_indicator](./AA-8.3#S0234)]
   entry [defining_identifier](./AA-3.1#S0022) [([discrete_subtype_definition](./AA-3.6#S0055))] [parameter_profile](./AA-6.1#S0204)
      [[aspect_specification](./AA-13.1#S0346)];

9.5.2:
accept_statement ::= 
   accept entry_[direct_name](./AA-4.1#S0092) [([entry_index](./AA-9.5#S0259))] [parameter_profile](./AA-6.1#S0204) [do
     [handled_sequence_of_statements](./AA-11.2#S0304)
   end [entry_[identifier](./AA-2.3#S0002)]];

9.5.2:
entry_index ::= [expression](./AA-4.4#S0132)

9.5.2:
entry_body ::= 
    entry [defining_identifier](./AA-3.1#S0022) [entry_body_formal_part](./AA-9.5#S0261)
       [[aspect_specification](./AA-13.1#S0346)]
    [entry_barrier](./AA-9.5#S0262) is
       [declarative_part](./AA-3.11#S0086)
    begin
       [handled_sequence_of_statements](./AA-11.2#S0304)
    end [entry_[identifier](./AA-2.3#S0002)];

9.5.2:
entry_body_formal_part ::= [([entry_index_specification](./AA-9.5#S0263))] [parameter_profile](./AA-6.1#S0204)

9.5.2:
entry_barrier ::= when [condition](./AA-4.5#S0150)

9.5.2:
entry_index_specification ::= for [defining_identifier](./AA-3.1#S0022) in [discrete_subtype_definition](./AA-3.6#S0055) [[aspect_specification](./AA-13.1#S0346)] 

9.5.3:
entry_call_statement ::= entry_[name](./AA-4.1#S0091) [[actual_parameter_part](./AA-6.4#S0219)];

9.5.4:
requeue_statement ::= requeue procedure_or_entry_[name](./AA-4.1#S0091) [with abort];

9.6:
delay_statement ::= [delay_until_statement](./AA-9.6#S0267) | [delay_relative_statement](./AA-9.6#S0268)

9.6:
delay_until_statement ::= delay until delay_[expression](./AA-4.4#S0132);

9.6:
delay_relative_statement ::= delay delay_[expression](./AA-4.4#S0132);

9.7:
select_statement ::= 
   [selective_accept](./AA-9.7#S0270)
  | [timed_entry_call](./AA-9.7#S0276)
  | [conditional_entry_call](./AA-9.7#S0279)
  | [asynchronous_select](./AA-9.7#S0280)

9.7.1:
selective_accept ::= 
  select
   [[guard](./AA-9.7#S0271)]
     [select_alternative](./AA-9.7#S0272)
{ or
   [[guard](./AA-9.7#S0271)]
     [select_alternative](./AA-9.7#S0272) }
[ else
   [sequence_of_statements](./AA-5.1#S0166) ]
  end select;

9.7.1:
guard ::= when [condition](./AA-4.5#S0150) =&gt

9.7.1:
select_alternative ::= 
   [accept_alternative](./AA-9.7#S0273)
  | [delay_alternative](./AA-9.7#S0274)
  | [terminate_alternative](./AA-9.7#S0275)

9.7.1:
accept_alternative ::= 
  [accept_statement](./AA-9.5#S0258) [[sequence_of_statements](./AA-5.1#S0166)]

9.7.1:
delay_alternative ::= 
  [delay_statement](./AA-9.6#S0266) [[sequence_of_statements](./AA-5.1#S0166)]

9.7.1:
terminate_alternative ::= terminate;

9.7.2:
timed_entry_call ::= 
  select
   [entry_call_alternative](./AA-9.7#S0277)
  or
   [delay_alternative](./AA-9.7#S0274)
  end select;

9.7.2:
entry_call_alternative ::= 
  [procedure_or_entry_call](./AA-9.7#S0278) [[sequence_of_statements](./AA-5.1#S0166)]

9.7.2:
procedure_or_entry_call ::= 
  [procedure_call_statement](./AA-6.4#S0217) | [entry_call_statement](./AA-9.5#S0264)

9.7.3:
conditional_entry_call ::= 
  select
   [entry_call_alternative](./AA-9.7#S0277)
  else
   [sequence_of_statements](./AA-5.1#S0166)
  end select;

9.7.4:
asynchronous_select ::= 
  select
   [triggering_alternative](./AA-9.7#S0281)
  then abort
   [abortable_part](./AA-9.7#S0283)
  end select;

9.7.4:
triggering_alternative ::= [triggering_statement](./AA-9.7#S0282) [[sequence_of_statements](./AA-5.1#S0166)]

9.7.4:
triggering_statement ::= [procedure_or_entry_call](./AA-9.7#S0278) | [delay_statement](./AA-9.6#S0266)

9.7.4:
abortable_part ::= [sequence_of_statements](./AA-5.1#S0166)

9.8:
abort_statement ::= abort task_[name](./AA-4.1#S0091) {, task_[name](./AA-4.1#S0091)};

10.1.1:
compilation ::= {[compilation_unit](./AA-10.1#S0286)}

10.1.1:
compilation_unit ::= 
    [context_clause](./AA-10.1#S0292) [library_item](./AA-10.1#S0287)
  | [context_clause](./AA-10.1#S0292) [subunit](./AA-10.1#S0302)

10.1.1:
library_item ::= [private] [library_unit_declaration](./AA-10.1#S0288)
  | [library_unit_body](./AA-10.1#S0290)
  | [private] [library_unit_renaming_declaration](./AA-10.1#S0289)

10.1.1:
library_unit_declaration ::= 
     [subprogram_declaration](./AA-6.1#S0195)	| [package_declaration](./AA-7.1#S0229)
   | [generic_declaration](./AA-12.1#S0310)	| [generic_instantiation](./AA-12.3#S0315)

10.1.1:
library_unit_renaming_declaration ::= 
   [package_renaming_declaration](./AA-8.5#S0241)
 | [generic_renaming_declaration](./AA-8.5#S0243)
 | [subprogram_renaming_declaration](./AA-8.5#S0242)

10.1.1:
library_unit_body ::= [subprogram_body](./AA-6.3#S0216) | [package_body](./AA-7.2#S0231)

10.1.1:
parent_unit_name ::= [name](./AA-4.1#S0091)

10.1.2:
context_clause ::= {[context_item](./AA-10.1#S0293)}

10.1.2:
context_item ::= [with_clause](./AA-10.1#S0294) | [use_clause](./AA-8.4#S0235)

10.1.2:
with_clause ::= [limited_with_clause](./AA-10.1#S0295) | [nonlimited_with_clause](./AA-10.1#S0296)

10.1.2:
limited_with_clause ::= limited [private] with library_unit_[name](./AA-4.1#S0091) {, library_unit_[name](./AA-4.1#S0091)};

10.1.2:
nonlimited_with_clause ::= [private] with library_unit_[name](./AA-4.1#S0091) {, library_unit_[name](./AA-4.1#S0091)};

10.1.3:
body_stub ::= 
   [subprogram_body_stub](./AA-10.1#S0298) | [package_body_stub](./AA-10.1#S0299) | [task_body_stub](./AA-10.1#S0300) | [protected_body_stub](./AA-10.1#S0301)

10.1.3:
subprogram_body_stub ::= 
   [[overriding_indicator](./AA-8.3#S0234)]
   [subprogram_specification](./AA-6.1#S0196) is separate
      [[aspect_specification](./AA-13.1#S0346)];

10.1.3:
package_body_stub ::= 
   package body [defining_identifier](./AA-3.1#S0022) is separate
      [[aspect_specification](./AA-13.1#S0346)];

10.1.3:
task_body_stub ::= 
   task body [defining_identifier](./AA-3.1#S0022) is separate
      [[aspect_specification](./AA-13.1#S0346)];

10.1.3:
protected_body_stub ::= 
   protected body [defining_identifier](./AA-3.1#S0022) is separate
      [[aspect_specification](./AA-13.1#S0346)];

10.1.3:
subunit ::= separate ([parent_unit_name](./AA-10.1#S0291)) [proper_body](./AA-3.11#S0090)

11.1:
exception_declaration ::= [defining_identifier_list](./AA-3.3#S0033) : exception
   [[aspect_specification](./AA-13.1#S0346)];

11.2:
handled_sequence_of_statements ::= 
     [sequence_of_statements](./AA-5.1#S0166)
  [exception
     [exception_handler](./AA-11.2#S0305)
    {[exception_handler](./AA-11.2#S0305)}]

11.2:
exception_handler ::= 
  when [[choice_parameter_specification](./AA-11.2#S0306):] [exception_choice](./AA-11.2#S0307) {'|' [exception_choice](./AA-11.2#S0307)} =&gt
     [sequence_of_statements](./AA-5.1#S0166)

11.2:
choice_parameter_specification ::= [defining_identifier](./AA-3.1#S0022)

11.2:
exception_choice ::= exception_[name](./AA-4.1#S0091) | others

11.3:
raise_statement ::= raise;
      | raise exception_[name](./AA-4.1#S0091) [with string_[expression](./AA-4.4#S0132)];

11.3:
raise_expression ::= raise exception_[name](./AA-4.1#S0091) [with string_[simple_expression](./AA-4.4#S0138)]

12.1:
generic_declaration ::= [generic_subprogram_declaration](./AA-12.1#S0311) | [generic_package_declaration](./AA-12.1#S0312)

12.1:
generic_subprogram_declaration ::= 
     [generic_formal_part](./AA-12.1#S0313)  [subprogram_specification](./AA-6.1#S0196)
        [[aspect_specification](./AA-13.1#S0346)];

12.1:
generic_package_declaration ::= 
     [generic_formal_part](./AA-12.1#S0313)  [package_specification](./AA-7.1#S0230);

12.1:
generic_formal_part ::= generic {[generic_formal_parameter_declaration](./AA-12.1#S0314) | [use_clause](./AA-8.4#S0235)}

12.1:
generic_formal_parameter_declaration ::= 
      [formal_object_declaration](./AA-12.4#S0319)
    | [formal_type_declaration](./AA-12.5#S0320)
    | [formal_subprogram_declaration](./AA-12.6#S0335)
    | [formal_package_declaration](./AA-12.7#S0340)

12.3:
generic_instantiation ::= 
     package [defining_program_unit_name](./AA-6.1#S0201) is
         new generic_package_[name](./AA-4.1#S0091) [[generic_actual_part](./AA-12.3#S0316)]
            [[aspect_specification](./AA-13.1#S0346)];
   | [[overriding_indicator](./AA-8.3#S0234)]
     procedure [defining_program_unit_name](./AA-6.1#S0201) is
         new generic_procedure_[name](./AA-4.1#S0091) [[generic_actual_part](./AA-12.3#S0316)]
            [[aspect_specification](./AA-13.1#S0346)];
   | [[overriding_indicator](./AA-8.3#S0234)]
     function [defining_designator](./AA-6.1#S0200) is
         new generic_function_[name](./AA-4.1#S0091) [[generic_actual_part](./AA-12.3#S0316)]
            [[aspect_specification](./AA-13.1#S0346)];

12.3:
generic_actual_part ::= 
   ([generic_association](./AA-12.3#S0317) {, [generic_association](./AA-12.3#S0317)})

12.3:
generic_association ::= 
   [generic_formal_parameter_[selector_name](./AA-4.1#S0099) =&gt] [explicit_generic_actual_parameter](./AA-12.3#S0318)

12.3:
explicit_generic_actual_parameter ::= [expression](./AA-4.4#S0132) | variable_[name](./AA-4.1#S0091)
   | subprogram_[name](./AA-4.1#S0091) | entry_[name](./AA-4.1#S0091) | [subtype_mark](./AA-3.2#S0028)
   | package_instance_[name](./AA-4.1#S0091)

12.4:
formal_object_declaration ::= 
    [defining_identifier_list](./AA-3.3#S0033) : [mode](./AA-6.1#S0208) [[null_exclusion](./AA-3.10#S0083)] [subtype_mark](./AA-3.2#S0028) [:= [default_expression](./AA-3.7#S0063)]
        [[aspect_specification](./AA-13.1#S0346)];
  |  [defining_identifier_list](./AA-3.3#S0033) : [mode](./AA-6.1#S0208) [access_definition](./AA-3.10#S0084) [:= [default_expression](./AA-3.7#S0063)]
        [[aspect_specification](./AA-13.1#S0346)];

12.5:
formal_type_declaration ::= 
      [formal_complete_type_declaration](./AA-12.5#S0321)
    | [formal_incomplete_type_declaration](./AA-12.5#S0322)

12.5:
formal_complete_type_declaration ::= 
    type [defining_identifier](./AA-3.1#S0022)[[discriminant_part](./AA-3.7#S0059)] is [formal_type_definition](./AA-12.5#S0323)
        [or use default_[subtype_mark](./AA-3.2#S0028)] [[aspect_specification](./AA-13.1#S0346)];

12.5:
formal_incomplete_type_declaration ::= 
    type [defining_identifier](./AA-3.1#S0022)[[discriminant_part](./AA-3.7#S0059)] [is tagged]
        [or use default_[subtype_mark](./AA-3.2#S0028)];

12.5:
formal_type_definition ::= 
      [formal_private_type_definition](./AA-12.5#S0324)
    | [formal_derived_type_definition](./AA-12.5#S0325)
    | [formal_discrete_type_definition](./AA-12.5#S0326)
    | [formal_signed_integer_type_definition](./AA-12.5#S0327)
    | [formal_modular_type_definition](./AA-12.5#S0328)
    | [formal_floating_point_definition](./AA-12.5#S0329)
    | [formal_ordinary_fixed_point_definition](./AA-12.5#S0330)
    | [formal_decimal_fixed_point_definition](./AA-12.5#S0331)
    | [formal_array_type_definition](./AA-12.5#S0332)
    | [formal_access_type_definition](./AA-12.5#S0333)
    | [formal_interface_type_definition](./AA-12.5#S0334)

12.5.1:
formal_private_type_definition ::= [[abstract] tagged] [limited] private

12.5.1:
formal_derived_type_definition ::= 
     [abstract] [limited | synchronized] new [subtype_mark](./AA-3.2#S0028) [[and [interface_list](./AA-3.9#S0078)]with private]

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
formal_array_type_definition ::= [array_type_definition](./AA-3.6#S0051)

12.5.4:
formal_access_type_definition ::= [access_type_definition](./AA-3.10#S0079)

12.5.5:
formal_interface_type_definition ::= [interface_type_definition](./AA-3.9#S0077)

12.6:
formal_subprogram_declaration ::= [formal_concrete_subprogram_declaration](./AA-12.6#S0336)
    | [formal_abstract_subprogram_declaration](./AA-12.6#S0337)

12.6:
formal_concrete_subprogram_declaration ::= 
     with [subprogram_specification](./AA-6.1#S0196) [is [subprogram_default](./AA-12.6#S0338)]
        [[aspect_specification](./AA-13.1#S0346)];

12.6:
formal_abstract_subprogram_declaration ::= 
     with [subprogram_specification](./AA-6.1#S0196) is abstract [[subprogram_default](./AA-12.6#S0338)]
        [[aspect_specification](./AA-13.1#S0346)];

12.6:
subprogram_default ::= [default_name](./AA-12.6#S0339) | &lt&gt | null

12.6:
default_name ::= [name](./AA-4.1#S0091)

12.7:
formal_package_declaration ::= 
    with package [defining_identifier](./AA-3.1#S0022) is new generic_package_[name](./AA-4.1#S0091)  [formal_package_actual_part](./AA-12.7#S0341)
        [[aspect_specification](./AA-13.1#S0346)];

12.7:
formal_package_actual_part ::= 
    ([others =&gt] &lt&gt)
  | [[generic_actual_part](./AA-12.3#S0316)]
  | ([formal_package_association](./AA-12.7#S0342) {, [formal_package_association](./AA-12.7#S0342)} [, others =&gt &lt&gt])

12.7:
formal_package_association ::= 
    [generic_association](./AA-12.3#S0317)
  | generic_formal_parameter_[selector_name](./AA-4.1#S0099) =&gt &lt&gt

13.1:
aspect_clause ::= [attribute_definition_clause](./AA-13.3#S0349)
      | [enumeration_representation_clause](./AA-13.4#S0350)
      | [record_representation_clause](./AA-13.5#S0352)
      | [at_clause](./AA-J.7#S0368)

13.1:
local_name ::= [direct_name](./AA-4.1#S0092)
      | [direct_name](./AA-4.1#S0092)'[attribute_designator](./AA-4.1#S0101)
      | library_unit_[name](./AA-4.1#S0091)

13.1.1:
aspect_specification ::= 
   with [aspect_mark](./AA-13.1#S0347) [=&gt [aspect_definition](./AA-13.1#S0348)] {,
           [aspect_mark](./AA-13.1#S0347) [=&gt [aspect_definition](./AA-13.1#S0348)] }

13.1.1:
aspect_mark ::= aspect_[identifier](./AA-2.3#S0002)['Class]

13.1.1:
aspect_definition ::= 
    [name](./AA-4.1#S0091) | [expression](./AA-4.4#S0132) | [identifier](./AA-2.3#S0002)
  | [aggregate](./AA-4.3#S0106) | [global_aspect_definition](./AA-6.1#S0209)

13.3:
attribute_definition_clause ::= 
      for [local_name](./AA-13.1#S0345)'[attribute_designator](./AA-4.1#S0101) use [expression](./AA-4.4#S0132);
    | for [local_name](./AA-13.1#S0345)'[attribute_designator](./AA-4.1#S0101) use [name](./AA-4.1#S0091);

13.4:
enumeration_representation_clause ::= 
    for first_subtype_[local_name](./AA-13.1#S0345) use [enumeration_aggregate](./AA-13.4#S0351);

13.4:
enumeration_aggregate ::= [array_aggregate](./AA-4.3#S0113)

13.5.1:
record_representation_clause ::= 
    for first_subtype_[local_name](./AA-13.1#S0345) use
       record [[mod_clause](./AA-J.8#S0369)]
          {[component_clause](./AA-13.5#S0353)}
       end record [[local_name](./AA-13.1#S0345)];

13.5.1:
component_clause ::= 
    component_[local_name](./AA-13.1#S0345) at [position](./AA-13.5#S0354) range [first_bit](./AA-13.5#S0355) .. [last_bit](./AA-13.5#S0356);

13.5.1:
position ::= static_[expression](./AA-4.4#S0132)

13.5.1:
first_bit ::= static_[simple_expression](./AA-4.4#S0138)

13.5.1:
last_bit ::= static_[simple_expression](./AA-4.4#S0138)

13.8:
code_statement ::= [qualified_expression](./AA-4.7#S0163);

13.11.3:
storage_pool_indicator ::= storage_pool_[name](./AA-4.1#S0091) | null | Standard

13.12:
restriction ::= restriction_[identifier](./AA-2.3#S0002)
    | restriction_parameter_[identifier](./AA-2.3#S0002) =&gt [restriction_parameter_argument](./AA-13.12#S0360)

13.12:
restriction_parameter_argument ::= [name](./AA-4.1#S0091) | [expression](./AA-4.4#S0132)

H.7:
extended_global_mode ::= 
    overriding [basic_global_mode](./AA-6.1#S0212)

H.7.1:
formal_parameter_set ::= 
    [formal_group_designator](./AA-H.7#S0363)
  | [formal_parameter_name](./AA-H.7#S0364)
  | ([formal_parameter_name](./AA-H.7#S0364){, [formal_parameter_name](./AA-H.7#S0364)})

H.7.1:
formal_group_designator ::= null | all

H.7.1:
formal_parameter_name ::= 
    formal_[subtype_mark](./AA-3.2#S0028)
  | formal_subprogram_[name](./AA-4.1#S0091)
  | formal_access_to_subprogram_object_[name](./AA-4.1#S0091)

H.7.1:
dispatching_operation_set ::= 
    [dispatching_operation_specifier](./AA-H.7#S0366)
  | ([dispatching_operation_specifier](./AA-H.7#S0366){, [dispatching_operation_specifier](./AA-H.7#S0366)})

H.7.1:
dispatching_operation_specifier ::= 
    dispatching_operation_[name](./AA-4.1#S0091) (object_[name](./AA-4.1#S0091))

J.3:
delta_constraint ::= delta static_[simple_expression](./AA-4.4#S0138) [[range_constraint](./AA-3.5#S0036)]

J.7:
at_clause ::= for [direct_name](./AA-4.1#S0092) use at [expression](./AA-4.4#S0132);

J.8:
mod_clause ::= at mod static_[expression](./AA-4.4#S0132);

