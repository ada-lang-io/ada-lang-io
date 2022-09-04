---
sidebar_position:  26
---

# Annex M Implementation-Defined Characteristics

:::warning
We're still working on the Reference manual output.  Internal links are broken,
as are a bunch of other things.
See the [tracking issue](https://github.com/ada-lang-io/ada-lang-io/issues/20)
:::
See C.3.2(24).

See D.13(1).

The Ada language allows for certain machine dependences in a controlled manner. Each Ada implementation must document all implementation-defined characteristics: 

Ramification: It need not document unspecified characteristics.

Some of the items in this list require documentation only for implementations that conform to Specialized Needs Annexes. 

Whether or not each recommendation given in Implementation Advice is followed. See 1.1.1(54).

Capacity limitations of the implementation. See 1.1.1(60).

Variations from the standard that are impractical to avoid given the implementation's execution environment. See 1.1.1(63).

Which code_statements cause external interactions. See 1.1.1(67).

The coded representation for the text of an Ada program. See 2.1(4).

The control functions allowed in comments. See 2.1(14).

The representation for an end of line. See 2.2(2).

Maximum supported line length and lexical element length. See 2.2(14).

Implementation-defined pragmas. See 2.8(14).

Effect of pragma Optimize. See 2.8(27).

See 3.5(31).

The sequence of characters of the value returned by S'Image when some of the graphic characters of S'Wide_Image are not defined in Character. See 3.5(38).

The predefined integer types declared in Standard. See 3.5.4(26).

Any nonstandard integer types and the operators defined for them. See 3.5.4(27).

Any nonstandard real types and the operators defined for them. See 3.5.6(8).

What combinations of requested decimal precision and range are supported for floating point types. See 3.5.7(7).

The predefined floating point types declared in Standard. See 3.5.7(16).

The small of an ordinary fixed point type. See 3.5.9(8).

What combinations of small, range, and digits are supported for fixed point types. See 3.5.9(10).

The result of Tags.Expanded_Name for types declared within an unnamed block_statement. See 3.9(11).

Implementation-defined attributes. See 4.1.4(12).

Any implementation-defined time types. See 9.6(6).

The time base associated with relative delays. See 9.6(20).

The time base of the type Calendar.Time. See 9.6(23).

The timezone used for package Calendar operations. See 9.6(24).

Any limit on delay_until_statements of select_statements. See 9.6(29).

Whether or not two nonoverlapping parts of a composite object are independently addressable, in the case where packing, record layout, or Component_Size is specified for the object. See 9.10(1).

The representation for a compilation. See 10.1(2).

Any restrictions on compilations that contain multiple compilation_units. See 10.1(4).

The mechanisms for creating an environment and for adding and replacing compilation units. See 10.1.4(3).

The manner of explicitly assigning library units to a partition. See 10.2(2).

The implementation-defined means, if any, of specifying which compilation units are needed by a given compilation unit. See 10.2(2).

The manner of designating the main subprogram of a partition. See 10.2(8).

The order of elaboration of library_items. See 10.2(19).

Parameter passing and function return for the main subprogram. See 10.2(22).

The mechanisms for building and running partitions. See 10.2(25).

The details of program execution, including program termination. See 10.2(26).

The semantics of any nonactive partitions supported by the implementation. See 10.2(29).

The information returned by Exception_Message. See 11.4.1(10).

The result of Exceptions.Exception_Name for types declared within an unnamed block_statement. See 11.4.1(12).

The information returned by Exception_Information. See 11.4.1(13).

See 11.4.1(23).

Implementation-defined check names. See 11.5(30).

The interpretation of each aspect of representation. See 13.1(22).

Any restrictions placed upon representation items. See 13.1(22).

The meaning of Size for indefinite subtypes. See 13.3(49).

The default external representation for a type tag. See 13.3(79).

What determines whether a compilation unit is the same in two different partitions. See 13.3(80).

Implementation-defined components. See 13.5.1(15).

If Word_Size = Storage_Unit, the default bit ordering. See 13.5.3(5).

The contents of the visible part of package Systemand its language-defined children. See 13.7(2).

The contents of the visible part of package System.Machine_Code, and the meaning of code_statements. See 13.8(7).

The effect of unchecked conversion. See 13.9(11).

The manner of choosing a storage pool for an access type when Storage_Pool is not specified for the type. See 13.11(18).

Whether or not the implementation provides user-accessible names for the standard pool type(s). See 13.11(18).

The meaning of Storage_Size. See 13.11(19).

Implementation-defined aspects of storage pools. See 13.11(23).

The set of restrictions allowed in a pragma Restrictions. See 13.12(7).

The consequences of violating limitations on Restrictions pragmas. See 13.12(9).

The representation used by the Read and Write attributes of elementary types in terms of stream elements. See 13.13.2(10).

The names and characteristics of the numeric subtypes declared in the visible part of package Standard. See A.1(3).

The accuracy actually achieved by the elementary functions. See A.5.1(1).

The sign of a zero result from some of the operators or functions in Numerics.Generic_Elementary_Functions, when Float_Type'Signed_Zeros is True. See A.5.1(46).

The value of Numerics.Float_Random.Max_Image_Width. See A.5.2(27).

The value of Numerics.Discrete_Random.Max_Image_Width. See A.5.2(27).

The algorithms for random number generation. See A.5.2(32).

The string representation of a random number generator's state. See A.5.2(38).

The minimum time interval between calls to the time-dependent Reset procedure that are guaranteed to initiate different random number sequences. See A.5.2(45).

The values of the Model_Mantissa, Model_Emin, Model_Epsilon, Model, Safe_First, and Safe_Last attributes, if the Numerics Annex is not supported. See A.5.3(73).

Any implementation-defined characteristics of the input-output packages. See A.7(14).

The value of Buffer_Size in Storage_IO. See A.9(10).

external files for standard input, standard output, and standard error See A.10(5).

The accuracy of the value produced by Put. See A.10.9(36).

The meaning of Argument_Count, Argument, and Command_Name. See A.15(1).

Implementation-defined convention names. See B.1(11).

The meaning of link names. See B.1(36).

The manner of choosing link names when neither the link name nor the address of an imported or exported entity is specified. See B.1(36).

The effect of pragma Linker_Options. See B.1(37).

The contents of the visible part of package Interfaces and its language-defined descendants. See B.2(1).

Implementation-defined children of package Interfaces. The contents of the visible part of package Interfaces. See B.2(11).

The types Floating, Long_Floating, Binary, Long_Binary, Decimal_Element, and COBOL_Character; and the initializations of the variables Ada_To_COBOL and COBOL_To_Ada, in Interfaces.COBOL See B.4(50).

Support for access to machine instructions. See C.1(1).

Implementation-defined aspects of access to machine operations. See C.1(9).

Implementation-defined aspects of interrupts. See C.3(2).

See C.3.1(17).

See C.3.1(19).

Implementation-defined aspects of preelaboration. See C.4(13).

The semantics of pragma Discard_Names. See C.5(7).

The result of the Task_Identification.Image attribute. See C.7.1(7).

The value of Current_Task when in a protected entry or interrupt handler. See C.7.1(17).

The effect of calling Current_Task from an entry body or interrupt handler. See C.7.1(19).

Implementation-defined aspects of Task_Attributes. See C.7.2(19).

Values of all Metrics. See D(2).

The declarations of Any_Priority and Priority. See D.1(11).

Implementation-defined execution resources. See D.1(15).

Whether, on a multiprocessor, a task that is waiting for access to a protected object keeps its processor busy. See D.2.1(3).

The affect of implementation defined execution resources on task dispatching. See D.2.1(9).

Implementation-defined policy_identifiers allowed in a pragma Task_Dispatching_Policy. See D.2.2(4).

Implementation-defined aspects of priority inversion. See D.2.2(17).

Implementation defined task dispatching. See D.2.2(19).

Implementation-defined policy_identifiers allowed in a pragma Locking_Policy. See D.3(4).

Default ceiling priorities. See D.3(10).

The ceiling of any protected object used internally by the implementation. See D.3(16).

Implementation-defined queuing policies. See D.4(1).

On a multiprocessor, any conditions that cause the completion of an aborted construct to be delayed later than what is specified for a single processor. See D.6(3).

Any operations that implicitly require heap storage allocation. See D.7(8).

Implementation-defined aspects of pragma Restrictions. See D.7(20).

Implementation-defined aspects of package Real_Time. See D.8(17).

Implementation-defined aspects of delay_statements. See D.9(8).

The upper bound on the duration of interrupt blocking caused by the implementation. See D.12(5).

The means for creating and executing distributed programs. See E(5).

Any events that can result in a partition becoming inaccessible. See E.1(7).

The scheduling policies, treatment of priorities, and management of shared resources between partitions in certain cases. See E.1(11).

Events that cause the version of a compilation unit to change. See E.3(5).

Whether the execution of the remote subprogram is immediately aborted as a result of cancellation. See E.4(13).

Implementation-defined aspects of the PCS. See E.5(25).

Implementation-defined interfaces in the PCS. See E.5(26).

The values of named numbers in the package Decimal. See F.2(7).

The value of Max_Picture_Length in the package Text_IO.Editing See F.3.3(16).

The value of Max_Picture_Length in the package Wide_Text_IO.Editing See F.3.4(5).

The accuracy actually achieved by the complex elementary functions and by other complex arithmetic operations. See G.1(1).

The sign of a zero result (or a component thereof) from any operator or function in Numerics.Generic_Complex_Types, when Real'Signed_Zeros is True. See G.1.1(53).

The sign of a zero result (or a component thereof) from any operator or function in Numerics.Generic_Complex_Elementary_Functions, when Complex_Types.Real'Signed_Zeros is True. See G.1.2(45).

Whether the strict mode or the relaxed mode is the default. See G.2(2).

The result interval in certain cases of fixed-to-float conversion. See G.2.1(10).

The result of a floating point arithmetic operation in overflow situations, when the Machine_Overflows attribute of the result type is False. See G.2.1(13).

The result interval for division (or exponentiation by a negative exponent), when the floating point hardware implements division as multiplication by a reciprocal. See G.2.1(16).

The definition of close result set, which determines the accuracy of certain fixed point multiplications and divisions. See G.2.3(5).

Conditions on a universal_real operand of a fixed point multiplication or division for which the result shall be in the perfect result set. See G.2.3(22).

The result of a fixed point arithmetic operation in overflow situations, when the Machine_Overflows attribute of the result type is False. See G.2.3(27).

The result of an elementary function reference in overflow situations, when the Machine_Overflows attribute of the result type is False. See G.2.4(4).

The value of the angle threshold, within which certain elementary functions, complex arithmetic operations, and complex elementary functions yield results conforming to a maximum relative error bound. See G.2.4(10).

The accuracy of certain elementary functions for parameters beyond the angle threshold. See G.2.4(10).

The result of a complex arithmetic operation or complex elementary function reference in overflow situations, when the Machine_Overflows attribute of the corresponding real type is False. See G.2.6(5).

The accuracy of certain complex arithmetic operations and certain complex elementary functions for parameters (or components thereof) beyond the angle threshold. See G.2.6(8).

Information regarding bounded errors and erroneous execution. See H.2(1).

Implementation-defined aspects of pragma Inspection_Point. See H.3.2(8).

Implementation-defined aspects of pragma Restrictions. See H.4(25).

Any restrictions on pragma Restrictions. See H.4(27).

See 3.9(28).

See 10.1.5(9).

See 13.2(9).

See 13.3(53).

See A.15(22).

See B.1(39).

See B.1(40).

See B.1(41).

See C.5(8).

See G.1.1(58).

See G.1.2(49).

See H.4(29).

