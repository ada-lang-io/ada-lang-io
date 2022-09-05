---
sidebar_position:  213
---

# M.3  Implementation Advice

{AI12-0442-1} This Reference Manual sometimes gives advice about handling certain target machine dependences. Each Ada implementation is required to document whether that advice is followed: 

Ramification: Some of the items in this list require documentation only for implementations that conform to Specialized Needs Annexes. 

Program_Error should be raised when an unsupported Specialized Needs Annex feature is used at run time. See 1.1.1(78).

Implementation-defined extensions to the functionality of a language-defined library unit should be provided by adding children to the library unit. See 1.1.1(79).

If a bounded error or erroneous execution is detected, Program_Error should be raised. See 1.1.1(115).

Implementation-defined pragmas should have no semantic effect for error-free programs. See 2.8(16/3).

Implementation-defined pragmas should not make an illegal program legal, unless they complete a declaration or configure the [library_item](./AA-10.1#S0287)s in an environment. See 2.8(19).

Long_Integer should be declared in Standard if the target supports 32-bit arithmetic. No other named integer subtypes should be declared in Standard. See 3.5.4(28).

For a two's complement target, modular types with a binary modulus up to System.Max_Int*2+2 should be supported. A nonbinary modulus up to Integer'Last should be supported. See 3.5.4(29).

Program_Error should be raised for the evaluation of S'Pos for an enumeration type, if the value of the operand does not correspond to the internal code for any enumeration literal of the type. See 3.5.5(8).

Long_Float should be declared in Standard if the target supports 11 or more digits of precision. No other named float subtypes should be declared in Standard. See 3.5.7(17).

Multidimensional arrays should be represented in row-major order, unless the array has convention Fortran. See 3.6.2(11/3).

Tags.Internal_Tag should return the tag of a type, if one exists, whose innermost master is a master of the point of the function call. See 3.9(27.1/3).

A real static expression with a nonformal type that is not part of a larger static expression should be rounded the same as the target system. See 4.9(38.1/2).

For each language-defined private type T, T'Image should generate an image that would be meaningful based only on the relevant public interfaces. See 4.10(56).

The value of Duration'Small should be no greater than 100 microseconds. See 9.6(30).

The time base for [delay_relative_statement](./AA-9.6#S0268)s should be monotonic. See 9.6(31/5).

Leap seconds should be supported if the target system supports them. Otherwise, operations in Calendar.Formatting should return results consistent with no leap seconds. See 9.6.1(89/2).

This paragraph was deleted.

A type declared in a preelaborated package should have the same representation in every elaboration of a given version of the package. See 10.2.1(12).

Exception_Information should provide information useful for debugging, and should include the Exception_Name and Exception_Message. See 11.4.1(19).

Exception_Message by default should be short, provide information useful for debugging, and should not include the Exception_Name. See 11.4.1(19).

Code executed for checks that have been suppressed should be minimized. See 11.5(30).

The recommended level of support for all representation items should be followed. See 13.1(30/5).

Storage allocated to objects of a packed type should be minimized. See 13.2(6).

The recommended level of support for the Pack aspect should be followed. See 13.2(9).

For an array X, X'Address should point at the first component of the array rather than the array bounds. See 13.3(14).

The recommended level of support for the Address attribute should be followed. See 13.3(19).

For any tagged specific subtype S, S'Class'Alignment should equal S'Alignment. See 13.3(28).

The recommended level of support for the Alignment attribute should be followed. See 13.3(35).

The Size of an array object should not include its bounds. See 13.3(41.1/2).

If the Size of a subtype is nonconfirming and allows for efficient independent addressability, then the Object_Size of the subtype (unless otherwise specified) should equal the Size of the subtype. See 13.3(52).

A Size clause on a composite subtype should not affect the internal layout of components. See 13.3(53).

The recommended level of support for the Size attribute should be followed. See 13.3(56).

An Object_Size clause on a composite type should not affect the internal layout of components. See 13.3(58).

If S is a definite first subtype for which Object_Size is not specified, S'Object_Size should be the smallest multiple of the storage element size larger than or equal to S'Size that is consistent with the alignment of S. See 13.3(58).

The recommended level of support for the Object_Size attribute should be followed. See 13.3(58).

The Size of most objects of a subtype should equal the Object_Size of the subtype. See 13.3(58).

The recommended level of support for the Component_Size attribute should be followed. See 13.3(73).

The recommended level of support for [enumeration_representation_clause](./AA-13.4#S0350)s should be followed. See 13.4(10).

The recommended level of support for [record_representation_clause](./AA-13.5#S0352)s should be followed. See 13.5.1(22).

If a component is represented using a pointer to the actual data of the component which is contiguous with the rest of the object, then the storage place attributes should reflect the place of the actual data. If a component is allocated discontiguously from the rest of the object, then a warning should be generated upon reference to one of its storage place attributes. See 13.5.2(5).

The recommended level of support for the nondefault bit ordering should be followed. See 13.5.3(8).

Type System.Address should be a private type. See 13.7(37).

Operations in System and its children should reflect the target environment; operations that do not make sense should raise Program_Error. See 13.7.1(16).

Since the Size of an array object generally does not include its bounds, the bounds should not be part of the converted data in an instance of Unchecked_Conversion. See 13.9(14/2).

There should not be unnecessary runtime checks on the result of an Unchecked_Conversion; the result should be returned by reference when possible. Restrictions on Unchecked_Conversions should be avoided. See 13.9(15).

The recommended level of support for Unchecked_Conversion should be followed. See 13.9(17).

Any cases in which heap storage is dynamically allocated other than as part of the evaluation of an [allocator](./AA-4.8#S0164) should be documented. See 13.11(24).

A default storage pool for an access-to-constant type should not have overhead to support deallocation of individual objects. See 13.11(25).

Usually, a storage pool for an access discriminant or access parameter should be created at the point of an [allocator](./AA-4.8#S0164), and be reclaimed when the designated object becomes inaccessible. For other anonymous access types, the pool should be created at the point where the type is elaborated and may have no mechanism for the deallocation of individual objects. See 13.11(26).

For a standard storage pool, an instance of Unchecked_Deallocation should actually reclaim the storage. See 13.11.2(17).

A call on an instance of Unchecked_Deallocation with a nonnull access value should raise Program_Error if the actual access type of the instance is a type for which the Storage_Size has been specified to be zero or is defined by the language to be zero. See 13.11.2(17.1/3).

Streams.Storage.Bounded.Stream_Type objects should be implemented without implicit pointers or dynamic allocation. See 13.13.1(37).

If not specified, the value of Stream_Size for an elementary type should be the number of bits that corresponds to the minimum number of stream elements required by the first subtype of the type, rounded up to the nearest factor or multiple of the word size that is also a multiple of the stream element size. See 13.13.2(1.6/2).

The recommended level of support for the Stream_Size attribute should be followed. See 13.13.2(1.8/2).

If an implementation provides additional named predefined integer types, then the names should end with "Integer". If an implementation provides additional named predefined floating point types, then the names should end with "Float". See A.1(52).

Implementation-defined operations on Wide_Character, Wide_String, Wide_Wide_Character, and Wide_Wide_String should be child units of Wide_Characters or Wide_Wide_Characters. See A.3.1(7/3).

The string returned by Wide_Characters.Handling.Character_Set_Version should include either "10646:" or "Unicode". See A.3.5(62).

Bounded string objects should not be implemented by implicit pointers and dynamic allocation. See A.4.4(106).

Strings.Hash should be good a hash function, returning a wide spread of values for different string values, and similar strings should rarely return the same value. See A.4.9(12/2).

If an implementation supports other string encoding schemes, a child of Ada.Strings similar to UTF_Encoding should be defined. See A.4.11(107/3).

Bounded buffer objects should be implemented without dynamic allocation. See A.4.12(36).

Any storage associated with an object of type Generator of the random number packages should be reclaimed on exit from the scope of the object. See A.5.2(46).

Each value of Initiator passed to Reset for the random number packages should initiate a distinct sequence of random numbers, or, if that is not possible, be at least a rapidly varying function of the initiator value. See A.5.2(47).

Get_Immediate should be implemented with unbuffered input; input should be available immediately; line-editing should be disabled. See A.10.7(23).

Package Directories.Information should be provided to retrieve other information about a file. See A.16(124/2).

Directories.Start_Search and Directories.Search should raise Name_Error for malformed patterns. See A.16(125).

Directories.Rename should be supported at least when both New_Name and Old_Name are simple names and New_Name does not identify an existing external file. See A.16(126/2).

Directories.Hierarchical_File_Names should be provided for systems with hierarchical file naming, and should not be provided on other systems. See A.16.1(36/3).

If the execution environment supports subprocesses, the current environment variables should be used to initialize the environment variables of a subprocess. See A.17(32/2).

Changes to the environment variables made outside the control of Environment_Variables should be reflected immediately. See A.17(33/2).

Containers.Hash_Type'Modulus should be at least 2**32. Containers.Count_Type'Last should be at least 2**311. See A.18.1(8/2).

The worst-case time complexity of Element for Containers.Vector should be O(log N). See A.18.2(256/2).

The worst-case time complexity of Append with Count = 1 when N is less than the capacity for Containers.Vector should be O(log N). See A.18.2(257/2).

The worst-case time complexity of Prepend with Count = 1 and Delete_First with Count=1 for Containers.Vectors should be O(N log N). See A.18.2(258/2).

The worst-case time complexity of a call on procedure Sort of an instance of Containers.Vectors.Generic_Sorting should be O(N**2), and the average time complexity should be better than O(N**2). See A.18.2(259/2).

Containers.Vectors.Generic_Sorting.Sort and Containers.Vectors.Generic_Sorting.Merge should minimize copying of elements. See A.18.2(260/2).

Containers.Vectors.Move should not copy elements, and should minimize copying of internal data structures. See A.18.2(261/2).

If an exception is propagated from a vector operation, no storage should be lost, nor any elements removed from a vector unless specified by the operation. See A.18.2(262/2).

The worst-case time complexity of Element, Insert with Count=1, and Delete with Count=1 for Containers.Doubly_Linked_Lists should be O(log N). See A.18.3(160/2).

A call on procedure Sort of an instance of Containers.Doubly_Linked_Lists.Generic_Sorting should have an average time complexity better than O(N**2) and worst case no worse than O(N**2). See A.18.3(161/2).

Containers.Doubly_Linked_Lists.Move should not copy elements, and should minimize copying of internal data structures. See A.18.3(162/2).

If an exception is propagated from a list operation, no storage should be lost, nor any elements removed from a list unless specified by the operation. See A.18.3(163/2).

Move for a map should not copy elements, and should minimize copying of internal data structures. See A.18.4(83/2).

If an exception is propagated from a map operation, no storage should be lost, nor any elements removed from a map unless specified by the operation. See A.18.4(84/2).

The average time complexity of Element, Insert, Include, Replace, Delete, Exclude, and Find operations that take a key parameter for Containers.Hashed_Maps should be O(log N). The average time complexity of the subprograms of Containers.Hashed_Maps that take a cursor parameter should be O(1). The average time complexity of Containers.Hashed_Maps.Reserve_Capacity should be O(N). See A.18.5(62/2).

The worst-case time complexity of Element, Insert, Include, Replace, Delete, Exclude, and Find operations that take a key parameter for Containers.Ordered_Maps should be O((log N)**2) or better. The worst-case time complexity of the subprograms of Containers.Ordered_Maps that take a cursor parameter should be O(1). See A.18.6(95/2).

Move for sets should not copy elements, and should minimize copying of internal data structures. See A.18.7(104/2).

If an exception is propagated from a set operation, no storage should be lost, nor any elements removed from a set unless specified by the operation. See A.18.7(105/2).

The average time complexity of the Insert, Include, Replace, Delete, Exclude, and Find operations of Containers.Hashed_Sets that take an element parameter should be O(log N). The average time complexity of the subprograms of Containers.Hashed_Sets that take a cursor parameter should be O(1). The average time complexity of Containers.Hashed_Sets.Reserve_Capacity should be O(N). See A.18.8(88/2).

The worst-case time complexity of the Insert, Include, Replace, Delete, Exclude, and Find operations of Containers.Ordered_Sets that take an element parameter should be O((log N)**2). The worst-case time complexity of the subprograms of Containers.Ordered_Sets that take a cursor parameter should be O(1). See A.18.9(116/2).

The worst-case time complexity of the Element, Parent, First_Child, Last_Child, Next_Sibling, Previous_Sibling, Insert_Child with Count=1, and Delete operations of Containers.Multiway_Trees should be O(log N). See A.18.10(230/3).

Containers.Multiway_Trees.Move should not copy elements, and should minimize copying of internal data structures. See A.18.10(231/3).

If an exception is propagated from a tree operation, no storage should be lost, nor any elements removed from a tree unless specified by the operation. See A.18.10(232/3).

Move and Swap in Containers.Indefinite_Holders should not copy any elements, and should minimize copying of internal data structures. See A.18.18(73/5).

If an exception is propagated from a holder operation, no storage should be lost, nor should the element be removed from a holder container unless specified by the operation. See A.18.18(74/3).

Bounded vector objects should be implemented without implicit pointers or dynamic allocation. See A.18.19(16/3).

The implementation advice for procedure Move to minimize copying does not apply to bounded vectors. See A.18.19(17/3).

Bounded list objects should be implemented without implicit pointers or dynamic allocation. See A.18.20(19/3).

The implementation advice for procedure Move to minimize copying does not apply to bounded lists. See A.18.20(20/3).

Bounded hashed map objects should be implemented without implicit pointers or dynamic allocation. See A.18.21(21/3).

The implementation advice for procedure Move to minimize copying does not apply to bounded hashed maps. See A.18.21(22/3).

Bounded ordered map objects should be implemented without implicit pointers or dynamic allocation. See A.18.22(18/3).

The implementation advice for procedure Move to minimize copying does not apply to bounded ordered maps. See A.18.22(19/3).

Bounded hashed set objects should be implemented without implicit pointers or dynamic allocation. See A.18.23(20/3).

The implementation advice for procedure Move to minimize copying does not apply to bounded hashed sets. See A.18.23(21/3).

Bounded ordered set objects should be implemented without implicit pointers or dynamic allocation. See A.18.24(17/3).

The implementation advice for procedure Move to minimize copying does not apply to bounded ordered sets. See A.18.24(18/3).

Bounded tree objects should be implemented without implicit pointers or dynamic allocation. See A.18.25(19/3).

The implementation advice for procedure Move to minimize copying does not apply to bounded trees. See A.18.25(20/3).

Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort should have an average time complexity better than O(N**2) and worst case no worse than O(N**2). See A.18.26(10/2).

Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort should minimize copying of elements. See A.18.26(11/2).

Containers.Generic_Sort should have an average time complexity better than O(N**2) and worst case no worse than O(N**2). See A.18.26(12/3).

Containers.Generic_Sort should minimize calls to the generic formal Swap. See A.18.26(13/3).

Bounded queue objects should be implemented without implicit pointers or dynamic allocation. See A.18.29(13/3).

Bounded priority queue objects should be implemented without implicit pointers or dynamic allocation. See A.18.31(14/3).

Bounded holder objects should be implemented without dynamic allocation. See A.18.32(15/5).

If  Export is supported for a language, the main program should be able to be written in that language. Subprograms named "adainit" and "adafinal" should be provided for elaboration and finalization of the environment task. See B.1(39/3).

Automatic elaboration of preelaborated packages should be provided when specifying the Export aspect as True is supported. See B.1(40/3).

For each supported convention L other than Intrinsic, specifying the aspects Import and Export should be supported for objects of L-compatible types and for subprograms, and aspect Convention should be supported for L-eligible types and for subprograms. See B.1(41/5).

If an interface to C, COBOL, or Fortran is provided, the corresponding package or packages described in Annex B, "Interface to Other Languages" should also be provided. See B.2(13/3).

The constants nul, wide_nul, char16_nul, and char32_nul in package Interfaces.C should have a representation of zero. See B.3(62.5/3).

If C interfacing is supported, the interface correspondences between Ada and C should be supported. See B.3(71).

If the C implementation supports unsigned long long and long long, unsigned_long_long and long_long should be supported. See B.3(71).

If COBOL interfacing is supported, the interface correspondences between Ada and COBOL should be supported. See B.4(98).

If Fortran interfacing is supported, the interface correspondences between Ada and Fortran should be supported. See B.5(26).

The machine code or intrinsics support should allow access to all operations normally available to assembly language programmers for the target environment. See C.1(3).

Interface to assembler should be supported; the default assembler should be associated with the convention identifier Assembler. See C.1(4/3).

If an entity is exported to assembly language, then the implementation should allocate it at an addressable location even if not otherwise referenced from the Ada code. A call to a machine code or assembler subprogram should be treated as if it can read or update every object that is specified as exported. See C.1(5).

Little or no overhead should be associated with calling intrinsic and machine-code subprograms. See C.1(10).

Intrinsic subprograms should be provided to access any machine operations that provide special capabilities or efficiency not normally available. See C.1(16).

If the Ceiling_Locking policy is not in effect and the target system allows for finer-grained control of interrupt blocking, a means for the application to specify which interrupts are to be blocked during protected actions should be provided. See C.3(28/2).

Interrupt handlers should be called directly by the hardware. See C.3.1(20).

Violations of any implementation-defined restrictions on interrupt handlers should be detected before run time. See C.3.1(21).

If implementation-defined forms of interrupt handler procedures are supported, then for each such form of a handler, a type analogous to Parameterless_Handler should be specified in a child package of Interrupts, with the same operations as in the predefined package Interrupts. See C.3.2(25).

Preelaborated packages should be implemented such that little or no code is executed at run time for the elaboration of entities. See C.4(14).

If aspect Discard_Names is True for an entity, then the amount of storage used for storing names associated with that entity should be reduced. See C.5(8/4).

A load or store of a volatile object whose size is a multiple of System.Storage_Unit and whose alignment is nonzero, should be implemented by accessing exactly the bits of the object and no others. See C.6(22/5).

A load or store of an atomic object should be implemented by a single load or store instruction. See C.6(23/2).

If the target domain requires deterministic memory use at run time, storage for task attributes should be pre-allocated statically and the number of attributes pre-allocated should be documented. See C.7.2(30).

Finalization of task attributes and reclamation of associated storage should be performed as soon as possible after task termination. See C.7.2(30.1/2).

Names that end with "_Locking" should be used for implementation-defined locking policies. See D.3(17).

Names that end with "_Queuing" should be used for implementation-defined queuing policies. See D.4(16).

The [abort_statement](./AA-9.8#S0284) should not require the task executing the statement to block. See D.6(9).

On a multi-processor, the delay associated with aborting a task on another processor should be bounded. See D.6(10).

When feasible, specified restrictions should be used to produce a more efficient implementation. See D.7(21).

When appropriate, mechanisms to change the value of Tick should be provided. See D.8(47).

Calendar.Clock and Real_Time.Clock should be transformations of the same time base. See D.8(48).

The "best" time base which exists in the underlying system should be available to the application through Real_Time.Clock. See D.8(49).

On a multiprocessor system, each processor should have a separate and disjoint ready queue. See D.13(9).

When appropriate, implementations should provide configuration mechanisms to change the value of Execution_Time.CPU_Tick. See D.14(29/2).

For a timing event, the handler should be executed directly by the real-time clock interrupt mechanism. See D.15(25).

Starting a protected action on a protected object statically assigned to a processor should not use busy-waiting. See D.16(16).

Each dispatching domain should have separate and disjoint ready queues. See D.16.1(31).

The PCS should allow for multiple tasks to call the RPC-receiver. See E.5(28).

The System.RPC.Write operation should raise Storage_Error if it runs out of space when writing an item. See E.5(29).

If COBOL (respectively, C) is supported in the target environment, then interfacing to COBOL (respectively, C) should be supported as specified in Annex B. See F(7/3).

Packed decimal should be used as the internal representation for objects of subtype S when S'Machine_Radix = 10. See F.1(2).

If Fortran (respectively, C) is supported in the target environment, then interfacing to Fortran (respectively, C) should be supported as specified in Annex B. See G(7/3).

Mixed real and complex operations (as well as pure-imaginary and complex operations) should not be performed by converting the real (resp. pure-imaginary) operand to complex. See G.1.1(56/5).

If Real'Signed_Zeros is True for Numerics.Generic_Complex_Types, a rational treatment of the signs of zero results and result components should be provided. See G.1.1(58).

If Complex_Types.Real'Signed_Zeros is True for Numerics.Generic_Complex_Elementary_Functions, a rational treatment of the signs of zero results and result components should be provided. See G.1.2(49).

For elementary functions, the forward trigonometric functions without a Cycle parameter should not be implemented by calling the corresponding version with a Cycle parameter. Log without a Base parameter should not be implemented by calling Log with a Base parameter. See G.2.4(19).

For complex arithmetic, the Compose_From_Polar function without a Cycle parameter should not be implemented by calling Compose_From_Polar with a Cycle parameter. See G.2.6(15).

Solve and Inverse for Numerics.Generic_Real_Arrays should be implemented using established techniques such as LU decomposition and the result should be refined by an iteration on the residuals. See G.3.1(88/3).

The equality operator should be used to test that a matrix in Numerics.Generic_Real_Arrays is symmetric. See G.3.1(90/2).

An implementation should minimize the circumstances under which the algorithm used for Numerics.Generic_Real_Arrays.Eigenvalues and Numerics.Generic_Real_Arrays.Eigensystem fails to converge. See G.3.1(91/3).

Solve and Inverse for Numerics.Generic_Complex_Arrays should be implemented using established techniques and the result should be refined by an iteration on the residuals. See G.3.2(158/3).

The equality and negation operators should be used to test that a matrix is Hermitian. See G.3.2(160/2).

An implementation should minimize the circumstances under which the algorithm used for Numerics.Generic_Complex_Arrays.Eigenvalues and Numerics.Generic_Complex_Arrays.Eigensystem fails to converge. See G.3.2(160.1/3).

Mixed real and complex operations should not be performed by converting the real operand to complex. See G.3.2(161/2).

The information produced by [pragma](./AA-2.8#S0019) Reviewable should be provided in both a human-readable and machine-readable form, and the latter form should be documented. See H.3.1(19).

Object code listings should be provided both in a symbolic format and in a numeric format. See H.3.1(20).

If the partition elaboration policy is Sequential and the Environment task becomes permanently blocked during elaboration, then the partition should be immediately terminated. See H.6(15/3).

When applied to a generic unit, a program unit pragma that is not a library unit pragma should apply to each instance of the generic unit for which there is not an overriding pragma applied directly to the instance. See J.15(9/5).

