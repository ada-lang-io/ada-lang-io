---
sidebar_position:  135
---

# A.18  Containers

{AI95-00302-03} This clause presents the specifications of the package Containers and several child packages, which provide facilities for storing collections of elements.

Glossary entry: A container is an object that contain other objects all of the same type, which could be class-wide. Several predefined container types are provided by the children of package Ada.Containers (see A.18.1).

Version=[5],Kind=(AddedNormal),Group=[C],Term=[container], Def=[a structured object that represents a collection of elements all of the same (potentially class-wide) type, such as a vector or a tree], Note1=[Several predefined container types are provided by the children of package Ada.Containers (see A.18.1).]

{AI95-00302-03} {AI12-0196-1} {AI12-0416-1} A variety of sequence and associative containers are provided. Each container package defines a cursor type as well as a container type. A cursor is a reference to an element within a container. Many operations on cursors are common to all of the containers. A cursor referencing an element in a container is considered to be overlapping only with the element itself. 

Reason: {AI12-0005-1} {AI12-0196-1} The last sentence is intended to clarify that operations that just use a cursor do not interfere if the cursor objects designated different elements of the container in terms of the concurrent call rules of Annex A. 

Ramification: {AI12-0196-1} A cursor is not considered to overlap with other elements of the associated container, thus parallel operations involving a set of cursors each operating on mutually exclusive sets of elements from the same container are expected to work. 

Discussion: {AI12-0416-1} We use the term "container" alone when it is clear from context what kind of entity (package, type, or object) that we are talking about. Otherwise, we use "container package", "container type", or "container object". Note that "container type" is defined in 4.3.5 for a different usage; in all of A.18 we mean "container type" to be one of the primary types declared in the child packages of package Containers, such as Vector, List, or Map. 

{AI12-0111-1} {AI12-0439-1} Some operations of the language-defined child units of Ada.Containers have access-to-subprogram parameters. To ensure such operations are well-defined, they guard against certain actions by the designated subprogram. An action on a container that can add or remove an element is considered to tamper with cursors, and these are prohibited during all such operations. An action on a container that can replace an element with one of a different size is considered to tamper with elements, and these are prohibited during certain of such operations. The details of the specific actions that are considered to tamper with cursors or elements are defined for each child unit of Ada.Containers.

{AI12-0111-1} Several of the language-defined child units of Ada.Containers include a nested package named Stable, which provides a view of a container that prohibits any operations that would tamper with elements. By using a Stable view for manipulating a container, the number of tampering checks performed while performing the operations can be reduced. The details of the Stable subpackage are defined separately for each child unit of Ada.Containers that includes such a nested package.

{AI95-00302-03} Within this clause we provide Implementation Advice for the desired average or worst case time complexity of certain operations on a container. This advice is expressed using the Landau symbol O(X). Presuming f is some function of a length parameter N and t(N) is the time the operation takes (on average or worst case, as specified) for the length N, a complexity of O(f(N)) means that there exists a finite A such that for any N, t(N)/f(N) &lt A. 

Discussion: Of course, an implementation can do better than a specified O(f(N)): for example, O(1) meets the requirements for O(log N).

This concept seems to have as many names as there are authors. We used "Landau symbol" because that's what our reference does. But we'd also seen this referred as big-O notation (sometimes written as big-oh), and as Bachmann notation. Whatever the name, it always has the above definition. 

If the advice suggests that the complexity should be less than O(f(N)), then for any arbitrarily small positive real D, there should exist a positive integer M such that for all N &gt M, t(N)/f(N) &lt D.

{AI05-0001-1} {AI05-0044-1} {AI12-0416-1} When a formal function is used to provide an ordering for a container, it is generally required to define a strict weak ordering. A function "&lt" defines a strict weak ordering if it is irreflexive, asymmetric, transitive, and in addition, if x &lt y for any values x and y, then for all other values z, (x &lt z) or (z &lt y). Elements are in a smallest first order using such an operator if, for every element y with a predecessor x in the order, (y &lt x) is false.

Reason: {AI12-0416-1} Given a "&lt" operator that provides a strict weak ordering, knowing that (y &lt x) is false is enough to know that (x &lt= y) is true. For a strict weak ordering, (x = y) when both (x &lt y) and (y &lt x) are false. Therefore, it is not necessary to use the "=" operator or test (x &lt y). We only need to discuss adjacent elements since a strict weak ordering is transitive. 


#### Language Design Principles

{AI95-00302-03} {AI05-0299-1} This subclause provides a number of useful containers for Ada. Only the most useful containers are provided. Ones that are relatively easy to code, redundant, or rarely used are omitted from this set, even if they are generally included in containers libraries.

The containers packages are modeled on the Standard Template Library (STL), an algorithms and data structure library popularized by Alexander Stepanov, and included in the C++ standard library. The structure and terminology differ from the STL where that better maps to common Ada usage. For instance, what the STL calls "iterators" are called "cursors" here.

The following major nonlimited containers are provided:

(Expandable) Vectors of any nonlimited type;

Doubly-linked Lists of any nonlimited type;

Hashed Maps keyed by any nonlimited hashable type, and containing any nonlimited type;

Ordered Maps keyed by any nonlimited ordered type, and containing any nonlimited type;

{AI05-0136-1} Hashed Sets of any nonlimited hashable type;

{AI05-0136-1} Ordered Sets of any nonlimited ordered type;

{AI05-0136-1} Multiway Trees of any nonlimited type;

{AI05-0069-1} Holders of any (indefinite) nonlimited type;

{AI05-0159-1} Synchronized queues of any definite nonlimited type; and

{AI05-0159-1} Priority queues of any definite nonlimited type. 

{AI05-0001-1} Separate versions for definite and indefinite element types are provided, as those for definite types can be implemented more efficiently. Similarly, a separate bounded version is provided in order to give more predictable memory usage.

Each container includes a cursor, which is a reference to an element within a container. Cursors generally remain valid as long as the container exists and the element referenced is not deleted. Many operations on cursors are common to all of the containers. This makes it possible to write generic algorithms that work on any kind of container.

The containers packages are structured so that additional packages can be added in the future. Indeed, we hope that these packages provide the basis for a more extensive secondary standard for containers.

If containers with similar functionality (but different performance characteristics) are provided (by the implementation or by a secondary standard), we suggest that a prefix be used to identify the class of the functionality: "Ada.Containers.Bounded_Sets" (for a set with a maximum number of elements); "Ada.Containers.Protected_Maps" (for a map which can be accessed by multiple tasks at one time); "Ada.Containers.Persistent_Vectors" (for a persistent vector which continues to exist between executions of a program) and so on.

Note that the language already includes several requirements that are important to the use of containers. These include:

{AI12-0196-1} Library packages must allow concurrent calls  multiple tasks can use the packages as long as they operate on separate containers. Thus, it is only necessary for a user to protect a container if a single container needs to be used by multiple tasks and concurrent calls to operations of the container have overlapping parameters.

Language-defined types must stream "properly". That means that the stream attributes can be used to implement persistence of containers when necessary, and containers can be passed between partitions of a program.

Equality of language-defined types must compose "properly". This means that the version of "=" directly used by users is the same one that will be used in generics and in predefined equality operators of types with components of the containers and/or cursors. This prevents the abstraction from breaking unexpectedly.

{AI05-0048-1} Redispatching is not allowed (unless it is required). That means that overriding a container operation will not change the behavior of any other predefined container operation. This provides a stable base for extensions. 

{AI12-0258-1} If a container's element type is controlled, the point at which the element is finalized will depend on the implementation of the container. For certain kinds of containers, we require finalization behavior based on the canonical implementation of the container (see the Implementation Requirements below). For the "normal" containers, we do not specify precisely where this will happen (it will happen no later than the finalization of the container, of course) in order to give implementations flexibility to cache, block, split , or reusethe nodes of the container.

This paragraph was deleted.{AI12-0258-1} 

The use of controlled types also brings up the possibility of failure of finalization (and thus deallocation) of an element. This is a "serious bug", as AI95-179 puts it, so we don't try to specify what happens in that case. The implementation should propagate the exception. 

Implementation Note: It is expected that exceptions propagated from these operations do not damage containers. That is, if Storage_Error is propagated because of an allocation failure, or Constraint_Error is propagated by the assignment of elements, the container can continue to be used without further exceptions. The intent is that it should be possible to recover from errors without losing data. We don't try to state this formally in most cases, because it is hard to define precisely what is and is not allowed behavior. 

Implementation Note: {AI12-0005-1} When this clause says that the behavior of something is unspecified, we really mean that any result of executing Ada code short of erroneous execution is allowed. We do not mean that memory not belonging to the parameters of the operation can be trashed. When we mean to allow erroneous behavior, we specifically say that execution is erroneous. All this means that, if the containers are written in Ada, checks should not be suppressed or removed assuming some behavior of other code, and that the implementation should take care to avoid creating internal dangling accesses by assuming behavior from generic formals that can't be guaranteed. We don't try to say this normatively because it would be fairly complex, and implementers are unlikely to increase their support costs by fielding implementations that are unstable if given buggy hash functions, et al. 


#### Static Semantics

{AI12-0035-1} Certain subprograms declared within instances of some of the generic packages presented in this clause are said to perform indefinite insertion. These subprograms are those corresponding (in the sense of the copying described in subclause 12.3) to subprograms that have formal parameters of a generic formal indefinite type and that are identified as performing indefinite insertion in the subclause defining the generic package.

{AI12-0035-1} If a subprogram performs indefinite insertion, then certain run-time checks are performed as part of a call to the subprogram; if any of these checks fail, then the resulting exception is propagated to the caller and the container is not modified by the call. These checks are performed for each parameter corresponding (in the sense of the copying described in 12.3) to a parameter in the corresponding generic whose type is a generic formal indefinite type. The checks performed for a given parameter are those checks explicitly specified in subclause 4.8 that would be performed as part of the evaluation of an initialized allocator whose access type is declared immediately within the instance, where:

the value of the [qualified_expression](./AA-4.7#S0163) is that of the parameter; and

the designated subtype of the access type is the subtype of the parameter; and

finalization of the collection of the access type has started if and only if the finalization of the instance has started. 

Discussion: The phrase "explicitly specified" means those checks for which subclause 4.8 includes the phrase "&ltsome exception&gt is raised if ...". It does not refer, for example, to any checks performed as part of any subtype conversion. In particular, this wording includes the checks described in subclause 4.8 to be performed in the case of a class-wide designated type, and of a designated subtype that has access discriminant parts. These checks are needed to prevent containers from outliving their contained (Element_Type or Key_Type) values. 

Implementation Note: These rules have a dual purpose. Mainly, we are requiring checks needed to prevent dangling references. As a side effect, we are also allowing checks needed to permit an implementation of a container generic to make use of access types in a straightforward way. As an example of the second purpose, suppose that an implementation does declare such an access type and suppose further that the finalization of the collection of the access type has started. These rules allow Program_Error to be propagated in this case (as specified in 4.8); this is necessary to allow an all-Ada implementation of these packages. 


#### Implementation Requirements

{AI12-0258-1} For an indefinite container (one whose type is defined in an instance of a child package of Containers whose [defining_identifier](./AA-3.1#S0022) contains "Indefinite"), each element of the container shall be created when it is inserted into the container and finalized when it is deleted from the container (or when the container object is finalized if the element has not been deleted). For a bounded container (one whose type is defined in an instance of a child package of Containers whose [defining_identifier](./AA-3.1#S0022) starts with "Bounded") that is not an indefinite container, all of the elements of the capacity of the container shall be created and default initialized when the container object is created; the elements shall be finalized when the container object is finalized. [For other kinds of containers, when elements are created and finalized is unspecified.]

Ramification: This allows a user to be able to reason about the behavior of elements that have controlled parts. In most cases, such elements need to be stored in an indefinite container. 

Implementation Note: If the containers are implemented in Ada, this implies that elements for an indefinite container are allocated individually, and that a bounded container contains an array of elements or other data structure that is initialized for the entire capacity of the container when it is created. There is no such restriction on the implementation of the "normal" containers; these can be handled in any way convenient to the implementation - in particular, node reuse is allowed. 

{AI12-0112-1} For an instance I of a container package with a container type, the specific type T of the object returned from a function that returns an object of an iterator interface, as well as the primitive operations of T, shall be nonblocking. The Global aspect specified for T and the primitive operations of T shall be (in all, out synchronized) or a specification that allows access to fewer global objects.

Implementation Note: This requires that the traversal and iteration operations of a container do not create, destroy, or assign any objects of a formal type of I, nor call any formal subprograms of I. Those objects and subprograms might be blocking (depending on the actual parameters). We put similar requirements on the individual traversal operations in the container package definitions. 

Reason: These requirements allows users to use container iterators inside of parallel constructs, regardless of the actual parameters to the instantiation. If such an iterator allowed blocking, it would be illegal inside of a parallel construct (see 9.5). If such an iterator allowed writing of unsynchronized global objects, it would be illegal when the default conflict checking policy is in effect (see 9.10.1). These requirements include sequential iterators; the iterator does not need to appear in a parallel loop to trigger these requirements. 

Discussion: We have to give these requirements as a text rule, as there is no place to declare suitable aspects. The specific type of a container iterator is declared by the implementation and is not part of the visible specification (iterator functions just return a value of a class-wide type). The iterator interface itself cannot impose such a requirement since it needs to be able to work with user-defined types that do need to allow blocking. We give this as a global requirement to avoid duplication. 


#### Extensions to Ada 95

{AI95-00302-03} {AI05-0299-1} This subclause is new. It just provides an introduction to the following subclauses. 


#### Wording Changes from Ada 2005

{AI05-0044-1} Correction: Added a definition of strict weak ordering. 


#### Extensions to Ada 2012

{AI12-0196-1} Correction: We now say that a cursor only overlaps with the element it designates, rather than with the whole container. This allows some reading operations to operate on the container in parallel without separate synchronization. 


#### Wording Changes from Ada 2012

{AI05-0035-1} Corrigendum: Added a definition of "performs indefinite insertion". This is used in other subclauses and any resulting inconsistencies are documented there.

{AI12-0111-1} Moved the basic description of tampering checks here, to cut duplication in description of the individual containers. Added a description of stable views of containers.

{AI12-0112-1} Added a global requirement that iterators returned from containers are nonblocking if the instance is nonblocking.

{AI12-0258-1} Correction: Defined when objects are created and finalized for Bounded and Indefinite containers, so that these can be used reliably with controlled element types. This is not incompatible as this behavior was previously unspecified; code depending on specific behavior was wrong.

{AI12-0005-1} {AI12-0416-1} Added a definition of "smallest first" ordering, so that the behavior of the Sort procedures when elements are equal is well-defined. 


## A.18.1  The Package Containers

{AI95-00302-03} The package Containers is the root of the containers subsystem. 


#### Static Semantics

{AI95-00302-03} The library package Containers has the following declaration: 

```ada
{AI12-0414-1} package Ada.Containers
   with Pure is

```

```ada
   type Hash_Type is mod implementation-defined;

```

```ada
   type Count_Type is range 0 .. implementation-defined;

```

```ada
{AI05-0001-1}    Capacity_Error : exception;

```

```ada
end Ada.Containers;

```

{AI95-00302-03} Hash_Type represents the range of the result of a hash function. Count_Type represents the (potential or actual) number of elements of a container. 

Implementation defined: The value of Containers.Hash_Type'Modulus. The value of Containers.Count_Type'Last.

{AI05-0262-1} Capacity_Error is raised when the capacity of a container is exceeded.


#### Implementation Advice

{AI95-00302-03} Hash_Type'Modulus should be at least 2**32. Count_Type'Last should be at least 2**311. 

Implementation Advice: Containers.Hash_Type'Modulus should be at least 2**32. Containers.Count_Type'Last should be at least 2**311.

Discussion: This is not a requirement so that these types can be declared properly on machines with native sizes that are not 32 bits. For instance, a 24-bit target could use 2**24 for Hash_Type'Modulus. 


#### Extensions to Ada 95

{AI95-00302-03} The package Containers is new. 


#### Incompatibilities With Ada 2005

{AI05-0001-1} Exception Capacity_Error is added to Containers. If Containers is referenced in a [use_clause](./AA-8.4#S0235), and an entity with the name Capacity_Error is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity Capacity_Error may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


## A.18.2  The Generic Package Containers.Vectors

The language-defined generic package Containers.Vectors provides private types Vector and Cursor, and a set of operations for each type. A vector container allows insertion and deletion at any position, but it is specifically optimized for insertion and deletion at the high end (the end with the higher index) of the container. A vector container also provides random access to its elements. 

A vector container behaves conceptually as an array that expands as necessary as items are inserted. The length of a vector is the number of elements that the vector contains. The capacity of a vector is the maximum number of elements that can be inserted into the vector prior to it being automatically expanded.

Elements in a vector container can be referred to by an index value of a generic formal type. The first element of a vector always has its index value equal to the lower bound of the formal type.

A vector container may contain empty elements. Empty elements do not have a specified value.

Implementation Note: Vectors are not intended to be sparse (that is, there are elements at all defined positions). Users are expected to use other containers (like a Map) when they need sparse structures (there is a Note to this effect at the end of this subclause).

The internal array is a conceptual model of a vector. There is no requirement for an implementation to be a single contiguous array. 


#### Static Semantics

{AI95-00302-03} The generic library package Containers.Vectors has the following declaration: 

```ada
{AI05-0084-1} {AI05-0212-1} {AI12-0112-1} with Ada.Iterator_Interfaces;
generic
   type Index_Type is range &lt&gt;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type)
      return Boolean is &lt&gt;
package Ada.Containers.Vectors
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For the Global aspect, any side-effects of the actual parameters of an instance are ignored. So Global =&gt in out synchronized means that the only global side-effects allowed are associated with the actual generic parameters of the instance or with any synchronized state. Unsynchronized package state is not allowed for any container package, and pure packages do not allow any package state at all (they typically have Global =&gt null).

{AI12-0112-1} Similarly, when Nonblocking is set to True for a generic unit, it still includes the blocking effects of the actual parameters to the instance. Thus, the only blocking allowed is that associated with the actual generic parameters. If none of the actual paramerters allow blocking, then no operation of the generic instance may block. 

```ada
   subtype Extended_Index is
      Index_Type'Base range
         Index_Type'First-1 ..
         Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;
   No_Index : constant Extended_Index := Extended_Index'First;

```

Ramification: {AI12-0112-1} The base type of a scalar type is always nonblocking and has Global =&gt null. Therefore, so long as this type is used in the implementation, whether or not the actual type for Index_Type allows blocking or side-effects does not matter. Therefore, we require that operations that only operate on the container implementation be nonblocking and have Global =&gt null regardless of the actual parameters. 

```ada
{AI05-0212-1} {AI12-0111-1} {AI12-0112-1} {AI12-0212-1} {AI12-0339-1} {AI12-0399-1} {AI12-0400-1}    type Vector is tagged private
      with Constant_Indexing =&gt Constant_Reference,
           Variable_Indexing =&gt Reference,
           Default_Iterator  =&gt Iterate,
           Iterator_Element  =&gt Element_Type,
           Iterator_View     =&gt Stable.Vector,
           Aggregate         =&gt (Empty          =&gt Empty,
                                 Add_Unnamed    =&gt Append,
                                 New_Indexed    =&gt New_Vector,
                                 Assign_Indexed =&gt Replace_Element),
           Stable_Properties =&gt (Length, Capacity,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =&gt
              Length (Vector) = 0 and then
              (not Tampering_With_Cursors_Prohibited (Vector)) and then
              (not Tampering_With_Elements_Prohibited (Vector)),
           Preelaborable_Initialization;

```

```ada
{AI12-0399-1}    type Cursor is private
      with Preelaborable_Initialization;

```

```ada
   Empty_Vector : constant Vector;

```

```ada
   No_Element : constant Cursor;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Has_Element (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

Discussion: {AI12-0112-1} Any operation that takes a cursor but no vector can read the vector associated with the cursor. We only know that there is some object of type Vector. Since we don't have a global specification that describes all objects of a specific type, we have to allow reading any object by specifying in all. For such functions, we don't allow writing any object, even those associated with generic formal parameters, thus we also specify Use_Formal =&gt null. 

```ada
{AI12-0112-1}    function Has_Element (Container : Vector; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

Discussion: {AI12-0112-1} For operations that do not depend on any of the operations of the generic formal parameters (including those of formal types), we specify that the operation has no side-effects of any kind. This requires specifying that there is no dependence on the generic formal parameters with Use_Formal =&gt null in addition to no usual side-effects with null. We also specify Nonblocking on such operations in order that the operation never blocks even if some of the actual parameters allow blocking. 

```ada
{AI05-0212-1}    package Vector_Iterator_Interfaces is new
       Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
   function "=" (Left, Right : Vector) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_Cursors_Prohibited
      (Container : Vector) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Tampering_With_Elements_Prohibited
      (Container : Vector) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Maximum_Length return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty (Capacity : Count_Type := implementation-defined)
      return Vector
      with Pre  =&gt Capacity &lt= Maximum_Length
                      or else raise Constraint_Error,
           Post =&gt
              Capacity (Empty'Result) &gt= Capacity and then
              not Tampering_With_Elements_Prohibited (Empty'Result) and then
              not Tampering_With_Cursors_Prohibited (Empty'Result) and then
              Length (Empty'Result) = 0;

```

```ada
{AI12-0112-1}    function To_Vector (Length : Count_Type) return Vector
      with Pre  =&gt Length &lt= Maximum_Length or else raise Constraint_Error,
           Post =&gt
              To_Vector'Result.Length = Length and then
              not Tampering_With_Elements_Prohibited (To_Vector'Result)
                and then
              not Tampering_With_Cursors_Prohibited (To_Vector'Result)
                and then
              To_Vector'Result.Capacity &gt= Length;

```

```ada
{AI12-0112-1}    function To_Vector
     (New_Item : Element_Type;
      Length   : Count_Type) return Vector
      with Pre  =&gt Length &lt= Maximum_Length or else raise Constraint_Error,
           Post =&gt
              To_Vector'Result.Length = Length and then
              not Tampering_With_Elements_Prohibited (To_Vector'Result)
                and then
              not Tampering_With_Cursors_Prohibited (To_Vector'Result)
                and then
              To_Vector'Result.Capacity &gt= Length;

```

```ada
{AI12-0212-1}    function New_Vector (First, Last : Index_Type) return Vector is
        (To_Vector (Count_Type (Last - First + 1)))
     with Pre =&gt First = Index_Type'First;

```

```ada
{AI12-0112-1}    function "&" (Left, Right : Vector) return Vector
      with Pre  =&gt Length (Left) &lt= Maximum_Length - Length (Right)
                     or else raise Constraint_Error,
           Post =&gt Length (Vectors."&"'Result) =
                     Length (Left) + Length (Right) and then
                   not Tampering_With_Elements_Prohibited
                     (Vectors."&"'Result) and then
                   not Tampering_With_Cursors_Prohibited
                     (Vectors."&"'Result) and then
                   Vectors."&"'Result.Capacity &gt= 
                     Length (Left) + Length (Right);

```

```ada
{AI12-0112-1}    function "&" (Left  : Vector;
                 Right : Element_Type) return Vector
      with Pre  =&gt Length (Left) &lt= Maximum_Length - 1
                     or else raise Constraint_Error,
           Post =&gt Vectors."&"'Result.Length = Length (Left) + 1 and then
                   not Tampering_With_Elements_Prohibited
                     (Vectors."&"'Result) and then
                   not Tampering_With_Cursors_Prohibited
                     (Vectors."&"'Result) and then
                   Vectors."&"'Result.Capacity &gt= Length (Left) + 1;

```

```ada
{AI12-0112-1}    function "&" (Left  : Element_Type;
                 Right : Vector) return Vector
      with Pre  =&gt Length (Right) &lt= Maximum_Length - 1
                     or else raise Constraint_Error,
           Post =&gt Length (Vectors."&"'Result) = Length (Right) + 1 and then
                   not Tampering_With_Elements_Prohibited
                     (Vectors."&"'Result) and then
                   not Tampering_With_Cursors_Prohibited
                     (Vectors."&"'Result) and then
                   Vectors."&"'Result.Capacity &gt= Length (Right) + 1;

```

```ada
{AI12-0112-1}    function "&" (Left, Right  : Element_Type) return Vector
      with Pre  =&gt Maximum_Length &gt= 2 or else raise Constraint_Error,
           Post =&gt Length ("&"'Result) = 2 and then
                   not Tampering_With_Elements_Prohibited
                     (Vectors."&"'Result) and then
                   not Tampering_With_Cursors_Prohibited
                     (Vectors."&"'Result) and then
                   Vectors."&"'Result.Capacity &gt= 2;

```

```ada
{AI12-0112-1}    function Capacity (Container : Vector) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Reserve_Capacity (Container : in out Vector;
                               Capacity  : in     Count_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Container.Capacity &gt= Capacity;

```

```ada
{AI12-0112-1}    function Length (Container : Vector) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Set_Length (Container : in out Vector;
                         Length    : in     Count_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length &lt= Maximum_Length
                      or else raise Constraint_Error),
           Post =&gt Container.Length = Length and then
                   Capacity (Container) &gt= Length;

```

```ada
{AI12-0112-1}    function Is_Empty (Container : Vector) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

```ada
{AI12-0112-1}    procedure Clear (Container : in out Vector)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container) = 0;

```

```ada
{AI12-0112-1}    function To_Cursor (Container : Vector;
                       Index     : Extended_Index) return Cursor
      with Post =&gt (if Index in
                      First_Index (Container) .. Last_Index (Container)
                    then Has_Element (Container, To_Cursor'Result)
                    else To_Cursor'Result = No_Element),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function To_Index (Position  : Cursor) return Extended_Index
      with Nonblocking, Global =&gt in all;

```

```ada
{AI12-0112-1}    function To_Index (Container : Vector;
                      Position  : Cursor) return Extended_Index
      with Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position) or else
                      raise Program_Error,
           Post =&gt (if Position = No_Element then To_Index'Result = No_Index
                    else To_Index'Result in First_Index (Container) ..
                           Last_Index (Container)),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Element (Container : Vector;
                     Index     : Index_Type)
      return Element_Type
      with Pre  =&gt Index in
                        First_Index (Container) .. Last_Index (Container)
                      or else raise Constraint_Error,
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

Discussion: {AI12-0112-1} Here the Nonblocking and Global contracts are saying that Element depends on the properties of the actual for Element_Type, but not on the properties of the actuals for Index_Type or "=". This is necessary as copying the element may require calling Adjust and Finalize for the actual Element_Type, and those may have side-effects or block. 

```ada
{AI12-0112-1}    function Element (Position : Cursor) return Element_Type
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    function Element (Container : Vector;
                     Position  : Cursor) return Element_Type
      with Pre =&gt (Position /= No_Element or else
                      raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Vector;
                              Index     : in     Index_Type;
                              New_Item  : in     Element_Type)
      with Pre =&gt (not Tampering_With_Elements_Prohibited (Container)
                     or else raise Program_Error) and then
                  (Index in
                     First_Index (Container) .. Last_Index (Container)
                     or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Vector;
                              Position  : in     Cursor;
                              New_item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Vector;
      Index     : in Index_Type;
      Process   : not null access procedure (Element : in Element_Type))
      with Pre  =&gt Index in
                      First_Index (Container) .. Last_Index (Container)
                      or else raise Constraint_Error;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Element : in Element_Type))
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Vector;
      Position  : in Cursor;
      Process   : not null access procedure (Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                    (Has_Element (Container, Position)
                       or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Update_Element
     (Container : in out Vector;
      Index     : in     Index_Type;
      Process   : not null access procedure
                      (Element : in out Element_Type))
      with Pre  =&gt Index in
                      First_Index (Container) .. Last_Index (Container)
                      or else raise Constraint_Error;

```

```ada
{AI12-0112-1}    procedure Update_Element
     (Container : in out Vector;
      Position  : in     Cursor;
      Process   : not null access procedure
                      (Element : in out Element_Type))
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                    (Has_Element (Container, Position)
                      or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

Discussion: {AI12-0112-1} Finalization of this type will update the tampering counter of an associated container. We know this has to be an object of type Vector, but we don't have a way to specify that. We need this separate Global in case an object of this type is declared to exist separately from the short-lived object associated with a call of the Constant_Reference function. 

```ada
{AI05-0212-1} {AI12-0112-1}    type Reference_Type (Element : not null access Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Vector;
                                Index     : in Index_Type)
      return Constant_Reference_Type
      with Pre    =&gt Index in
                        First_Index (Container) .. Last_Index (Container)
                        or else raise Constraint_Error,
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Vector;
                       Index     : in Index_Type)
      return Reference_Type
      with Pre    =&gt Index in
                        First_Index (Container) .. Last_Index (Container)
                        or else raise Constraint_Error,
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Vector;
                                Position  : in Cursor)
      return Constant_Reference_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                    (Has_Element (Container, Position)
                       or else raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Vector;
                       Position  : in Cursor)
      return Reference_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                    (Has_Element (Container, Position)
                       or else raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0001-1} {AI12-0112-1}    procedure Assign (Target : in out Vector; Source : in Vector)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target) and then
                   Capacity (Target) &gt= Length (Target);

```

```ada
{AI05-0001-1} {AI12-0112-1}    function Copy (Source : Vector; Capacity : Count_Type := 0)
      return Vector
      with Pre =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                      or else raise Capacity_Error,
           Post =&gt Length (Copy'Result) = Length (Source) and then
                   not Tampering_With_Elements_Prohibited (Copy'Result)
                      and then
                   not Tampering_With_Cursors_Prohibited (Copy'Result)
                      and then
                   Copy'Result.Capacity &gt= (if Capacity = 0 then
                      Length (Source) else Capacity);

```

```ada
{AI12-0112-1}    procedure Move (Target : in out Vector;
                   Source : in out Vector)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                       or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                       or else raise Program_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                       Length (Target) = Length (Source)'Old and then
                       Length (Source) = 0 and then
                       Capacity (Target) &gt= Length (Source)'Old);

```

```ada
{AI12-0112-1} {AI12-0400-1}    procedure Insert_Vector (Container : in out Vector;
                            Before    : in     Extended_Index;
                            New_Item  : in     Vector)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before in
                      First_Index (Container) .. Last_Index (Container) + 1
                      or else raise Constraint_Error) and then
                   (Length (Container) &lt= Maximum_Length - Length (New_Item)
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Length (New_Item) =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1} {AI12-0400-1}    procedure Insert_Vector (Container : in out Vector;
                            Before    : in     Cursor;
                            New_Item  : in     Vector)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Length (New_Item)
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Length (New_Item) =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1} {AI12-0400-1}    procedure Insert_Vector (Container : in out Vector;
                            Before    : in     Cursor;
                            New_Item  : in     Vector;
                            Position  :    out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Length (New_Item)
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Length (New_Item) =
                      Length (Container) and then
                   Has_Element (Container, Position) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Vector;
                     Before    : in     Extended_Index;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before in
                      First_Index (Container) .. Last_Index (Container) + 1
                      or else raise Constraint_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count =
                      Length (Container) and then
                   Has_Element (Container, Position) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Vector;
                     Before    : in     Extended_Index;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before in
                      First_Index (Container) .. Last_Index (Container) + 1
                      or else raise Constraint_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     Position  :    out Cursor;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count = Length (Container)
                   and then Has_Element (Container, Position) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1} {AI12-0400-1}    procedure Prepend_Vector (Container : in out Vector;
                             New_Item  : in     Vector)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Length (New_Item)
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Length (New_Item) =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Prepend (Container : in out Vector;
                      New_Item  : in     Element_Type;
                      Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1} {AI12-0400-1}    procedure Append_Vector (Container : in out Vector;
                            New_Item  : in     Vector)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Length (New_Item)
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Length (New_Item) =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1} {AI12-0400-1}    procedure Append (Container : in out Vector;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                       or else raise Constraint_Error),
           Post =&gt 
              Length (Container)'Old + Count = Length (Container) and then
              Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1} {AI12-0212-1} {AI12-0400-1}    procedure Append (Container : in out Vector;
                     New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - 1
                       or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + 1 = Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert_Space (Container : in out Vector;
                           Before    : in     Extended_Index;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before in
                      First_Index (Container) .. Last_Index (Container) + 1
                      or else raise Constraint_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count =
                      Length (Container) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert_Space (Container : in out Vector;
                           Before    : in     Cursor;
                           Position  :    out Cursor;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                      Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Maximum_Length - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count =
                      Length (Container) and then
                   Has_Element (Container, Position) and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Vector;
                     Index     : in     Extended_Index;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Index in
                      First_Index (Container) .. Last_Index (Container) + 1
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old - Count &lt=
                      Length (Container);

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Vector;
                     Position  : in out Cursor;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Length (Container)'Old - Count &lt=
                      Length (Container) and then
                   Position = No_Element;

```

```ada
{AI12-0112-1}    procedure Delete_First (Container : in out Vector;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Delete_Last (Container : in out Vector;
                          Count     : in     Count_Type := 1)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Reverse_Elements (Container : in out Vector)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error;

```

```ada
{AI12-0112-1}    procedure Swap (Container : in out Vector;
                   I, J      : in     Index_Type)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                       or else raise Program_Error) and then
                  (I in First_Index (Container) .. Last_Index (Container)
                       or else raise Constraint_Error) and then
                  (J in First_Index (Container) .. Last_Index (Container)
                       or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    procedure Swap (Container : in out Vector;
                   I, J      : in     Cursor)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                       or else raise Program_Error) and then
                   (I /= No_Element or else Constraint_Error) and then
                   (J /= No_Element or else Constraint_Error) and then
                   (Has_Element (Container, I)
                       or else raise Program_Error) and then
                   (Has_Element (Container, J)
                       or else raise Program_Error);

```

```ada
{AI12-0112-1}    function First_Index (Container : Vector) return Index_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt First_Index'Result = Index_Type'First;

```

```ada
{AI12-0112-1}    function First (Container : Vector) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, First'Result)
                    else First'Result = No_Element);

```

```ada
{AI12-0112-1}    function First_Element (Container : Vector)
      return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Last_Index (Container : Vector) return Extended_Index
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if Length (Container) = 0
                    then Last_Index'Result = No_Index
                    else Count_Type(Last_Index'Result - Index_Type'First) =
                         Length (Container) - 1);

```

```ada
{AI12-0112-1}    function Last (Container : Vector) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, Last'Result)
                    else Last'Result = No_Element);

```

```ada
{AI12-0112-1}    function Last_Element (Container : Vector)
      return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Next (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Container : Vector; Position : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                       or else raise Program_Error,
           Post =&gt (if Position = No_Element then Next'Result = No_Element
                    elsif Has_Element (Container, Next'Result) then
                       To_Index (Container, Next'Result) =
                       To_Index (Container, Position) + 1
                    elsif Next'Result = No_Element then
                       Position = Last (Container)
                    else False);

```

```ada
{AI12-0112-1}    procedure Next (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Next (Container : in     Vector;
                   Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                       or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Previous (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element
                    then Previous'Result = No_Element);

```

```ada
{AI12-0112-1}    function Previous (Container : Vector;
                      Position  : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                       or else raise Program_Error,
           Post =&gt (if Position = No_Element
                    then Previous'Result = No_Element
                    elsif Has_Element (Container, Previous'Result) then
                       To_Index (Container, Previous'Result) =
                       To_Index (Container, Position) - 1
                    elsif Previous'Result = No_Element then
                       Position = First (Container)
                    else False);

```

```ada
{AI12-0112-1}    procedure Previous (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Previous (Container : in     Vector;
                       Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                       or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
   function Find_Index (Container : Vector;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First)
      return Extended_Index;

```

```ada
{AI12-0112-1}    function Find (Container : Vector;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      return Cursor
      with Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                       or else raise Program_Error,
           Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

```ada
   function Reverse_Find_Index (Container : Vector;
                                Item      : Element_Type;
                                Index     : Index_Type := Index_Type'Last)
      return Extended_Index;

```

```ada
{AI12-0112-1}    function Reverse_Find (Container : Vector;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      return Cursor
      with Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                       or else raise Program_Error,
           Post =&gt (if Reverse_Find'Result /= No_Element
                    then Has_Element (Container, Reverse_Find'Result));

```

```ada
   function Contains (Container : Vector;
                      Item      : Element_Type) return Boolean;

```

```ada
This paragraph was deleted.{AI05-0212-1} 

```

```ada
{AI12-0112-1}    procedure Iterate
     (Container : in Vector;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI12-0112-1}    procedure Reverse_Iterate
     (Container : in Vector;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate (Container : in Vector)
      return Vector_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
      with Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Iterate (Container : in Vector; Start : in Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
      with Pre    =&gt (Start /= No_Element
                            or else raise Constraint_Error) and then
                        (Has_Element (Container, Start)
                            or else raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0112-1}    generic
      with function "&lt" (Left, Right : Element_Type)
         return Boolean is &lt&gt;
   package Generic_Sorting
   with Nonblocking, Global =&gt null is

```

```ada
      function Is_Sorted (Container : Vector) return Boolean;

```

```ada
{AI12-0112-1}       procedure Sort (Container : in out Vector)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                          or else raise Program_Error;

```

```ada
{AI12-0112-1}       procedure Merge (Target  : in out Vector;
                       Source  : in out Vector)
         with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                          or else raise Program_Error) and then
                      (not Tampering_With_Cursors_Prohibited (Source)
                          or else raise Program_Error) and then
                      (Length (Target) &lt= Maximum_Length - Length (Source)
                          or else raise Constraint_Error) and then
                      ((Length (Source) = 0 or else
                          not Target'Has_Same_Storage (Source))
                          or else raise Program_Error),
              Post =&gt (declare
                          Result_Length : constant Count_Type :=
                             Length (Source)'Old + Length (Target)'Old;
                       begin
                          (Length (Source) = 0 and then
                           Length (Target) = Result_Length and then
                           Capacity (Target) &gt= Result_Length));

```

```ada
   end Generic_Sorting;

```

```ada
{AI12-0111-1}    package Stable is

```

```ada
{AI12-0111-1} {AI12-0339-1} {AI12-0400-1} {AI12-0407-1}       type Vector (Base : not null access Vectors.Vector) is
         tagged limited private
         with Constant_Indexing =&gt Constant_Reference,
              Variable_Indexing =&gt Reference,
              Default_Iterator  =&gt Iterate,
              Iterator_Element  =&gt Element_Type,
              Stable_Properties =&gt (Length, Capacity),
              Global            =&gt null,
              Default_Initial_Condition =&gt Length (Vector) = 0,
              Preelaborable_Initialization;

```

Discussion: {AI05-0112-1} The Global of null assumes that the user of a stable object is including effects associated with the access discriminant. For operations with in parameters (after any overriding), the object designated by the access discriminant is assumed to be read, and for other operations (including initialization and finalization) the object designated by the access discriminant is assumed to be read and updated. 

```ada
{AI12-0111-1}       type Cursor is private
         with Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       Empty_Vector : constant Vector;

```

```ada
{AI12-0111-1}       No_Element : constant Cursor;

```

```ada
{AI12-0111-1}       function Has_Element (Position : Cursor) return Boolean
         with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0111-1}       package Vector_Iterator_Interfaces is new
         Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
{AI12-0111-1}       procedure Assign (Target : in out Vectors.Vector;
                        Source : in Vector)
         with Post =&gt Length (Source) = Length (Target) and then
                     Capacity (Target) &gt= Length (Target);

```

```ada
{AI12-0111-1}       function Copy (Source : Vectors.Vector) return Vector
         with Post =&gt Length (Copy'Result) = Length (Source);

```

```ada
{AI12-0111-1}       type Constant_Reference_Type
            (Element : not null access constant Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       type Reference_Type
            (Element : not null access Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       -- Additional subprograms as described in the text
      -- are declared here.

```

```ada
{AI12-0111-1}    private

```

```ada
{AI12-0111-1}       ... -- not specified by the language

```

```ada
{AI12-0111-1}    end Stable;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Vectors;

```

{AI95-00302-03} The actual function for the generic formal function "=" on Element_Type values is expected to define a reflexive and symmetric relationship and return the same result value each time it is called with a particular pair of values. If it behaves in some other manner, the functions defined to use it return an unspecified value. The exact arguments and number of calls of this generic formal function by the functions defined to use it are unspecified.

Ramification: The "functions defined to use it" are Find, Find_Index, Reverse_Find, Reverse_Find_Index, and "=" for Vectors. This list is a bit too long to give explicitly.

If the actual function for "=" is not symmetric and consistent, the result returned by any of the functions defined to use "=" cannot be predicted. The implementation is not required to protect against "=" raising an exception, or returning random results, or any other "bad" behavior. And it can call "=" in whatever manner makes sense. But note that only the results of the functions defined to use "=" are unspecified; other subprograms are not allowed to break if "=" is bad. 

{AI95-00302-03} The type Vector is used to represent vectors. The type Vector needs finalization (see 7.6).

{AI95-00302-03} Empty_Vector represents the empty vector object. It has a length of 0. If an object of type Vector is not otherwise initialized, it is initialized to the same value as Empty_Vector.

{AI95-00302-03} No_Element represents a cursor that designates no element. If an object of type Cursor is not otherwise initialized, it is initialized to the same value as No_Element.

{AI95-00302-03} {AI12-0434-1} The primitive "=" operator for type Cursor returns True if both cursors are No_Element, or designate the same element in the same container.

To be honest: {AI12-0434-1} "The primitive "=" operator" is the one with two parameters of type Cursor which returns Boolean. We're not talking about some other (hidden) primitive function named "=". 

{AI95-00302-03} Execution of the default implementation of the Input, Output, Read, or Write attribute of type Cursor raises Program_Error.

Reason: A cursor will probably be implemented in terms of one or more access values, and the effects of streaming access values is unspecified. Rather than letting the user stream junk by accident, we mandate that streaming of cursors raise Program_Error by default. The attributes can always be specified if there is a need to support streaming. 

{AI05-0001-1} {AI05-0262-1} {AI12-0437-1} Vector'Write for a Vector object V writes Length(V) elements of the vector to the stream. It may also write additional information about the vector.

{AI05-0001-1} {AI05-0262-1} Vector'Read reads the representation of a vector from the stream, and assigns to Item a vector with the same length and elements as was written by Vector'Write.

Implementation Note: The Reference Manual requires streaming of all language-defined nonlimited types (including containers) to "work" (see 13.13.2). In addition, we do not want all of the elements that make up the capacity of the vector streamed, as those beyond the length of the container have undefined contents (and might cause bad things when read back in). This will require a custom stream attribute implementation; the language-defined default implementation will not work (even for a bounded form, as that would most likely stream the entire capacity of the vector). There is a separate requirement that the unbounded and Bounded form use the same streaming representation for the same element type, see A.18.19. 

{AI95-00302-03} No_Index represents a position that does not correspond to any element. The subtype Extended_Index includes the indices covered by Index_Type plus the value No_Index and, if it exists, the successor to the Index_Type'Last.

Discussion: We require the existence of Index_Type'First  1, so that No_Index and Last_Index of an empty vector is well-defined. We don't require the existence of Index_Type'Last + 1, as it is only used as the position of insertions (and needs to be allowed only when inserting an empty vector). 

{AI95-00302-03} {AI12-0111-1} {AI12-0112-1} [Some operations  check for "tampering with cursors" of a container because they depend on the set of elements of the container remaining constant, and others check for "tampering with elements" of a container because they depend on elements of the container not being replaced.] When tampering with cursors is prohibited for a particular vector object V, Program_Error is propagated by the finalization of V[, as well as by a call that passes V to certain of the operations of this package, as indicated by the precondition of such an operation]. Similarly, when tampering with elements is prohibited for V, Program_Error is propagated by a call that passes V to certain of the other operations of this package, as indicated by the precondition of such an operation.

Paragraphs 91 through 97 are removed as preconditions now describe these rules. 

Ramification: We don't need to explicitly mention [assignment_statement](./AA-5.2#S0173), because that finalizes the target object as part of the operation, and finalization of an object is already defined as tampering with cursors. 

```ada
function Has_Element (Position : Cursor) return Boolean
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0212-1} Returns True if Position designates an element, and returns False otherwise.

To be honest: {AI05-0005-1} {AI05-0212-1} This function might not detect cursors that designate deleted elements; such cursors are invalid (see below) and the result of calling Has_Element with an invalid cursor is unspecified (but not erroneous). 

```ada
function Has_Element (Container : Vector; Position : Cursor)
   return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if Position designates an element in Container, and returns False otherwise.

Ramification: If Position is No_Element, Has_Element returns False. 

```ada
function "=" (Left, Right : Vector) return Boolean;

```

{AI95-00302-03} {AI05-0264-1} If Left and Right denote the same vector object, then the function returns True. If Left and Right have different lengths, then the function returns False. Otherwise, it compares each element in Left to the corresponding element in Right using the generic formal equality operator. If any such comparison returns False, the function returns False; otherwise, it returns True. Any exception raised during evaluation of element equality is propagated.

Implementation Note: This wording describes the canonical semantics. However, the order and number of calls on the formal equality function is unspecified for all of the operations that use it in this package, so an implementation can call it as many or as few times as it needs to get the correct answer. Specifically, there is no requirement to call the formal equality additional times once the answer has been determined. 

```ada
function Tampering_With_Cursors_Prohibited
   (Container : Vector) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if tampering with cursors or tampering with elements is currently prohibited for Container, and returns False otherwise.

Reason: {AI12-0112-1} Prohibiting tampering with elements also needs to prohibit tampering with cursors, as deleting an element is similar to replacing it. 

Implementation Note: {AI12-0112-1} Various contracts elsewhere in this specification require that this function be implemented with synchronized data. Moreover, it is possible for tampering to be prohibited by multiple operations (sequentially or in parallel). Therefore, tampering needs to be implemented with an atomic or protected counter. The counter is initialized to zero, and is incremented when tampering is prohibited, and decremented when leaving an area that prohibited tampering. Function Tampering_With_Cursors_Prohibited returns True if the counter is nonzero. (Note that any case where the result is not well-defined for one task is incorrect use of shared variables and would be erroneous by the rules of 9.10, so no special protection is needed to read the counter.) 

```ada
function Tampering_With_Elements_Prohibited
   (Container : Vector) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Always returns False[, regardless of whether tampering with elements is prohibited].

Reason: {AI12-0111-1} A definite element cannot change size, so we allow operations that tamper with elements even when tampering with elements is prohibited. That's not true for the indefinite containers, which is why this kind of tampering exists. 

```ada
function Maximum_Length return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns the maximum Length of a Vector, based on the index type.

Implementation Note: This is just:

```ada
  
  Count_Type (Index_Type'Last - Index_Type'First + 1)

```

but since the inner calculation can overflow or the type conversion can fail, this can't be evaluated in general with an expression function. Note that if this expression raises Constraint_Error, then the result is Count_Type'Last, since the Capacity of a Vector cannot exceed Count_Type'Last. 

```ada
function Empty (Capacity : Count_Type := implementation-defined)
   return Vector
   with Pre  =&gt Capacity &lt= Maximum_Length
                   or else raise Constraint_Error,
        Post =&gt 
           Capacity (Empty'Result) &gt= Capacity and then
           not Tampering_With_Elements_Prohibited (Empty'Result) and then
           not Tampering_With_Cursors_Prohibited (Empty'Result) and then
           Length (Empty'Result) = 0;

```

{AI12-0339-1} Returns an empty vector.

```ada
function To_Vector (Length : Count_Type) return Vector
   with Pre  =&gt Length &lt= Maximum_Length or else raise Constraint_Error,
        Post =&gt
           To_Vector'Result.Length = Length and then
           not Tampering_With_Elements_Prohibited (To_Vector'Result)
             and then
           not Tampering_With_Cursors_Prohibited (To_Vector'Result)
             and then
           To_Vector'Result.Capacity &gt= Length;

```

{AI95-00302-03} Returns a vector with a length of Length, filled with empty elements.

```ada
function To_Vector
  (New_Item : Element_Type;
   Length   : Count_Type) return Vector
   with Pre  =&gt Length &lt= Maximum_Length or else raise Constraint_Error,
        Post =&gt
           To_Vector'Result.Length = Length and then
           not Tampering_With_Elements_Prohibited (To_Vector'Result)
             and then
           not Tampering_With_Cursors_Prohibited (To_Vector'Result)
             and then
        To_Vector'Result.Capacity &gt= Length;

```

{AI95-00302-03} Returns a vector with a length of Length, filled with elements initialized to the value New_Item.

```ada
function "&" (Left, Right : Vector) return Vector
   with Pre  =&gt Length (Left) &lt= Maximum_Length - Length (Right)
                 or else raise Constraint_Error,
        Post =&gt Length (Vectors."&"'Result) =
                   Length (Left) + Length (Right) and then
                not Tampering_With_Elements_Prohibited (Vectors."&"'Result)
                   and then
                not Tampering_With_Cursors_Prohibited (Vectors."&"'Result)
                   and then
                Vectors."&"'Result.Capacity &gt=
                   Length (Left) + Length (Right);

```

{AI95-00302-03} Returns a vector comprising the elements of Left followed by the elements of Right.

```ada
function "&" (Left  : Vector;
              Right : Element_Type) return Vector
   with Pre  =&gt Length (Left) &lt= Maximum_Length - 1
                   or else raise Constraint_Error,
        Post =&gt Vectors."&"'Result.Length = Length (Left) + 1 and then
                not Tampering_With_Elements_Prohibited (Vectors."&"'Result)
                   and then
                not Tampering_With_Cursors_Prohibited (Vectors."&"'Result)
                   and then
                Vectors."&"'Result.Capacity &gt= Length (Left) + 1;

```

{AI95-00302-03} Returns a vector comprising the elements of Left followed by the element Right.

```ada
function "&" (Left  : Element_Type;
              Right : Vector) return Vector
   with Pre  =&gt Length (Right) &lt= Maximum_Length - 1
                   or else raise Constraint_Error,
        Post =&gt Length (Vectors."&"'Result) = Length (Right) + 1 and then
                not Tampering_With_Elements_Prohibited (Vectors."&"'Result)
                   and then
                not Tampering_With_Cursors_Prohibited (Vectors."&"'Result)
                   and then
                Vectors."&"'Result.Capacity &gt= Length (Right) + 1;

```

{AI95-00302-03} Returns a vector comprising the element Left followed by the elements of Right.

```ada
function "&" (Left, Right  : Element_Type) return Vector
   with Pre  =&gt Maximum_Length &gt= 2 or else raise Constraint_Error,
        Post =&gt Length ("&"'Result) = 2 and then
                not Tampering_With_Elements_Prohibited (Vectors."&"'Result)
                   and then
                not Tampering_With_Cursors_Prohibited (Vectors."&"'Result)
                   and then
                Vectors."&"'Result.Capacity &gt= 2;

```

{AI95-00302-03} Returns a vector comprising the element Left followed by the element Right.

```ada
{AI12-0112-1} function Capacity (Container : Vector) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} Returns the capacity of Container.

```ada
{AI12-0112-1} procedure Reserve_Capacity (Container : in out Vector;
                            Capacity  : in     Count_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                 or else raise Program_Error,
        Post =&gt Container.Capacity &gt= Capacity;

```

{AI95-00302-03} {AI05-0001-1} {AI05-0264-1} If the capacity of Container is already greater than or equal to Capacity, then Reserve_Capacity has no effect. Otherwise, Reserve_Capacity allocates additional storage as necessary to ensure that the length of the resulting vector can become at least the value Capacity without requiring an additional call to Reserve_Capacity, and is large enough to hold the current length of Container. Reserve_Capacity then, as necessary, moves elements into the new storage and deallocates any storage no longer needed. Any exception raised during allocation is propagated and Container is not modified.

Discussion: Expanding the internal array can be done by allocating a new, longer array, copying the elements, and deallocating the original array. This may raise Storage_Error, or cause an exception from a controlled subprogram. We require that a failed Reserve_Capacity does not lose any elements if an exception occurs, but we do not require a specific order of evaluations or copying.

This routine is used to preallocate the internal array to the specified capacity such that future Inserts do not require memory allocation overhead. Therefore, the implementation should allocate the needed memory to make that true at this point, even though the visible semantics could be preserved by waiting until the memory is needed. This doesn't apply to the indefinite element container, because elements will have to be allocated individually.

The implementation does not have to contract the internal array if the capacity is reduced, as any capacity greater than or equal to the specified capacity is allowed.

```ada
function Length (Container : Vector) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} Returns the number of elements in Container.

```ada
procedure Set_Length (Container : in out Vector;
                      Length    : in     Count_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length &lt= Maximum_Length or else raise Constraint_Error),
        Post =&gt Container.Length = Length and then
                Capacity (Container) &gt= Length;

```

{AI95-00302-03} {AI05-0264-1} If Length is larger than the capacity of Container, Set_Length calls Reserve_Capacity (Container, Length), then sets the length of the Container to Length. If Length is greater than the original length of Container, empty elements are added to Container; otherwise, elements are removed from Container.

Ramification: No elements are moved by this operation; any new empty elements are added at the end. This follows from the rules that a cursor continues to designate the same element unless the routine is defined to make the cursor ambiguous or invalid; this operation does not do that. 

```ada
function Is_Empty (Container : Vector) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

{AI95-00302-03} {AI12-0112-1} Returns True if Container is empty.

```ada
procedure Clear (Container : in out Vector)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = 0;

```

{AI95-00302-03} Removes all the elements from Container. The capacity of Container does not change.

```ada
function To_Cursor (Container : Vector;
                    Index     : Extended_Index) return Cursor
   with Post =&gt (if Index in
                    First_Index (Container) .. Last_Index (Container)
                 then Has_Element (Container, To_Cursor'Result)
                 else To_Cursor'Result = No_Element),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} {AI12-0112-1} {AI12-0196-1} Returns a cursor designating the element at position Index in Container; returns No_Element if Index does not designate an element. For the purposes of determining whether the parameters overlap in a call to To_Cursor, the Container parameter is not considered to overlap with any object [(including itself)].

Reason: {AI12-0196-1} Without the preceding rule, concurrent calls to To_Cursor on the same container would interfere by the concurrent call rules in Annex A, since the container object of the concurrent calls would overlap with itself. We want these to not interfere, for example to allow the Vector elements to be split into separate "chunks" for parallel processing. 

```ada
function To_Index (Position  : Cursor) return Extended_Index
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} If Position is No_Element, No_Index is returned. Otherwise, the index (within its containing vector) of the element designated by Position is returned.

Ramification: This implies that the index is determinable from a bare cursor alone. The basic model is that a vector cursor is implemented as a record containing an access to the vector container and an index value. This does constrain implementations, but it also allows all of the cursor operations to be defined in terms of the corresponding index operation (which should be primary for a vector). 

```ada
function To_Index (Container : Vector;
                   Position  : Cursor) return Extended_Index
   with Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)  or else
                   raise Program_Error,
        Post =&gt (if Position = No_Element then To_Index'Result = No_Index
                 else To_Index'Result in First_Index (Container) ..
                        Last_Index (Container)),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns the index (within Container) of the element designated by Position; returns No_Index if Position does not designate an element. For the purposes of determining whether the parameters overlap in a call to To_Index, the Container parameter is not considered to overlap with any object [(including itself)].

```ada
function Element (Container : Vector;
                  Index     : Index_Type)
   return Element_Type
   with Pre  =&gt Index in First_Index (Container) .. Last_Index (Container)
                   or else raise Constraint_Error,
        Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

{AI95-00302-03} {AI12-0112-1} Element returns the element at position Index.

```ada
function Element (Position  : Cursor) return Element_Type
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

{AI95-00302-03} {AI12-0112-1} Element returns the element designated by Position.

```ada
function Element (Container : Vector;
                  Position  : Cursor) return Element_Type
   with Pre =&gt (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

{AI12-0112-1} Element returns the element designated by Position in Container.

```ada
procedure Replace_Element (Container : in out Vector;
                           Index     : in     Index_Type;
                           New_Item  : in     Element_Type)
   with Pre =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (Index in First_Index (Container) .. Last_Index (Container)
                   or else raise Constraint_Error);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} {AI12-0196-1} Replace_Element assigns the value New_Item to the element at position Index. Any exception raised during the assignment is propagated. The element at position Index is not an empty element after successful call to Replace_Element. For the purposes of determining whether the parameters overlap in a call to Replace_Element, the Container parameter is not considered to overlap with any object [(including itself)], and the Index parameter is considered to overlap with the element at position Index.

```ada
procedure Replace_Element (Container : in out Vector;
                           Position  : in     Cursor;
                           New_Item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} {AI12-0196-1} Replace_Element assigns New_Item to the element designated by Position. Any exception raised during the assignment is propagated. The element at Position is not an empty element after successful call to Replace_Element. For the purposes of determining whether the parameters overlap in a call to Replace_Element, the Container parameter is not considered to overlap with any object [(including itself)].

Ramification: {AI05-0212-1} Replace_Element, Update_Element, and Reference are the only ways that an element can change from empty to nonempty. Also see the note following Update_Element. 

```ada
procedure Query_Element
  (Container : in Vector;
   Index     : in Index_Type;
   Process   : not null access procedure (Element : in Element_Type))
   with Pre  =&gt Index in First_Index (Container) .. Last_Index (Container)
                   or else raise Constraint_Error;

```

{AI95-00302-03} {AI05-0265-1} {AI12-0112-1} Query_Element calls Process.all with the element at position Index as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

Reason: {AI05-0005-1} The "tamper with the elements" check is intended to prevent the Element parameter of Process from being replaced or deleted outside of Process. The check prevents data loss (if Element_Type is passed by copy) or erroneous execution (if Element_Type is an unconstrained type in an indefinite container). 

```ada
procedure Query_Element
  (Position : in Cursor;
   Process  : not null access procedure (Element : in Element_Type))
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error
        Global =&gt in all;

```

{AI95-00302-03} {AI05-0021-1} {AI05-0265-1} {AI12-0112-1} Query_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of the vector that contains the element designated by Position is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Query_Element
  (Container : in Vector;
   Position  : in Cursor;
   Process   : not null access procedure (Element : in Element_Type))
   with Pre  =&gt (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error);

```

{AI12-0112-1} Query_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Update_Element
  (Container : in out Vector;
   Index     : in     Index_Type;
   Process   : not null access procedure
                   (Element : in out Element_Type))
   with Pre  =&gt Index in First_Index (Container) .. Last_Index (Container)
                    or else raise Constraint_Error;

```

{AI95-00302-03} {AI05-0265-1} {AI12-0112-1} Update_Element calls Process.all with the element at position Index as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

If Element_Type is unconstrained and definite, then the actual Element parameter of Process.all shall be unconstrained.

Ramification: This means that the elements cannot be directly allocated from the heap; it must be possible to change the discriminants of the element in place. 

The element at position Index is not an empty element after successful completion of this operation.

Ramification: Since reading an empty element is a bounded error, attempting to use this procedure to replace empty elements may fail. Use Replace_Element to do that reliably. 

```ada
procedure Update_Element
  (Container : in out Vector;
   Position  : in     Cursor;
   Process   : not null access procedure
                   (Element : in out Element_Type))
   with Pre  =&gt (Position /= No_Element
                    or else raise Constraint_Error) and then
                 (Has_Element (Container, Position)
                    or else raise Program_Error);

```

{AI95-00302-03} {AI05-0264-1} {AI05-0265-1} {AI12-0112-1} Update_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

If Element_Type is unconstrained and definite, then the actual Element parameter of Process.all shall be unconstrained.

The element designated by Position is not an empty element after successful completion of this operation.

```ada
{AI12-0112-1} type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0112-1} type Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

{AI05-0212-1} The types Constant_Reference_Type and Reference_Type need finalization.

This paragraph was deleted.{AI12-0112-1} 

Reason: It is expected that Reference_Type (and Constant_Reference_Type) will be a controlled type, for which finalization will have some action to terminate the tampering check for the associated container. If the object is created by default, however, there is no associated container. Since this is useless, and supporting this case would take extra work, we define it to raise an exception. 

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in Vector;
                             Index     : in Index_Type)
   return Constant_Reference_Type
   with Pre    =&gt Index in First_Index (Container) .. Last_Index (Container)
                    or else raise Constraint_Error,
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read access to an individual element of a vector given an index value.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Constant_Reference returns an object whose discriminant is an access value that designates the element at position Index. Tampering with the elements of Container is prohibited while the object returned by Constant_Reference exists and has not been finalized.

```ada
{AI12-0112-1} function Reference (Container : aliased in out Vector;
                    Index     : in Index_Type)
   return Reference_Type
   with Pre    =&gt Index in First_Index (Container) .. Last_Index (Container)
                    or else raise Constraint_Error,
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read and write access to an individual element of a vector given an index value.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Reference returns an object whose discriminant is an access value that designates the element at position Index. Tampering with the elements of Container is prohibited while the object returned by Reference exists and has not been finalized.

The element at position Index is not an empty element after successful completion of this operation.

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in Vector;
                             Position  : in Cursor)
   return Constant_Reference_Type
   with Pre  =&gt (Position /= No_Element
                    or else raise Constraint_Error) and then
                 (Has_Element (Container, Position)
                    or else raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read access to an individual element of a vector given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Constant_Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Constant_Reference exists and has not been finalized.

```ada
{AI12-0112-1} function Reference (Container : aliased in out Vector;
                    Position  : in Cursor)
   return Reference_Type
   with Pre  =&gt (Position /= No_Element
                    or else raise Constraint_Error) and then
                 (Has_Element (Container, Position)
                    or else raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read and write access to an individual element of a vector given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Reference exists and has not been finalized.

The element designated by Position is not an empty element after successful completion of this operation.

```ada
procedure Assign (Target : in out Vector; Source : in Vector)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Source) = Length (Target) and then
                Capacity (Target) &gt= Length (Target);

```

{AI05-0001-1} {AI05-0248-1} {AI05-0262-1} If Target denotes the same object as Source, the operation has no effect. If the length of Source is greater than the capacity of Target, Reserve_Capacity (Target, Length (Source)) is called. The elements of Source are then copied to Target as for an [assignment_statement](./AA-5.2#S0173) assigning Source to Target (this includes setting the length of Target to be that of Source). 

Discussion: {AI05-0005-1} This routine exists for compatibility with the bounded vector container. For an unbounded vector, Assign(A, B) and A := B behave identically. For a bounded vector, := will raise an exception if the container capacities are different, while Assign will not raise an exception if there is enough room in the target. 

```ada
function Copy (Source : Vector; Capacity : Count_Type := 0)
   return Vector
   with Pre =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                   or else raise Capacity_Error,
        Post =&gt Length (Copy'Result) = Length (Source) and then
                not Tampering_With_Elements_Prohibited (Copy'Result)
                   and then
                not Tampering_With_Cursors_Prohibited (Copy'Result)
                   and then
                Copy'Result.Capacity &gt= (if Capacity = 0 then
                   Length (Source) else Capacity);

```

{AI05-0001-1} {AI12-0112-1} Returns a vector whose elements are initialized from the corresponding elements of Source.

```ada
procedure Move (Target : in out Vector;
                Source : in out Vector)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                   or else raise Program_Error),
        Post =&gt (if not Target'Has_Same_Storage (Source) then
                    Length (Target) = Length (Source)'Old and then
                    Length (Source) = 0 and then
                    Capacity (Target) &gt= Length (Source)'Old);

```

{AI95-00302-03} {AI05-0001-1} {AI05-0248-1} {AI12-0112-1} If Target denotes the same object as Source, then the operation has no effect. Otherwise, Move first calls Reserve_Capacity (Target, Length (Source)) and then Clear (Target); then, each element from Source is removed from Source and inserted into Target in the original order.

Discussion: The idea is that the internal array is removed from Source and moved to Target. (See the Implementation Advice for Move). If Capacity (Target) /= 0, the previous internal array may need to be deallocated. We don't mention this explicitly, because it is covered by the "no memory loss" Implementation Requirement. 

```ada
procedure Insert_Vector (Container : in out Vector;
                         Before    : in     Extended_Index;
                         New_Item  : in     Vector)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before in
                   First_Index (Container) .. Last_Index (Container) + 1
                   or else raise Constraint_Error) and then
                (Length (Container) &lt= Maximum_Length - Length (New_Item)
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Length (New_Item) =
                   Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} {AI12-0400-1} If Length(New_Item) is 0, then Insert_Vector does nothing. Otherwise, it computes the new length NL as the sum of the current length and Length (New_Item); if the value of Last appropriate for length NL would be greater than Index_Type'Last, then Constraint_Error is propagated.

{AI12-0400-1} If the current vector capacity is less than NL, Reserve_Capacity (Container, NL) is called to increase the vector capacity. Then Insert_Vector slides the elements in the range Before .. Last_Index (Container) up by Length(New_Item) positions, and then copies the elements of New_Item to the positions starting at Before. Any exception raised during the copying is propagated.

Ramification: Moving the elements does not necessarily involve copying. Similarly, since Reserve_Capacity does not require the copying of elements, it does not need to be explicitly called (the implementation can combine the operations if it wishes to). 

```ada
procedure Insert_Vector (Container : in out Vector;
                         Before    : in     Cursor;
                         New_Item  : in     Vector)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Length (New_Item)
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Length (New_Item) =
                   Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} {AI12-0400-1} If Length(New_Item) is 0, then Insert_Vector does nothing. If Before is No_Element, then the call is equivalent to Insert_Vector (Container, Last_Index (Container) + 1, New_Item); otherwise, the call is equivalent to Insert_Vector (Container, To_Index (Before), New_Item);

Ramification: The check on Before checks that the cursor does not belong to some other Container. This check implies that a reference to the container is included in the cursor value. This wording is not meant to require detection of dangling cursors; such cursors are defined to be invalid, which means that execution is erroneous, and any result is allowed (including not raising an exception). 

```ada
procedure Insert_Vector (Container : in out Vector;
                         Before    : in     Cursor;
                         New_Item  : in     Vector;
                         Position  :    out Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Length (New_Item)
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Length (New_Item) =
                   Length (Container) and then
                Has_Element (Container, Position) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} {AI12-0112-1} {AI12-0400-1} If Before equals No_Element, then let T be Last_Index (Container) + 1; otherwise, let T be To_Index (Before). Insert_Vector (Container, T, New_Item) is called, and then Position is set to To_Cursor (Container, T).

Discussion: {AI12-0400-1} The messy wording is needed because Before is invalidated by Insert_Vector, and we don't want Position to be invalid after this call. An implementation probably only needs to copy Before to Position. 

```ada
{AI12-0112-1} procedure Insert (Container : in out Vector;
                  Before    : in     Extended_Index;
                  New_Item  : in     Element_Type;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before in
                   First_Index (Container) .. Last_Index (Container) + 1
                   or else raise Constraint_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));

```ada
{AI12-0112-1} procedure Insert (Container : in out Vector;
                  Before    : in     Cursor;
                  New_Item  : in     Element_Type;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, Before, To_Vector (New_Item, Count));

```ada
{AI12-0112-1} procedure Insert (Container : in out Vector;
                  Before    : in     Cursor;
                  New_Item  : in     Element_Type;
                  Position  :    out Cursor;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Has_Element (Container, Position) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, Before, To_Vector (New_Item, Count), Position);

Ramification: {AI05-0257-1} If Count equals 0, Position will designate the element designated by Before, rather than a newly inserted element. Otherwise, Position will designate the first newly inserted element. 

```ada
procedure Insert (Container : in out Vector;
                  Before    : in     Extended_Index;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before in
                   First_Index (Container) .. Last_Index (Container) + 1
                   or else raise Constraint_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} If Count is 0, then Insert does nothing. Otherwise, it computes the new length NL as the sum of the current length and Count; if the value of Last appropriate for length NL would be greater than Index_Type'Last, then Constraint_Error is propagated.

If the current vector capacity is less than NL, Reserve_Capacity (Container, NL) is called to increase the vector capacity. Then Insert slides the elements in the range Before .. Last_Index (Container) up by Count positions, and then inserts elements that are initialized by default (see 3.3.1) in the positions starting at Before.

```ada
procedure Insert (Container : in out Vector;
                  Before    : in     Cursor;
                  Position  :    out Cursor;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Has_Element (Container, Position) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} {AI12-0112-1} If Before equals No_Element, then let T be Last_Index (Container) + 1; otherwise, let T be To_Index (Before). Insert (Container, T, Count) is called, and then Position is set to To_Cursor (Container, T).

Reason: This routine exists mainly to ease conversion between Vector and List containers. Unlike Insert_Space, this routine default initializes the elements it inserts, which can be more expensive for some element types. 

```ada
{AI12-0080-1} {AI12-0112-1} {AI12-0400-1} procedure Prepend_Vector (Container : in out Vector;
                          New_Item  : in     Vector)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Length (New_Item)
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Length (New_Item) =
                   Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, First_Index (Container), New_Item).

```ada
{AI12-0112-1} procedure Prepend (Container : in out Vector;
                   New_Item  : in     Element_Type;
                   Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, First_Index (Container), New_Item, Count).

```ada
{AI12-0112-1} {AI12-0400-1} procedure Append_Vector (Container : in out Vector;
                         New_Item  : in     Vector)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Length (New_Item)
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Length (New_Item) =
                   Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item).

```ada
{AI12-0112-1} {AI12-0400-1} procedure Append (Container : in out Vector;
                  New_Item  : in     Element_Type;
                  Count     : in     Count_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item, Count).

```ada
procedure Append (Container : in out Vector;
                  New_Item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - 1
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + 1 = Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI12-0112-1} {AI12-0212-1} {AI12-0400-1} Equivalent to Insert (Container, Last_Index (Container) + 1, New_Item, 1).

```ada
procedure Insert_Space (Container : in out Vector;
                        Before    : in     Extended_Index;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before in
                   First_Index (Container) .. Last_Index (Container) + 1
                   or else raise Constraint_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} If Count is 0, then Insert_Space does nothing. Otherwise, it computes the new length NL as the sum of the current length and Count; if the value of Last appropriate for length NL would be greater than Index_Type'Last, then Constraint_Error is propagated.

If the current vector capacity is less than NL, Reserve_Capacity (Container, NL) is called to increase the vector capacity. Then Insert_Space slides the elements in the range Before .. Last_Index (Container) up by Count positions, and then inserts empty elements in the positions starting at Before.

```ada
procedure Insert_Space (Container : in out Vector;
                        Before    : in     Cursor;
                        Position  :    out Cursor;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                   Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Maximum_Length - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container) and then
                Has_Element (Container, Position) and then
                Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} {AI12-0112-1} If Before equals No_Element, then let T be Last_Index (Container) + 1; otherwise, let T be To_Index (Before). Insert_Space (Container, T, Count) is called, and then Position is set to To_Cursor (Container, T).

```ada
procedure Delete (Container : in out Vector;
                  Index     : in     Extended_Index;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Index in
                   First_Index (Container) .. Last_Index (Container) + 1
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} If Count is 0, Delete has no effect. Otherwise, Delete slides the elements (if any) starting at position Index + Count down to Index. Any exception raised during element assignment is propagated.

Ramification: If Index + Count &gt= Last_Index(Container), this effectively truncates the vector (setting Last_Index to Index  1 and consequently sets Length to Index  Index_Type'First). 

```ada
procedure Delete (Container : in out Vector;
                  Position  : in out Cursor;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Length (Container)'Old - Count &lt= Length (Container)
                and then Position = No_Element;

```

{AI95-00302-03} {AI12-0112-1} Delete (Container, To_Index (Position), Count) is called, and then Position is set to No_Element.

```ada
{AI12-0112-1} procedure Delete_First (Container : in out Vector;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

{AI95-00302-03} Equivalent to Delete (Container, First_Index (Container), Count).

```ada
{AI12-0112-1} procedure Delete_Last (Container : in out Vector;
                       Count     : in     Count_Type := 1)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

{AI95-00302-03} {AI05-0264-1} If Length (Container) &lt= Count, then Delete_Last is equivalent to Clear (Container). Otherwise, it is equivalent to Delete (Container, Index_Type'Val(Index_Type'Pos(Last_Index (Container))  Count + 1), Count).

```ada
{AI05-0092-1} {AI12-0112-1} procedure Reverse_Elements (Container : in out Vector)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error;

```

{AI95-00302-03} Reorders the elements of Container in reverse order.

Discussion: This can copy the elements of the vector - all cursors referencing the vector are ambiguous afterwards and may designate different elements afterwards. 

```ada
procedure Swap (Container : in out Vector;
                I, J      : in     Index_Type)
   with Pre =&gt (not Tampering_With_Elements_Prohibited (Container)
                  or else raise Program_Error) and then
               (I in First_Index (Container) .. Last_Index (Container)
                  or else raise Constraint_Error) and then
               (J in First_Index (Container) .. Last_Index (Container)
                  or else raise Constraint_Error);

```

{AI95-00302-03} {AI12-0112-1} Swap exchanges the values of the elements at positions I and J.

To be honest: The implementation is not required to actually copy the elements if it can do the swap some other way. But it is allowed to copy the elements if needed. 

```ada
procedure Swap (Container : in out Vector;
                I, J      : in     Cursor)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (I /= No_Element or else Constraint_Error) and then
                (J /= No_Element or else Constraint_Error) and then
                (Has_Element (Container, I)
                   or else raise Program_Error) and then
                (Has_Element (Container, J)
                   or else raise Program_Error);

```

{AI95-00302-03} {AI12-0112-1} Swap exchanges the values of the elements designated by I and J.

Ramification: After a call to Swap, I designates the element value previously designated by J, and J designates the element value previously designated by I. The cursors do not become ambiguous from this operation. 

To be honest: The implementation is not required to actually copy the elements if it can do the swap some other way. But it is allowed to copy the elements if needed. 

```ada
{AI12-0112-1} function First_Index (Container : Vector) return Index_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt First_Index'Result = Index_Type'First;

```

{AI95-00302-03} Returns the value Index_Type'First.

Discussion: We'd rather call this "First", but then calling most routines in here with First (Some_Vect) would be ambiguous. 

```ada
{AI12-0112-1} function First (Container : Vector) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container)
                 then Has_Element (Container, First'Result)
                 else First'Result = No_Element);

```

{AI95-00302-03} If Container is empty, First returns No_Element. Otherwise, it returns a cursor that designates the first element in Container.

```ada
{AI12-0112-1} function First_Element (Container : Vector)
   return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (Container, First_Index (Container)).

```ada
{AI12-0112-1} function Last_Index (Container : Vector) return Extended_Index
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if Length (Container) = 0 then Last_Index'Result = No_Index
                 else Count_Type(Last_Index'Result - Index_Type'First) =
                      Length (Container) - 1);

```

{AI95-00302-03} If Container is empty, Last_Index returns No_Index. Otherwise, it returns the position of the last element in Container.

```ada
{AI12-0112-1} function Last (Container : Vector) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container)
                 then Has_Element (Container, Last'Result)
                 else Last'Result = No_Element);

```

{AI95-00302-03} If Container is empty, Last returns No_Element. Otherwise, it returns a cursor that designates the last element in Container.

```ada
{AI12-0112-1} function Last_Element (Container : Vector)
   return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (Container, Last_Index (Container)).

```ada
{AI12-0112-1} function Next (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

{AI95-00302-03} If Position equals No_Element or designates the last element of the container, then Next returns the value No_Element. Otherwise, it returns a cursor that designates the element with index To_Index (Position) + 1 in the same vector as Position.

```ada
function Next (Container : Vector;
               Position : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then Next'Result = No_Element
                 elsif Has_Element (Container, Next'Result) then
                    To_Index (Container, Next'Result) =
                    To_Index (Container, Position) + 1
                 elsif Next'Result = No_Element then
                    Position = Last (Container)
                 else False);

```

{AI12-0112-1} Returns a cursor designating the next element in Container, if any.

```ada
{AI12-0112-1} procedure Next (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Next (Position).

```ada
procedure Next (Container : in     Vector;
                Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Next (Container, Position).

```ada
{AI12-0112-1} function Previous (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element 
                 then Previous'Result = No_Element);

```

{AI95-00302-03} If Position equals No_Element or designates the first element of the container, then Previous returns the value No_Element. Otherwise, it returns a cursor that designates the element with index To_Index (Position)  1 in the same vector as Position.

```ada
function Previous (Container : Vector;
                   Position  : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then Previous'Result = No_Element
                 elsif Has_Element (Container, Previous'Result) then
                   To_Index (Container, Previous'Result) =
                   To_Index (Container, Position) - 1
                 elsif Previous'Result = No_Element then
                   Position = First (Container)
                 else False);

```

{AI12-0112-1} Returns a cursor designating the previous element in Container, if any.

```ada
{AI12-0112-1} procedure Previous (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Previous (Position).

```ada
procedure Previous (Container : in     Vector;
                    Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Previous (Container, Position).

```ada
function Find_Index (Container : Vector;
                     Item      : Element_Type;
                     Index     : Index_Type := Index_Type'First)
   return Extended_Index;

```

{AI95-00302-03} Searches the elements of Container for an element equal to Item (using the generic formal equality operator). The search starts at position Index and proceeds towards Last_Index (Container). If no equal element is found, then Find_Index returns No_Index. Otherwise, it returns the index of the first equal element encountered.

```ada
function Find (Container : Vector;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
   return Cursor
   with Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Find'Result /= No_Element
                 then Has_Element (Container, Find'Result));

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} Find searches the elements of Container for an element equal to Item (using the generic formal equality operator). The search starts at the first element if Position equals No_Element, and at the element designated by Position otherwise. It proceeds towards the last element of Container. If no equal element is found, then Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

```ada
function Reverse_Find_Index (Container : Vector;
                             Item      : Element_Type;
                             Index     : Index_Type := Index_Type'Last)
   return Extended_Index;

```

{AI95-00302-03} Searches the elements of Container for an element equal to Item (using the generic formal equality operator). The search starts at position Index or, if Index is greater than Last_Index (Container), at position Last_Index (Container). It proceeds towards First_Index (Container). If no equal element is found, then Reverse_Find_Index returns No_Index. Otherwise, it returns the index of the first equal element encountered.

```ada
function Reverse_Find (Container : Vector;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   return Cursor
   with Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Reverse_Find'Result /= No_Element
                 then Has_Element (Container, Reverse_Find'Result));

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} Reverse_Find searches the elements of Container for an element equal to Item (using the generic formal equality operator). The search starts at the last element if Position equals No_Element, and at the element designated by Position otherwise. It proceeds towards the first element of Container. If no equal element is found, then Reverse_Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

```ada
function Contains (Container : Vector;
                   Item      : Element_Type) return Boolean;

```

{AI95-00302-03} Equivalent to Has_Element (Find (Container, Item)).

Paragraphs 225 and 226 were moved above. 

```ada
{AI12-0112-1} procedure Iterate
  (Container : in Vector;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0265-1} Invokes Process.all with a cursor that designates each element in Container, in index order. Tampering with the cursors of Container is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

Discussion: The purpose of the "tamper with the cursors" check is to prevent erroneous execution from the Position parameter of Process.all becoming invalid. This check takes place when the operations that tamper with the cursors of the container are called. The check cannot be made later (say in the body of Iterate), because that could cause the Position cursor to be invalid and potentially cause execution to become erroneous -- defeating the purpose of the check.

There is no check needed if an attempt is made to insert or delete nothing (that is, Count = 0 or Length(Item) = 0).

The check is easy to implement: each container needs a counter. The counter is incremented when Iterate is called, and decremented when Iterate completes. If the counter is nonzero when an operation that inserts or deletes is called, Finalize is called, or one of the other operations in the list occurs, Program_Error is raised. 

```ada
{AI12-0112-1} procedure Reverse_Iterate
  (Container : in Vector;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0212-1} Iterates over the elements in Container as per procedure Iterate, except that elements are traversed in reverse index order.

```ada
{AI12-0212-1} {AI12-0266-1} function Iterate (Container : in Vector)
   return Vector_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
   with Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0266-1} Iterate returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each node in Container, starting with the first node and moving the cursor as per the Next function when used as a forward iterator, and starting with the last node and moving the cursor as per the Previous function when used as a reverse iterator, and processing all nodes concurrently when used as a parallel iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

```ada
function Iterate (Container : in Vector; Start : in Cursor)
   return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   with Pre    =&gt (Start /= No_Element
                        or else raise Constraint_Error) and then
                     (Has_Element (Container, Start)
                        or else raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0262-1} {AI05-0265-1} {AI05-0269-1} {AI12-0212-1} Iterate returns a reversible iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each node in Container, starting with the node designated by Start and moving the cursor as per the Next function when used as a forward iterator, or moving the cursor as per the Previous function when used as a reverse iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

Discussion: Exits are allowed from the loops created using the iterator objects. In particular, to stop the iteration at a particular cursor, just add 

```ada
exit when Cur = Stop;

```

in the body of the loop (assuming that Cur is the loop parameter and Stop is the cursor that you want to stop at). 

{AI05-0044-1} {AI05-0262-1} The actual function for the generic formal function "&lt" of Generic_Sorting is expected to return the same value each time it is called with a particular pair of element values. It should define a strict weak ordering relationship (see A.18); it should not modify Container. If the actual for "&lt" behaves in some other manner, the behavior of the subprograms of Generic_Sorting are unspecified. The number of times the subprograms of Generic_Sorting call "&lt" is unspecified.

```ada
function Is_Sorted (Container : Vector) return Boolean;

```

{AI95-00302-03} Returns True if the elements are sorted smallest first as determined by the generic formal "&lt" operator; otherwise, Is_Sorted returns False. Any exception raised during evaluation of "&lt" is propagated.

```ada
{AI12-0112-1} procedure Sort (Container : in out Vector)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error;

```

{AI95-00302-03} Reorders the elements of Container such that the elements are sorted smallest first as determined by the generic formal "&lt" operator provided. Any exception raised during evaluation of "&lt" is propagated.

Ramification: This implies swapping the elements, usually including an intermediate copy. This means that the elements will usually be copied. (As with Swap, if the implementation can do this some other way, it is allowed to.) Since the elements are nonlimited, this usually will not be a problem. Note that there is Implementation Advice below that the implementation should use a sort that minimizes copying of elements.

The sort is not required to be stable (and the fast algorithm required will not be stable). If a stable sort is needed, the user can include the original location of the element as an extra "sort key". We considered requiring the implementation to do that, but it is mostly extra overhead -- usually there is something already in the element that provides the needed stability. 

```ada
procedure Merge (Target  : in out Vector;
                 Source  : in out Vector)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                   or else raise Program_Error) and then
                (Length (Target) &lt= Maximum_Length - Length (Source)
                   or else raise Constraint_Error) and then
                ((Length (Source) = 0 or else
                   not Target'Has_Same_Storage (Source))
                   or else raise Program_Error),
        Post =&gt (declare
                    Result_Length : constant Count_Type :=
                       Length (Source)'Old + Length (Target)'Old;
                 begin
                    (Length (Source) = 0 and then
                     Length (Target) = Result_Length and then
                     Capacity (Target) &gt= Result_Length));

```

{AI95-00302-03} {AI05-0021-1} {AI12-0112-1} Merge removes elements from Source and inserts them into Target; afterwards, Target contains the union of the elements that were initially in Source and Target; Source is left empty. If Target and Source are initially sorted smallest first, then Target is ordered smallest first as determined by the generic formal "&lt" operator; otherwise, the order of elements in Target is unspecified. Any exception raised during evaluation of "&lt" is propagated.

Discussion: It is a bounded error if either of the vectors is unsorted, see below. The bounded error can be recovered by sorting Target after the merge call, or the vectors can be pretested with Is_Sorted. 

Implementation Note: The Merge operation will usually require copying almost all of the elements. One implementation strategy would be to extend Target to the appropriate length, then copying elements from the back of the vectors working towards the front. An alternative approach would be to allocate a new internal data array of the appropriate length, copy the elements into it in an appropriate order, and then replacing the data array in Target with the temporary. 

{AI12-0111-1} The nested package Vectors.Stable provides a type Stable.Vector that represents a stable vector, which is one that cannot grow and shrink. Such a vector can be created by calling the To_Vector or Copy functions, or by establishing a stabilized view of an ordinary vector.

{AI12-0111-1} The subprograms of package Containers.Vectors that have a parameter or result of type Vector are included in the nested package Stable with the same specification, except that the following are omitted:

{AI12-0111-1} {AI12-0400-1} Tampering_With_Cursors_Prohibited, Tampering_With_Elements_Prohibited, Reserve_Capacity, Assign, Move, Insert, Insert_Space, Insert_Vector, Append, Append_Vector, Prepend, Prepend_Vector, Clear, Delete, Delete_First, Delete_Last, and Set_Length 

The generic package Generic_Sorting is also included with the same specification, except that Merge is omitted.

Ramification: The names Vector and Cursor mean the types declared in the nested package in these subprogram specifications. 

Reason: The omitted routines are those that tamper with cursors or elements (or test that state). The model is that it is impossible to tamper with cursors or elements of a stable view since no such operations are included. Thus tampering checks are not needed for a stable view, and we omit the operations associated with those checks. 

{AI12-0111-1} The operations of this package are equivalent to those for ordinary vectors, except that the calls to Tampering_With_Cursors_Prohibited and Tampering_With_Elements_Prohibited that occur in preconditions are replaced by False, and any that occur in postconditions are replaced by True.

{AI12-0111-1} {AI12-0439-1} If a stable vector is declared with the Base discriminant designating a pre-existing ordinary vector, the stable vector represents a stabilized view of the underlying ordinary vector, and any operation on the stable vector is reflected on the underlying ordinary vector. While a stabilized view exists, any operation that tampers with elements performed on the underlying vector is prohibited. The finalization of a stable vector that provides such a view removes this restriction on the underlying ordinary vector [(though some other restriction can exist due to other concurrent iterations or stabilized views)].

{AI12-0111-1} {AI12-0438-1} If a stable vector is declared without specifying Base, the object is necessarily initialized. The initializing expression of the stable vector, [typically a call on To_Vector or Copy], determines the Length of the vector. The Length of a stable vector never changes after initialization.

Proof: {AI12-0438-1} Initialization is required as the type is indefinite, see 3.3.1. 


#### Bounded (Run-Time) Errors

{AI95-00302-03} {AI05-0212-1} Reading the value of an empty element by calling Element, Query_Element, Update_Element, Constant_Reference, Reference, Swap, Is_Sorted, Sort, Merge, "=", Find, or Reverse_Find is a bounded error. The implementation may treat the element as having any normal value (see 13.9.1) of the element type, or raise Constraint_Error or Program_Error before modifying the vector.

Ramification: For instance, a default initialized element could be returned. Or some previous value of an element. But returning random junk is not allowed if the type has default initial value(s).

Assignment and streaming of empty elements are not bounded errors. This is consistent with regular composite types, for which assignment and streaming of uninitialized components do not cause a bounded error, but reading the uninitialized component does cause a bounded error.

There are other operations which are defined in terms of the operations listed above. 

{AI95-00302-03} Calling Merge in an instance of Generic_Sorting with either Source or Target not ordered smallest first using the provided generic formal "&lt" operator is a bounded error. Either Program_Error is raised after Target is updated as described for Merge, or the operation works as defined.

{AI05-0022-1} {AI05-0248-1} It is a bounded error for the actual function associated with a generic formal subprogram, when called as part of an operation of this package, to tamper with elements of any Vector parameter of the operation. Either Program_Error is raised, or the operation works as defined on the value of the Vector either prior to, or subsequent to, some or all of the modifications to the Vector.

{AI05-0027-1} It is a bounded error to call any subprogram declared in the visible part of Containers.Vectors when the associated container has been finalized. If the operation takes Container as an in out parameter, then it raises Constraint_Error or Program_Error. Otherwise, the operation either proceeds as it would for an empty container, or it raises Constraint_Error or Program_Error.

{AI95-00302-03} A Cursor value is ambiguous if any of the following have occurred since it was created:

{AI12-0400-1} Insert, Insert_Space, Insert_Vector, or Delete has been called on the vector that contains the element the cursor designates with an index value (or a cursor designating an element at such an index value) less than or equal to the index value of the element designated by the cursor; or

The vector that contains the element it designates has been passed to the Sort or Merge procedures of an instance of Generic_Sorting, or to the Reverse_Elements procedure.

{AI95-00302-03} It is a bounded error to call any subprogram other than "=" or Has_Element declared in Containers.Vectors with an ambiguous (but not invalid, see below) cursor parameter. Possible results are:

The cursor may be treated as if it were No_Element;

The cursor may designate some element in the vector (but not necessarily the element that it originally designated);

Constraint_Error may be raised; or

Program_Error may be raised.

Reason: Cursors are made ambiguous if an Insert or Delete occurs that moves the elements in the internal array including the designated ones. After such an operation, the cursor probably still designates an element (although it might not after a deletion), but it is a different element. That violates the definition of cursor - it designates a particular element.

For "=" or Has_Element, the cursor works normally (it would not be No_Element). We don't want to trigger an exception simply for comparing a bad cursor.

While it is possible to check for these cases or ensure that cursors survive such operations, in many cases the overhead necessary to make the check (or ensure cursors continue to designate the same element) is substantial in time or space. 


#### Erroneous Execution

{AI95-00302-03} A Cursor value is invalid if any of the following have occurred since it was created: 

The vector that contains the element it designates has been finalized;

{AI05-0160-1} The vector that contains the element it designates has been used as the Target of a call to Assign, or as the target of an [assignment_statement](./AA-5.2#S0173);

[The vector that contains the element it designates has been used as the Source or Target of a call to Move;] or 

Proof: {AI05-0001-1} Move has been reworded in terms of Assign and Clear, which are covered by other bullets, so this text is redundant. 

{AI05-0160-1} {AI05-0262-1} The element it designates has been deleted or removed from the vector that previously contained the element. 

Ramification: {AI05-0160-1} An element can be removed via calls to Set_Length, Clear, and Merge; and indirectly via calls to Assign and Move. 

{AI95-00302-03} The result of "=" or Has_Element is unspecified if it is called with an invalid cursor parameter. Execution is erroneous if any other subprogram declared in Containers.Vectors is called with an invalid cursor parameter.

Discussion: The list above (combined with the bounded error cases) is intended to be exhaustive. In other cases, a cursor value continues to designate its original element. For instance, cursor values survive the appending of new elements. 

{AI05-0212-1} Execution is erroneous if the vector associated with the result of a call to Reference or Constant_Reference is finalized before the result object returned by the call to Reference or Constant_Reference is finalized.

Reason: Each object of Reference_Type and Constant_Reference_Type probably contains some reference to the originating container. If that container is prematurely finalized (which is only possible via Unchecked_Deallocation, as accessibility checks prevent passing a container to Reference that will not live as long as the result), the finalization of the object of Reference_Type will try to access a nonexistent object. This is a normal case of a dangling pointer created by Unchecked_Deallocation; we have to explicitly mention it here as the pointer in question is not visible in the specification of the type. (This is the same reason we have to say this for invalid cursors.) 


#### Implementation Requirements

{AI95-00302-03} No storage associated with a vector object shall be lost upon assignment or scope exit.

{AI95-00302-03} {AI05-0262-1} The execution of an [assignment_statement](./AA-5.2#S0173) for a vector shall have the effect of copying the elements from the source vector object to the target vector object and changing the length of the target object to that of the source object.

Implementation Note: {AI05-0298-1} {AI12-0005-1} An assignment of a Vector is a "deep" copy; that is the elements are copied as well as the data structures. We say "effect of" in order to allow the implementation to avoid copying elements immediately if it wishes. For instance, an implementation that avoided copying until one of the containers is modified would be allowed. (Note that such an implementation would require care, as Query_Element and Constant_Reference both could be used to access an element which later needs to be reallocated while the parameter or reference still exists, potentially leaving the parameter or reference pointing at the wrong element.) 


#### Implementation Advice

{AI95-00302-03} Containers.Vectors should be implemented similarly to an array. In particular, if the length of a vector is N, then

the worst-case time complexity of Element should be O(log N); 

Implementation Advice: The worst-case time complexity of Element for Containers.Vector should be O(log N).

the worst-case time complexity of Append with Count=1 when N is less than the capacity of the vector should be O(log N); and 

Implementation Advice: The worst-case time complexity of Append with Count = 1 when N is less than the capacity for Containers.Vector should be O(log N).

the worst-case time complexity of Prepend with Count=1 and Delete_First with Count=1 should be O(N log N). 

Implementation Advice: The worst-case time complexity of Prepend with Count = 1 and Delete_First with Count=1 for Containers.Vectors should be O(N log N).

Reason: We do not mean to overly constrain implementation strategies here. However, it is important for portability that the performance of large containers has roughly the same factors on different implementations. If a program is moved to an implementation that takes O(N) time to access elements, that program could be unusable when the vectors are large. We allow O(log N) access because the proportionality constant and caching effects are likely to be larger than the log factor, and we don't want to discourage innovative implementations. 

{AI95-00302-03} The worst-case time complexity of a call on procedure Sort of an instance of Containers.Vectors.Generic_Sorting should be O(N**2), and the average time complexity should be better than O(N**2). 

Implementation Advice: The worst-case time complexity of a call on procedure Sort of an instance of Containers.Vectors.Generic_Sorting should be O(N**2), and the average time complexity should be better than O(N**2).

Ramification: In other words, we're requiring the use of a better than O(N**2) sorting algorithm, such as Quicksort. No bubble sorts allowed! 

{AI95-00302-03} Containers.Vectors.Generic_Sorting.Sort and Containers.Vectors.Generic_Sorting.Merge should minimize copying of elements. 

Implementation Advice: Containers.Vectors.Generic_Sorting.Sort and Containers.Vectors.Generic_Sorting.Merge should minimize copying of elements.

To be honest: We do not mean "absolutely minimize" here; we're not intending to require a single copy for each element. Rather, we want to suggest that the sorting algorithm chosen is one that does not copy items unnecessarily. Bubble sort would not meet this advice, for instance. 

{AI95-00302-03} Move should not copy elements, and should minimize copying of internal data structures. 

Implementation Advice: Containers.Vectors.Move should not copy elements, and should minimize copying of internal data structures.

Implementation Note: Usually that can be accomplished simply by moving the pointer(s) to the internal data structures from the Source vector to the Target vector. 

{AI95-00302-03} If an exception is propagated from a vector operation, no storage should be lost, nor any elements removed from a vector unless specified by the operation. 

Implementation Advice: If an exception is propagated from a vector operation, no storage should be lost, nor any elements removed from a vector unless specified by the operation.

Reason: This is important so that programs can recover from errors. But we don't want to require heroic efforts, so we just require documentation of cases where this can't be accomplished. 

NOTE 1   {AI12-0440-1} All elements of a vector occupy locations in the internal array. If a sparse container is required, a Hashed_Map can be used rather than a vector.

NOTE 2   If Index_Type'Base'First = Index_Type'First an instance of Ada.Containers.Vectors will raise Constraint_Error. A value below Index_Type'First is required so that an empty vector has a meaningful value of Last_Index.

Discussion: This property is the main reason why only integer types (as opposed to any discrete type) are allowed as the index type of a vector. An enumeration or modular type would require a subtype in order to meet this requirement. 


#### Extensions to Ada 95

{AI95-00302-03} The package Containers.Vectors is new. 


#### Incompatibilities With Ada 2005

{AI05-0001-1} Subprograms Assign and Copy are added to Containers.Vectors. If an instance of Containers.Vectors is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Containers.Vectors is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 2005

{AI05-0212-1} Added iterator, reference, and indexing support to make vector containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0001-1} Generalized the definition of Reserve_Capacity and Move. Specified which elements are read/written by stream attributes.

{AI05-0022-1} Correction: Added a Bounded (Run-Time) Error to cover tampering by generic actual subprograms.

{AI05-0027-1} Correction: Added a Bounded (Run-Time) Error to cover access to finalized vector containers.

{AI05-0044-1} Correction: Redefined "&lt" actuals to require a strict weak ordering; the old definition allowed indeterminant comparisons that would not have worked in a container.

{AI05-0084-1} Correction: Added a pragma Remote_Types so that containers can be used in distributed programs.

{AI05-0160-1} Correction: Revised the definition of invalid cursors to cover missing (and new) cases.

{AI05-0265-1} Correction: Defined when a container prohibits tampering in order to more clearly define where the check is made and the exception raised. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Tampering with elements is now defined to be equivalent to tampering with cursors for ordinary containers. If a program requires tampering detection to work, it might fail in Ada 2022. Specifically, if a program requires Program_Error to be raised by a routine that (only) tampers with elements in Ada 2012 (such as Replace_Element) when called in a context that does not allow tampering with elements (such as Update_Element), the routine will work as defined instead of raising Program_Error in Ada 2022. Needless to say, this shouldn't happen outside of test programs. Note that such contexts still prohibit tampering with cursors, so routines like Insert and Delete will still raise Program_Error in this case.

{AI12-0112-1} Trying to insert or concatenate more than Count_Type'Last elements will now raise Constraint_Error rather than Capacity_Error. This is extremely unlikely to happen, as Count_Type'Last is typically at least 2**31-1, so most such vectors will exceed memory before reaching this error. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} {AI12-0339-1} A number of new subprograms, types, and even a nested package were added to Containers.Vectors to better support contracts and stable views. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic.

{AI12-0005-1} {AI12-0212-1} {AI12-0400-1} Vector objects now support aggregates. This introduces a potential incompatibility for overloaded routines, including the "&" operations defined in this package. If the Element_Type of the vector is a type that allows aggregates (such as a record type), then calls to the "&" operations with an aggregate element will become ambiguous in Ada 2022, while they would have been legal in Ada 2012. This can be fixed by qualifying the aggregate with the element type.

{AI12-0400-1} Correction: The Insert, Append, and Prepend operations that operate on two vectors have been renamed Insert_Vector, Append_Vector, and Prepend_Vector, respectively. This was done in order to eliminate the aggregate ambiguity for the commonly used single element Append and Insert routines. The renamed routines are rarely used in Ada 2012 code, so the impact should be minimal. 


#### Extensions to Ada 2012

{AI12-0196-1} Correction: To_Cursor and Replace_Element are now defined such that they can be used concurrently so long as they operate on different elements. This allows some container operations to be used in parallel without separate synchronization.

{AI12-0212-1} Vectors now support indexed container aggregates, so [aggregate](./AA-4.3#S0106) syntax can be used to create Vectors.

{AI12-0266-1} The iterator for the entire container now can return a parallel iterator which can be used to process the container in parallel. 


#### Wording Changes from Ada 2012

{AI12-0110-1} Corrigendum: Clarified that tampering checks precede all other checks made by a subprogram (but come after those associated with the call).

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5).

{AI12-0400-1} Correction: Split the Append routine into two routines rather than having a single routine with a default parameter, in order that a routine with the appropriate profile for the Aggregate aspect exists. This change should not change the behavior of any existing code. 


## A.18.3  The Generic Package Containers.Doubly_Linked_Lists

{AI95-00302-03} The language-defined generic package Containers.Doubly_Linked_Lists provides private types List and Cursor, and a set of operations for each type. A list container is optimized for insertion and deletion at any position. 

{AI95-00302-03} A doubly-linked list container object manages a linked list of internal nodes, each of which contains an element and pointers to the next (successor) and previous (predecessor) internal nodes. A cursor designates a particular node within a list (and by extension the element contained in that node). A cursor keeps designating the same node (and element) as long as the node is part of the container, even if the node is moved in the container.

{AI95-00302-03} The length of a list is the number of elements it contains. 


#### Static Semantics

{AI95-00302-03} The generic library package Containers.Doubly_Linked_Lists has the following declaration: 

```ada
{AI05-0084-1} {AI05-0212-1} {AI12-0112-1} with Ada.Iterator_Interfaces;
generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type)
      return Boolean is &lt&gt;
package Ada.Containers.Doubly_Linked_Lists
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects in this generic package, see the notes on the equivalent operations in the specification of the Containers.Vectors package (see A.18.2). 

```ada
{AI05-0212-1} {AI12-0111-1} {AI12-0112-1} {AI12-0212-1} {AI12-0339-1} {AI12-0391-1} {AI12-0399-1} {AI12-0400-1}    type List is tagged private
      with Constant_Indexing =&gt Constant_Reference,
           Variable_Indexing =&gt Reference,
           Default_Iterator  =&gt Iterate,
           Iterator_Element  =&gt Element_Type,
           Iterator_View     =&gt Stable.List,
           Aggregate         =&gt (Empty       =&gt Empty,
                                 Add_Unnamed =&gt Append),
           Stable_Properties =&gt (Length,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =&gt
              Length (List) = 0 and then
              (not Tampering_With_Cursors_Prohibited (List)) and then
              (not Tampering_With_Elements_Prohibited (List)),
           Preelaborable_Initialization;

```

```ada
{AI12-0399-1}    type Cursor is private
      with Preelaborable_Initialization;

```

```ada
   Empty_List : constant List;

```

```ada
   No_Element : constant Cursor;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Has_Element (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Has_Element (Container : List; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1}    package List_Iterator_Interfaces is new
       Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
   function "=" (Left, Right : List) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_Cursors_Prohibited
      (Container : List) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Tampering_With_Elements_Prohibited
      (Container : List) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty return List
      is (Empty_List)
      with Post =&gt
             not Tampering_With_Elements_Prohibited (Empty'Result) and then
             not Tampering_With_Cursors_Prohibited (Empty'Result) and then
             Length (Empty'Result) = 0;

```

```ada
{AI12-0112-1}    function Length (Container : List) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Empty (Container : List) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

```ada
{AI12-0112-1}    procedure Clear (Container : in out List)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Length (Container) = 0;

```

```ada
{AI12-0112-1}    function Element (Position : Cursor) return Element_Type
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    function Element (Container : List;
                     Position  : Cursor) return Element_Type
      with Pre =&gt (Position /= No_Element or else
                      raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out List;
                              Position  : in     Cursor;
                              New_item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Element : in Element_Type))
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in List;
      Position  : in Cursor;
      Process   : not null access procedure (Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                    (Has_Element (Container, Position)
                       or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Update_Element
     (Container : in out List;
      Position  : in     Cursor;
      Process   : not null access procedure
                      (Element : in out Element_Type))
      with Pre  =&gt (Position /= No_Element 
                       or else raise Constraint_Error) and then
                    (Has_Element (Container, Position) 
                       or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Reference_Type (Element : not null access Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in List;
                                Position  : in Cursor)
      return Constant_Reference_Type
      with Pre  =&gt (Position /= No_Element or else
                       raise Constraint_Error) and then
                    (Has_Element (Container, Position) or else
                       raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out List;
                       Position  : in Cursor)
      return Reference_Type
      with Pre  =&gt (Position /= No_Element or else
                       raise Constraint_Error) and then
                    (Has_Element (Container, Position) or else
                       raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0001-1} {AI12-0112-1}    procedure Assign (Target : in out List; Source : in List)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target);

```

```ada
{AI05-0001-1} {AI12-0112-1}    function Copy (Source : List)
      return List
      with Post =&gt 
              Length (Copy'Result) = Length (Source) and then
              not Tampering_With_Elements_Prohibited (Copy'Result) and then
              not Tampering_With_Cursors_Prohibited (Copy'Result);

```

```ada
{AI12-0112-1}    procedure Move (Target : in out List;
                   Source : in out List)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                      or else raise Program_Error),
            Post =&gt (if not Target'Has_Same_Storage (Source) then
                      Length (Target) = Length (Source'Old) and then
                      Length (Source) = 0);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out List;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count = Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out List;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count = Length (Container)
                   and then Has_Element (Container, Position);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out List;
                     Before    : in     Cursor;
                     Position  :    out Cursor;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - Count
                      or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count = Length (Container)
                   and then Has_Element (Container, Position);

```

```ada
{AI12-0112-1}    procedure Prepend (Container : in out List;
                      New_Item  : in     Element_Type;
                      Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                    (Length (Container) &lt= Count_Type'Last - Count
                       or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count = Length (Container);

```

```ada
{AI12-0112-1} {AI12-0400-1}    procedure Append (Container : in out List;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - Count
                       or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + Count = Length (Container);

```

```ada
{AI12-0391-1} {AI12-0400-1}    procedure Append (Container : in out List;
                     New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt Length (Container)'Old + 1 = Length (Container);

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out List;
                     Position  : in out Cursor;
                     Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Length (Container)'Old - Count &lt= Length (Container)
                   and then Position = No_Element;

```

```ada
{AI12-0112-1}    procedure Delete_First (Container : in out List;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Delete_Last (Container : in out List;
                          Count     : in     Count_Type := 1)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Reverse_Elements (Container : in out List)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error;

```

```ada
{AI12-0112-1}    procedure Swap (Container : in out List;
                   I, J      : in     Cursor)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                      or else raise Program_Error) and then
                   (I /= No_Element or else Constraint_Error) and then
                   (J /= No_Element or else Constraint_Error) and then
                   (Has_Element (Container, I)
                      or else raise Program_Error) and then
                   (Has_Element (Container, J)
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Swap_Links (Container : in out List;
                         I, J      : in     Cursor)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                       or else raise Program_Error) and then
                   (I /= No_Element or else Constraint_Error) and then
                   (J /= No_Element or else Constraint_Error) and then
                   (Has_Element (Container, I)
                       or else raise Program_Error) and then
                   (Has_Element (Container, J)
                       or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Splice (Target   : in out List;
                     Before   : in     Cursor;
                     Source   : in out List)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Target, Before)
                      or else raise Program_Error) and then
                   (Target'Has_Same_Storage (Source) or else
                    Length (Target) &lt= Count_Type'Last - Length (Source)
                      or else raise Constraint_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                      (declare
                         Result_Length : constant Count_Type :=
                            Length (Source)'Old + Length (Target)'Old;
                       begin
                         Length (Source) = 0 and then
                         Length (Target) = Result_Length));

```

```ada
{AI12-0112-1}    procedure Splice (Target   : in out List;
                     Before   : in     Cursor;
                     Source   : in out List;
                     Position : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                      or else raise Program_Error) and then
                   (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Source, Position)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Target, Before)
                      or else raise Program_Error) and then
                   (Target'Has_Same_Storage (Source) or else
                    Length (Target) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt (declare
                      Org_Target_Length : constant Count_Type :=
                         Length (Target)'Old;
                      Org_Source_Length : constant Count_Type :=
                         Length (Source)'Old;
                    begin
                       (if Target'Has_Same_Storage (Source) then
                           Position = Position'Old
                        else 
                           Length (Source) = Org_Source_Length - 1 and then
                           Length (Target) = Org_Target_Length + 1 and then
                           Has_Element (Target, Position)));

```

```ada
   procedure Splice (Container: in out List;
                     Before   : in     Cursor;
                     Position : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Has_Element (Container, Before)
                      or else raise Program_Error),
           Post =&gt  Length (Container) = Length (Container)'Old;

```

```ada
{AI12-0112-1}    function First (Container : List) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, First'Result)
                    else First'Result = No_Element);

```

```ada
{AI12-0112-1}    function First_Element (Container : List)
      return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Last (Container : List) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, Last'Result)
                    else Last'Result = No_Element);

```

```ada
{AI12-0112-1}    function Last_Element (Container : List)
      return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Next (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Container : List;
                  Position : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then Next'Result = No_Element
                    elsif Next'Result = No_Element then
                      Position = Last (Container)
                    else Has_Element (Container, Next'Result));

```

```ada
{AI12-0112-1}    function Previous (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then 
                      Previous'Result = No_Element);

```

```ada
{AI12-0112-1}    function Previous (Container : List;
                      Position  : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then 
                      Previous'Result = No_Element
                    elsif Previous'Result = No_Element then
                      Position = First (Container)
                    else Has_Element (Container, Previous'Result));

```

```ada
{AI12-0112-1}    procedure Next (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Next (Container : in     List;
                   Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    procedure Previous (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Previous (Container : in     List;
                       Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element then 
                      Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Find (Container : List;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      return Cursor
      with Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

```ada
{AI12-0112-1}    function Reverse_Find (Container : List;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      return Cursor
      with Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Reverse_Find'Result /= No_Element
                    then Has_Element (Container, Reverse_Find'Result));

```

```ada
   function Contains (Container : List;
                      Item      : Element_Type) return Boolean;

```

```ada
This paragraph was deleted.{AI05-0212-1} 

```

```ada
{AI12-0112-1}    procedure  Iterate
     (Container : in List;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI12-0112-1}    procedure Reverse_Iterate
     (Container : in List;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate (Container : in List)
      return List_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
      with Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Iterate (Container : in List; Start : in Cursor)
      return List_Iterator_Interfaces.Reversible_Iterator'Class
      with Pre    =&gt (Start /= No_Element
                        or else raise Constraint_Error) and then
                     (Has_Element (Container, Start)
                        or else raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0112-1}    generic
      with function "&lt" (Left, Right : Element_Type)
         return Boolean is &lt&gt;
   package Generic_Sorting
   with Nonblocking, Global =&gt null is

```

```ada
      function Is_Sorted (Container : List) return Boolean;

```

```ada
{AI12-0112-1}       procedure Sort (Container : in out List)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                          or else raise Program_Error;

```

```ada
{AI12-0112-1}       procedure Merge (Target  : in out List;
                       Source  : in out List)
         with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                         or else raise Program_Error) and then
                      (not Tampering_With_Elements_Prohibited (Source)
                         or else raise Program_Error) and then
                      (Length (Target) &lt= Count_Type'Last - Length (Source)
                         or else raise Constraint_Error) and then
                      ((Length (Source) = 0 or else
                         not Target'Has_Same_Storage (Source))
                         or else raise Constraint_Error),
              Post =&gt (declare
                         Result_Length : constant Count_Type :=
                            Length (Source)'Old + Length (Target)'Old;
                       begin
                         (Length (Source) = 0 and then
                          Length (Target) = Result_Length));

```

```ada
   end Generic_Sorting;

```

```ada
{AI12-0111-1}    package Stable is

```

```ada
{AI12-0111-1} {AI12-0339-1} {AI12-0391-1} {AI12-0400-1} {AI12-0407-1}       type List (Base : not null access Doubly_Linked_Lists.List) is
         tagged limited private
         with Constant_Indexing =&gt Constant_Reference,
              Variable_Indexing =&gt Reference,
              Default_Iterator  =&gt Iterate,
              Iterator_Element  =&gt Element_Type,
              Stable_Properties =&gt (Length),
              Global =&gt null,
              Default_Initial_Condition =&gt Length (List) = 0,
              Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       type Cursor is private
         with Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       Empty_List : constant List;

```

```ada
{AI12-0111-1}       No_Element : constant Cursor;

```

```ada
{AI12-0111-1}       function Has_Element (Position : Cursor) return Boolean
         with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0111-1}       package List_Iterator_Interfaces is new
         Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
{AI12-0111-1}       procedure Assign (Target : in out Doubly_Linked_Lists.List;
                        Source : in List)
         with Post =&gt Length (Source) = Length (Target);

```

```ada
{AI12-0111-1}       function Copy (Source : Doubly_Linked_Lists.List) return List
         with Post =&gt Length (Copy'Result) = Length (Source);

```

```ada
{AI12-0111-1}       type Constant_Reference_Type
            (Element : not null access constant Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       type Reference_Type
            (Element : not null access Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       -- Additional subprograms as described in the text
      -- are declared here.

```

```ada
{AI12-0111-1}    private

```

```ada
{AI12-0111-1}       ... -- not specified by the language

```

```ada
{AI12-0111-1}    end Stable;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Doubly_Linked_Lists;

```

{AI95-00302-03} The actual function for the generic formal function "=" on Element_Type values is expected to define a reflexive and symmetric relationship and return the same result value each time it is called with a particular pair of values. If it behaves in some other manner, the functions Find, Reverse_Find, and "=" on list values return an unspecified value. The exact arguments and number of calls of this generic formal function by the functions Find, Reverse_Find, and "=" on list values are unspecified.

Ramification: If the actual function for "=" is not symmetric and consistent, the result returned by the listed functions cannot be predicted. The implementation is not required to protect against "=" raising an exception, or returning random results, or any other "bad" behavior. And it can call "=" in whatever manner makes sense. But note that only the results of Find, Reverse_Find, and List "=" are unspecified; other subprograms are not allowed to break if "=" is bad (they aren't expected to use "="). 

{AI95-00302-03} The type List is used to represent lists. The type List needs finalization (see 7.6).

{AI95-00302-03} Empty_List represents the empty List object. It has a length of 0. If an object of type List is not otherwise initialized, it is initialized to the same value as Empty_List.

{AI95-00302-03} No_Element represents a cursor that designates no element. If an object of type Cursor is not otherwise initialized, it is initialized to the same value as No_Element.

{AI95-00302-03} {AI12-0434-1} The primitive "=" operator for type Cursor returns True if both cursors are No_Element, or designate the same element in the same container.

To be honest: {AI12-0434-1} "The primitive "=" operator" is the one with two parameters of type Cursor which returns Boolean. We're not talking about some other (hidden) primitive function named "=". 

{AI95-00302-03} Execution of the default implementation of the Input, Output, Read, or Write attribute of type Cursor raises Program_Error.

Reason: A cursor will probably be implemented in terms of one or more access values, and the effects of streaming access values is unspecified. Rather than letting the user stream junk by accident, we mandate that streaming of cursors raise Program_Error by default. The attributes can always be specified if there is a need to support streaming. 

{AI05-0001-1} {AI05-0262-1} {AI12-0437-1} List'Write for a List object L writes Length(L) elements of the list to the stream. It may also write additional information about the list.

{AI05-0001-1} {AI05-0262-1} List'Read reads the representation of a list from the stream, and assigns to Item a list with the same length and elements as was written by List'Write.

Ramification: Streaming more elements than the container length is wrong. For implementation implications of this rule, see the Implementation Note in A.18.2. 

{AI95-00302-03} {AI12-0111-1} {AI12-0112-1} [Some operations  check for "tampering with cursors" of a container because they depend on the set of elements of the container remaining constant, and others check for "tampering with elements" of a container because they depend on elements of the container not being replaced.] When tampering with cursors is prohibited for a particular list object L, Program_Error is propagated by the finalization of L[, as well as by a call that passes L to certain of the operations of this package, as indicated by the precondition of such an operation]. Similarly, when tampering with elements is prohibited for L, Program_Error is propagated by a call that passes L to certain of the other operations of this package, as indicated by the precondition of such an operation.

Paragraphs 62 through 69 are removed as preconditions now describe these rules. 

Ramification: We don't need to explicitly mention [assignment_statement](./AA-5.2#S0173), because that finalizes the target object as part of the operation, and finalization of an object is already defined as tampering with cursors. 

```ada
function Has_Element (Position : Cursor) return Boolean
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0212-1} Returns True if Position designates an element, and returns False otherwise.

To be honest: {AI05-0005-1} {AI05-0212-1} This function might not detect cursors that designate deleted elements; such cursors are invalid (see below) and the result of calling Has_Element with an invalid cursor is unspecified (but not erroneous). 

```ada
function Has_Element (Container : List; Position : Cursor)
   return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if Position designates an element in Container, and returns False otherwise.

Ramification: If Position is No_Element, Has_Element returns False. 

```ada
function "=" (Left, Right : List) return Boolean;

```

{AI95-00302-03} {AI05-0264-1} If Left and Right denote the same list object, then the function returns True. If Left and Right have different lengths, then the function returns False. Otherwise, it compares each element in Left to the corresponding element in Right using the generic formal equality operator. If any such comparison returns False, the function returns False; otherwise, it returns True. Any exception raised during evaluation of element equality is propagated. 

Implementation Note: This wording describes the canonical semantics. However, the order and number of calls on the formal equality function is unspecified for all of the operations that use it in this package, so an implementation can call it as many or as few times as it needs to get the correct answer. Specifically, there is no requirement to call the formal equality additional times once the answer has been determined. 

```ada
function Tampering_With_Cursors_Prohibited
   (Container : List) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if tampering with cursors or tampering with elements is currently prohibited for Container, and returns False otherwise.

Reason: {AI12-0112-1} Prohibiting tampering with elements also needs to prohibit tampering with cursors, as deleting an element is similar to replacing it. 

Implementation Note: {AI12-0112-1} Various contracts elsewhere in this specification require that this function be implemented with synchronized data. Moreover, it is possible for tampering to be prohibited by multiple operations (sequentially or in parallel). Therefore, tampering needs to be implemented with an atomic or protected counter. The counter is initialized to zero, and is incremented when tampering is prohibited, and decremented when leaving an area that prohibited tampering. Function Tampering_With_Cursors_Prohibited returns True if the counter is nonzero. (Note that any case where the result is not well-defined for one task is incorrect use of shared variables and would be erroneous by the rules of 9.10, so no special protection is needed to read the counter.) 

```ada
function Tampering_With_Elements_Prohibited
   (Container : List) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Always returns False[, regardless of whether tampering with elements is prohibited].

Reason: {AI12-0111-1} A definite element cannot change size, so we allow operations that tamper with elements even when tampering with elements is prohibited. That's not true for the indefinite containers, which is why this kind of tampering exists. 

```ada
function Length (Container : List) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} Returns the number of elements in Container.

```ada
function Is_Empty (Container : List) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

{AI95-00302-03} {AI12-0112-1} Returns True if Container is empty.

```ada
procedure Clear (Container : in out List)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = 0;

```

{AI95-00302-03} Removes all the elements from Container.

```ada
function Element (Position : Cursor) return Element_Type
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

{AI95-00302-03} {AI12-0112-1} Element returns the element designated by Position.

```ada
function Element (Container : List;
                  Position  : Cursor) return Element_Type
   with Pre =&gt (Position /= No_Element or else
                   raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error),
        Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

{AI12-0112-1} Element returns the element designated by Position in Container.

```ada
procedure Replace_Element (Container : in out List;
                           Position  : in     Cursor;
                           New_item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} {AI12-0196-1} Replace_Element assigns the value New_Item to the element designated by Position. For the purposes of determining whether the parameters overlap in a call to Replace_Element, the Container parameter is not considered to overlap with any object [(including itself)].

```ada
procedure Query_Element
  (Position : in Cursor;
   Process  : not null access procedure (Element : in Element_Type))
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} {AI05-0021-1} {AI05-0265-1} {AI12-0112-1} Query_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of the list that contains the element designated by Position is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Query_Element
  (Container : in List;
   Position  : in Cursor;
   Process   : not null access procedure (Element : in Element_Type))
   with Pre  =&gt (Position /= No_Element
                    or else raise Constraint_Error) and then
                 (Has_Element (Container, Position)
                    or else raise Program_Error);

```

{AI12-0112-1} Query_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Update_Element
  (Container : in out List;
   Position  : in     Cursor;
   Process   : not null access procedure
                   (Element : in out Element_Type))
   with Pre  =&gt (Position /= No_Element 
                    or else raise Constraint_Error) and then
                 (Has_Element (Container, Position) 
                    or else raise Program_Error);

```

{AI95-00302-03} {AI05-0264-1} {AI05-0265-1} {AI12-0112-1} Update_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

If Element_Type is unconstrained and definite, then the actual Element parameter of Process.all shall be unconstrained.

Ramification: This means that the elements cannot be directly allocated from the heap; it must be possible to change the discriminants of the element in place. 

```ada
{AI12-0112-1} type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0112-1} type Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

{AI05-0212-1} The types Constant_Reference_Type and Reference_Type need finalization.

This paragraph was deleted.{AI12-0112-1} 

Reason: It is expected that Reference_Type (and Constant_Reference_Type) will be a controlled type, for which finalization will have some action to terminate the tampering check for the associated container. If the object is created by default, however, there is no associated container. Since this is useless, and supporting this case would take extra work, we define it to raise an exception. 

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in List;
                             Position  : in Cursor)
   return Constant_Reference_Type
   with Pre  =&gt (Position /= No_Element or else
                    raise Constraint_Error) and then
                 (Has_Element (Container, Position) or else
                    raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read access to an individual element of a list given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Constant_Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Constant_Reference exists and has not been finalized.

```ada
{AI12-0112-1} function Reference (Container : aliased in out List;
                    Position  : in Cursor)
   return Reference_Type
   with Pre  =&gt (Position /= No_Element or else
                    raise Constraint_Error) and then
                 (Has_Element (Container, Position) or else 
                    raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read and write access to an individual element of a list given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Reference exists and has not been finalized.

```ada
{AI12-0112-1} procedure Assign (Target : in out List; Source : in List)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Source) = Length (Target);

```

{AI05-0001-1} {AI05-0248-1} If Target denotes the same object as Source, the operation has no effect. Otherwise, the elements of Source are copied to Target as for an [assignment_statement](./AA-5.2#S0173) assigning Source to Target. 

Discussion: {AI05-0005-1} This routine exists for compatibility with the bounded list container. For an unbounded list, Assign(A, B) and A := B behave identically. For a bounded list, := will raise an exception if the container capacities are different, while Assign will not raise an exception if there is enough room in the target. 

```ada
{AI12-0112-1} function Copy (Source : List)
   return List
   with Post =&gt 
           Length (Copy'Result) = Length (Source) and then
           not Tampering_With_Elements_Prohibited (Copy'Result) and then
           not Tampering_With_Cursors_Prohibited (Copy'Result);

```

{AI05-0001-1} Returns a list whose elements match the elements of Source.

```ada
{AI12-0112-1} procedure Move (Target : in out List;
                Source : in out List)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                    or else raise Program_Error),
        Post =&gt (if not Target'Has_Same_Storage (Source) then
                    Length (Target) = Length (Source'Old) and then
                    Length (Source) = 0);

```

{AI95-00302-03} {AI05-0001-1} {AI05-0248-1} {AI05-0262-1} If Target denotes the same object as Source, then the operation has no effect. Otherwise, the operation is equivalent to Assign (Target, Source) followed by Clear (Source).

```ada
procedure Insert (Container : in out List;
                  Before    : in     Cursor;
                  New_Item  : in     Element_Type;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container);

```

{AI95-00302-03} {AI12-0112-1} Insert inserts Count copies of New_Item prior to the element designated by Before. If Before equals No_Element, the new elements are inserted after the last node (if any). Any exception raised during allocation of internal storage is propagated, and Container is not modified.

Ramification: The check on Before checks that the cursor does not belong to some other Container. This check implies that a reference to the container is included in the cursor value. This wording is not meant to require detection of dangling cursors; such cursors are defined to be invalid, which means that execution is erroneous, and any result is allowed (including not raising an exception). 

```ada
procedure Insert (Container : in out List;
                  Before    : in     Cursor;
                  New_Item  : in     Element_Type;
                  Position  :    out Cursor;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container)
                and then Has_Element (Container, Position);

```

{AI95-00302-03} {AI05-0257-1} {AI12-0112-1} Insert allocates Count copies of New_Item, and inserts them prior to the element designated by Before. If Before equals No_Element, the new elements are inserted after the last element (if any). Position designates the first newly-inserted element, or if Count equals 0, then Position is assigned the value of Before. Any exception raised during allocation of internal storage is propagated, and Container is not modified.

```ada
procedure Insert (Container : in out List;
                  Before    : in     Cursor;
                  Position  :    out Cursor;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container)
                and then Has_Element (Container, Position);

```

{AI95-00302-03} {AI05-0257-1} {AI12-0112-1} Insert inserts Count new elements prior to the element designated by Before. If Before equals No_Element, the new elements are inserted after the last node (if any). The new elements are initialized by default (see 3.3.1). Position designates the first newly-inserted element, or if Count equals 0, then Position is assigned the value of Before. Any exception raised during allocation of internal storage is propagated, and Container is not modified.

```ada
{AI12-0112-1} procedure Prepend (Container : in out List;
                   New_Item  : in     Element_Type;
                   Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, First (Container), New_Item, Count).

```ada
{AI12-0112-1} {AI12-0400-1} procedure Append (Container : in out List;
                  New_Item  : in     Element_Type;
                  Count     : in     Count_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - Count
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + Count = Length (Container);

```

{AI95-00302-03} Equivalent to Insert (Container, No_Element, New_Item, Count).

```ada
{AI12-0391-1} {AI12-0400-1} procedure Append (Container : in out List;
                  New_Item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                   or else raise Constraint_Error),
        Post =&gt Length (Container)'Old + 1 = Length (Container);

```

{AI12-0391-1} Equivalent to Insert (Container, No_Element, New_Item, 1).

```ada
procedure Delete (Container : in out List;
                  Position  : in out Cursor;
                  Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Length (Container)'Old - Count &lt= Length (Container)
                and then Position = No_Element;

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} Delete removes (from Container) Count elements starting at the element designated by Position (or all of the elements starting at Position if there are fewer than Count elements starting at Position). Finally, Position is set to No_Element.

```ada
{AI12-0112-1} procedure Delete_First (Container : in out List;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

{AI95-00302-03} {AI05-0021-1} If Length (Container) &lt= Count, then Delete_First is equivalent to Clear (Container). Otherwise, it removes the first Count nodes from Container.

```ada
{AI12-0112-1} procedure Delete_Last (Container : in out List;
                       Count     : in     Count_Type := 1)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt Length (Container)'Old - Count &lt= Length (Container);

```

{AI95-00302-03} {AI05-0264-1} If Length (Container) &lt= Count, then Delete_Last is equivalent to Clear (Container). Otherwise, it removes the last Count nodes from Container.

```ada
{AI12-0112-1} procedure Reverse_Elements (Container : in out List)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error;

```

{AI95-00302-03} Reorders the elements of Container in reverse order.

Discussion: Unlike the similar routine for a vector, elements should not be copied; rather, the nodes should be exchanged. Cursors are expected to reference the same elements afterwards. 

```ada
procedure Swap (Container : in out List;
                I, J      : in     Cursor)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (I /= No_Element or else Constraint_Error) and then
                (J /= No_Element or else Constraint_Error) and then
                (Has_Element (Container, I)
                   or else raise Program_Error) and then
                (Has_Element (Container, J)
                   or else raise Program_Error);

```

{AI95-00302-03} {AI12-0112-1} Swap exchanges the values of the elements designated by I and J.

Ramification: After a call to Swap, I designates the element value previously designated by J, and J designates the element value previously designated by I. The cursors do not become ambiguous from this operation. 

To be honest: The implementation is not required to actually copy the elements if it can do the swap some other way. But it is allowed to copy the elements if needed. 

```ada
procedure Swap_Links (Container : in out List;
                      I, J      : in     Cursor)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (I /= No_Element or else Constraint_Error) and then
                (J /= No_Element or else Constraint_Error) and then
                (Has_Element (Container, I)
                   or else raise Program_Error) and then
                (Has_Element (Container, J)
                   or else raise Program_Error);

```

{AI95-00302-03} {AI12-0112-1} Swap_Links exchanges the nodes designated by I and J.

Ramification: Unlike Swap, this exchanges the nodes, not the elements. No copying is performed. I and J designate the same elements after this call as they did before it. This operation can provide better performance than Swap if the element size is large. 

```ada
procedure Splice (Target   : in out List;
                  Before   : in     Cursor;
                  Source   : in out List)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Target, Before)
                   or else raise Program_Error) and then
                (Target'Has_Same_Storage (Source) or else
                 Length (Target) &lt= Count_Type'Last - Length (Source)
                   or else raise Constraint_Error),
        Post =&gt (if not Target'Has_Same_Storage (Source) then
                   (declare
                      Result_Length : constant Count_Type :=
                         Length (Source)'Old + Length (Target)'Old;
                    begin
                      Length (Source) = 0 and then
                      Length (Target) = Result_Length));

```

{AI95-00302-03} {AI12-0112-1} If Source denotes the same object as Target, the operation has no effect. Otherwise, Splice reorders elements such that they are removed from Source and moved to Target, immediately prior to Before. If Before equals No_Element, the nodes of Source are spliced after the last node of Target.

```ada
procedure Splice (Target   : in out List;
                  Before   : in     Cursor;
                  Source   : in out List;
                  Position : in out Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                   or else raise Program_Error) and then
                (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Source, Position)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Target, Before)
                   or else raise Program_Error) and then
                (Target'Has_Same_Storage (Source) or else
                 Length (Target) &lt= Count_Type'Last - 1
                   or else raise Constraint_Error),
        Post =&gt (declare
                   Org_Target_Length : constant Count_Type :=
                      Length (Target)'Old;
                   Org_Source_Length : constant Count_Type :=
                      Length (Source)'Old;
                 begin
                    (if Target'Has_Same_Storage (Source) then
                        Position = Position'Old
                     else Length (Source) = Org_Source_Length - 1 and then
                        Length (Target) = Org_Target_Length + 1 and then
                        Has_Element (Target, Position)));

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} If Source denotes the same object as Target, then there is no effect if Position equals Before, else the element designated by Position is moved immediately prior to Before, or, if Before equals No_Element, after the last element. Otherwise, the element designated by Position is removed from Source and moved to Target, immediately prior to Before, or, if Before equals No_Element, after the last element of Target. Position is updated to represent an element in Target. 

Ramification: If Source is the same as Target, and Position = Before, or Next(Position) = Before, Splice has no effect, as the element does not have to move to meet the postcondition. 

```ada
procedure Splice (Container: in out List;
                  Before   : in     Cursor;
                  Position : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Has_Element (Container, Before)
                   or else raise Program_Error),
        Post =&gt  Length (Container) = Length (Container)'Old;

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} If Position equals Before there is no effect. Otherwise, the element designated by Position is moved immediately prior to Before, or, if Before equals No_Element, after the last element.

```ada
{AI12-0112-1} function First (Container : List) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container)
                 then Has_Element (Container, First'Result)
                 else First'Result = No_Element);

```

{AI95-00302-03} If Container is empty, First returns No_Element. Otherwise, it returns a cursor that designates the first node in Container.

```ada
{AI12-0112-1} function First_Element (Container : List)
   return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (Container, First_Index (Container)).

```ada
{AI12-0112-1} function Last (Container : List) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container)
                 then Has_Element (Container, Last'Result)
                 else Last'Result = No_Element);

```

{AI95-00302-03} If Container is empty, Last returns No_Element. Otherwise, it returns a cursor that designates the last node in Container.

```ada
{AI12-0112-1} function Last_Element (Container : List)
   return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (Last (Container)).

```ada
{AI12-0112-1} function Next (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

{AI95-00302-03} If Position equals No_Element or designates the last element of the container, then Next returns the value No_Element. Otherwise, it returns a cursor that designates the successor of the element designated by Position.

```ada
function Next (Container : List;
               Position  : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then Next'Result = No_Element
                 elsif Next'Result = No_Element then
                   Position = Last (Container)
                 else Has_Element (Container, Next'Result));

```

{AI12-0112-1} Returns a cursor designating the successor of the element designated by Position in Container.

```ada
{AI12-0112-1} function Previous (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element then 
                   Previous'Result = No_Element);

```

{AI95-00302-03} If Position equals No_Element or designates the first element of the container, then Previous returns the value No_Element. Otherwise, it returns a cursor that designates the predecessor of the element designated by Position.

```ada
function Previous (Container : List;
                   Position : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then 
                   Previous'Result = No_Element
                 elsif Previous'Result = No_Element then
                   Position = First (Container)
                 else Has_Element (Container, Previous'Result));

```

{AI12-0112-1} Returns a cursor designating the predecessor of the element designated by Position in Container, if any.

```ada
{AI12-0112-1} procedure Next (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Next (Position).

```ada
procedure Next (Container : in     List;
                Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Next (Container, Position).

```ada
{AI12-0112-1} procedure Previous (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Previous (Position).

```ada
procedure Previous (Container : in     List;
                    Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Previous (Container, Position).

```ada
function Find (Container : List;
               Item      : Element_Type;
               Position  : Cursor := No_Element)
   return Cursor
   with Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Find'Result /= No_Element
                 then Has_Element (Container, Find'Result));

```

{AI95-00302-03} {AI12-0112-1} Find searches the elements of Container for an element equal to Item (using the generic formal equality operator). The search starts at the element designated by Position, or at the first element if Position equals No_Element. It proceeds towards Last (Container). If no equal element is found, then Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

```ada
function Reverse_Find (Container : List;
                       Item      : Element_Type;
                       Position  : Cursor := No_Element)
   return Cursor
   with Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Reverse_Find'Result /= No_Element
                 then Has_Element (Container, Reverse_Find'Result));

```

{AI95-00302-03} {AI12-0112-1} Find searches the elements of Container for an element equal to Item (using the generic formal equality operator). The search starts at the element designated by Position, or at the last element if Position equals No_Element. It proceeds towards First (Container). If no equal element is found, then Reverse_Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

```ada
function Contains (Container : List;
                   Item      : Element_Type) return Boolean;

```

{AI95-00302-03} Equivalent to Find (Container, Item) /= No_Element.

Paragraphs 139 and 140 were moved above. 

```ada
{AI12-0112-1} procedure Iterate
  (Container : in List;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0265-1} Iterate calls Process.all with a cursor that designates each node in Container, starting with the first node and moving the cursor as per the Next function. Tampering with the cursors of Container is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

Implementation Note: The purpose of the tamper with cursors check is to prevent erroneous execution from the Position parameter of Process.all becoming invalid. This check takes place when the operations that tamper with the cursors of the container are called. The check cannot be made later (say in the body of Iterate), because that could cause the Position cursor to be invalid and potentially cause execution to become erroneous -- defeating the purpose of the check.

See Iterate for vectors (A.18.2) for a suggested implementation of the check. 

```ada
{AI12-0112-1} procedure Reverse_Iterate
  (Container : in List;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0212-1} Iterates over the nodes in Container as per procedure Iterate, except that elements are traversed in reverse order, starting with the last node and moving the cursor as per the Previous function.

```ada
{AI12-0112-1} {AI12-0266-1} function Iterate (Container : in List)
   return List_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
   with Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0266-1} Iterate returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each node in Container, starting with the first node and moving the cursor as per the Next function when used as a forward iterator, and starting with the last node and moving the cursor as per the Previous function when used as a reverse iterator, and processing all nodes concurrently when used as a parallel iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

```ada
function Iterate (Container : in List; Start : in Cursor)
   return List_Iterator_Interfaces.Reversible_Iterator'Class
   with Pre    =&gt (Start /= No_Element
                        or else raise Constraint_Error) and then
                     (Has_Element (Container, Start)
                        or else raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0262-1} {AI05-0265-1} {AI05-0269-1} {AI12-0112-1} Iterate returns a reversible iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each node in Container, starting with the node designated by Start and moving the cursor as per the Next function when used as a forward iterator, or moving the cursor as per the Previous function when used as a reverse iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

Discussion: Exits are allowed from the loops created using the iterator objects. In particular, to stop the iteration at a particular cursor, just add 

```ada
exit when Cur = Stop;

```

in the body of the loop (assuming that Cur is the loop parameter and Stop is the cursor that you want to stop at). 

{AI05-0044-1} {AI05-0262-1} The actual function for the generic formal function "&lt" of Generic_Sorting is expected to return the same value each time it is called with a particular pair of element values. It should define a strict weak ordering relationship (see A.18); it should not modify Container. If the actual for "&lt" behaves in some other manner, the behavior of the subprograms of Generic_Sorting are unspecified. The number of times the subprograms of Generic_Sorting call "&lt" is unspecified.

```ada
function Is_Sorted (Container : List) return Boolean;

```

{AI95-00302-03} Returns True if the elements are sorted smallest first as determined by the generic formal "&lt" operator; otherwise, Is_Sorted returns False. Any exception raised during evaluation of "&lt" is propagated.

```ada
{AI12-0112-1} procedure Sort (Container : in out List)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error;

```

{AI95-00302-03} Reorders the nodes of Container such that the elements are sorted smallest first as determined by the generic formal "&lt" operator provided. The sort is stable. Any exception raised during evaluation of "&lt" is propagated.

Ramification: Unlike array sorts, we do require stable sorts here. That's because algorithms in the merge sort family (as described by Knuth) can be both fast and stable. Such sorts use the extra memory as offered by the links to provide better performance.

Note that list sorts never copy elements; it is the nodes, not the elements, that are reordered. 

```ada
procedure Merge (Target  : in out List;
                 Source  : in out List)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error) and then
                (not Tampering_With_Elements_Prohibited (Source)
                   or else raise Program_Error) and then
                (Length (Target) &lt= Count_Type'Last - Length (Source)
                   or else raise Constraint_Error) and then
                ((Length (Source) = 0 or else
                   not Target'Has_Same_Storage (Source))
                   or else raise Constraint_Error),
        Post =&gt (declare
                   Result_Length : constant Count_Type :=
                      Length (Source)'Old + Length (Target)'Old;
                 begin
                   (Length (Source) = 0 and then
                    Length (Target) = Result_Length));

```

{AI95-00302-03} {AI05-0021-1} {AI12-0112-1} Merge removes elements from Source and inserts them into Target; afterwards, Target contains the union of the elements that were initially in Source and Target; Source is left empty. If Target and Source are initially sorted smallest first, then Target is ordered smallest first as determined by the generic formal "&lt" operator; otherwise, the order of elements in Target is unspecified. Any exception raised during evaluation of "&lt" is propagated.

Ramification: It is a bounded error if either of the lists is unsorted, see below. The bounded error can be recovered by sorting Target after the merge call, or the lists can be pretested with Is_Sorted. 

{AI12-0111-1} The nested package Doubly_Linked_Lists.Stable provides a type Stable.List that represents a stable list, which is one that cannot grow and shrink. Such a list can be created by calling the Copy function, or by establishing a stabilized view of an ordinary list.

{AI12-0111-1} The subprograms of package Containers.Doubly_Linked_Lists that have a parameter or result of type List are included in the nested package Stable with the same specification, except that the following are omitted:

Tampering_With_Cursors_Prohibited, Tampering_With_Elements_Prohibited, Assign, Move, Insert, Append, Prepend, Clear, Delete, Delete_First, Delete_Last, Splice, Swap_Links, and Reverse_Elements 

Ramification: The names List and Cursor mean the types declared in the nested package in these subprogram specifications. 

Reason: The omitted routines are those that tamper with cursors or elements (or test that state). The model is that it is impossible to tamper with cursors or elements of a stable view since no such operations are included. Thus tampering checks are not needed for a stable view, and we omit the operations associated with those checks.

The Generic_Sorting generic is omitted entirely, as only function Is_Sorting does not tamper with cursors. It isn't useful enough by itself to include. 

{AI12-0111-1} The operations of this package are equivalent to those for ordinary lists, except that the calls to Tampering_With_Cursors_Prohibited and Tampering_With_Elements_Prohibited that occur in preconditions are replaced by False, and any that occur in postconditions are replaced by True.

{AI12-0111-1} {AI12-0439-1} If a stable list is declared with the Base discriminant designating a pre-existing ordinary list, the stable list represents a stabilized view of the underlying ordinary list, and any operation on the stable list is reflected on the underlying ordinary list. While a stabilized view exists, any operation that tampers with elements performed on the underlying list is prohibited. The finalization of a stable list that provides such a view removes this restriction on the underlying ordinary list [(though some other restriction can exist due to other concurrent iterations or stabilized views)].

{AI12-0111-1} {AI12-0438-1} If a stable list is declared without specifying Base, the object is necessarily initialized. The initializing expression of the stable list, [typically a call on Copy], determines the Length of the list. The Length of a stable list never changes after initialization.

Proof: {AI12-0438-1} Initialization is required as the type is indefinite, see 3.3.1. 


#### Bounded (Run-Time) Errors

{AI95-00302-03} Calling Merge in an instance of Generic_Sorting with either Source or Target not ordered smallest first using the provided generic formal "&lt" operator is a bounded error. Either Program_Error is raised after Target is updated as described for Merge, or the operation works as defined.

{AI05-0022-1} {AI05-0248-1} It is a bounded error for the actual function associated with a generic formal subprogram, when called as part of an operation of this package, to tamper with elements of any List parameter of the operation. Either Program_Error is raised, or the operation works as defined on the value of the List either prior to, or subsequent to, some or all of the modifications to the List.

{AI05-0027-1} It is a bounded error to call any subprogram declared in the visible part of Containers.Doubly_Linked_Lists when the associated container has been finalized. If the operation takes Container as an in out parameter, then it raises Constraint_Error or Program_Error. Otherwise, the operation either proceeds as it would for an empty container, or it raises Constraint_Error or Program_Error. 


#### Erroneous Execution

{AI95-00302-03} A Cursor value is invalid if any of the following have occurred since it was created: 

The list that contains the element it designates has been finalized;

{AI05-0160-1} The list that contains the element it designates has been used as the Target of a call to Assign, or as the target of an [assignment_statement](./AA-5.2#S0173);

[The list that contains the element it designates has been used as the Source or Target of a call to Move;] or 

Proof: {AI05-0001-1} Move has been reworded in terms of Assign and Clear, which are covered by other bullets, so this text is redundant. 

{AI05-0160-1} {AI05-0262-1} The element it designates has been removed from the list that previously contained the element. 

To be honest: {AI05-0160-1} The cursor modified by the four parameter Splice is not invalid, even though the element it designates has been removed from the source list, because that cursor has been modified to designate that element in the target list  the cursor no longer designates an element in the source list. 

Ramification: {AI05-0160-1} This can happen directly via calls to Delete, Delete_Last, Clear, Splice with a Source parameter, and Merge; and indirectly via calls to Delete_First, Assign, and Move. 

{AI95-00302-03} The result of "=" or Has_Element is unspecified if it is called with an invalid cursor parameter. Execution is erroneous if any other subprogram declared in Containers.Doubly_Linked_Lists is called with an invalid cursor parameter. 

Discussion: The list above is intended to be exhaustive. In other cases, a cursor value continues to designate its original element. For instance, cursor values survive the insertion and deletion of other nodes.

While it is possible to check for these cases, in many cases the overhead necessary to make the check is substantial in time or space. Implementations are encouraged to check for as many of these cases as possible and raise Program_Error if detected. 

{AI05-0212-1} Execution is erroneous if the list associated with the result of a call to Reference or Constant_Reference is finalized before the result object returned by the call to Reference or Constant_Reference is finalized. 

Reason: Each object of Reference_Type and Constant_Reference_Type probably contains some reference to the originating container. If that container is prematurely finalized (which is only possible via Unchecked_Deallocation, as accessibility checks prevent passing a container to Reference that will not live as long as the result), the finalization of the object of Reference_Type will try to access a nonexistent object. This is a normal case of a dangling pointer created by Unchecked_Deallocation; we have to explicitly mention it here as the pointer in question is not visible in the specification of the type. (This is the same reason we have to say this for invalid cursors.) 


#### Implementation Requirements

{AI95-00302-03} {AI12-0437-1} No storage associated with a doubly-linked list object shall be lost upon assignment or scope exit.

{AI95-00302-03} {AI05-0262-1} The execution of an [assignment_statement](./AA-5.2#S0173) for a list shall have the effect of copying the elements from the source list object to the target list object and changing the length of the target object to that of the source object.

Implementation Note: {AI05-0298-1} An assignment of a List is a "deep" copy; that is the elements are copied as well as the data structures. We say "effect of" in order to allow the implementation to avoid copying elements immediately if it wishes. For instance, an implementation that avoided copying until one of the containers is modified would be allowed. (Note that this implementation would require care, see A.18.2 for more.) 


#### Implementation Advice

{AI95-00302-03} Containers.Doubly_Linked_Lists should be implemented similarly to a linked list. In particular, if N is the length of a list, then the worst-case time complexity of Element, Insert with Count=1, and Delete with Count=1 should be O(log N). 

Implementation Advice: The worst-case time complexity of Element, Insert with Count=1, and Delete with Count=1 for Containers.Doubly_Linked_Lists should be O(log N).

Reason: We do not mean to overly constrain implementation strategies here. However, it is important for portability that the performance of large containers has roughly the same factors on different implementations. If a program is moved to an implementation that takes O(N) time to access elements, that program could be unusable when the lists are large. We allow O(log N) access because the proportionality constant and caching effects are likely to be larger than the log factor, and we don't want to discourage innovative implementations. 

{AI95-00302-03} The worst-case time complexity of a call on procedure Sort of an instance of Containers.Doubly_Linked_Lists.Generic_Sorting should be O(N**2), and the average time complexity should be better than O(N**2). 

Implementation Advice: A call on procedure Sort of an instance of Containers.Doubly_Linked_Lists.Generic_Sorting should have an average time complexity better than O(N**2) and worst case no worse than O(N**2).

Ramification: In other words, we're requiring the use of a better than O(N**2) sorting algorithm, such as Quicksort. No bubble sorts allowed! 

{AI95-00302-03} Move should not copy elements, and should minimize copying of internal data structures. 

Implementation Advice: Containers.Doubly_Linked_Lists.Move should not copy elements, and should minimize copying of internal data structures.

Implementation Note: Usually that can be accomplished simply by moving the pointer(s) to the internal data structures from the Source container to the Target container. 

{AI95-00302-03} If an exception is propagated from a list operation, no storage should be lost, nor any elements removed from a list unless specified by the operation. 

Implementation Advice: If an exception is propagated from a list operation, no storage should be lost, nor any elements removed from a list unless specified by the operation.

Reason: This is important so that programs can recover from errors. But we don't want to require heroic efforts, so we just require documentation of cases where this can't be accomplished. 

NOTE 1   {AI95-00302-03} {AI12-0442-1} Sorting a list never copies elements, and is a stable sort (equal elements remain in the original order). This is different than sorting an array or vector, which will often copy elements, and hence is probably not a stable sort. 


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Doubly_Linked_Lists is new. 


#### Inconsistencies With Ada 2005

{AI05-0248-1} {AI05-0257-1} Correction: The Insert versions that return a Position parameter are now defined to return Position = Before if Count = 0. This was unspecified for Ada 2005; so this will only be inconsistent if an implementation did something else and a program depended on that something else - this should be very rare. 


#### Incompatibilities With Ada 2005

{AI05-0001-1} Subprograms Assign and Copy are added to Containers.Doubly_Linked_Lists. If an instance of Containers.Doubly_Linked_Lists is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Containers.Doubly_Linked_Lists is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 2005

{AI05-0212-1} Added iterator, reference, and indexing support to make list containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0001-1} Generalized the definition of Move. Specified which elements are read/written by stream attributes.

{AI05-0022-1} Correction: Added a Bounded (Run-Time) Error to cover tampering by generic actual subprograms.

{AI05-0027-1} Correction: Added a Bounded (Run-Time) Error to cover access to finalized list containers.

{AI05-0044-1} Correction: Redefined "&lt" actuals to require a strict weak ordering; the old definition allowed indeterminant comparisons that would not have worked in a container.

{AI05-0084-1} Correction: Added a pragma Remote_Types so that containers can be used in distributed programs.

{AI05-0160-1} Correction: Revised the definition of invalid cursors to cover missing (and new) cases.

{AI05-0257-1} Correction: Added missing wording to describe the Position after Inserting 0 elements.

{AI05-0265-1} Correction: Defined when a container prohibits tampering in order to more clearly define where the check is made and the exception raised. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for ordinary containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} {AI12-0339-1} {AI12-0391-1} A number of new subprograms, types, and even a nested package were added to Containers.Doubly_Linked_Lists to better support contracts and stable views. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0196-1} Replace_Element is now defined such that it can be used concurrently so long as it operates on different elements. This allows some container operations to be used in parallel without separate synchronization.

{AI12-0212-1} {AI12-0391-1} Lists now support positional container aggregates, so [aggregate](./AA-4.3#S0106) syntax can be used to create Lists.

{AI12-0266-1} The iterator for the entire container now can return a parallel iterator which can be used to process the container in parallel. 


#### Wording Changes from Ada 2012

{AI12-0110-1} Corrigendum: Clarified that tampering checks precede all other checks made by a subprogram (but come after those associated with the call).

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5).

{AI12-0400-1} Correction: Split the Append routine into two routines rather than having a single routine with a default parameter, in order that a routine with the appropriate profile for the Aggregate aspect exists. This change should not change the behavior of any existing code. 


## A.18.4  Maps

{AI95-00302-03} The language-defined generic packages Containers.Hashed_Maps and Containers.Ordered_Maps provide private types Map and Cursor, and a set of operations for each type. A map container allows an arbitrary type to be used as a key to find the element associated with that key. A hashed map uses a hash function to organize the keys, while an ordered map orders the keys per a specified relation. 

{AI95-00302-03} {AI05-0299-1} This subclause describes the declarations that are common to both kinds of maps. See A.18.5 for a description of the semantics specific to Containers.Hashed_Maps and A.18.6 for a description of the semantics specific to Containers.Ordered_Maps. 


#### Static Semantics

{AI95-00302-03} The actual function for the generic formal function "=" on Element_Type values is expected to define a reflexive and symmetric relationship and return the same result value each time it is called with a particular pair of values. If it behaves in some other manner, the function "=" on map values returns an unspecified value. The exact arguments and number of calls of this generic formal function by the function "=" on map values are unspecified.

Ramification: If the actual function for "=" is not symmetric and consistent, the result returned by "=" for Map objects cannot be predicted. The implementation is not required to protect against "=" raising an exception, or returning random results, or any other "bad" behavior. And it can call "=" in whatever manner makes sense. But note that only the result of "=" for Map objects is unspecified; other subprograms are not allowed to break if "=" is bad (they aren't expected to use "="). 

{AI95-00302-03} The type Map is used to represent maps. The type Map needs finalization (see 7.6).

{AI95-00302-03} A map contains pairs of keys and elements, called nodes. Map cursors designate nodes, but also can be thought of as designating an element (the element contained in the node) for consistency with the other containers. There exists an equivalence relation on keys, whose definition is different for hashed maps and ordered maps. A map never contains two or more nodes with equivalent keys. The length of a map is the number of nodes it contains.

{AI95-00302-03} Each nonempty map has two particular nodes called the first node and the last node (which may be the same). Each node except for the last node has a successor node. If there are no other intervening operations, starting with the first node and repeatedly going to the successor node will visit each node in the map exactly once until the last node is reached. The exact definition of these terms is different for hashed maps and ordered maps.

{AI95-00302-03} {AI12-0111-1} {AI12-0112-1} [Some operations  check for "tampering with cursors" of a container because they depend on the set of elements of the container remaining constant, and others check for "tampering with elements" of a container because they depend on elements of the container not being replaced.] When tampering with cursors is prohibited for a particular map object M, Program_Error is propagated by the finalization of M[, as well as by a call that passes M to certain of the operations of this package, as indicated by the precondition of such an operation]. Similarly, when tampering with elements is prohibited for M, Program_Error is propagated by a call that passes M to certain of the other operations of this package, as indicated by the precondition of such an operation.

Paragraphs 8 through 15 are removed as preconditions now describe these rules. 

Ramification: We don't need to explicitly mention [assignment_statement](./AA-5.2#S0173), because that finalizes the target object as part of the operation, and finalization of an object is already defined as tampering with cursors. 

{AI95-00302-03} Empty_Map represents the empty Map object. It has a length of 0. If an object of type Map is not otherwise initialized, it is initialized to the same value as Empty_Map.

{AI95-00302-03} No_Element represents a cursor that designates no node. If an object of type Cursor is not otherwise initialized, it is initialized to the same value as No_Element.

{AI95-00302-03} {AI12-0434-1} The primitive "=" operator for type Cursor returns True if both cursors are No_Element, or designate the same element in the same container.

To be honest: {AI12-0434-1} "The primitive "=" operator" is the one with two parameters of type Cursor which returns Boolean. We're not talking about some other (hidden) primitive function named "=". 

{AI95-00302-03} Execution of the default implementation of the Input, Output, Read, or Write attribute of type Cursor raises Program_Error.

Reason: A cursor will probably be implemented in terms of one or more access values, and the effects of streaming access values is unspecified. Rather than letting the user stream junk by accident, we mandate that streaming of cursors raise Program_Error by default. The attributes can always be specified if there is a need to support streaming. 

{AI05-0001-1} {AI05-0262-1} {AI12-0437-1} Map'Write for a Map object M writes Length(M) elements of the map to the stream. It may also write additional information about the map.

{AI05-0001-1} {AI05-0262-1} Map'Read reads the representation of a map from the stream, and assigns to Item a map with the same length and elements as was written by Map'Write.

Ramification: Streaming more elements than the container length is wrong. For implementation implications of this rule, see the Implementation Note in A.18.2. 

```ada
{AI12-0112-1} function Has_Element (Position : Cursor) return Boolean
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0212-1} Returns True if Position designates an element, and returns False otherwise.

To be honest: {AI05-0005-1} {AI05-0212-1} This function might not detect cursors that designate deleted elements; such cursors are invalid (see below) and the result of calling Has_Element with an invalid cursor is unspecified (but not erroneous). 

```ada
{AI12-0112-1} function Has_Element (Container : Map; Position : Cursor)
   return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if Position designates an element in Container, and returns False otherwise.

Ramification: {AI12-0112-1} If Position is No_element, Has_Element returns False. 

```ada
function "=" (Left, Right : Map) return Boolean;

```

{AI95-00302-03} If Left and Right denote the same map object, then the function returns True. If Left and Right have different lengths, then the function returns False. Otherwise, for each key K in Left, the function returns False if:

a key equivalent to K is not present in Right; or

the element associated with K in Left is not equal to the element associated with K in Right (using the generic formal equality operator for elements). 

If the function has not returned a result after checking all of the keys, it returns True. Any exception raised during evaluation of key equivalence or element equality is propagated. 

Implementation Note: This wording describes the canonical semantics. However, the order and number of calls on the formal equality function is unspecified for all of the operations that use it in this package, so an implementation can call it as many or as few times as it needs to get the correct answer. Specifically, there is no requirement to call the formal equality additional times once the answer has been determined. 

```ada
function Tampering_With_Cursors_Prohibited
   (Container : Map) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if tampering with cursors or tampering with elements is currently prohibited for Container, and returns False otherwise.

Reason: {AI12-0112-1} Prohibiting tampering with elements also needs to prohibit tampering with cursors, as deleting an element is similar to replacing it. 

Implementation Note: {AI12-0112-1} Various contracts elsewhere in this specification require that this function be implemented with synchronized data. Moreover, it is possible for tampering to be prohibited by multiple operations (sequentially or in parallel). Therefore, tampering needs to be implemented with an atomic or protected counter. The counter is initialized to zero, and is incremented when tampering is prohibited, and decremented when leaving an area that prohibited tampering. Function Tampering_With_Cursors_Prohibited returns True if the counter is nonzero. (Note that any case where the result is not well-defined for one task is incorrect use of shared variables and would be erroneous by the rules of 9.10, so no special protection is needed to read the counter.) 

```ada
function Tampering_With_Elements_Prohibited
   (Container : Map) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Always returns False[, regardless of whether tampering with elements is prohibited].

Reason: {AI12-0111-1} A definite element cannot change size, so we allow operations that tamper with elements even when tampering with elements is prohibited. That's not true for the indefinite containers, which is why this kind of tampering exists. 

```ada
{AI12-0112-1} function Length (Container : Map) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} Returns the number of nodes in Container.

```ada
function Is_Empty (Container : Map) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

{AI95-00302-03} {AI12-0112-1} Returns True if Container is empty.

```ada
{AI12-0112-1} procedure Clear (Container : in out Map)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = 0;

```

{AI95-00302-03} Removes all the nodes from Container.

```ada
function Key (Position : Cursor) return Key_Type
   with Pre  =&gt Position /= No_Element 
                    or else raise Constraint_Error,
        Nonblocking, Global =&gt in all, Use_Formal =&gt Key_Type;

```

{AI95-00302-03} {AI12-0112-1} Key returns the key component of the node designated by Position.

```ada
function Key (Container : Map;
              Position : Cursor) return Key_Type
   with Pre  =&gt (Position /= No_Element
                    or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Nonblocking, Global =&gt null, Use_Formal =&gt Key_Type;

```

{AI12-0112-1} Key returns the key component of the node designated by Position.

```ada
function Element (Position : Cursor) return Element_Type
   with Pre  =&gt Position /= No_Element 
                    or else raise Constraint_Error,
        Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

{AI95-00302-03} {AI12-0112-1} Element returns the element component of the node designated by Position.

```ada
function Element (Container : Map;
                  Position  : Cursor) return Element_Type
   with Pre  =&gt (Position /= No_Element
                    or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

{AI12-0112-1} Element returns the element component of the node designated by Position.

```ada
procedure Replace_Element (Container : in out Map;
                           Position  : in     Cursor;
                           New_item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI95-00302-03} {AI05-0264-1} {AI12-0112-1} {AI12-0196-1} Replace_Element assigns New_Item to the element of the node designated by Position. For the purposes of determining whether the parameters overlap in a call to Replace_Element, the Container parameter is not considered to overlap with any object [(including itself)].

```ada
procedure Query_Element
  (Position : in Cursor;
   Process  : not null access procedure (Key     : in Key_Type;
                                         Element : in Element_Type))
   with Pre  =&gt Position /= No_Element 
                   or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} {AI05-0021-1} {AI05-0265-1} {AI12-0112-1} Query_Element calls Process.all with the key and element from the node designated by Position as the arguments. Tampering with the elements of the map that contains the element designated by Position is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Query_Element
  (Container : in Map;
   Position  : in Cursor;
   Process   : not null access procedure (Key     : in Key_Type;
                                          Element : in Element_Type))
  with Pre  =&gt (Position /= No_Element 
                  or else raise Constraint_Error) and then
               (Has_Element (Container, Position) 
                  or else raise Program_Error);

```

{AI12-0112-1} Query_Element calls Process.all with the key and element from the node designated by Position as the arguments. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Update_Element
  (Container : in out Map;
   Position  : in     Cursor;
   Process   : not null access procedure (Key     : in     Key_Type;
                                          Element : in out Element_Type))
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI95-00302-03} {AI05-0264-1} {AI05-0265-1} {AI12-0112-1} Update_Element calls Process.all with the key and element from the node designated by Position as the arguments. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

If Element_Type is unconstrained and definite, then the actual Element parameter of Process.all shall be unconstrained.

Ramification: This means that the elements cannot be directly allocated from the heap; it must be possible to change the discriminants of the element in place. 

```ada
{AI12-0112-1} type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gtin out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0112-1} type Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

{AI05-0212-1} The types Constant_Reference_Type and Reference_Type need finalization.

This paragraph was deleted.{AI12-0112-1} 

Reason: It is expected that Reference_Type (and Constant_Reference_Type) will be a controlled type, for which finalization will have some action to terminate the tampering check for the associated container. If the object is created by default, however, there is no associated container. Since this is useless, and supporting this case would take extra work, we define it to raise an exception. 

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in Map;
                             Position  : in Cursor)
   return Constant_Reference_Type
   with Pre  =&gt (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read access to an individual element of a Map given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Constant_Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Constant_Reference exists and has not been finalized.

```ada
{AI12-0112-1} function Reference (Container : aliased in out Map;
                    Position  : in Cursor)
   return Reference_Type
   with Pre  =&gt (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read and write access to an individual element of a Map given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Reference exists and has not been finalized.

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in Map;
                             Key       : in Key_Type)
   return Constant_Reference_Type
   with Pre  =&gt Find (Container, Key) /= No_Element 
                   or else raise Constraint_Error,
        Post =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read access to an individual element of a map given a key value.

Equivalent to Constant_Reference (Container, Find (Container, Key)).

```ada
{AI12-0112-1} function Reference (Container : aliased in out Map;
                    Key       : in Key_Type)
   return Reference_Type
   with Pre  =&gt Find (Container, Key) /= No_Element 
                   or else raise Constraint_Error,
        Post =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read and write access to an individual element of a map given a key value.

Equivalent to Reference (Container, Find (Container, Key)).

```ada
{AI12-0112-1} procedure Assign (Target : in out Map; Source : in Map)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Source) = Length (Target);

```

{AI05-0001-1} {AI05-0248-1} If Target denotes the same object as Source, the operation has no effect. Otherwise, the key/element pairs of Source are copied to Target as for an [assignment_statement](./AA-5.2#S0173) assigning Source to Target. 

Discussion: {AI05-0005-1} This routine exists for compatibility with the bounded map containers. For an unbounded map, Assign(A, B) and A := B behave identically. For a bounded map, := will raise an exception if the container capacities are different, while Assign will not raise an exception if there is enough room in the target. 

```ada
{AI12-0112-1} procedure Move (Target : in out Map;
                Source : in out Map)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                    or else raise Program_Error),
        Post =&gt (if not Target'Has_Same_Storage (Source) then
                    Length (Target) = Length (Source'Old) and then
                    Length (Source) = 0);

```

{AI95-00302-03} {AI05-0001-1} {AI05-0248-1} {AI05-0262-1} If Target denotes the same object as Source, then the operation has no effect. Otherwise, the operation is equivalent to Assign (Target, Source) followed by Clear (Source).

```ada
{AI12-0112-1} procedure Insert (Container : in out Map;
                  Key       : in     Key_Type;
                  New_Item  : in     Element_Type;
                  Position  :    out Cursor;
                  Inserted  :    out Boolean)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Has_Element (Container, Position) and then
                  (if Inserted then
                     Length (Container) = Original_Length + 1
                   else
                     Length (Container) = Original_Length));

```

{AI95-00302-03} Insert checks if a node with a key equivalent to Key is already present in Container. If a match is found, Inserted is set to False and Position designates the element with the matching key. Otherwise, Insert allocates a new node, initializes it to Key and New_Item, and adds it to Container; Inserted is set to True and Position designates the newly-inserted node. Any exception raised during allocation is propagated and Container is not modified.

```ada
{AI12-0112-1} procedure Insert (Container : in out Map;
                  Key       : in     Key_Type;
                  Position  :    out Cursor;
                  Inserted  :    out Boolean)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Has_Element (Container, Position) and then
                  (if Inserted then
                     Length (Container) = Original_Length + 1
                   else
                     Length (Container) = Original_Length));

```

{AI95-00302-03} Insert inserts Key into Container as per the five-parameter Insert, with the difference that an element initialized by default (see 3.3.1) is inserted.

```ada
{AI12-0112-1} procedure Insert (Container : in out Map;
                  Key       : in     Key_Type;
                  New_Item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt Length (Container) = Length (Container)'Old + 1;

```

{AI95-00302-03} Insert inserts Key and New_Item into Container as per the five-parameter Insert, with the difference that if a node with a key equivalent to Key is already in the map, then Constraint_Error is propagated.

Ramification: This is equivalent to: 

```ada
declare
  Inserted : Boolean; C : Cursor;
begin
  Insert (Container, Key, New_Item, C, Inserted);
  if not Inserted then
     raise Constraint_Error;
  end if;
end;

```

but doesn't require the hassle of out parameters. 

```ada
{AI12-0112-1} procedure Include (Container : in out Map;
                   Key       : in     Key_Type;
                   New_Item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Length (Container)
                      in Original_Length | Original_Length + 1);

```

{AI95-00302-03} Include inserts Key and New_Item into Container as per the five-parameter Insert, with the difference that if a node with a key equivalent to Key is already in the map, then this operation assigns Key and New_Item to the matching node. Any exception raised during assignment is propagated.

Ramification: This is equivalent to: 

```ada
declare
  C : Cursor := Find (Container, Key);
begin
  if C = No_Element then
     Insert (Container, Key, New_Item);
  else
     Replace (Container, Key, New_Item);
  end if;
end;

```

but this avoids doing the search twice. 

```ada
{AI12-0112-1} procedure Replace (Container : in out Map;
                   Key       : in     Key_Type;
                   New_Item  : in     Element_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = Length (Container)'Old;

```

{AI95-00302-03} Replace checks if a node with a key equivalent to Key is present in Container. If a match is found, Replace assigns Key and New_Item to the matching node; otherwise, Constraint_Error is propagated.

Discussion: We update the key as well as the element, as the key might include additional information that does not participate in equivalence. If only the element needs to be updated, use Replace_Element (Find (Container, Key), New_Element). 

```ada
{AI12-0112-1} procedure Exclude (Container : in out Map;
                   Key       : in     Key_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Length (Container)
                      in Original_Length - 1 | Original_Length);

```

{AI95-00302-03} Exclude checks if a node with a key equivalent to Key is present in Container. If a match is found, Exclude removes the node from the map.

Ramification: Exclude should work on an empty map; nothing happens in that case. 

```ada
{AI12-0112-1} procedure Delete (Container : in out Map;
                  Key       : in     Key_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = Length (Container)'Old - 1;

```

{AI95-00302-03} Delete checks if a node with a key equivalent to Key is present in Container. If a match is found, Delete removes the node from the map; otherwise, Constraint_Error is propagated.

```ada
procedure Delete (Container : in out Map;
                  Position  : in out Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Position /= No_Element 
                    or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Length (Container) = Length (Container)'Old - 1 and then
                Position = No_Element;

```

{AI95-00302-03} {AI12-0112-1} Delete removes the node designated by Position from the map.

Ramification: The check on Position checks that the cursor does not belong to some other map. This check implies that a reference to the map is included in the cursor value. This wording is not meant to require detection of dangling cursors; such cursors are defined to be invalid, which means that execution is erroneous, and any result is allowed (including not raising an exception). 

```ada
{AI12-0112-1} function First (Container : Map) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container)
                 then Has_Element (Container, First'Result)
                 else First'Result = No_Element);

```

{AI95-00302-03} If Length (Container) = 0, then First returns No_Element. Otherwise, First returns a cursor that designates the first node in Container.

```ada
{AI12-0112-1} function Next (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

{AI95-00302-03} Returns a cursor that designates the successor of the node designated by Position. If Position designates the last node, then No_Element is returned. If Position equals No_Element, then No_Element is returned.

```ada
function Next (Container : Map;
               Position  : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then Next'Result = No_Element
                 elsif Next'Result = No_Element then
                   Position = Last (Container)
                 else Has_Element (Container, Next'Result));

```

{AI12-0112-1} Returns a cursor designating the successor of the node designated by Position in Container.

```ada
{AI12-0112-1} procedure Next (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Next (Position).

```ada
procedure Next (Container : in     Map;
                Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Next (Container, Position).

```ada
{AI12-0112-1} function Find (Container : Map;
               Key       : Key_Type) return Cursor
      with Post =&gt (if Find'Result = No_Element
                    then Has_Element (Container, Find'Result));

```

{AI95-00302-03} If Length (Container) equals 0, then Find returns No_Element. Otherwise, Find checks if a node with a key equivalent to Key is present in Container. If a match is found, a cursor designating the matching node is returned; otherwise, No_Element is returned.

```ada
function Element (Container : Map;
                  Key       : Key_Type) return Element_Type;

```

{AI95-00302-03} Equivalent to Element (Find (Container, Key)).

```ada
function Contains (Container : Map;
                   Key       : Key_Type) return Boolean;

```

{AI95-00302-03} Equivalent to Find (Container, Key) /= No_Element.

Paragraphs 72 and 73 were moved above. 

```ada
{AI12-0112-1} procedure Iterate
  (Container : in Map;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0265-1} Iterate calls Process.all with a cursor that designates each node in Container, starting with the first node and moving the cursor according to the successor relation. Tampering with the cursors of Container is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

Implementation Note: The "tamper with cursors" check takes place when the operations that insert or delete elements, and so on, are called.

See Iterate for vectors (A.18.2) for a suggested implementation of the check. 

{AI12-0111-1} The nested package Stable provides a type Stable.Map that represents a stable map, which is one that cannot grow and shrink. Such a map can be created by calling the Copy function, or by establishing a stabilized view of an ordinary map.

{AI12-0111-1} The subprograms of the map package that have a parameter or result of type Map are included in the nested package Stable with the same specification, except that the following are omitted:

Tampering_With_Cursors_Prohibited, Tampering_With_Elements_Prohibited, Assign, Move, Insert, Include, Clear, Delete, Exclude, (for Ordered_Maps) Delete_First and Delete_Last, and (for Hashed_Maps) Reserve_Capacity 

Ramification: The names Map and Cursor mean the types declared in the nested package in these subprogram specifications. 

Reason: The omitted routines are those that tamper with cursors or elements (or test that state). The model is that it is impossible to tamper with cursors or elements of a stable view since no such operations are included. Thus tampering checks are not needed for a stable view, and we omit the operations associated with those checks. 

{AI12-0111-1} The operations of this package are equivalent to those for ordinary maps, except that the calls to Tampering_With_Cursors_Prohibited and Tampering_With_Elements_Prohibited that occur in preconditions are replaced by False, and any that occur in postconditions are replaced by True.

{AI12-0111-1} {AI12-0439-1} If a stable map is declared with the Base discriminant designating a pre-existing ordinary map, the stable map represents a stabilized view of the underlying ordinary map, and any operation on the stable map is reflected on the underlying ordinary map. While a stabilized view exists, any operation that tampers with elements performed on the underlying map is prohibited. The finalization of a stable map that provides such a view removes this restriction on the underlying ordinary map [(though some other restriction can exist due to other concurrent iterations or stabilized views)].

{AI12-0111-1} {AI12-0438-1} If a stable map is declared without specifying Base, the object is necessarily initialized. The initializing expression of the stable map, [typically a call on Copy], determines the Length of the map. The Length of a stable map never changes after initialization.

Proof: {AI12-0438-1} Initialization is required as the type is indefinite, see 3.3.1. 


#### Bounded (Run-Time) Errors

{AI05-0022-1} {AI05-0248-1} It is a bounded error for the actual function associated with a generic formal subprogram, when called as part of an operation of a map package, to tamper with elements of any map parameter of the operation. Either Program_Error is raised, or the operation works as defined on the value of the map either prior to, or subsequent to, some or all of the modifications to the map.

{AI05-0027-1} It is a bounded error to call any subprogram declared in the visible part of a map package when the associated container has been finalized. If the operation takes Container as an in out parameter, then it raises Constraint_Error or Program_Error. Otherwise, the operation either proceeds as it would for an empty container, or it raises Constraint_Error or Program_Error. 


#### Erroneous Execution

{AI95-00302-03} A Cursor value is invalid if any of the following have occurred since it was created: 

The map that contains the node it designates has been finalized;

{AI05-0160-1} The map that contains the node it designates has been used as the Target of a call to Assign, or as the target of an [assignment_statement](./AA-5.2#S0173);

The map that contains the node it designates has been used as the Source or Target of a call to Move; or

{AI05-0160-1} {AI05-0262-1} The node it designates has been removed from the map that previously contained the node. 

Ramification: {AI05-0160-1} This can happen directly via calls to Clear, Exclude, and Delete. 

The result of "=" or Has_Element is unspecified if these functions are called with an invalid cursor parameter. Execution is erroneous if any other subprogram declared in Containers.Hashed_Maps or Containers.Ordered_Maps is called with an invalid cursor parameter.

Discussion: The list above is intended to be exhaustive. In other cases, a cursor value continues to designate its original element. For instance, cursor values survive the insertion and deletion of other nodes.

While it is possible to check for these cases, in many cases the overhead necessary to make the check is substantial in time or space. Implementations are encouraged to check for as many of these cases as possible and raise Program_Error if detected. 

{AI05-0212-1} Execution is erroneous if the map associated with the result of a call to Reference or Constant_Reference is finalized before the result object returned by the call to Reference or Constant_Reference is finalized. 

Reason: Each object of Reference_Type and Constant_Reference_Type probably contains some reference to the originating container. If that container is prematurely finalized (which is only possible via Unchecked_Deallocation, as accessibility checks prevent passing a container to Reference that will not live as long as the result), the finalization of the object of Reference_Type will try to access a nonexistent object. This is a normal case of a dangling pointer created by Unchecked_Deallocation; we have to explicitly mention it here as the pointer in question is not visible in the specification of the type. (This is the same reason we have to say this for invalid cursors.) 


#### Implementation Requirements

{AI95-00302-03} {AI12-0437-1} No storage associated with a map object shall be lost upon assignment or scope exit.

{AI95-00302-03} {AI05-0262-1} The execution of an [assignment_statement](./AA-5.2#S0173) for a map shall have the effect of copying the elements from the source map object to the target map object and changing the length of the target object to that of the source object.

Implementation Note: {AI05-0298-1} An assignment of a Map is a "deep" copy; that is the elements are copied as well as the data structures. We say "effect of" in order to allow the implementation to avoid copying elements immediately if it wishes. For instance, an implementation that avoided copying until one of the containers is modified would be allowed. (Note that this implementation would require care, see A.18.2 for more.) 


#### Implementation Advice

{AI95-00302-03} Move should not copy elements, and should minimize copying of internal data structures. 

Implementation Advice: Move for a map should not copy elements, and should minimize copying of internal data structures.

Implementation Note: Usually that can be accomplished simply by moving the pointer(s) to the internal data structures from the Source container to the Target container. 

{AI95-00302-03} If an exception is propagated from a map operation, no storage should be lost, nor any elements removed from a map unless specified by the operation. 

Implementation Advice: If an exception is propagated from a map operation, no storage should be lost, nor any elements removed from a map unless specified by the operation.

Reason: This is important so that programs can recover from errors. But we don't want to require heroic efforts, so we just require documentation of cases where this can't be accomplished. 


#### Wording Changes from Ada 95

{AI95-00302-03} This description of maps is new; the extensions are documented with the specific packages. 


#### Extensions to Ada 2005

{AI05-0212-1} Added reference support to make map containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0001-1} Added procedure Assign; the extension and incompatibility is documented with the specific packages.

{AI05-0001-1} Generalized the definition of Move. Specified which elements are read/written by stream attributes.

{AI05-0022-1} Correction: Added a Bounded (Run-Time) Error to cover tampering by generic actual subprograms.

{AI05-0027-1} Correction: Added a Bounded (Run-Time) Error to cover access to finalized map containers.

{AI05-0160-1} Correction: Revised the definition of invalid cursors to cover missing (and new) cases.

{AI05-0265-1} Correction: Defined when a container prohibits tampering in order to more clearly define where the check is made and the exception raised. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for ordinary containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Extensions to Ada 2012

{AI12-0196-1} Correction: Replace_Element is now defined such that it can be used concurrently so long as it operates on different elements. This allows some container operations to be used in parallel without separate synchronization. 


#### Wording Changes from Ada 2012

{AI12-0110-1} Correction: Clarified that tampering checks precede all other checks made by a subprogram (but come after those associated with the call).

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.5  The Generic Package Containers.Hashed_Maps


#### Static Semantics

{AI95-00302-03} The generic library package Containers.Hashed_Maps has the following declaration: 

```ada
{AI05-0084-1} {AI05-0212-1} {AI12-0112-1} with Ada.Iterator_Interfaces;
generic
   type Key_Type is private;
   type Element_Type is private;
   with function Hash (Key : Key_Type) return Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type)
      return Boolean;
   with function "=" (Left, Right : Element_Type)
      return Boolean is &lt&gt;
package Ada.Containers.Hashed_Maps
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects in this generic package, see the notes on the equivalent operations in the specification of the Containers.Vectors package (see A.18.2). 

```ada
{AI05-0212-1} {AI12-0111-1} {AI12-0112-1} {AI12-0212-1} {AI12-0339-1} {AI12-0339-1}    type Map is tagged private
      with Constant_Indexing =&gt Constant_Reference,
           Variable_Indexing =&gt Reference,
           Default_Iterator  =&gt Iterate,
           Iterator_Element  =&gt Element_Type,
           Iterator_View     =&gt Stable.Map,
           Aggregate         =&gt (Empty     =&gt Empty,
                                 Add_Named =&gt Insert),
           Stable_Properties =&gt (Length,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =&gt
              Length (Map) = 0 and then
              (not Tampering_With_Cursors_Prohibited (Map)) and then
              (not Tampering_With_Elements_Prohibited (Map)),
           Preelaborable_Initialization;

```

Discussion: {AI12-0112-1} Unlike a Vector, the Stable_Properties of a Hashed_Map do not include the Capacity. If we had included it, some of the otherwise shared definitions would need different postconditions for Hashed_Maps and Ordered_Maps. If we were starting these containers from scratch, we probably would have approached the sharing of definitions differently so that we could avoid issues like this, but a major reorganization of this existing material would be too much change. 

```ada
{AI12-0399-1}    type Cursor is private
      with Preelaborable_Initialization;

```

```ada
   Empty_Map : constant Map;

```

```ada
   No_Element : constant Cursor;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Has_Element (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Has_Element (Container : Map; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1}    package Map_Iterator_Interfaces is new
       Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
   function "=" (Left, Right : Map) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_Cursors_Prohibited
      (Container : Map) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Tampering_With_Elements_Prohibited
      (Container : Map) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty (Capacity : Count_Type := implementation-defined)
      return Map
      with Post =&gt
              Capacity (Empty'Result) &gt= Capacity and then
              not Tampering_With_Elements_Prohibited (Empty'Result) and then
              not Tampering_With_Cursors_Prohibited (Empty'Result) and then
              Length (Empty'Result) = 0;

```

```ada
{AI12-0112-1}    function Capacity (Container : Map) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Reserve_Capacity (Container : in out Map;
                               Capacity  : in     Count_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
           Post =&gt Container.Capacity &gt= Capacity;

```

```ada
{AI12-0112-1}    function Length (Container : Map) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Empty (Container : Map) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

```ada
{AI12-0112-1}    procedure Clear (Container : in out Map)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Capacity (Container) = Capacity (Container)'Old and then
                   Length (Container) = 0;

```

```ada
{AI12-0112-1}    function Key (Position : Cursor) return Key_Type
      with Pre  =&gt Position /= No_Element 
                       or else raise Constraint_Error,
           Nonblocking, Global =&gt in all, Use_Formal =&gt Key_Type;

```

```ada
{AI12-0112-1}    function Key (Container : Map;
                 Position : Cursor) return Key_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                       or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Key_Type;

```

```ada
{AI12-0112-1}    function Element (Position : Cursor) return Element_Type
      with Pre  =&gt Position /= No_Element 
                       or else raise Constraint_Error,
           Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    function Element (Container : Map;
                     Position  : Cursor) return Element_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Map;
                              Position  : in     Cursor;
                              New_item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Key     : in Key_Type;
                                            Element : in Element_Type))
      with Pre  =&gt Position /= No_Element 
                      or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Map;
      Position  : in Cursor;
      Process   : not null access procedure (Key     : in Key_Type;
                                             Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Update_Element
     (Container : in out Map;
      Position  : in     Cursor;
      Process   : not null access procedure
                      (Key     : in     Key_Type;
                       Element : in out Element_Type))
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Reference_Type (Element : not null access Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Map;
                                Position  : in Cursor)
      return Constant_Reference_Type
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Map;
                       Position  : in Cursor)
      return Reference_Type
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Map;
                                Key       : in Key_Type)
      return Constant_Reference_Type
      with Pre  =&gt Find (Container, Key) /= No_Element 
                      or else raise Constraint_Error,
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Map;
                       Key       : in Key_Type)
      return Reference_Type
      with Pre  =&gt Find (Container, Key) /= No_Element 
                      or else raise Constraint_Error,
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0001-1} {AI12-0112-1}    procedure Assign (Target : in out Map; Source : in Map)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target) and then
                   Capacity (Target) &gt= Length (Source);

```

```ada
{AI05-0001-1} {AI12-0112-1}    function Copy (Source : Map; Capacity : Count_Type := 0)
      return Map
      with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                    or else raise Capacity_Error,
           Post =&gt
              Length (Copy'Result) = Length (Source) and then
              not Tampering_With_Elements_Prohibited (Copy'Result) and then
              not Tampering_With_Cursors_Prohibited (Copy'Result) and then
              Copy'Result.Capacity = (if Capacity = 0 then
                 Length (Source) else Capacity);

```

```ada
{AI12-0112-1}    procedure Move (Target : in out Map;
                   Source : in out Map)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                      or else raise Program_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                      Length (Target) = Length (Source'Old) and then
                      Length (Source) = 0);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Has_Element (Container, Position) and then
                     (if Inserted then
                        Length (Container) = Original_Length + 1
                      else
                        Length (Container) = Original_Length)) and then
                    Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Has_Element (Container, Position) and then
                     (if Inserted then
                        Length (Container) = Original_Length + 1
                      else
                        Length (Container) = Original_Length)) and then
                    Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt Length (Container) = Length (Container)'Old + 1 and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Include (Container : in out Map;
                      Key       : in     Key_Type;
                      New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container)
                         in Original_Length | Original_Length + 1) and then
                    Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Replace (Container : in out Map;
                      Key       : in     Key_Type;
                      New_Item  : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old;

```

```ada
{AI12-0112-1}    procedure Exclude (Container : in out Map;
                      Key       : in     Key_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container)
                         in Original_Length - 1 | Original_Length);

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Map;
                     Key       : in     Key_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old - 1;

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Map;
                     Position  : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Position /= No_Element 
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Length (Container) = Length (Container)'Old - 1 and then
                   Position = No_Element;

```

```ada
{AI12-0112-1}    function First (Container : Map) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, First'Result)
                    else First'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Container : Map;
                  Position : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then Next'Result = No_Element
                    elsif Next'Result = No_Element then
                      Position = Last (Container)
                    else Has_Element (Container, Next'Result));

```

```ada
{AI12-0112-1}    procedure Next (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Next (Container : in     Map;
                   Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Find (Container : Map;
                  Key       : Key_Type)
      return Cursor
      with Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

```ada
   function Element (Container : Map;
                     Key       : Key_Type)
      return Element_Type;

```

```ada
   function Contains (Container : Map;
                      Key       : Key_Type) return Boolean;

```

```ada
This paragraph was deleted.{AI05-0212-1} 

```

```ada
{AI12-0112-1}    function Equivalent_Keys (Left, Right : Cursor)
      return Boolean
      with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                         or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function Equivalent_Keys (Left  : Cursor;
                             Right : Key_Type)
      return Boolean
      with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function Equivalent_Keys (Left  : Key_Type;
                             Right : Cursor)
      return Boolean
      with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Iterate
     (Container : in Map;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate (Container : in Map)
      return Map_Iterator_Interfaces.Parallel_Iterator'Class
      with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0111-1}    package Stable is

```

```ada
{AI12-0111-1} {AI12-0339-1} {AI12-0399-1} {AI12-0407-1}       type Map (Base : not null access Hashed_Maps.Map) is
         tagged limited private
         with Constant_Indexing =&gt Constant_Reference,
              Variable_Indexing =&gt Reference,
              Default_Iterator  =&gt Iterate,
              Iterator_Element  =&gt Element_Type,
              Stable_Properties =&gt (Length),
              Global            =&gt null,
              Default_Initial_Condition =&gt Length (Map) = 0,
              Preelaborable_Initialization;

```

```ada
{AI12-0111-1} {AI12-0399-1}       type Cursor is private
      with Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       Empty_Map : constant Map;

```

```ada
{AI12-0111-1}       No_Element : constant Cursor;

```

```ada
{AI12-0111-1}       function Has_Element (Position : Cursor) return Boolean
         with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0111-1}       package Map_Iterator_Interfaces is new
         Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
{AI12-0111-1}       procedure Assign (Target : in out Hashed_Maps.Map;
                        Source : in Map)
         with Post =&gt Length (Source) = Length (Target);

```

```ada
{AI12-0111-1}       function Copy (Source : Hashed_Maps.Map) return Map
         with Post =&gt Length (Copy'Result) = Length (Source);

```

```ada
{AI12-0111-1}       type Constant_Reference_Type
            (Element : not null access constant Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       type Reference_Type
            (Element : not null access Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       -- Additional subprograms as described in the text
      -- are declared here.

```

```ada
{AI12-0111-1}    private

```

```ada
{AI12-0111-1}       ... -- not specified by the language

```

```ada
{AI12-0111-1}    end Stable;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Hashed_Maps;

```

{AI95-00302-03} An object of type Map contains an expandable hash table, which is used to provide direct access to nodes. The capacity of an object of type Map is the maximum number of nodes that can be inserted into the hash table prior to it being automatically expanded.

Implementation Note: The expected implementation for a Map uses a hash table which is grown when it is too small, with linked lists hanging off of each bucket. Note that in that implementation a cursor needs a back pointer to the Map object to implement iteration; that could either be in the nodes, or in the cursor object. To provide an average O(1) access time, capacity would typically equal the number of buckets in such an implementation, so that the average bucket linked list length would be no more than 1.0.

There is no defined relationship between elements in a hashed map. Typically, iteration will return elements in the order that they are hashed in. 

{AI95-00302-03} Two keys K1 and K2 are defined to be equivalent if Equivalent_Keys (K1, K2) returns True.

{AI95-00302-03} The actual function for the generic formal function Hash is expected to return the same value each time it is called with a particular key value. For any two equivalent key values, the actual for Hash is expected to return the same value. If the actual for Hash behaves in some other manner, the behavior of this package is unspecified. Which subprograms of this package call Hash, and how many times they call it, is unspecified.

Implementation Note: The implementation is not required to protect against Hash raising an exception, or returning random numbers, or any other "bad" behavior. It's not practical to do so, and a broken Hash function makes the container unusable.

The implementation can call Hash whenever it is needed; we don't want to specify how often that happens. The result must remain the same (this is logically a pure function), or the behavior is unspecified. 

{AI95-00302-03} The actual function for the generic formal function Equivalent_Keys on Key_Type values is expected to return the same value each time it is called with a particular pair of key values. It should define an equivalence relationship, that is, be reflexive, symmetric, and transitive. If the actual for Equivalent_Keys behaves in some other manner, the behavior of this package is unspecified. Which subprograms of this package call Equivalent_Keys, and how many times they call it, is unspecified.

Implementation Note: As with Hash, the implementation is not required to protect against Equivalent_Keys raising an exception or returning random results. Similarly, the implementation can call this operation whenever it is needed. The result must remain the same (this is a logically pure function), or the behavior is unspecified. 

{AI95-00302-03} If the value of a key stored in a node of a map is changed other than by an operation in this package such that at least one of Hash or Equivalent_Keys give different results, the behavior of this package is unspecified.

Implementation Note: The implementation is not required to protect against changes to key values other than via the operations declared in the Hashed_Maps package.

To see how this could happen, imagine an instance of Hashed_Maps where the key type is an access-to-variable type and Hash returns a value derived from the components of the designated object. Then, any operation that has a key value could modify those components and change the hash value:

```ada
Key (Map).Some_Component := New_Value;

```

This is really a design error on the part of the user of the map; it shouldn't be possible to modify keys stored in a map. But we can't prevent this error anymore than we can prevent someone passing as Hash a random number generator. 

{AI95-00302-03} Which nodes are the first node and the last node of a map, and which node is the successor of a given node, are unspecified, other than the general semantics described in A.18.4.

Implementation Note: Typically the first node will be the first node in the first bucket, the last node will be the last node in the last bucket, and the successor will be obtained by following the collision list, and going to the next bucket at the end of each bucket. 

```ada
function Empty (Capacity : Count_Type := implementation-defined)
   return Map
   with Post =&gt
           Capacity (Empty'Result) &gt= Capacity and then
           not Tampering_With_Elements_Prohibited (Empty'Result) and then
           not Tampering_With_Cursors_Prohibited (Empty'Result) and then
           Length (Empty'Result) = 0;

```

{AI12-0339-1} Returns an empty map.

```ada
{AI12-0112-1} function Capacity (Container : Map) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} Returns the capacity of Container.

```ada
{AI12-0112-1} procedure Reserve_Capacity (Container : in out Map;
                            Capacity  : in     Count_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                 or else raise Program_Error,
        Post =&gt Container.Capacity &gt= Capacity;

```

{AI95-00302-03} Reserve_Capacity allocates a new hash table such that the length of the resulting map can become at least the value Capacity without requiring an additional call to Reserve_Capacity, and is large enough to hold the current length of Container. Reserve_Capacity then rehashes the nodes in Container onto the new hash table. It replaces the old hash table with the new hash table, and then deallocates the old hash table. Any exception raised during allocation is propagated and Container is not modified.

This paragraph was deleted.{AI12-0112-1} 

Implementation Note: This routine is used to preallocate the internal hash table to the specified capacity such that future Inserts do not require expansion of the hash table. Therefore, the implementation should allocate the needed memory to make that true at this point, even though the visible semantics could be preserved by waiting until enough elements are inserted.

{AI05-0005-1} While Reserve_Capacity can be used to reduce the capacity of a map, we do not specify whether an implementation actually supports reduction of the capacity. Since the actual capacity can be anything greater than or equal to Capacity, an implementation never has to reduce the capacity.

Reserve_Capacity tampers with the cursors, as rehashing probably will change the order that elements are stored in the map. 

```ada
{AI12-0112-1} procedure Clear (Container : in out Map)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Capacity (Container) = Capacity (Container)'Old and then
                Length (Container) = 0;

```

{AI95-00302-03} In addition to the semantics described in A.18.4, Clear does not affect the capacity of Container.

```ada
{AI12-0112-1} procedure Assign (Target : in out Map; Source : in Map)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target) and then
                   Capacity (Target) &gt= Length (Source);

```

{AI05-0001-1} {AI05-0248-1} In addition to the semantics described in A.18.4, if the length of Source is greater than the capacity of Target, Reserve_Capacity (Target, Length (Source)) is called before assigning any elements.

```ada
function Copy (Source : Map; Capacity : Count_Type := 0)
   return Map
   with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                   or else raise Capacity_Error,
        Post =&gt
           Length (Copy'Result) = Length (Source) and then
           not Tampering_With_Elements_Prohibited (Copy'Result) and then
           not Tampering_With_Cursors_Prohibited (Copy'Result) and then
           Copy'Result.Capacity = (if Capacity = 0 then
              Length (Source) else Capacity);

```

{AI05-0001-1} {AI12-0112-1} Returns a map whose keys and elements are initialized from the keys and elements of Source.

Implementation Note: In:

```ada
procedure Move (Target : in out Map;
                Source : in out Map);

```

The intended implementation is that the internal hash table of Target is first deallocated; then the internal hash table is removed from Source and moved to Target. 

```ada
{AI12-0112-1} procedure Insert (Container : in out Map;
                  Key       : in     Key_Type;
                  New_Item  : in     Element_Type;
                  Position  :    out Cursor;
                  Inserted  :    out Boolean)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Has_Element (Container, Position) and then
                  (if Inserted then
                     Length (Container) = Original_Length + 1
                   else
                     Length (Container) = Original_Length)) and then
                 Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} In addition to the semantics described in A.18.4, if Length (Container) equals Capacity (Container), then Insert first calls Reserve_Capacity to increase the capacity of Container to some larger value.

Implementation Note: Insert should only compare keys that hash to the same bucket in the hash table.

We specify when Reserve_Capacity is called to bound the overhead of capacity expansion operations (which are potentially expensive). Moreover, expansion can be predicted by comparing Capacity(Map) to Length(Map). Since we don't specify by how much the hash table is expanded, this only can be used to predict the next expansion, not later ones.

Implementation Note: In:

```ada
procedure Exclude (Container : in out Map;
                   Key       : in     Key_Type);

```

Exclude should only compare keys that hash to the same bucket in the hash table.

Implementation Note: In:

```ada
procedure Delete (Container : in out Map;
                  Key       : in     Key_Type);

```

Delete should only compare keys that hash to the same bucket in the hash table. The node containing the element may be deallocated now, or it may be saved and reused later.

Implementation Note: In:

```ada
function First (Container : Map) return Cursor;

```

In a typical implementation, this will be the first node in the lowest numbered hash bucket that contains a node.

Implementation Note: In:

```ada
function Next (Position  : Cursor) return Cursor;

```

In a typical implementation, this will return the next node in a bucket; if Position is the last node in a bucket, this will return the first node in the next nonempty bucket.

A typical implementation will need to a keep a pointer at the map container in the cursor in order to implement this function. 

Implementation Note: In:

```ada
function Find (Container : Map;
               Key       : Key_Type) return Cursor;

```

Find should only compare keys that hash to the same bucket in the hash table.

```ada
{AI12-0112-1} function Equivalent_Keys (Left, Right : Cursor)
   return Boolean
   with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                      or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Equivalent_Keys (Key (Left), Key (Right)).

```ada
{AI12-0112-1} function Equivalent_Keys (Left  : Cursor;
                          Right : Key_Type) return Boolean
   with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Equivalent_Keys (Key (Left), Right).

```ada
{AI12-0112-1} function Equivalent_Keys (Left  : Key_Type;
                          Right : Cursor) return Boolean
   with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Equivalent_Keys (Left, Key (Right)).

```ada
{AI12-0112-1} {AI12-0266-1} function Iterate (Container : in Map)
   return Map_Iterator_Interfaces.Parallel_Iterator'Class
   with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0266-1} Iterate returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each node in Container, starting with the first node and moving the cursor according to the successor relation when used as a forward iterator, and processing all nodes concurrently when used as a parallel iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.


#### Implementation Advice

{AI95-00302-03} If N is the length of a map, the average time complexity of the subprograms Element, Insert, Include, Replace, Delete, Exclude, and Find that take a key parameter should be O(log N). The average time complexity of the subprograms that take a cursor parameter should be O(1). The average time complexity of Reserve_Capacity should be O(N). 

Implementation Advice: The average time complexity of Element, Insert, Include, Replace, Delete, Exclude, and Find operations that take a key parameter for Containers.Hashed_Maps should be O(log N). The average time complexity of the subprograms of Containers.Hashed_Maps that take a cursor parameter should be O(1). The average time complexity of Containers.Hashed_Maps.Reserve_Capacity should be O(N).

Reason: We do not mean to overly constrain implementation strategies here. However, it is important for portability that the performance of large containers has roughly the same factors on different implementations. If a program is moved to an implementation for which Find is O(N), that program could be unusable when the maps are large. We allow O(log N) access because the proportionality constant and caching effects are likely to be larger than the log factor, and we don't want to discourage innovative implementations. 


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Hashed_Maps is new. 


#### Incompatibilities With Ada 2005

{AI05-0001-1} Subprograms Assign and Copy are added to Containers.Hashed_Maps. If an instance of Containers.Hashed_Maps is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Containers.Hashed_Maps is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 2005

{AI05-0212-1} Added iterator and indexing support to make hashed map containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0084-1} Correction: Added a pragma Remote_Types so that containers can be used in distributed programs. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} {AI12-0339-1} A number of new subprograms, types, and even a nested package were added to Containers.Hashed_Maps to better support contracts and stable views. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0212-1} Maps now support named container aggregates, so [aggregate](./AA-4.3#S0106) syntax can be used to create Maps.

{AI12-0266-1} The iterator for the container now can return a parallel iterator which can be used to process the container in parallel. 


#### Wording Changes from Ada 2012

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.6  The Generic Package Containers.Ordered_Maps


#### Static Semantics

{AI95-00302-03} The generic library package Containers.Ordered_Maps has the following declaration: 

```ada
{AI05-0084-1} {AI05-0212-1} {AI12-0112-1} with Ada.Iterator_Interfaces;
generic
   type Key_Type is private;
   type Element_Type is private;
   with function "&lt" (Left, Right : Key_Type) return Boolean is &lt&gt;
   with function "=" (Left, Right : Element_Type) return Boolean is &lt&gt;
package Ada.Containers.Ordered_Maps
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects in this generic package, see the notes on the equivalent operations in the specification of the Containers.Vectors package (see A.18.2). 

```ada
{AI12-0112-1}    function Equivalent_Keys (Left, Right : Key_Type) return Boolean
      is (not ((Left &lt Right) or (Right &lt Left)));

```

```ada
{AI05-0212-1} {AI12-0111-1} {AI12-0112-1} {AI12-0212-1} {AI12-0339-1} {AI12-0399-1}    type Map is tagged private
      with Constant_Indexing =&gt Constant_Reference,
           Variable_Indexing =&gt Reference,
           Default_Iterator  =&gt Iterate,
           Iterator_Element  =&gt Element_Type,
           Iterator_View     =&gt Stable.Map,
           Aggregate         =&gt (Empty     =&gt Empty,
                                 Add_Named =&gt Insert),
           Stable_Properties =&gt (Length,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =&gt
              Length (Map) = 0 and then
              (not Tampering_With_Cursors_Prohibited (Map)) and then
              (not Tampering_With_Elements_Prohibited (Map)),
           Preelaborable_Initialization;

```

```ada
{AI12-0399-1}    type Cursor is private
      with Preelaborable_Initialization;

```

```ada
   Empty_Map : constant Map;

```

```ada
   No_Element : constant Cursor;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Has_Element (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Has_Element (Container : Map; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1}    package Map_Iterator_Interfaces is new
       Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
   function "=" (Left, Right : Map) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_Cursors_Prohibited
      (Container : Map) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Tampering_With_Elements_Prohibited
      (Container : Map) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty return Map
      is (Empty_Map)
      with Post =&gt
              not Tampering_With_Elements_Prohibited (Empty'Result) and then
              not Tampering_With_Cursors_Prohibited (Empty'Result) and then
              Length (Empty'Result) = 0;

```

```ada
{AI12-0112-1}    function Length (Container : Map) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Empty (Container : Map) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

```ada
{AI12-0112-1}    procedure Clear (Container : in out Map)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container) = 0;

```

```ada
{AI12-0112-1}    function Key (Position : Cursor) return Key_Type
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Nonblocking, Global =&gt in all, Use_Formal =&gt Key_Type;

```

```ada
{AI12-0112-1}    function Key (Container : Map;
                 Position : Cursor) return Key_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                       or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Key_Type;

```

```ada
{AI12-0112-1}    function Element (Position : Cursor) return Element_Type
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    function Element (Container : Map;
                     Position  : Cursor) return Element_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                       or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Map;
                              Position  : in     Cursor;
                              New_item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Key     : in Key_Type;
                                            Element : in Element_Type))
      with Pre  =&gt Position /= No_Element 
                       or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Map;
      Position  : in Cursor;
      Process   : not null access procedure (Key     : in Key_Type;
                                             Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element 
                       or else raise Constraint_Error) and then
                    (Has_Element (Container, Position) 
                       or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Update_Element
     (Container : in out Map;
      Position  : in     Cursor;
      Process   : not null access procedure
                      (Key     : in     Key_Type;
                       Element : in out Element_Type))
      with Pre  =&gt (Position /= No_Element 
                       or else raise Constraint_Error) and then
                    (Has_Element (Container, Position) 
                       or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Reference_Type (Element : not null access Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Map;
                                Position  : in Cursor)
      return Constant_Reference_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Map;
                       Position  : in Cursor)
      return Reference_Type
      with Pre  =&gt (Position /= No_Element
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Map;
                                Key       : in Key_Type)
      return Constant_Reference_Type
      with Pre  =&gt Find (Container, Key) /= No_Element 
                      or else raise Constraint_Error,
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Map;
                       Key       : in Key_Type)
      return Reference_Type
      with Pre  =&gt Find (Container, Key) /= No_Element 
                      or else raise Constraint_Error,
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0001-1} {AI12-0112-1}    procedure Assign (Target : in out Map; Source : in Map)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target);

```

```ada
{AI05-0001-1} {AI12-0112-1}    function Copy (Source : Map)
      return Map
      with Post =&gt
         Length (Copy'Result) = Length (Source) and then
         not Tampering_With_Elements_Prohibited (Copy'Result) and then
         not Tampering_With_Cursors_Prohibited (Copy'Result);

```

```ada
{AI12-0112-1}    procedure Move (Target : in out Map;
                   Source : in out Map)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                       or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                       or else raise Program_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                       Length (Target) = Length (Source'Old) and then
                       Length (Source) = 0);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Has_Element (Container, Position) and then
                     (if Inserted then
                        Length (Container) = Original_Length + 1
                      else
                        Length (Container) = Original_Length));

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Has_Element (Container, Position) and then
                     (if Inserted then
                        Length (Container) = Original_Length + 1
                      else
                        Length (Container) = Original_Length));

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt Length (Container) = Length (Container)'Old + 1;

```

```ada
{AI12-0112-1}    procedure Include (Container : in out Map;
                      Key       : in     Key_Type;
                      New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                       or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container)
                         in Original_Length | Original_Length + 1);

```

```ada
{AI12-0112-1}    procedure Replace (Container : in out Map;
                      Key       : in     Key_Type;
                      New_Item  : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old;

```

```ada
{AI12-0112-1}    procedure Exclude (Container : in out Map;
                      Key       : in     Key_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container)
                         in Original_Length - 1 | Original_Length);

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Map;
                     Key       : in     Key_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old - 1;

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Map;
                     Position  : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Position /= No_Element 
                       or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Length (Container) = Length (Container)'Old - 1 and then
                   Position = No_Element;

```

```ada
{AI12-0112-1}    procedure Delete_First (Container : in out Map)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      (if Original_Length = 0 then Length (Container) = 0
                       else Length (Container) = Original_Length - 1));

```

```ada
{AI12-0112-1}    procedure Delete_Last (Container : in out Map)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      (if Original_Length = 0 then Length (Container) = 0
                       else Length (Container) = Original_Length - 1));

```

```ada
{AI12-0112-1}    function First (Container : Map) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, First'Result)
                    else First'Result = No_Element);

```

```ada
{AI12-0112-1}    function First_Element (Container : Map) return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function First_Key (Container : Map) return Key_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Last (Container : Map) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, Last'Result)
                    else Last'Result = No_Element);

```

```ada
{AI12-0112-1}    function Last_Element (Container : Map) return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Last_Key (Container : Map) return Key_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Next (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Container : Map;
                  Position : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then Next'Result = No_Element
                    elsif Next'Result = No_Element then
                      Position = Last (Container)
                    else Has_Element (Container, Next'Result));

```

```ada
{AI12-0112-1}    procedure Next (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Next (Container : in     Map;
                   Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Previous (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then
                       Previous'Result = No_Element);

```

```ada
{AI12-0112-1}    function Previous (Container : Map;
                      Position  : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then
                      Previous'Result = No_Element
                    elsif Previous'Result = No_Element then
                      Position = First (Container)
                    else Has_Element (Container, Previous'Result));

```

```ada
{AI12-0112-1}    procedure Previous (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Previous (Container : in     Map;
                       Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Find (Container : Map;
                  Key       : Key_Type) return Cursor
      with Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

```ada
   function Element (Container : Map;
                     Key       : Key_Type) return Element_Type;

```

```ada
{AI12-0112-1}    function Floor (Container : Map;
                   Key       : Key_Type) return Cursor
      with Post =&gt (if Floor'Result /= No_Element
                    then Has_Element (Container, Floor'Result));

```

```ada
{AI12-0112-1}    function Ceiling (Container : Map;
                     Key       : Key_Type) return Cursor
      with Post =&gt (if Ceiling'Result /= No_Element
                    then Has_Element (Container, Ceiling'Result));

```

```ada
   function Contains (Container : Map;
                      Key       : Key_Type) return Boolean;

```

```ada
This paragraph was deleted.{AI05-0212-1} 

```

```ada
{AI12-0112-1}    function "&lt" (Left, Right : Cursor) return Boolean
      with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                         or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&gt" (Left, Right : Cursor) return Boolean
      with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                         or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&lt" (Left : Cursor; Right : Key_Type) return Boolean
      with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&gt" (Left : Cursor; Right : Key_Type) return Boolean
      with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&lt" (Left : Key_Type; Right : Cursor) return Boolean
      with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&gt" (Left : Key_Type; Right : Cursor) return Boolean
      with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure  Iterate
     (Container : in Map;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI12-0112-1}    procedure Reverse_Iterate
     (Container : in Map;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate (Container : in Map)
      return Map_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
      with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI05-0262-1} {AI12-0112-1}    function Iterate (Container : in Map; Start : in Cursor)
      return Map_Iterator_Interfaces.Reversible_Iterator'Class
      with Pre  =&gt (Start /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Start)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0111-1}    package Stable is

```

```ada
{AI12-0111-1} {AI12-0339-1} {AI12-0399-1} {AI12-0407-1}       type Map (Base : not null access Ordered_Maps.Map) is
         tagged limited private
         with Constant_Indexing =&gt Constant_Reference,
              Variable_Indexing =&gt Reference,
              Default_Iterator  =&gt Iterate,
              Iterator_Element  =&gt Element_Type,
              Stable_Properties =&gt (Length),
              Global            =&gt null,
              Default_Initial_Condition =&gt Length (Map) = 0,
              Preelaborable_Initialization;

```

```ada
{AI12-0111-1} {AI12-0399-1}       type Cursor is private
         with Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       Empty_Map : constant Map;

```

```ada
{AI12-0111-1}       No_Element : constant Cursor;

```

```ada
{AI12-0111-1}       function Has_Element (Position : Cursor) return Boolean
         with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0111-1}       package Map_Iterator_Interfaces is new
         Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
{AI12-0111-1}       procedure Assign (Target : in out Ordered_Maps.Map;
                        Source : in Map)
         with Post =&gt Length (Source) = Length (Target);

```

```ada
{AI12-0111-1}       function Copy (Source : Ordered_Maps.Map) return Map
         with Post =&gt Length (Copy'Result) = Length (Source);

```

```ada
{AI12-0111-1}       type Constant_Reference_Type
            (Element : not null access constant Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       type Reference_Type
            (Element : not null access Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       -- Additional subprograms as described in the text
      -- are declared here.

```

```ada
{AI12-0111-1}    private

```

```ada
{AI12-0111-1}       ... -- not specified by the language

```

```ada
{AI12-0111-1}    end Stable;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Ordered_Maps;

```

{AI95-00302-03} Two keys K1 and K2 are equivalent if both K1 &lt K2 and K2 &lt K1 return False, using the generic formal "&lt" operator for keys. Function Equivalent_Keys returns True if Left and Right are equivalent, and False otherwise.

{AI95-00302-03} {AI05-0044-1} The actual function for the generic formal function "&lt" on Key_Type values is expected to return the same value each time it is called with a particular pair of key values. It should define a strict weak ordering relationship (see A.18). If the actual for "&lt" behaves in some other manner, the behavior of this package is unspecified. Which subprograms of this package call "&lt" and how many times they call it, is unspecified.

Implementation Note: The implementation is not required to protect against "&lt" raising an exception, or returning random results, or any other "bad" behavior. It's not practical to do so, and a broken "&lt" function makes the container unusable.

The implementation can call "&lt" whenever it is needed; we don't want to specify how often that happens. The result must remain the same (this is a logically pure function), or the behavior is unspecified. 

{AI95-00302-03} If the value of a key stored in a map is changed other than by an operation in this package such that at least one of "&lt" or "=" give different results, the behavior of this package is unspecified.

Implementation Note: The implementation is not required to protect against changes to key values other than via the operations declared in the Ordered_Maps package.

To see how this could happen, imagine an instance of Ordered_Maps package where the key type is an access-to-variable type and "&lt" returns a value derived from comparing the components of the designated objects. Then, any operation that has a key value (even if the key value is constant) could modify those components and change the result of "&lt":

```ada
Key (Map).Some_Component := New_Value;

```

This is really a design error on the part of the user of the map; it shouldn't be possible to modify keys stored in a map such that "&lt" changes. But we can't prevent this error anymore than we can prevent someone passing as "&lt" a routine that produces random answers. 

{AI95-00302-03} {AI05-0262-1} The first node of a nonempty map is the one whose key is less than the key of all the other nodes in the map. The last node of a nonempty map is the one whose key is greater than the key of all the other elements in the map. The successor of a node is the node with the smallest key that is larger than the key of the given node. The predecessor of a node is the node with the largest key that is smaller than the key of the given node. All comparisons are done using the generic formal "&lt" operator for keys.

```ada
{AI12-0112-1} function Copy (Source : Map)
   return Map
   with Post =&gt
      Length (Copy'Result) = Length (Source) and then
      not Tampering_With_Elements_Prohibited (Copy'Result) and then
      not Tampering_With_Cursors_Prohibited (Copy'Result);

```

{AI05-0001-1} Returns a map whose keys and elements are initialized from the corresponding keys and elements of Source.

```ada
{AI12-0112-1} procedure Delete_First (Container : in out Map)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   (if Original_Length = 0 then Length (Container) = 0
                    else Length (Container) = Original_Length - 1));

```

{AI95-00302-03} {AI05-0264-1} If Container is empty, Delete_First has no effect. Otherwise, the node designated by First (Container) is removed from Container. Delete_First tampers with the cursors of Container.

```ada
{AI12-0112-1} procedure Delete_Last (Container : in out Map)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   (if Original_Length = 0 then Length (Container) = 0
                    else Length (Container) = Original_Length - 1));

```

{AI95-00302-03} {AI05-0264-1} If Container is empty, Delete_Last has no effect. Otherwise, the node designated by Last (Container) is removed from Container. Delete_Last tampers with the cursors of Container.

```ada
{AI12-0112-1} function First_Element (Container : Map) return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (First (Container)).

```ada
{AI12-0112-1} function First_Key (Container : Map) return Key_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Key (First (Container)).

```ada
{AI12-0112-1} function Last (Container : Map) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container)
                 then Has_Element (Container, Last'Result)
                 else Last'Result = No_Element);

```

{AI95-00302-03} Returns a cursor that designates the last node in Container. If Container is empty, returns No_Element.

```ada
{AI12-0112-1} function Last_Element (Container : Map) return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (Last (Container)).

```ada
{AI12-0112-1} function Last_Key (Container : Map) return Key_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Key (Last (Container)).

```ada
{AI12-0112-1} function Previous (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element then
                    Previous'Result = No_Element);

```

{AI95-00302-03} {AI05-0262-1} If Position equals No_Element, then Previous returns No_Element. Otherwise, Previous returns a cursor designating the predecessor node of the one designated by Position. If Position designates the first element, then Previous returns No_Element.

```ada
function Previous (Container : Map;
                   Position : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then
                   Previous'Result = No_Element
                 elsif Previous'Result = No_Element then
                   Position = First (Container)
                 else Has_Element (Container, Previous'Result));

```

{AI12-0112-1} Returns a cursor designating the predecessor of the node designated by Position in Container, if any.

```ada
{AI12-0112-1} procedure Previous (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Previous (Position).

```ada
procedure Previous (Container : in     Map;
                    Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Previous (Container, Position).

```ada
{AI12-0112-1} function Floor (Container : Map;
                Key       : Key_Type) return Cursor
   with Post =&gt (if Floor'Result /= No_Element
                 then Has_Element (Container, Floor'Result));

```

{AI95-00302-03} {AI05-0264-1} Floor searches for the last node whose key is not greater than Key, using the generic formal "&lt" operator for keys. If such a node is found, a cursor that designates it is returned. Otherwise, No_Element is returned.

```ada
{AI12-0112-1} function Ceiling (Container : Map;
                  Key       : Key_Type) return Cursor
   with Post =&gt (if Ceiling'Result /= No_Element
                 then Has_Element (Container, Ceiling'Result));

```

{AI95-00302-03} {AI05-0264-1} Ceiling searches for the first node whose key is not less than Key, using the generic formal "&lt" operator for keys. If such a node is found, a cursor that designates it is returned. Otherwise, No_Element is returned.

```ada
{AI12-0112-1} function "&lt" (Left, Right : Cursor) return Boolean
   with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                      or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Key (Left) &lt Key (Right).

```ada
{AI12-0112-1} function "&gt" (Left, Right : Cursor) return Boolean
   with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                      or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Key (Right) &lt Key (Left).

```ada
{AI12-0112-1} function "&lt" (Left : Cursor; Right : Key_Type) return Boolean
   with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Key (Left) &lt Right.

```ada
{AI12-0112-1} function "&gt" (Left : Cursor; Right : Key_Type) return Boolean
   with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

{AI95-00302-03} Equivalent to Right &lt Key (Left).

```ada
{AI12-0112-1} function "&lt" (Left : Key_Type; Right : Cursor) return Boolean
   with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

{AI95-00302-03} Equivalent to Left &lt Key (Right).

```ada
{AI12-0112-1} function "&gt" (Left : Key_Type; Right : Cursor) return Boolean
   with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

{AI95-00302-03} Equivalent to Key (Right) &lt Left.

```ada
{AI12-0112-1} procedure Reverse_Iterate
  (Container : in Map;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0212-1} Iterates over the nodes in Container as per procedure Iterate, with the difference that the nodes are traversed in predecessor order, starting with the last node.

```ada
{AI12-0112-1} {AI12-0266-1} function Iterate (Container : in Map)
   return Map_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
   with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0266-1} Iterate returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each node in Container, starting with the first node and moving the cursor according to the successor relation when used as a forward iterator, and starting with the last node and moving the cursor according to the predecessor relation when used as a reverse iterator, and processing all nodes concurrently when used as a parallel iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

```ada
function Iterate (Container : in Map; Start : in Cursor)
   return Map_Iterator_Interfaces.Reversible_Iterator'Class
   with Pre  =&gt (Start /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Start)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0262-1} {AI05-0265-1} {AI05-0269-1} {AI12-0112-1} Iterate returns a reversible iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each node in Container, starting with the node designated by Start and moving the cursor according to the successor relation when used as a forward iterator, or moving the cursor according to the predecessor relation when used as a reverse iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

Discussion: Exits are allowed from the loops created using the iterator objects. In particular, to stop the iteration at a particular cursor, just add 

```ada
exit when Cur = Stop;

```

in the body of the loop (assuming that Cur is the loop parameter and Stop is the cursor that you want to stop at). 


#### Implementation Advice

{AI95-00302-03} If N is the length of a map, then the worst-case time complexity of the Element, Insert, Include, Replace, Delete, Exclude, and Find operations that take a key parameter should be O((log N)**2) or better. The worst-case time complexity of the subprograms that take a cursor parameter should be O(1). 

Implementation Advice: The worst-case time complexity of Element, Insert, Include, Replace, Delete, Exclude, and Find operations that take a key parameter for Containers.Ordered_Maps should be O((log N)**2) or better. The worst-case time complexity of the subprograms of Containers.Ordered_Maps that take a cursor parameter should be O(1).

Implementation Note: A balanced (red-black) tree for keys has O(log N) worst-case performance. Note that a O(N) worst-case implementation (like a list) would be wrong. 

Reason: We do not mean to overly constrain implementation strategies here. However, it is important for portability that the performance of large containers has roughly the same factors on different implementations. If a program is moved to an implementation that takes O(N) to find elements, that program could be unusable when the maps are large. We allow the extra log N factors because the proportionality constant and caching effects are likely to be larger than the log factor, and we don't want to discourage innovative implementations. 


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Ordered_Maps is new. 


#### Incompatibilities With Ada 2005

{AI05-0001-1} Subprograms Assign and Copy are added to Containers.Ordered_Maps. If an instance of Containers.Ordered_Maps is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Containers.Ordered_Maps is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 2005

{AI05-0212-1} Added iterator and indexing support to make ordered map containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0044-1} Correction: Redefined "&lt" actuals to require a strict weak ordering; the old definition allowed indeterminant comparisons that would not have worked in a container.

{AI05-0084-1} Correction: Added a pragma Remote_Types so that containers can be used in distributed programs. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} {AI12-0339-1} A number of new subprograms, types, and even a nested package were added to Containers.Ordered_Maps to better support contracts and stable views. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0212-1} Maps now support named container aggregates, so [aggregate](./AA-4.3#S0106) syntax can be used to create Maps.

{AI12-0266-1} The iterator for the entire container now can return a parallel iterator which can be used to process the container in parallel. 


#### Wording Changes from Ada 2012

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.7  Sets

{AI95-00302-03} The language-defined generic packages Containers.Hashed_Sets and Containers.Ordered_Sets provide private types Set and Cursor, and a set of operations for each type. A set container allows elements of an arbitrary type to be stored without duplication. A hashed set uses a hash function to organize elements, while an ordered set orders its element per a specified relation. 

{AI95-00302-03} {AI05-0299-1} This subclause describes the declarations that are common to both kinds of sets. See A.18.8 for a description of the semantics specific to Containers.Hashed_Sets and A.18.9 for a description of the semantics specific to Containers.Ordered_Sets. 


#### Static Semantics

{AI95-00302-03} The actual function for the generic formal function "=" on Element_Type values is expected to define a reflexive and symmetric relationship and return the same result value each time it is called with a particular pair of values. If it behaves in some other manner, the function "=" on set values returns an unspecified value. The exact arguments and number of calls of this generic formal function by the function "=" on set values are unspecified.

Ramification: If the actual function for "=" is not symmetric and consistent, the result returned by the "=" for Set objects cannot be predicted. The implementation is not required to protect against "=" raising an exception, or returning random results, or any other "bad" behavior. And it can call "=" in whatever manner makes sense. But note that only the result of "=" for Set objects is unspecified; other subprograms are not allowed to break if "=" is bad (they aren't expected to use "="). 

{AI95-00302-03} The type Set is used to represent sets. The type Set needs finalization (see 7.6).

{AI95-00302-03} A set contains elements. Set cursors designate elements. There exists an equivalence relation on elements, whose definition is different for hashed sets and ordered sets. A set never contains two or more equivalent elements. The length of a set is the number of elements it contains.

{AI95-00302-03} Each nonempty set has two particular elements called the first element and the last element (which may be the same). Each element except for the last element has a successor element. If there are no other intervening operations, starting with the first element and repeatedly going to the successor element will visit each element in the set exactly once until the last element is reached. The exact definition of these terms is different for hashed sets and ordered sets.

{AI95-00302-03} {AI12-0111-1} {AI12-0112-1} [Some operations  check for "tampering with cursors" of a container because they depend on the set of elements of the container remaining constant and  on elements of the container not being replaced.] When tampering with cursors is prohibited for a particular set object S, Program_Error is propagated by the finalization of S[, as well as by a call that passes S to certain of the operations of this package, as indicated by the precondition of such an operation].

Discussion: {AI12-0112-1} Note that Replace_Element tampers with cursors because it might delete and reinsert the element if it moves in the set. That could change the order of iteration, which is what this check is designed to prevent. Replace also tampers with cursors, as it is defined in terms of Replace_Element.

{AI12-0112-1} These inclusions mean that there are no operations that would tamper with elements that do not tamper with cursors. As such, we do not define tampering with elements at all for set containers. Earlier versions of Ada did so just so the description of subprograms are the same between containers, but since we've changed those to pre- and postconditions which are necessarily specific to each container, there no longer seems to be any reason to define tampering with elements for sets. 

Paragraphs 8 through 14 are removed as preconditions now describe these rules. 

Ramification: We don't need to explicitly mention [assignment_statement](./AA-5.2#S0173), because that finalizes the target object as part of the operation, and finalization of an object is already defined as tampering with cursors. 

{AI95-00302-03} Empty_Set represents the empty Set object. It has a length of 0. If an object of type Set is not otherwise initialized, it is initialized to the same value as Empty_Set.

{AI95-00302-03} No_Element represents a cursor that designates no element. If an object of type Cursor is not otherwise initialized, it is initialized to the same value as No_Element.

{AI95-00302-03} {AI12-0434-1} The primitive "=" operator for type Cursor returns True if both cursors are No_Element, or designate the same element in the same container.

To be honest: {AI12-0434-1} "The primitive "=" operator" is the one with two parameters of type Cursor which returns Boolean. We're not talking about some other (hidden) primitive function named "=". 

{AI95-00302-03} Execution of the default implementation of the Input, Output, Read, or Write attribute of type Cursor raises Program_Error.

Reason: A cursor will probably be implemented in terms of one or more access values, and the effects of streaming access values is unspecified. Rather than letting the user stream junk by accident, we mandate that streaming of cursors raise Program_Error by default. The attributes can always be specified if there is a need to support streaming. 

{AI05-0001-1} {AI05-0262-1} {AI12-0437-1} Set'Write for a Set object S writes Length(S) elements of the set to the stream. It may also write additional information about the set.

{AI05-0001-1} {AI05-0262-1} Set'Read reads the representation of a set from the stream, and assigns to Item a set with the same length and elements as was written by Set'Write.

Ramification: Streaming more elements than the container length is wrong. For implementation implications of this rule, see the Implementation Note in A.18.2. 

```ada
{AI12-0112-1} function Has_Element (Position : Cursor) return Boolean
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0212-1} Returns True if Position designates an element, and returns False otherwise.

To be honest: {AI05-0005-1} {AI05-0212-1} This function might not detect cursors that designate deleted elements; such cursors are invalid (see below) and the result of calling Has_Element with an invalid cursor is unspecified (but not erroneous). 

```ada
{AI12-0112-1} function Has_Element (Container : Set; Position : Cursor)
   return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if Position designates an element in Container, and returns False otherwise.

Ramification: {AI12-0112-1} If Position is No_Element, Has_Element returns False. 

```ada
function "=" (Left, Right : Set) return Boolean;

```

{AI95-00302-03} If Left and Right denote the same set object, then the function returns True. If Left and Right have different lengths, then the function returns False. Otherwise, for each element E in Left, the function returns False if an element equal to E (using the generic formal equality operator) is not present in Right. If the function has not returned a result after checking all of the elements, it returns True. Any exception raised during evaluation of element equality is propagated. 

Implementation Note: This wording describes the canonical semantics. However, the order and number of calls on the formal equality function is unspecified for all of the operations that use it in this package, so an implementation can call it as many or as few times as it needs to get the correct answer. Specifically, there is no requirement to call the formal equality additional times once the answer has been determined. 

```ada
function Equivalent_Sets (Left, Right : Set) return Boolean;

```

{AI95-00302-03} If Left and Right denote the same set object, then the function returns True. If Left and Right have different lengths, then the function returns False. Otherwise, for each element E in Left, the function returns False if an element equivalent to E is not present in Right. If the function has not returned a result after checking all of the elements, it returns True. Any exception raised during evaluation of element equivalence is propagated.

```ada
function Tampering_With_Cursors_Prohibited
   (Container : Set) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if tampering with cursors is currently prohibited for Container, and returns False otherwise.

Implementation Note: {AI12-0112-1} Various contracts elsewhere in this specification require that this function be implemented with synchronized data. Moreover, it is possible for tampering to be prohibited by multiple operations (sequentially or in parallel). Therefore, tampering needs to be implemented with an atomic or protected counter. The counter is initialized to zero, and is incremented when tampering is prohibited, and decremented when leaving an area that prohibited tampering. Function Tampering_With_Cursors_Prohibited returns True if the counter is nonzero. (Note that any case where the result is not well-defined for one task is incorrect use of shared variables and would be erroneous by the rules of 9.10, so no special protection is needed to read the counter.) 

```ada
{AI12-0112-1} function To_Set (New_Item : Element_Type) return Set
   with Post =&gt Length (To_Set'Result) = 1 and then
              not Tampering_with_Cursors_Prohibited (To_Set'Result);

```

{AI95-00302-03} Returns a set containing the single element New_Item.

```ada
{AI12-0112-1} function Length (Container : Set) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} Returns the number of elements in Container.

```ada
function Is_Empty (Container : Set) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

{AI95-00302-03} {AI12-0112-1} Returns True if Container is empty.

```ada
{AI12-0112-1} procedure Clear (Container : in out Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = 0;

```

{AI95-00302-03} Removes all the elements from Container.

```ada
function Element (Position : Cursor) return Element_Type
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

{AI95-00302-03} {AI12-0112-1} Element returns the element designated by Position.

```ada
function Element (Container : Set;
                  Position  : Cursor) return Element_Type
   with Pre  =&gt (Position /= No_Element
                    or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

{AI12-0112-1} Element returns the element designated by Position.

```ada
procedure Replace_Element (Container : in out Set;
                           Position  : in     Cursor;
                           New_item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI95-00302-03} {AI12-0112-1} {AI12-0196-1} Replace_Element assigns New_Item to the element designated by Position. Any exception raised by the assignment is propagated. For the purposes of determining whether the parameters overlap in a call to Replace_Element, the Container parameter is not considered to overlap with any object [(including itself)].

Implementation Note: The final assignment may require that the node of the element be moved in the Set's data structures. That could mean that implementing this operation exactly as worded above could require the overhead of searching twice. Implementations are encouraged to avoid this extra overhead when possible, by prechecking if the old element is equivalent to the new one, by inserting a placeholder node while checking for an equivalent element, and similar optimizations.

The cursor still designates the same element after this operation; only the value of that element has changed. Cursors cannot include information about the relative position of an element in a Set (as they must survive insertions and deletions of other elements), so this should not pose an implementation hardship. 

```ada
procedure Query_Element
  (Position : in Cursor;
   Process  : not null access procedure (Element : in Element_Type))
   with Pre  =&gt Position /= No_Element 
                   or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} {AI05-0021-1} {AI05-0265-1} {AI12-0112-1} Query_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of the set that contains the element designated by Position is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Query_Element
  (Container : in Set;
   Position  : in Cursor;
   Process  : not null access procedure (Element : in Element_Type))
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI12-0112-1} Query_Element calls Process.all with the key and element from the node designated by Position as the arguments. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
{AI12-0112-1} type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

{AI05-0212-1} The type Constant_Reference_Type needs finalization.

This paragraph was deleted.{AI12-0112-1} 

Reason: It is expected that Constant_Reference_Type will be a controlled type, for which finalization will have some action to terminate the tampering check for the associated container. If the object is created by default, however, there is no associated container. Since this is useless, and supporting this case would take extra work, we define it to raise an exception. 

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in Set;
                             Position  : in Cursor)
   return Constant_Reference_Type
   with Pre  =&gt (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read access to an individual element of a set given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Constant_Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the cursors of Container is prohibited while the object returned by Constant_Reference exists and has not been finalized.

```ada
{AI12-0112-1} procedure Assign (Target : in out Set; Source : in Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Source) = Length (Target);

```

{AI05-0001-1} {AI05-0248-1} If Target denotes the same object as Source, the operation has no effect. Otherwise, the elements of Source are copied to Target as for an [assignment_statement](./AA-5.2#S0173) assigning Source to Target. 

Discussion: {AI05-0005-1} This routine exists for compatibility with the bounded set containers. For an unbounded set, Assign(A, B) and A := B behave identically. For a bounded set, := will raise an exception if the container capacities are different, while Assign will not raise an exception if there is enough room in the target. 

```ada
{AI12-0112-1} procedure Move (Target : in out Set;
                Source : in out Set)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                    or else raise Program_Error),
        Post =&gt (if not Target'Has_Same_Storage (Source) then
                    Length (Target) = Length (Source'Old) and then
                    Length (Source) = 0);

```

{AI95-00302-03} {AI05-0001-1} {AI05-0248-1} {AI05-0262-1} If Target denotes the same object as Source, then the operation has no effect. Otherwise, the operation is equivalent to Assign (Target, Source) followed by Clear (Source).

```ada
{AI12-0112-1} procedure Insert (Container : in out Set;
                  New_Item  : in     Element_Type;
                  Position  :    out Cursor;
                  Inserted  :    out Boolean)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Has_Element (Container, Position) and then
                  (if Inserted then
                     Length (Container) = Original_Length + 1
                   else
                     Length (Container) = Original_Length));

```

{AI95-00302-03} Insert checks if an element equivalent to New_Item is already present in Container. If a match is found, Inserted is set to False and Position designates the matching element. Otherwise, Insert adds New_Item to Container; Inserted is set to True and Position designates the newly-inserted element. Any exception raised during allocation is propagated and Container is not modified.

```ada
{AI12-0112-1} procedure Insert (Container : in out Set;
                  New_Item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt Length (Container) = Length (Container)'Old + 1;

```

{AI95-00302-03} Insert inserts New_Item into Container as per the four-parameter Insert, with the difference that if an element equivalent to New_Item is already in the set, then Constraint_Error is propagated.

Discussion: This is equivalent to: 

```ada
declare
  Inserted : Boolean; C : Cursor;
begin
  Insert (Container, New_Item, C, Inserted);
  if not Inserted then
     raise Constraint_Error;
  end if;
end;

```

but doesn't require the hassle of out parameters. 

```ada
{AI12-0112-1} procedure Include (Container : in out Set;
                   New_Item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                    or else raise Constraint_Error),
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Length (Container)
                      in Original_Length | Original_Length + 1);

```

{AI95-00302-03} Include inserts New_Item into Container as per the four-parameter Insert, with the difference that if an element equivalent to New_Item is already in the set, then it is replaced. Any exception raised during assignment is propagated.

```ada
{AI12-0112-1} procedure Replace (Container : in out Set;
                   New_Item  : in     Element_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = Length (Container)'Old;

```

{AI95-00302-03} Replace checks if an element equivalent to New_Item is already in the set. If a match is found, that element is replaced with New_Item; otherwise, Constraint_Error is propagated.

```ada
{AI12-0112-1} procedure Exclude (Container : in out Set;
                   Item       : in     Element_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Length (Container) in
                      Original_Length - 1 | Original_Length);

```

{AI95-00302-03} Exclude checks if an element equivalent to Item is present in Container. If a match is found, Exclude removes the element from the set.

```ada
{AI12-0112-1} procedure Delete (Container : in out Set;
                  Item       : in     Element_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = Length (Container)'Old - 1;

```

{AI95-00302-03} Delete checks if an element equivalent to Item is present in Container. If a match is found, Delete removes the element from the set; otherwise, Constraint_Error is propagated.

```ada
procedure Delete (Container : in out Set;
                  Position  : in out Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error) and then
                (Position /= No_Element 
                    or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Length (Container) = Length (Container)'Old - 1 and then
                Position = No_Element;

```

{AI95-00302-03} {AI12-0112-1} Delete removes the element designated by Position from the set.

Ramification: The check on Position checks that the cursor does not belong to some other set. This check implies that a reference to the set is included in the cursor value. This wording is not meant to require detection of dangling cursors; such cursors are defined to be invalid, which means that execution is erroneous, and any result is allowed (including not raising an exception). 

```ada
{AI12-0112-1} procedure Union (Target : in out Set;
                 Source : in     Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

{AI95-00302-03} Union inserts into Target the elements of Source that are not equivalent to some element already in Target.

Implementation Note: If the objects are the same, the result is the same as the original object. The implementation needs to take care so that aliasing effects do not make the result trash; Union (S, S); must work. 

```ada
{AI12-0112-1} function Union (Left, Right : Set) return Set
   with Post =&gt Length (Union'Result) &lt= 
                   Length (Left) + Length (Right) and then
                not Tampering_With_Cursors_Prohibited (Union'Result);

```

{AI95-00302-03} Returns a set comprising all of the elements of Left, and the elements of Right that are not equivalent to some element of Left.

```ada
{AI12-0112-1} procedure Intersection (Target : in out Set;
                        Source : in     Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

{AI95-00302-03} {AI05-0004-1} Intersection deletes from Target the elements of Target that are not equivalent to some element of Source.

Implementation Note: If the objects are the same, the result is the same as the original object. The implementation needs to take care so that aliasing effects do not make the result trash; Intersection (S, S); must work. 

```ada
{AI12-0112-1} function Intersection (Left, Right : Set) return Set
   with Post =&gt Length (Intersection'Result) &lt= 
                   Length (Left) + Length (Right) and then
                not Tampering_With_Cursors_Prohibited (Intersection'Result);

```

{AI95-00302-03} Returns a set comprising all the elements of Left that are equivalent to the some element of Right.

```ada
{AI12-0112-1} procedure Difference (Target : in out Set;
                      Source : in     Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

{AI95-00302-03} If Target denotes the same object as Source, then Difference clears Target. Otherwise, it deletes from Target the elements that are equivalent to some element of Source.

```ada
{AI12-0112-1} function Difference (Left, Right : Set) return Set
   with Post =&gt Length (Difference'Result) &lt= Length (Left) + 
                   Length (Right) and then
                not Tampering_With_Cursors_Prohibited (Difference'Result);

```

{AI95-00302-03} Returns a set comprising the elements of Left that are not equivalent to some element of Right.

```ada
{AI12-0112-1} procedure Symmetric_Difference (Target : in out Set;
                                Source : in     Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

{AI95-00302-03} If Target denotes the same object as Source, then Symmetric_Difference clears Target. Otherwise, it deletes from Target the elements that are equivalent to some element of Source, and inserts into Target the elements of Source that are not equivalent to some element of Target.

```ada
{AI12-0112-1} function Symmetric_Difference (Left, Right : Set) return Set
   with Post =&gt Length (Symmetric_Difference'Result) &lt= 
                   Length (Left) + Length (Right) and then
                not Tampering_With_Cursors_Prohibited (
                   Symmetric_Difference'Result);

```

{AI95-00302-03} Returns a set comprising the elements of Left that are not equivalent to some element of Right, and the elements of Right that are not equivalent to some element of Left.

```ada
function Overlap (Left, Right : Set) return Boolean;

```

{AI95-00302-03} {AI05-0264-1} If an element of Left is equivalent to some element of Right, then Overlap returns True. Otherwise, it returns False.

Discussion: This operation is commutative. If Overlap returns False, the two sets are disjoint. 

```ada
function Is_Subset (Subset : Set;
                    Of_Set : Set) return Boolean;

```

{AI95-00302-03} {AI05-0264-1} If an element of Subset is not equivalent to some element of Of_Set, then Is_Subset returns False. Otherwise, it returns True.

Discussion: This operation is not commutative, so we use parameter names that make it clear in named notation which set is which. 

```ada
{AI12-0112-1} function First (Container : Set) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container)
                 then Has_Element (Container, First'Result)
                 else First'Result = No_Element);

```

{AI95-00302-03} If Length (Container) = 0, then First returns No_Element. Otherwise, First returns a cursor that designates the first element in Container.

```ada
{AI12-0112-1} function Next (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

{AI95-00302-03} Returns a cursor that designates the successor of the element designated by Position. If Position designates the last element, then No_Element is returned. If Position equals No_Element, then No_Element is returned.

```ada
function Next (Container : Set;
               Position  : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then Next'Result = No_Element
                 elsif Next'Result = No_Element then
                   Position = Last (Container)
                 else Has_Element (Container, Next'Result));

```

{AI12-0112-1} Returns a cursor designating the successor of the node designated by Position in Container.

```ada
{AI12-0112-1} procedure Next (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Next (Position).

```ada
procedure Next (Container : in     Set;
                Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Next (Container, Position).

This paragraph was deleted.{AI95-00302-03} {AI05-0004-1} 

```ada
{AI12-0112-1} function Find (Container : Set;
               Item      : Element_Type) return Cursor
      with Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

{AI95-00302-03} If Length (Container) equals 0, then Find returns No_Element. Otherwise, Find checks if an element equivalent to Item is present in Container. If a match is found, a cursor designating the matching element is returned; otherwise, No_Element is returned.

```ada
function Contains (Container : Set;
                   Item      : Element_Type) return Boolean;

```

{AI05-0004-1} Equivalent to Find (Container, Item) /= No_Element.

Paragraphs 83 and 84 were moved above. 

```ada
{AI12-0112-1} procedure Iterate
  (Container : in Set;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0265-1} Iterate calls Process.all with a cursor that designates each element in Container, starting with the first element and moving the cursor according to the successor relation. Tampering with the cursors of Container is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

Implementation Note: The "tamper with cursors" check takes place when the operations that insert or delete elements, and so on are called.

See Iterate for vectors (A.18.2) for a suggested implementation of the check. 

{AI95-00302-03} Both Containers.Hashed_Set and Containers.Ordered_Set declare a nested generic package Generic_Keys, which provides operations that allow set manipulation in terms of a key (typically, a portion of an element) instead of a complete element. The formal function Key of Generic_Keys extracts a key value from an element. It is expected to return the same value each time it is called with a particular element. The behavior of Generic_Keys is unspecified if Key behaves in some other manner.

{AI95-00302-03} A key is expected to unambiguously determine a single equivalence class for elements. The behavior of Generic_Keys is unspecified if the formal parameters of this package behave in some other manner.

```ada
{AI12-0112-1} function Key (Position : Cursor) return Key_Type
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Key (Element (Position)).

```ada
function Key (Container : Set;
              Position : Cursor) return Key_Type
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error);

```

{AI12-0112-1} Equivalent to Key (Element (Container, Position)).

{AI95-00302-03} The subprograms in package Generic_Keys named Contains, Find, Element, Delete, and Exclude, are equivalent to the corresponding subprograms in the parent package, with the difference that the Key parameter is used to locate an element in the set.

```ada
{AI12-0112-1} procedure Replace (Container : in out Set;
                   Key       : in     Key_Type;
                   New_Item  : in     Element_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Length (Container) = Length (Container)'Old;

```

{AI95-00302-03} Equivalent to Replace_Element (Container, Find (Container, Key), New_Item).

```ada
procedure Update_Element_Preserving_Key
  (Container : in out Set;
   Position  : in     Cursor;
   Process   : not null access procedure
                                 (Element : in out Element_Type))
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI95-00302-03} {AI05-0265-1} {AI12-0112-1} Update_Element_Preserving_Key uses Key to save the key value K of the element designated by Position. Update_Element_Preserving_Key then calls Process.all with that element as the argument. Tampering with the cursors of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated. After Process.all returns, Update_Element_Preserving_Key checks if K determines the same equivalence class as that for the new element; if not, the element is removed from the set and Program_Error is propagated.

Reason: The key check ensures that the invariants of the set are preserved by the modification. The "tampers with the elements" check prevents data loss (if Element_Type is by-copy) or erroneous execution (if element type is unconstrained and indefinite). 

If Element_Type is unconstrained and definite, then the actual Element parameter of Process.all shall be unconstrained.

Ramification: This means that the elements cannot be directly allocated from the heap; it must be possible to change the discriminants of the element in place. 

```ada
{AI12-0112-1} type Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

{AI05-0212-1} The type Reference_Type needs finalization.

This paragraph was deleted.{AI12-0112-1} 

```ada
{AI12-0112-1} function Reference_Preserving_Key (Container : aliased in out Set;
                                   Position  : in Cursor)
   return Reference_Type
   with Pre  =&gt (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Implicit_Dereference aspect) provides a convenient way to gain read and write access to an individual element of a set given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Reference_Preserving_Key uses Key to save the key value K; then returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the cursors of Container is prohibited while the object returned by Reference_Preserving_Key exists and has not been finalized. When the object returned by Reference_Preserving_Key is finalized, a check is made if K determines the same equivalence class as that for the new element; if not, the element is removed from the set and Program_Error is propagated.

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in Set;
                             Key       : in Key_Type)
   return Constant_Reference_Type
   with Pre  =&gt Find (Container, Key) /= No_Element 
                   or else raise Constraint_Error,
        Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Implicit_Dereference aspect) provides a convenient way to gain read access to an individual element of a set given a key value.

Equivalent to Constant_Reference (Container, Find (Container, Key)).

```ada
{AI12-0112-1} function Reference_Preserving_Key (Container : aliased in out Set;
                                   Key       : in Key_Type)
   return Reference_Type
   with Pre  =&gt Find (Container, Key) /= No_Element 
                   or else raise Constraint_Error,
        Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Implicit_Dereference aspect) provides a convenient way to gain read and write access to an individual element of a set given a key value.

Equivalent to Reference_Preserving_Key (Container, Find (Container, Key)). 

{AI12-0111-1} The nested package Stable provides a type Stable.Set that represents a stable set, which is one that cannot grow and shrink. Such a set can be created by calling the Copy function, or by establishing a stabilized view of an ordinary set.

{AI12-0111-1} The subprograms of the set package that have a parameter or result of type Set are included in the nested package Stable with the same specification, except that the following are omitted:

Tampering_With_Cursors_Prohibited, Assign, Move, Insert, Include, Clear, Delete, Exclude, Replace, Replace_Element, procedures Union, Intersection, Difference, and Symmetric_Difference, (for Ordered_sets) Delete_First and Delete_Last, and (for Hashed_sets) Reserve_Capacity 

Discussion: The Generic_Keys package is not included in the Stable package. The functions Union, Intersection, Difference, and Symmetric_Difference are included in the Stable package. 

Ramification: The names Set and Cursor mean the types declared in the nested package in these subprogram specifications. 

Reason: The omitted routines are those that tamper with cursors (or test that state). The model is that it is impossible to tamper with cursors of a stable view since no such operations are included. Thus tampering checks are not needed for a stable view, and we omit the operations associated with those checks. 

{AI12-0111-1} The operations of this package are equivalent to those for ordinary sets, except that the calls to Tampering_With_Cursors_Prohibited that occur in preconditions are replaced by False, and any that occur in postconditions are replaced by True.

{AI12-0111-1} {AI12-0439-1} If a stable set is declared with the Base discriminant designating a pre-existing ordinary set, the stable set represents a stabilized view of the underlying ordinary set, and any operation on the stable set is reflected on the underlying ordinary set. While a stabilized view exists, any operation that tampers with cursors performed on the underlying set is prohibited. The finalization of a stable set that provides such a view removes this restriction on the underlying ordinary set [(though some other restriction can exist due to other concurrent iterations or stabilized views)].

{AI12-0111-1} {AI12-0438-1} If a stable set is declared without specifying Base, the object is necessarily initialized. The initializing expression of the stable set, [typically a call on Copy], determines the Length of the set. The Length of a stable set never changes after initialization.

Proof: {AI12-0438-1} Initialization is required as the type is indefinite, see 3.3.1. 


#### Bounded (Run-Time) Errors

{AI05-0022-1} {AI05-0248-1} It is a bounded error for the actual function associated with a generic formal subprogram, when called as part of an operation of a set package, to tamper with elements of any set parameter of the operation. Either Program_Error is raised, or the operation works as defined on the value of the set either prior to, or subsequent to, some or all of the modifications to the set.

{AI05-0027-1} It is a bounded error to call any subprogram declared in the visible part of a set package when the associated container has been finalized. If the operation takes Container as an in out parameter, then it raises Constraint_Error or Program_Error. Otherwise, the operation either proceeds as it would for an empty container, or it raises Constraint_Error or Program_Error. 


#### Erroneous Execution

{AI95-00302-03} A Cursor value is invalid if any of the following have occurred since it was created: 

The set that contains the element it designates has been finalized;

{AI05-0160-1} The set that contains the element it designates has been used as the Target of a call to Assign, or as the target of an [assignment_statement](./AA-5.2#S0173);

The set that contains the element it designates has been used as the Source or Target of a call to Move; or

{AI05-0160-1} {AI05-0262-1} The element it designates has been removed from the set that previously contained the element. 

Ramification: {AI05-0160-1} This can happen directly via calls to Clear, Exclude, Delete, and Update_Element_Preserving_Key, and indirectly via calls to procedures Intersection, Difference, and Symmetric_Difference. 

{AI95-00302-03} The result of "=" or Has_Element is unspecified if these functions are called with an invalid cursor parameter. Execution is erroneous if any other subprogram declared in Containers.Hashed_Sets or Containers.Ordered_Sets is called with an invalid cursor parameter.

Discussion: The list above is intended to be exhaustive. In other cases, a cursor value continues to designate its original element. For instance, cursor values survive the insertion and deletion of other elements.

While it is possible to check for these cases, in many cases the overhead necessary to make the check is substantial in time or space. Implementations are encouraged to check for as many of these cases as possible and raise Program_Error if detected. 

{AI05-0212-1} Execution is erroneous if the set associated with the result of a call to Reference or Constant_Reference is finalized before the result object returned by the call to Reference or Constant_Reference is finalized. 

Reason: Each object of Reference_Type and Constant_Reference_Type probably contains some reference to the originating container. If that container is prematurely finalized (which is only possible via Unchecked_Deallocation, as accessibility checks prevent passing a container to Reference that will not live as long as the result), the finalization of the object of Reference_Type will try to access a nonexistent object. This is a normal case of a dangling pointer created by Unchecked_Deallocation; we have to explicitly mention it here as the pointer in question is not visible in the specification of the type. (This is the same reason we have to say this for invalid cursors.) 


#### Implementation Requirements

{AI95-00302-03} {AI12-0437-1} No storage associated with a set object shall be lost upon assignment or scope exit.

{AI95-00302-03} {AI05-0262-1} The execution of an [assignment_statement](./AA-5.2#S0173) for a set shall have the effect of copying the elements from the source set object to the target set object and changing the length of the target object to that of the source object.

Implementation Note: {AI05-0298-1} An assignment of a Set is a "deep" copy; that is the elements are copied as well as the data structures. We say "effect of" in order to allow the implementation to avoid copying elements immediately if it wishes. For instance, an implementation that avoided copying until one of the containers is modified would be allowed. (Note that this implementation would require care, see A.18.2 for more.) 


#### Implementation Advice

{AI95-00302-03} Move should not copy elements, and should minimize copying of internal data structures. 

Implementation Advice: Move for sets should not copy elements, and should minimize copying of internal data structures.

Implementation Note: Usually that can be accomplished simply by moving the pointer(s) to the internal data structures from the Source container to the Target container. 

{AI95-00302-03} If an exception is propagated from a set operation, no storage should be lost, nor any elements removed from a set unless specified by the operation. 

Implementation Advice: If an exception is propagated from a set operation, no storage should be lost, nor any elements removed from a set unless specified by the operation.

Reason: This is important so that programs can recover from errors. But we don't want to require heroic efforts, so we just require documentation of cases where this can't be accomplished. 


#### Wording Changes from Ada 95

{AI95-00302-03} This description of sets is new; the extensions are documented with the specific packages. 


#### Extensions to Ada 2005

{AI05-0212-1} Added reference support to make set containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0001-1} Added procedure Assign; the extension and incompatibility is documented with the specific packages.

{AI05-0001-1} Generalized the definition of Move. Specified which elements are read/written by stream attributes.

{AI05-0022-1} Correction: Added a Bounded (Run-Time) Error to cover tampering by generic actual subprograms.

{AI05-0027-1} Correction: Added a Bounded (Run-Time) Error to cover access to finalized set containers.

{AI05-0160-1} Correction: Revised the definition of invalid cursors to cover missing (and new) cases.

{AI05-0265-1} Correction: Defined when a container prohibits tampering in order to more clearly define where the check is made and the exception raised. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Procedures Union, Intersection, Difference, and Symmeteric_Difference are now defined to tamper with the cursors of the Target parameter. A program which attempts to use one of these operations while tampering is prohibited will raise Program_Error. However, since the operations do modify the container, the effects would have been unpredictable, so this change will likely fix bugs. 


#### Extensions to Ada 2012

{AI12-0196-1} Correction: Replace_Element is now defined such that it can be used concurrently so long as it operates on different elements. This allows some container operations to be used in parallel without separate synchronization. 


#### Wording Changes from Ada 2012

{AI12-0110-1} Corrigendum: Clarified that tampering checks precede all other checks made by a subprogram (but come after those associated with the call).

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.8  The Generic Package Containers.Hashed_Sets


#### Static Semantics

{AI95-00302-03} The generic library package Containers.Hashed_Sets has the following declaration: 

```ada
{AI05-0084-1} {AI05-0212-1} {AI12-0112-1} with Ada.Iterator_Interfaces;
generic
   type Element_Type is private;
   with function Hash (Element : Element_Type) return Hash_Type;
   with function Equivalent_Elements (Left, Right : Element_Type)
                 return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is &lt&gt;
package Ada.Containers.Hashed_Sets
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects in this generic package, see the notes on the equivalent operations in the specification of the Containers.Vectors package (see A.18.2). 

```ada
{AI05-0212-1} {AI12-0111-1} {AI12-0112-1} {AI12-0212-1} {AI12-0339-1} {AI12-0399-1}    type Set is tagged private
      with Constant_Indexing =&gt Constant_Reference,
           Default_Iterator  =&gt Iterate,
           Iterator_Element  =&gt Element_Type,
           Iterator_View     =&gt Stable.Set,
           Aggregate         =&gt (Empty       =&gt Empty,
                                 Add_Unnamed =&gt Include),
           Stable_Properties =&gt (Length,
                                 Tampering_With_Cursors_Prohibited),
           Default_Initial_Condition =&gt
              Length (Set) = 0 and then
              (not Tampering_With_Cursors_Prohibited (Set)),
           Preelaborable_Initialization;

```

Discussion: {AI12-0112-1} Unlike a Vector, the Stable_Properties of a Hashed_Set do not include the Capacity. If we had included it, some of the otherwise shared definitions would need different postconditions for Hashed_Sets and Ordered_Sets. If we were starting these containers from scratch, we probably would have approached the sharing of definitions differently so that we could avoid issues like this, but a major reorganization of this existing material would be too much change. 

```ada
{AI12-0399-1}    type Cursor is private
      with Preelaborable_Initialization;

```

```ada
   Empty_Set : constant Set;

```

```ada
   No_Element : constant Cursor;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Has_Element (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Has_Element (Container : Set; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1}    package Set_Iterator_Interfaces is new
       Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
   function "=" (Left, Right : Set) return Boolean;

```

```ada
   function Equivalent_Sets (Left, Right : Set) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_Cursors_Prohibited
      (Container : Set) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty (Capacity : Count_Type := implementation-defined)
      return Set
      with Post =&gt
              Capacity (Empty'Result) &gt= Capacity and then
              not Tampering_With_Cursors_Prohibited (Empty'Result) and then
              Length (Empty'Result) = 0;

```

```ada
{AI12-0112-1}    function To_Set (New_Item : Element_Type) return Set
      with Post =&gt Length (To_Set'Result) = 1 and then
                 not Tampering_with_Cursors_Prohibited (To_Set'Result);

```

```ada
{AI12-0112-1}    function Capacity (Container : Set) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Reserve_Capacity (Container : in out Set;
                               Capacity  : in     Count_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Container.Capacity &gt= Capacity;

```

```ada
{AI12-0112-1}    function Length (Container : Set) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Empty (Container : Set) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

```ada
{AI12-0112-1}    procedure Clear (Container : in out Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Capacity (Container) = Capacity (Container)'Old and then
                   Length (Container) = 0;

```

```ada
{AI12-0112-1}    function Element (Position : Cursor) return Element_Type
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    function Element (Container : Set;
                     Position  : Cursor) return Element_Type
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Set;
                              Position  : in     Cursor;
                              New_item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Element : in Element_Type))
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Set;
      Position  : in Cursor;
      Process   : not null access procedure (Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Set;
                                Position  : in Cursor)
      return Constant_Reference_Type
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0001-1} {AI12-0112-1}    procedure Assign (Target : in out Set; Source : in Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target) and then
                   Capacity (Target) &gt= Length (Source);

```

```ada
{AI05-0001-1} {AI12-0112-1}    function Copy (Source : Set; Capacity : Count_Type := 0)
      return Set
      with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                      or else raise Capacity_Error,
           Post =&gt
              Length (Copy'Result) = Length (Source) and then
              not Tampering_With_Cursors_Prohibited (Copy'Result) and then
              Copy'Result.Capacity = (if Capacity = 0 then
                 Length (Source) else Capacity);

```

```ada
{AI12-0112-1}    procedure Move (Target : in out Set;
                   Source : in out Set)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                      or else raise Program_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                      Length (Target) = Length (Source'Old) and then
                      Length (Source) = 0);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Set;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Has_Element (Container, Position) and then
                     (if Inserted then
                        Length (Container) = Original_Length + 1
                      else
                        Length (Container) = Original_Length)) and then
                    Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Set;
                     New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt Length (Container) = Length (Container)'Old + 1 and then
                   Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Include (Container : in out Set;
                      New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container)
                         in Original_Length | Original_Length + 1) and then
                    Capacity (Container) &gt= Length (Container);

```

```ada
{AI12-0112-1}    procedure Replace (Container : in out Set;
                      New_Item  : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old;

```

```ada
{AI12-0112-1}    procedure Exclude (Container : in out Set;
                      Item      : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container) in
                         Original_Length - 1 | Original_Length);

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Set;
                     Item      : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old - 1;

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Set;
                     Position  : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Length (Container) = Length (Container)'Old - 1 and then
                   Position = No_Element;

```

```ada
{AI12-0112-1}    procedure Union (Target : in out Set;
                    Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Union (Left, Right : Set) return Set
      with Post =&gt Length (Union'Result) &lt= 
                      Length (Left) + Length (Right) and then
                   not Tampering_With_Cursors_Prohibited (Union'Result);

```

```ada
   function "or" (Left, Right : Set) return Set renames Union;

```

```ada
{AI12-0112-1}    procedure Intersection (Target : in out Set;
                           Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Intersection (Left, Right : Set) return Set
      with Post =&gt 
              Length (Intersection'Result) &lt= 
                 Length (Left) + Length (Right) and then
              not Tampering_With_Cursors_Prohibited (Intersection'Result);

```

```ada
   function "and" (Left, Right : Set) return Set renames Intersection;

```

```ada
{AI12-0112-1}    procedure Difference (Target : in out Set;
                         Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Difference (Left, Right : Set) return Set
      with Post =&gt 
              Length (Difference'Result) &lt= 
                 Length (Left) + Length (Right) and then
              not Tampering_With_Cursors_Prohibited (Difference'Result);

```

```ada
   function "-" (Left, Right : Set) return Set renames Difference;

```

```ada
{AI12-0112-1}    procedure Symmetric_Difference (Target : in out Set;
                                   Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Symmetric_Difference (Left, Right : Set) return Set
      with Post =&gt 
              Length (Symmetric_Difference'Result) &lt= 
                 Length (Left) + Length (Right) and then
              not Tampering_With_Cursors_Prohibited (
                 Symmetric_Difference'Result);

```

```ada
   function "xor" (Left, Right : Set) return Set
     renames Symmetric_Difference;

```

```ada
   function Overlap (Left, Right : Set) return Boolean;

```

```ada
   function Is_Subset (Subset : Set;
                       Of_Set : Set) return Boolean;

```

```ada
{AI12-0112-1}    function First (Container : Set) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, First'Result)
                    else First'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Container : Set;
                  Position : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then Next'Result = No_Element
                    elsif Next'Result = No_Element then
                      Position = Last (Container)
                    else Has_Element (Container, Next'Result));

```

```ada
{AI12-0112-1}    procedure Next (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Next (Container : in     Set;
                   Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Find (Container : Set;
                  Item      : Element_Type)
      return Cursor
      with Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

```ada
   function Contains (Container : Set;
                      Item      : Element_Type) return Boolean;

```

```ada
This paragraph was deleted.{AI05-0212-1} 

```

```ada
{AI12-0112-1}    function Equivalent_Elements (Left, Right : Cursor)
      return Boolean
      with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                         or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function Equivalent_Elements (Left  : Cursor;
                                 Right : Element_Type)
      return Boolean
      with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function Equivalent_Elements (Left  : Element_Type;
                                 Right : Cursor)
      return Boolean
      with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Iterate
     (Container : in Set;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate (Container : in Set)
      return Set_Iterator_Interfaces.Parallel_Iterator'Class
      with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0112-1}    generic
      type Key_Type (&lt&gt) is private;
      with function Key (Element : Element_Type) return Key_Type;
      with function Hash (Key : Key_Type) return Hash_Type;
      with function Equivalent_Keys (Left, Right : Key_Type)
                                     return Boolean;
   package Generic_Keys
   with Nonblocking, Global =&gt null is

```

```ada
{AI12-0112-1}       function Key (Position : Cursor) return Key_Type
         with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
              Global =&gt in all;

```

```ada
{AI12-0112-1}       function Key (Container : Set;
                    Position : Cursor) return Key_Type
         with Pre  =&gt (Position = No_Element
                         or else raise Constraint_Error) and then
                      (Has_Element (Container, Position)
                         or else raise Program_Error);

```

```ada
      function Element (Container : Set;
                        Key       : Key_Type)
        return Element_Type;

```

```ada
{AI12-0112-1}       procedure Replace (Container : in out Set;
                         Key       : in     Key_Type;
                         New_Item  : in     Element_Type)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                          or else raise Program_Error,
              Post =&gt Length (Container) = Length (Container)'Old;

```

```ada
{AI12-0112-1}       procedure Exclude (Container : in out Set;
                         Key       : in     Key_Type)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                          or else raise Program_Error,
              Post =&gt (declare
                         Original_Length : constant Count_Type :=
                            Length (Container)'Old;
                       begin
                         Length (Container)
                            in Original_Length - 1 | Original_Length);

```

```ada
{AI12-0112-1}       procedure Delete (Container : in out Set;
                        Key       : in     Key_Type)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                          or else raise Program_Error,
              Post =&gt Length (Container) = Length (Container)'Old - 1;

```

```ada
{AI12-0112-1}       function Find (Container : Set;
                     Key       : Key_Type)
         return Cursor
         with Post =&gt (if Find'Result = No_Element
                       then Has_Element (Container, Find'Result));

```

```ada
      function Contains (Container : Set;
                         Key       : Key_Type)
         return Boolean;

```

```ada
{AI12-0112-1}       procedure Update_Element_Preserving_Key
        (Container : in out Set;
         Position  : in     Cursor;
         Process   : not null access procedure
                         (Element : in out Element_Type))
         with Pre  =&gt (Position /= No_Element or else
                         raise Constraint_Error) and then
                      (Has_Element (Container, Position) or else
                         raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}       type Reference_Type
            (Element : not null access Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt in out synchronized,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}       function Reference_Preserving_Key (Container : aliased in out Set;
                                         Position  : in Cursor)
         return Reference_Type
         with Pre  =&gt (Position /= No_Element
                          or else raise Constraint_Error) and then
                      (Has_Element (Container, Position)
                          or else raise Program_Error),
              Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI05-0212-1} {AI12-0112-1}       function Constant_Reference (Container : aliased in Set;
                                   Key       : in Key_Type)
         return Constant_Reference_Type
         with Pre  =&gt Find (Container, Key) /= No_Element
                         or else raise Constraint_Error,
              Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI05-0212-1} {AI12-0112-1}       function Reference_Preserving_Key (Container : aliased in out Set;
                                         Key       : in Key_Type)
         return Reference_Type
         with Pre  =&gt Find (Container, Key) /= No_Element
                         or else raise Constraint_Error,
              Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
   end Generic_Keys;

```

```ada
{AI12-0111-1}    package Stable is

```

```ada
{AI12-0111-1} {AI12-0339-1} {AI12-0399-1} {AI12-0407-1}       type Set (Base : not null access Hashed_Sets.Set) is
         tagged limited private
         with Constant_Indexing =&gt Constant_Reference,
              Default_Iterator  =&gt Iterate,
              Iterator_Element  =&gt Element_Type,
              Stable_Properties =&gt (Length),
              Global            =&gt null,
              Default_Initial_Condition =&gt Length (Set) = 0,
              Preelaborable_Initialization;

```

```ada
{AI12-0111-1} {AI12-0399-1}       type Cursor is private
         with Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       Empty_Set : constant Set;

```

```ada
{AI12-0111-1}       No_Element : constant Cursor;

```

```ada
{AI12-0111-1}       function Has_Element (Position : Cursor) return Boolean
         with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0111-1}       package Set_Iterator_Interfaces is new
         Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
{AI12-0111-1}       procedure Assign (Target : in out Hashed_Sets.Set;
                        Source : in Set)
         with Post =&gt Length (Source) = Length (Target);

```

```ada
{AI12-0111-1}       function Copy (Source : Hashed_Sets.Set) return Set
         with Post =&gt Length (Copy'Result) = Length (Source);

```

```ada
{AI12-0111-1}       type Constant_Reference_Type
            (Element : not null access constant Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       -- Additional subprograms as described in the text
      -- are declared here.

```

```ada
{AI12-0111-1}    private

```

```ada
{AI12-0111-1}       ... -- not specified by the language

```

```ada
{AI12-0111-1}    end Stable;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Hashed_Sets;

```

{AI95-00302-03} An object of type Set contains an expandable hash table, which is used to provide direct access to elements. The capacity of an object of type Set is the maximum number of elements that can be inserted into the hash table prior to it being automatically expanded.

{AI95-00302-03} Two elements E1 and E2 are defined to be equivalent if Equivalent_Elements (E1, E2) returns True.

{AI95-00302-03} The actual function for the generic formal function Hash is expected to return the same value each time it is called with a particular element value. For any two equivalent elements, the actual for Hash is expected to return the same value. If the actual for Hash behaves in some other manner, the behavior of this package is unspecified. Which subprograms of this package call Hash, and how many times they call it, is unspecified.

{AI95-00302-03} The actual function for the generic formal function Equivalent_Elements is expected to return the same value each time it is called with a particular pair of Element values. It should define an equivalence relationship, that is, be reflexive, symmetric, and transitive. If the actual for Equivalent_Elements behaves in some other manner, the behavior of this package is unspecified. Which subprograms of this package call Equivalent_Elements, and how many times they call it, is unspecified.

{AI05-0044-1} If the actual function for the generic formal function "=" returns True for any pair of nonequivalent elements, then the behavior of the container function "=" is unspecified.

{AI95-00302-03} If the value of an element stored in a set is changed other than by an operation in this package such that at least one of Hash or Equivalent_Elements give different results, the behavior of this package is unspecified.

Discussion: See A.18.5, "The Generic Package Containers.Hashed_Maps" for a suggested implementation, and for justification of the restrictions regarding Hash and Equivalent_Elements. Note that sets only need to store elements, not key/element pairs. 

{AI95-00302-03} Which elements are the first element and the last element of a set, and which element is the successor of a given element, are unspecified, other than the general semantics described in A.18.7.

```ada
function Empty (Capacity : Count_Type := implementation-defined)
   return Set
   with Post =&gt
           Capacity (Empty'Result) &gt= Capacity and then
           not Tampering_With_Cursors_Prohibited (Empty'Result) and then
           Length (Empty'Result) = 0;

```

{AI12-0339-1} Returns an empty set.

```ada
{AI12-0112-1} function Capacity (Container : Set) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI95-00302-03} Returns the capacity of Container.

```ada
{AI12-0112-1} procedure Reserve_Capacity (Container : in out Set;
                            Capacity  : in     Count_Type)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt Container.Capacity &gt= Capacity;

```

{AI95-00302-03} Reserve_Capacity allocates a new hash table such that the length of the resulting set can become at least the value Capacity without requiring an additional call to Reserve_Capacity, and is large enough to hold the current length of Container. Reserve_Capacity then rehashes the elements in Container onto the new hash table. It replaces the old hash table with the new hash table, and then deallocates the old hash table. Any exception raised during allocation is propagated and Container is not modified.

This paragraph was deleted.{AI12-0112-1} 

Reason: Reserve_Capacity tampers with the cursors, as rehashing probably will change the relationships of the elements in Container. 

```ada
{AI12-0112-1} procedure Clear (Container : in out Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt Capacity (Container) = Capacity (Container)'Old and then
                Length (Container) = 0;

```

{AI95-00302-03} In addition to the semantics described in A.18.7, Clear does not affect the capacity of Container.

```ada
{AI12-0112-1} procedure Assign (Target : in out Set; Source : in Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target) and then
                   Capacity (Target) &gt= Length (Source);

```

{AI05-0001-1} {AI05-0248-1} In addition to the semantics described in A.18.7, if the length of Source is greater than the capacity of Target, Reserve_Capacity (Target, Length (Source)) is called before assigning any elements.

```ada
function Copy (Source : Set; Capacity : Count_Type := 0)
   return Set
   with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                   or else raise Capacity_Error,
        Post =&gt
           Length (Copy'Result) = Length (Source) and then
           not Tampering_With_Cursors_Prohibited (Copy'Result) and then
           Copy'Result.Capacity = (if Capacity = 0 then
              Length (Source) else Capacity);

```

{AI05-0001-1} {AI12-0112-1} Returns a set whose elements are initialized from the elements of Source.

```ada
{AI12-0112-1} procedure Insert (Container : in out Set;
                  New_Item  : in     Element_Type;
                  Position  :    out Cursor;
                  Inserted  :    out Boolean)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Length (Container) &lt= Count_Type'Last - 1
                   or else raise Constraint_Error),
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   Has_Element (Container, Position) and then
                  (if Inserted then
                     Length (Container) = Original_Length + 1
                   else
                     Length (Container) = Original_Length)) and then
                 Capacity (Container) &gt= Length (Container);

```

{AI95-00302-03} In addition to the semantics described in A.18.7, if Length (Container) equals Capacity (Container), then Insert first calls Reserve_Capacity to increase the capacity of Container to some larger value.

```ada
function First (Container : Set) return Cursor;

```

{AI95-00302-03} If Length (Container) = 0, then First returns No_Element. Otherwise, First returns a cursor that designates the first hashed element in Container.

```ada
{AI12-0112-1} function Equivalent_Elements (Left, Right : Cursor)
   return Boolean
   with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                     or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Equivalent_Elements (Element (Left), Element (Right)).

```ada
{AI12-0112-1} function Equivalent_Elements (Left  : Cursor;
                              Right : Element_Type) return Boolean
   with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Equivalent_Elements (Element (Left), Right).

```ada
{AI12-0112-1} function Equivalent_Elements (Left  : Element_Type;
                              Right : Cursor) return Boolean
   with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Equivalent_Elements (Left, Element (Right)).

```ada
{AI12-0112-1} {AI12-0266-1} function Iterate (Container : in Set)
   return Set_Iterator_Interfaces.Parallel_Iterator'Class
   with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0266-1} Iterate returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each element in Container, starting with the first element and moving the cursor according to the successor relation when used as a forward iterator, and processing all nodes concurrently when used as a parallel iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

{AI95-00302-03} For any element E, the actual function for the generic formal function Generic_Keys.Hash is expected to be such that Hash (E) = Generic_Keys.Hash (Key (E)). If the actuals for Key or Generic_Keys.Hash behave in some other manner, the behavior of Generic_Keys is unspecified. Which subprograms of Generic_Keys call Generic_Keys.Hash, and how many times they call it, is unspecified.

{AI95-00302-03} For any two elements E1 and E2, the boolean values Equivalent_Elements (E1, E2) and Equivalent_Keys (Key (E1), Key (E2)) are expected to be equal. If the actuals for Key or Equivalent_Keys behave in some other manner, the behavior of Generic_Keys is unspecified. Which subprograms of Generic_Keys call Equivalent_Keys, and how many times they call it, is unspecified.


#### Implementation Advice

{AI95-00302-03} If N is the length of a set, the average time complexity of the subprograms Insert, Include, Replace, Delete, Exclude, and Find that take an element parameter should be O(log N). The average time complexity of the subprograms that take a cursor parameter should be O(1). The average time complexity of Reserve_Capacity should be O(N). 

Implementation Advice: The average time complexity of the Insert, Include, Replace, Delete, Exclude, and Find operations of Containers.Hashed_Sets that take an element parameter should be O(log N). The average time complexity of the subprograms of Containers.Hashed_Sets that take a cursor parameter should be O(1). The average time complexity of Containers.Hashed_Sets.Reserve_Capacity should be O(N).

Implementation Note: {AI95-00302-03} See A.18.5, "The Generic Package Containers.Hashed_Maps" for implementation notes regarding some of the operations of this package. 


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Hashed_Sets is new. 


#### Incompatibilities With Ada 2005

{AI05-0001-1} Subprograms Assign and Copy are added to Containers.Hashed_Sets. If an instance of Containers.Hashed_Sets is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Containers.Hashed_Sets is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 2005

{AI05-0212-1} Added iterator and indexing support to make hashed set containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0044-1} Correction: Added wording to require the formal function be such that equal elements are also equivalent.

{AI05-0084-1} Correction: Added a pragma Remote_Types so that containers can be used in distributed programs. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} {AI12-0339-1} A number of new subprograms, types, and even a nested package were added to Containers.Hashed_Sets to better support contracts and stable views. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0212-1} Sets now support positional container aggregates, so [aggregate](./AA-4.3#S0106) syntax can be used to create Sets.

{AI12-0266-1} The iterator for the container now can return a parallel iterator which can be used to process the container in parallel. 


#### Wording Changes from Ada 2012

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.9  The Generic Package Containers.Ordered_Sets


#### Static Semantics

{AI95-00302-03} The generic library package Containers.Ordered_Sets has the following declaration: 

```ada
{AI05-0084-1} {AI05-0212-1} {AI12-0112-1} with Ada.Iterator_Interfaces;
generic
   type Element_Type is private;
   with function "&lt" (Left, Right : Element_Type) return Boolean is &lt&gt;
   with function "=" (Left, Right : Element_Type) return Boolean is &lt&gt;
package Ada.Containers.Ordered_Sets
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects in this generic package, see the notes on the equivalent operations in the specification of the Containers.Vectors package (see A.18.2). 

```ada
   function Equivalent_Elements (Left, Right : Element_Type) return Boolean;

```

```ada
{AI05-0212-1} {AI12-0111-1} {AI12-0112-1} {AI12-0212-1} {AI12-0339-1} {AI12-0399-1}    type Set is tagged private
      with Constant_Indexing =&gt Constant_Reference,
           Default_Iterator  =&gt Iterate,
           Iterator_Element  =&gt Element_Type,
           Iterator_View     =&gt Stable.Set,
           Aggregate         =&gt (Empty       =&gt Empty,
                                 Add_Unnamed =&gt Include),
           Stable_Properties =&gt (Length,
                                 Tampering_With_Cursors_Prohibited),
           Default_Initial_Condition =&gt
              Length (Set) = 0 and then
              (not Tampering_With_Cursors_Prohibited (Set)),
           Preelaborable_Initialization;

```

```ada
{AI12-0399-1}    type Cursor is private
      with Preelaborable_Initialization;

```

```ada
   Empty_Set : constant Set;

```

```ada
   No_Element : constant Cursor;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Has_Element (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Has_Element (Container : Set; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1}    package Set_Iterator_Interfaces is new
       Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
   function "=" (Left, Right : Set) return Boolean;

```

```ada
   function Equivalent_Sets (Left, Right : Set) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_Cursors_Prohibited
      (Container : Set) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty return Set
      is (Empty_Set)
      with Post =&gt
              not Tampering_With_Cursors_Prohibited (Empty'Result) and then
              Length (Empty'Result) = 0;

```

```ada
{AI12-0112-1}    function To_Set (New_Item : Element_Type) return Set
      with Post =&gt Length (To_Set'Result) = 1 and then
                 not Tampering_with_Cursors_Prohibited (To_Set'Result);

```

```ada
{AI12-0112-1}    function Length (Container : Set) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Empty (Container : Set) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Is_Empty'Result = (Length (Container) = 0);

```

```ada
{AI12-0112-1}    procedure Clear (Container : in out Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Length (Container) = 0;

```

```ada
{AI12-0112-1}    function Element (Position : Cursor) return Element_Type
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    function Element (Container : Set;
                     Position  : Cursor) return Element_Type
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Set;
                              Position  : in     Cursor;
                              New_item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Element : in Element_Type))
      with Pre  =&gt Position /= No_Element
                      or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Set;
      Position  : in Cursor;
      Process   : not null access procedure (Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Set;
                                Position  : in Cursor)
      return Constant_Reference_Type
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0001-1} {AI12-0112-1}    procedure Assign (Target : in out Set; Source : in Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Source) = Length (Target);

```

```ada
{AI05-0001-1} {AI12-0112-1}    function Copy (Source : Set) return Set
      with Post =&gt Length (Copy'Result) = Length (Source) and then
                   not Tampering_With_Cursors_Prohibited (Copy'Result);

```

```ada
{AI12-0112-1}    procedure Move (Target : in out Set;
                   Source : in out Set)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                      or else raise Program_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                      Length (Target) = Length (Source'Old) and then
                      Length (Source) = 0);

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Set;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Inserted  :    out Boolean)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Has_Element (Container, Position) and then
                     (if Inserted then
                        Length (Container) = Original_Length + 1
                      else
                        Length (Container) = Original_Length));

```

```ada
{AI12-0112-1}    procedure Insert (Container : in out Set;
                     New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt Length (Container) = Length (Container)'Old + 1;

```

```ada
{AI12-0112-1}    procedure Include (Container : in out Set;
                      New_Item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Length (Container) &lt= Count_Type'Last - 1
                      or else raise Constraint_Error),
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container)
                         in Original_Length | Original_Length + 1);

```

```ada
{AI12-0112-1}    procedure Replace (Container : in out Set;
                      New_Item  : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old;

```

```ada
{AI12-0112-1}    procedure Exclude (Container : in out Set;
                      Item      : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      Length (Container)
                         in Original_Length - 1 | Original_Length);

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Set;
                     Item      : in     Element_Type)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt Length (Container) = Length (Container)'Old - 1;

```

```ada
{AI12-0112-1}    procedure Delete (Container : in out Set;
                     Position  : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Length (Container) = Length (Container)'Old - 1 and then
                   Position = No_Element;

```

```ada
{AI12-0112-1}    procedure Delete_First (Container : in out Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      (if Original_Length = 0 then Length (Container) = 0
                       else Length (Container) = Original_Length - 1));

```

```ada
{AI12-0112-1}    procedure Delete_Last (Container : in out Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error,
           Post =&gt (declare
                      Original_Length : constant Count_Type :=
                         Length (Container)'Old;
                    begin
                      (if Original_Length = 0 then Length (Container) = 0
                       else Length (Container) = Original_Length - 1));

```

```ada
{AI12-0112-1}    procedure Union (Target : in out Set;
                    Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Union (Left, Right : Set) return Set
      with Post =&gt Length (Union'Result) &lt= 
                      Length (Left) + Length (Right) and then
                   not Tampering_With_Cursors_Prohibited (Union'Result);

```

```ada
   function "or" (Left, Right : Set) return Set renames Union;

```

```ada
{AI12-0112-1}    procedure Intersection (Target : in out Set;
                           Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Intersection (Left, Right : Set) return Set
      with Post =&gt 
              Length (Intersection'Result) &lt= 
                 Length (Left) + Length (Right) and then
              not Tampering_With_Cursors_Prohibited (Intersection'Result);

```

```ada
   function "and" (Left, Right : Set) return Set renames Intersection;

```

```ada
{AI12-0112-1}    procedure Difference (Target : in out Set;
                         Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Difference (Left, Right : Set) return Set
      with Post =&gt 
              Length (Difference'Result) &lt= 
                 Length (Left) + Length (Right) and then
              not Tampering_With_Cursors_Prohibited (Difference'Result);

```

```ada
   function "-" (Left, Right : Set) return Set renames Difference;

```

```ada
{AI12-0112-1}    procedure Symmetric_Difference (Target : in out Set;
                                   Source : in     Set)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Length (Target) &lt= Length (Target)'Old + Length (Source);

```

```ada
{AI12-0112-1}    function Symmetric_Difference (Left, Right : Set) return Set
      with Post =&gt 
              Length (Symmetric_Difference'Result) &lt= 
                 Length (Left) + Length (Right) and then
              not Tampering_With_Cursors_Prohibited (
                 Symmetric_Difference'Result);

```

```ada
   function "xor" (Left, Right : Set) return Set renames
      Symmetric_Difference;

```

```ada
   function Overlap (Left, Right : Set) return Boolean;

```

```ada
   function Is_Subset (Subset : Set;
                       Of_Set : Set) return Boolean;

```

```ada
{AI12-0112-1}    function First (Container : Set) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container)
                    then Has_Element (Container, First'Result)
                    else First'Result = No_Element);

```

```ada
{AI12-0112-1}    function First_Element (Container : Set)
      return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Last (Container : Set) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt (if not Is_Empty (Container) then
                       Has_Element (Container, Last'Result)
                    else Last'Result = No_Element);

```

```ada
{AI12-0112-1}    function Last_Element (Container : Set)
      return Element_Type
      with Pre =&gt (not Is_Empty (Container) 
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Next (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then Next'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next (Container : Set;
                  Position : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then Next'Result = No_Element
                    elsif Next'Result = No_Element then
                       Position = Last (Container)
                    else Has_Element (Container, Next'Result));

```

```ada
{AI12-0112-1}    procedure Next (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Next (Container : in     Set;
                   Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Previous (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element then
                       Previous'Result = No_Element);

```

```ada
{AI12-0112-1}    function Previous (Container : Set;
                      Position  : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element then
                       Previous'Result = No_Element
                    elsif Previous'Result = No_Element then
                       Position = First (Container)
                    else Has_Element (Container, Previous'Result));

```

```ada
{AI12-0112-1}    procedure Previous (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, 
           Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Previous (Container : in     Set;
                       Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Position = No_Element or else
                   Has_Element (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Find (Container : Set;
                  Item      : Element_Type) return Cursor
      with Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

```ada
   function Floor (Container : Set;
                   Item      : Element_Type) return Cursor
      with Post =&gt (if Floor'Result /= No_Element
                    then Has_Element (Container, Floor'Result));

```

```ada
   function Ceiling (Container : Set;
                     Item      : Element_Type) return Cursor
      with Post =&gt (if Ceiling'Result /= No_Element
                    then Has_Element (Container, Ceiling'Result));

```

```ada
   function Contains (Container : Set;
                      Item      : Element_Type) return Boolean;

```

```ada
This paragraph was deleted.{AI05-0212-1} 

```

```ada
{AI12-0112-1}    function "&lt" (Left, Right : Cursor) return Boolean
      with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                         or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&gt" (Left, Right : Cursor) return Boolean
      with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                         or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&lt" (Left : Cursor; Right : Element_Type) return Boolean
      with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&gt" (Left : Cursor; Right : Element_Type) return Boolean
      with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&lt" (Left : Element_Type; Right : Cursor) return Boolean
      with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function "&gt" (Left : Element_Type; Right : Cursor) return Boolean
      with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure  Iterate
     (Container : in Set;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI12-0112-1}    procedure Reverse_Iterate
     (Container : in Set;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate (Container : in Set)
      return Set_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
      with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI05-0262-1} {AI12-0112-1}    function Iterate (Container : in Set; Start : in Cursor)
      return Set_Iterator_Interfaces.Reversible_Iterator'Class
      with Pre  =&gt (Start /= No_Element
                         or else raise Constraint_Error) and then
                      (Has_Element (Container, Start)
                         or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0112-1}    generic
      type Key_Type (&lt&gt) is private;
      with function Key (Element : Element_Type) return Key_Type;
      with function "&lt" (Left, Right : Key_Type)
         return Boolean is &lt&gt;
   package Generic_Keys
   with Nonblocking, Global =&gt null is

```

```ada
       function Equivalent_Keys (Left, Right : Key_Type)
          return Boolean;

```

```ada
{AI12-0112-1}       function Key (Position : Cursor) return Key_Type
         with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
              Global =&gt in all;

```

```ada
{AI12-0112-1}       function Key (Container : Set;
                    Position : Cursor) return Key_Type
         with Pre  =&gt (Position /= No_Element 
                         or else raise Constraint_Error) and then
                      (Has_Element (Container, Position)
                         or else raise Program_Error);

```

```ada
       function Element (Container : Set;
                         Key       : Key_Type)
          return Element_Type;

```

```ada
{AI12-0112-1}       procedure Replace (Container : in out Set;
                         Key       : in     Key_Type;
                         New_Item  : in     Element_Type)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                           or else raise Program_Error,
              Post =&gt Length (Container) = Length (Container)'Old;

```

```ada
{AI12-0112-1}       procedure Exclude (Container : in out Set;
                         Key       : in     Key_Type)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                           or else raise Program_Error,
              Post =&gt (declare
                         Original_Length : constant Count_Type :=
                            Length (Container)'Old;
                       begin
                         Length (Container) in 
                            Original_Length - 1 | Original_Length);

```

```ada
{AI12-0112-1}       procedure Delete (Container : in out Set;
                        Key       : in     Key_Type)
         with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                           or else raise Program_Error,
              Post =&gt Length (Container) = Length (Container)'Old - 1;

```

```ada
{AI12-0112-1}       function Find (Container : Set;
                     Key       : Key_Type) return Cursor
         with Post =&gt (if Find'Result /= No_Element
                       then Has_Element (Container, Find'Result));

```

```ada
{AI12-0112-1}        function Floor (Container : Set;
                       Key       : Key_Type) return Cursor
         with Post =&gt (if Floor'Result /= No_Element
                       then Has_Element (Container, Floor'Result));

```

```ada
{AI12-0112-1}        function Ceiling (Container : Set;
                         Key       : Key_Type) return Cursor
         with Post =&gt (if Ceiling'Result /= No_Element
                       then Has_Element (Container, Ceiling'Result));

```

```ada
       function Contains (Container : Set;
                          Key       : Key_Type) return Boolean;

```

```ada
{AI12-0112-1}       procedure Update_Element_Preserving_Key
        (Container : in out Set;
         Position  : in     Cursor;
         Process   : not null access procedure
                         (Element : in out Element_Type))
         with Pre  =&gt (Position /= No_Element
                         or else raise Constraint_Error) and then
                      (Has_Element (Container, Position) 
                         or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}       type Reference_Type
            (Element : not null access Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt in out synchronized,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}       function Reference_Preserving_Key (Container : aliased in out Set;
                                         Position  : in Cursor)
         return Reference_Type
         with Pre  =&gt (Position /= No_Element
                         or else raise Constraint_Error) and then
                      (Has_Element (Container, Position)
                         or else raise Program_Error),
              Post =&gt Tampering_With_Cursors_Prohibited (Container);;

```

```ada
{AI05-0212-1} {AI12-0112-1}       function Constant_Reference (Container : aliased in Set;
                                   Key       : in Key_Type)
         return Constant_Reference_Type
         with Pre  =&gt Find (Container, Key) /= No_Element
                         or else raise Constraint_Error,
              Post =&gt Tampering_With_Cursors_Prohibited (Container);;

```

```ada
{AI05-0212-1} {AI12-0112-1}       function Reference_Preserving_Key (Container : aliased in out Set;
                                         Key       : in Key_Type)
         return Reference_Type
         with Pre  =&gt Find (Container, Key) /= No_Element 
                         or else raise Constraint_Error,
              Post =&gt Tampering_With_Cursors_Prohibited (Container);;

```

```ada
   end Generic_Keys;

```

```ada
{AI12-0111-1}    package Stable is

```

```ada
{AI12-0111-1} {AI12-0339-1} {AI12-0399-1} {AI12-0407-1} {AI12-0445-1}       type Set (Base : not null access Ordered_Sets.Set) is
         tagged limited private
         with Constant_Indexing =&gt Constant_Reference,
              Default_Iterator  =&gt Iterate,
              Iterator_Element  =&gt Element_Type,
              Stable_Properties =&gt (Length),
              Global            =&gt null,
              Default_Initial_Condition =&gt Length (Set) = 0,
              Preelaborable_Initialization;

```

```ada
{AI12-0111-1} {AI12-0399-1}       type Cursor is private
         with Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       Empty_Set : constant Set;

```

```ada
{AI12-0111-1}       No_Element : constant Cursor;

```

```ada
{AI12-0111-1}       function Has_Element (Position : Cursor) return Boolean
         with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0111-1}       package Set_Iterator_Interfaces is new
         Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
{AI12-0111-1} {AI12-0445-1}       procedure Assign (Target : in out Ordered_Sets.Set;
                        Source : in Set)
         with Post =&gt Length (Source) = Length (Target);

```

```ada
{AI12-0111-1} {AI12-0445-1}       function Copy (Source : Ordered_Sets.Set) return Set
         with Post =&gt Length (Copy'Result) = Length (Source);

```

```ada
{AI12-0111-1}       type Constant_Reference_Type
            (Element : not null access constant Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null, Use_Formal =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       -- Additional subprograms as described in the text
      -- are declared here.

```

```ada
{AI12-0111-1}    private

```

```ada
{AI12-0111-1}       ... -- not specified by the language

```

```ada
{AI12-0111-1}    end Stable;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Ordered_Sets;

```

{AI95-00302-03} Two elements E1 and E2 are equivalent if both E1 &lt E2 and E2 &lt E1 return False, using the generic formal "&lt" operator for elements. Function Equivalent_Elements returns True if Left and Right are equivalent, and False otherwise.

{AI95-00302-03} {AI05-0044-1} The actual function for the generic formal function "&lt" on Element_Type values is expected to return the same value each time it is called with a particular pair of key values. It should define a strict weak ordering relationship (see A.18). If the actual for "&lt" behaves in some other manner, the behavior of this package is unspecified. Which subprograms of this package call "&lt" and how many times they call it, is unspecified.

{AI05-0044-1} If the actual function for the generic formal function "=" returns True for any pair of nonequivalent elements, then the behavior of the container function "=" is unspecified.

{AI95-00302-03} If the value of an element stored in a set is changed other than by an operation in this package such that at least one of "&lt" or "=" give different results, the behavior of this package is unspecified.

Discussion: See A.18.6, "The Generic Package Containers.Ordered_Maps" for a suggested implementation, and for justification of the restrictions regarding "&lt" and "=". Note that sets only need to store elements, not key/element pairs. 

{AI95-00302-03} {AI05-0262-1} The first element of a nonempty set is the one which is less than all the other elements in the set. The last element of a nonempty set is the one which is greater than all the other elements in the set. The successor of an element is the smallest element that is larger than the given element. The predecessor of an element is the largest element that is smaller than the given element. All comparisons are done using the generic formal "&lt" operator for elements.

```ada
{AI12-0112-1} function Copy (Source : Set) return Set
   with Post =&gt Length (Copy'Result) = Length (Source) and then
                not Tampering_With_Cursors_Prohibited (Copy'Result);

```

{AI05-0001-1} Returns a set whose elements are initialized from the corresponding elements of Source.

```ada
{AI12-0112-1} procedure Delete_First (Container : in out Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   (if Original_Length = 0 then Length (Container) = 0
                    else Length (Container) = Original_Length - 1));

```

{AI95-00302-03} {AI05-0264-1} If Container is empty, Delete_First has no effect. Otherwise, the element designated by First (Container) is removed from Container. Delete_First tampers with the cursors of Container.

```ada
{AI12-0112-1} procedure Delete_Last (Container : in out Set)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error,
        Post =&gt (declare
                   Original_Length : constant Count_Type :=
                      Length (Container)'Old;
                 begin
                   (if Original_Length = 0 then Length (Container) = 0
                    else Length (Container) = Original_Length - 1));

```

{AI95-00302-03} {AI05-0264-1} If Container is empty, Delete_Last has no effect. Otherwise, the element designated by Last (Container) is removed from Container. Delete_Last tampers with the cursors of Container.

```ada
{AI12-0112-1} function First_Element (Container : Set) return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (First (Container)).

```ada
{AI12-0112-1} function Last (Container : Set) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt (if not Is_Empty (Container) then 
                    Has_Element (Container, Last'Result)
                 else Last'Result = No_Element);

```

{AI95-00302-03} Returns a cursor that designates the last element in Container. If Container is empty, returns No_Element.

```ada
{AI12-0112-1} function Last_Element (Container : Set) return Element_Type
   with Pre =&gt (not Is_Empty (Container) 
                   or else raise Constraint_Error);

```

{AI95-00302-03} Equivalent to Element (Last (Container)).

```ada
{AI12-0112-1} function Previous (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element then
                   Previous'Result = No_Element);

```

{AI95-00302-03} {AI05-0262-1} If Position equals No_Element, then Previous returns No_Element. Otherwise, Previous returns a cursor designating the predecessor element of the one designated by Position. If Position designates the first element, then Previous returns No_Element.

```ada
function Previous (Container : Set;
                   Position : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element then
                   Previous'Result = No_Element
                 elsif Previous'Result = No_Element then
                   Position = First (Container)
                 else Has_Element (Container, Previous'Result));

```

{AI12-0112-1} Returns a cursor designating the predecessor of the node designated by Position in Container, if any.

```ada
{AI12-0112-1} procedure Previous (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI95-00302-03} Equivalent to Position := Previous (Position).

```ada
procedure Previous (Container : in     Set;
                    Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Position = No_Element or else
                Has_Element (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Previous (Container, Position).

```ada
{AI12-0112-1} function Floor (Container : Set;
                Item      : Element_Type) return Cursor
   with Post =&gt (if Floor'Result /= No_Element
                 then Has_Element (Container, Floor'Result));

```

{AI95-00302-03} {AI05-0264-1} Floor searches for the last element which is not greater than Item. If such an element is found, a cursor that designates it is returned. Otherwise, No_Element is returned.

```ada
{AI12-0112-1} function Ceiling (Container : Set;
                  Item      : Element_Type) return Cursor
   with Post =&gt (if Ceiling'Result /= No_Element
                 then Has_Element (Container, Ceiling'Result));

```

{AI95-00302-03} {AI05-0264-1} Ceiling searches for the first element which is not less than Item. If such an element is found, a cursor that designates it is returned. Otherwise, No_Element is returned.

```ada
{AI12-0112-1} function "&lt" (Left, Right : Cursor) return Boolean
   with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                      or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Element (Left) &lt Element (Right).

```ada
{AI12-0112-1} function "&gt" (Left, Right : Cursor) return Boolean
   with Pre    =&gt (Left /= No_Element and then Right /= No_Element)
                      or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Element (Right) &lt Element (Left).

```ada
{AI12-0112-1} function "&lt" (Left : Cursor; Right : Element_Type) return Boolean
   with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI95-00302-03} Equivalent to Element (Left) &lt Right.

```ada
{AI12-0112-1} function "&gt" (Left : Cursor; Right : Element_Type) return Boolean
   with Pre    =&gt Left /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

{AI95-00302-03} Equivalent to Right &lt Element (Left).

```ada
{AI12-0112-1} function "&lt" (Left : Element_Type; Right : Cursor) return Boolean
   with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

{AI95-00302-03} Equivalent to Left &lt Element (Right).

```ada
{AI12-0112-1} function "&gt" (Left : Element_Type; Right : Cursor) return Boolean
   with Pre    =&gt Right /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

{AI95-00302-03} Equivalent to Element (Right) &lt Left.

```ada
{AI12-0112-1} procedure Reverse_Iterate
  (Container : in Set;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI95-00302-03} {AI05-0212-1} Iterates over the elements in Container as per procedure Iterate, with the difference that the elements are traversed in predecessor order, starting with the last element.

```ada
{AI12-0112-1} {AI12-0266-1} function Iterate (Container : in Set)
   return Set_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
   with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0266-1} Iterate returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each element in Container, starting with the first element and moving the cursor according to the successor relation when used as a forward iterator, and starting with the last element and moving the cursor according to the predecessor relation when used as a reverse iterator, and processing all nodes concurrently when used as a parallel iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

```ada
function Iterate (Container : in Set; Start : in Cursor)
   return Set_Iterator_Interfaces.Reversible_Iterator'Class
   with Pre  =&gt (Start /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Start)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0262-1} {AI05-0265-1} {AI05-0269-1} {AI12-0112-1} Iterate returns a reversible iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each element in Container, starting with the element designated by Start and moving the cursor according to the successor relation when used as a forward iterator, or moving the cursor according to the predecessor relation when used as a reverse iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

Discussion: Exits are allowed from the loops created using the iterator objects. In particular, to stop the iteration at a particular cursor, just add 

```ada
exit when Cur = Stop;

```

in the body of the loop (assuming that Cur is the loop parameter and Stop is the cursor that you want to stop at). 

{AI95-00302-03} For any two elements E1 and E2, the boolean values (E1 &lt E2) and (Key(E1) &lt Key(E2)) are expected to be equal. If the actuals for Key or Generic_Keys."&lt" behave in some other manner, the behavior of this package is unspecified. Which subprograms of this package call Key and Generic_Keys."&lt", and how many times the functions are called, is unspecified.

{AI95-00302-03} In addition to the semantics described in A.18.7, the subprograms in package Generic_Keys named Floor and Ceiling, are equivalent to the corresponding subprograms in the parent package, with the difference that the Key subprogram parameter is compared to elements in the container using the Key and "&lt" generic formal functions. The function named Equivalent_Keys in package Generic_Keys returns True if both Left &lt Right and Right &lt Left return False using the generic formal "&lt" operator, and returns True otherwise.


#### Implementation Advice

{AI95-00302-03} If N is the length of a set, then the worst-case time complexity of the Insert, Include, Replace, Delete, Exclude, and Find operations that take an element parameter should be O((log N)**2) or better. The worst-case time complexity of the subprograms that take a cursor parameter should be O(1). 

Implementation Advice: The worst-case time complexity of the Insert, Include, Replace, Delete, Exclude, and Find operations of Containers.Ordered_Sets that take an element parameter should be O((log N)**2). The worst-case time complexity of the subprograms of Containers.Ordered_Sets that take a cursor parameter should be O(1).

Implementation Note: {AI95-00302-03} See A.18.6, "The Generic Package Containers.Ordered_Maps" for implementation notes regarding some of the operations of this package. 


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Ordered_Sets is new. 


#### Incompatibilities With Ada 2005

{AI05-0001-1} Subprograms Assign and Copy are added to Containers.Ordered_Sets. If an instance of Containers.Ordered_Sets is referenced in a [use_clause](./AA-8.4#S0235), and an entity E with the same [defining_identifier](./AA-3.1#S0022) as a new entity in Containers.Ordered_Sets is defined in a package that is also referenced in a [use_clause](./AA-8.4#S0235), the entity E may no longer be use-visible, resulting in errors. This should be rare and is easily fixed if it does occur. 


#### Extensions to Ada 2005

{AI05-0212-1} Added iterator and indexing support to make ordered set containers more convenient to use. 


#### Wording Changes from Ada 2005

{AI05-0044-1} Correction: Added wording to require the formal function be such that equal elements are also equivalent.

{AI05-0044-1} Correction: Redefined "&lt" actuals to require a strict weak ordering; the old definition allowed indeterminant comparisons that would not have worked in a container.

{AI05-0084-1} Correction: Added a pragma Remote_Types so that containers can be used in distributed programs. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} {AI12-0339-1} A number of new subprograms, types, and even a nested package were added to Containers.Ordered_Sets to better support contracts and stable views. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0212-1} Sets now support positional container aggregates, so [aggregate](./AA-4.3#S0106) syntax can be used to create Sets.

{AI12-0266-1} The iterator for the entire container now can return a parallel iterator which can be used to process the container in parallel. 


#### Wording Changes from Ada 2012

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.10  The Generic Package Containers.Multiway_Trees

{AI05-0136-1} The language-defined generic package Containers.Multiway_Trees provides private types Tree and Cursor, and a set of operations for each type. A multiway tree container is well-suited to represent nested structures.

Discussion: {AI05-0136-1} This tree just provides a basic structure, and make no promises about balancing or other automatic organization. In this sense, it is different than the indexed (Map, Set) forms. Rather, it provides a building block on which to construct more complex and more specialized tree containers. 

{AI05-0136-1} {AI12-0078-1} {AI12-0159-1} A multiway tree container object manages a tree of nodes, consisting of a root node and a set of internal nodes; each internal node contains an element and pointers to the parent, first child, last child, next (successor) sibling, and previous (predecessor) sibling internal nodes. A cursor designates a particular node within a tree (and by extension the element contained in that node, if any). A cursor keeps designating the same node (and element) as long as the node is part of the container, even if the node is moved within the container.

{AI05-0136-1} {AI05-0269-1} {AI12-0078-1} A subtree is a particular node (which roots the subtree) and all of its child nodes (including all of the children of the child nodes, recursively). The root node is always present and has neither an associated element value nor any parent node; it has pointers to its first child and its last child, if any. The root node provides a place to add nodes to an otherwise empty tree and represents the base of the tree.

{AI05-0136-1} {AI05-0269-1} A node that has no children is called a leaf node. The ancestors of a node are the node itself, its parent node, the parent of the parent node, and so on until a node with no parent is reached. Similarly, the descendants of a node are the node itself, its child nodes, the children of each child node, and so on.

{AI05-0136-1} {AI05-0262-1} {AI05-0269-1} The nodes of a subtree can be visited in several different orders. For a depth-first order, after visiting a node, the nodes of its child list are each visited in depth-first order, with each child node visited in natural order (first child to last child).

Ramification: For the depth-first order, when each child node is visited, the child list of the child node is visited before the next sibling of the child node is visited. 


#### Static Semantics

{AI05-0136-1} The generic library package Containers.Multiway_Trees has the following declaration: 

```ada
{AI05-0136-1} {AI05-0212-1} {AI12-0112-1} with Ada.Iterator_Interfaces;
generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is &lt&gt;
package Ada.Containers.Multiway_Trees
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects in this generic package, see the notes on the equivalent operations in the specification of the Containers.Vectors package (see A.18.2). 

```ada
{AI05-0136-1} {AI05-0212-1} {AI12-0111-1} {AI12-0112-1} {AI12-0399-1}    type Tree is tagged private
      with Constant_Indexing =&gt Constant_Reference,
           Variable_Indexing =&gt Reference,
           Default_Iterator  =&gt Iterate,
           Iterator_Element  =&gt Element_Type,
           Iterator_View     =&gt Stable.Tree,
           Stable_Properties =&gt (Node_Count,
                                 Tampering_With_Cursors_Prohibited,
                                 Tampering_With_Elements_Prohibited),
           Default_Initial_Condition =&gt
              Node_Count (Tree) = 1 and then
              (not Tampering_With_Cursors_Prohibited (Tree)) and then
              (not Tampering_With_Elements_Prohibited (Tree)),
           Preelaborable_Initialization;

```

```ada
{AI12-0399-1}    type Cursor is private
      with Preelaborable_Initialization;

```

```ada
   Empty_Tree : constant Tree;

```

```ada
   No_Element : constant Cursor;

```

```ada
{AI12-0112-1}    function Equal_Element (Left, Right : Element_Type)
      return Boolean renames "=";

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Has_Element (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Has_Element (Container : Tree; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1}    package Tree_Iterator_Interfaces is new
      Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
   function Equal_Subtree (Left_Position : Cursor;
                           Right_Position: Cursor) return Boolean;

```

```ada
   function "=" (Left, Right : Tree) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_Cursors_Prohibited
      (Container : Tree) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Tampering_With_Elements_Prohibited
      (Container : Tree) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty return Tree
      is (Empty_Tree)
      with Post =&gt
            not Tampering_With_Elements_Prohibited (Empty'Result) and then
            not Tampering_With_Cursors_Prohibited (Empty'Result) and then
            Node_Count (Empty'Result) = 1;

```

```ada
{AI12-0112-1}    function Is_Empty (Container : Tree) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Is_Empty'Result = (Node_Count (Container) = 1);

```

```ada
{AI12-0112-1}    function Node_Count (Container : Tree) return Count_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Subtree_Node_Count (Position : Cursor) return Count_Type
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Subtree_Node_Count (Container : Tree; Position : Cursor)
      return Count_Type
      with Pre =&gt Meaningful_For (Container, Position)
                      or else raise Program_Error,
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Depth (Position : Cursor) return Count_Type
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Depth (Container : Tree; Position : Cursor)
      return Count_Type
      with Pre =&gt Meaningful_For (Container, Position)
                      or else raise Program_Error,
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Root (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Root (Container : Tree; Position : Cursor)
      return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Leaf (Position : Cursor) return Boolean
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Leaf (Container : Tree; Position : Cursor)
      return Boolean
      with Pre =&gt Meaningful_For (Container, Position)
                      or else raise Program_Error,
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Is_Ancestor_Of (Container : Tree;
                            Parent   : Cursor;
                            Position : Cursor) return Boolean
      with Pre =&gt (Meaningful_For (Container, Position)
                      or else raise Program_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Root (Container : Tree) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Post =&gt Root'Result /= No_Element and then
                   not Has_Element (Container, Root'Result);

```

```ada
{AI12-0112-1}    function Meaningful_For (Container : Tree; Position : Cursor)
      return Boolean is
      (Position = No_Element or else
       Is_Root (Container, Position) or else
       Has_Element (Container, Position))
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

Reason: {AI12-0112-1} When this function is true, the Position can be meaningfully used with operations for Container. We define this because many operations allow the root (which does not have an element, so Has_Element returns False), so many preconditions get unwieldy. We allow No_Element as it is allowed by many queries, and for existing routines, it raises a different exception (Constraint_Error rather than Program_Error) than a cursor for the wrong container does. 

```ada
{AI12-0112-1}    procedure Clear (Container : in out Tree)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error,
           Post =&gt Node_Count (Container) = 1;

```

```ada
{AI12-0112-1}    function Element (Position : Cursor) return Element_Type
      with Pre =&gt (Position /= No_Element or else
                      raise Constraint_Error) and then
                   (Has_Element (Position) or else raise Program_Error),
           Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    function Element (Container : Tree;
                     Position  : Cursor) return Element_Type
      with Pre =&gt (Position /= No_Element or else
                      raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Tree;
                              Position  : in     Cursor;
                              New_item  : in     Element_Type)
      with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Position : in Cursor;
      Process  : not null access procedure (Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Position) or else raise Program_Error),
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Tree;
      Position  : in Cursor;
      Process   : not null access procedure (Element : in Element_Type))
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    procedure Update_Element
     (Container : in out Tree;
      Position  : in     Cursor;
      Process   : not null access procedure
                      (Element : in out Element_Type))
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Reference_Type (Element : not null access Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Tree;
                                Position  : in Cursor)
      return Constant_Reference_Type
      with Pre  =&gt (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Tree;
                       Position  : in Cursor)
      return Reference_Type
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position) 
                      or else raise Program_Error),
           Post   =&gt Tampering_With_Cursors_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Assign (Target : in out Tree; Source : in Tree)
      with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error,
           Post =&gt Node_Count (Source) = Node_Count (Target);

```

```ada
{AI12-0112-1}    function Copy (Source : Tree) return Tree
      with Post =&gt
              Node_Count (Copy'Result) = Node_Count (Source) and then
              not Tampering_With_Elements_Prohibited (Copy'Result) and then
              not Tampering_With_Cursors_Prohibited (Copy'Result);

```

```ada
{AI12-0112-1}    procedure Move (Target : in out Tree;
                   Source : in out Tree)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                      or else raise Program_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                  Node_Count (Target) = Node_Count (Source'Old) and then
                  Node_Count (Source) = 1);

```

```ada
{AI12-0112-1}    procedure Delete_Leaf (Container : in out Tree;
                          Position  : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error) and then
                   (Is_Leaf (Container, Position)
                      or else raise Constraint_Error),
           Post =&gt
              Node_Count (Container)'Old = Node_Count (Container)+1 and then
              Position = No_Element;

```

```ada
{AI12-0112-1}    procedure Delete_Subtree (Container : in out Tree;
                             Position  : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error),
           Post =&gt Node_Count (Container)'Old = Node_Count (Container) +
                      Subtree_Node_Count (Container, Position)'Old and then
                   Position = No_Element;

```

```ada
{AI12-0112-1}    procedure Swap (Container : in out Tree;
                   I, J      : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (I /= No_Element or else Constraint_Error) and then
                   (J /= No_Element or else Constraint_Error) and then
                   (Has_Element (Container, I)
                      or else raise Program_Error) and then
                   (Has_Element (Container, J)
                      or else raise Program_Error);

```

```ada
{AI12-0112-1}    function Find (Container : Tree;
                  Item      : Element_Type)
      return Cursor
      with Post =&gt (if Find'Result /= No_Element
                    then Has_Element (Container, Find'Result));

```

```ada
{AI05-0136-1} {AI05-0248-1} {AI12-0112-1}    function Find_In_Subtree (Position : Cursor;
                             Item     : Element_Type)
      return Cursor
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Post =&gt (if Find_In_Subtree'Result = No_Element
                    then Has_Element (Find_In_Subtree'Result)),
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function Find_In_Subtree (Container : Tree;
                             Position  : Cursor;
                             Item      : Element_Type)
      return Cursor
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Position)
                      or else raise Program_Error),
           Post =&gt (if Find_In_Subtree'Result /= No_Element
                    then Has_Element (Container, Find_In_Subtree'Result));

```

```ada
{AI05-0136-1} {AI05-0248-1} {AI12-0112-1}    function Ancestor_Find (Position : Cursor;
                           Item     : Element_Type)
      return Cursor
      with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Post =&gt (if Ancestor_Find'Result = No_Element
                    then Has_Element (Ancestor_Find'Result)),
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function Ancestor_Find (Container : Tree;
                           Position  : Cursor;
                           Item      : Element_Type)
      return Cursor
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Position)
                      or else raise Program_Error),
           Post =&gt (if Ancestor_Find'Result = No_Element
                    then Has_Element (Container, Ancestor_Find'Result));

```

```ada
   function Contains (Container : Tree;
                      Item      : Element_Type) return Boolean;

```

```ada
{AI12-0112-1}    procedure Iterate
     (Container : in Tree;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit;

```

```ada
{AI12-0112-1}    procedure Iterate_Subtree
     (Position  : in Cursor;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit,
           Pre  =&gt Position /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Iterate_Subtree
     (Container : in Tree;
      Position  : in Cursor;
      Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit,
           Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Position)
                      or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate (Container : in Tree)
      return Tree_Iterator_Interfaces.Parallel_Iterator'Class
      with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate_Subtree (Position : in Cursor)
      return Tree_Iterator_Interfaces.Parallel_Iterator'Class
      with Pre    =&gt Position /= No_Element or else raise Constraint_Error,
           Global =&gt in all;

```

```ada
{AI12-0112-1}    function Iterate_Subtree (Container : in Tree; Position : in Cursor)
      return Tree_Iterator_Interfaces.Parallel_Iterator'Class
      with Pre  =&gt (Position /= No_Element 
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Position)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0112-1}    function Child_Count (Parent : Cursor) return Count_Type
      with Post =&gt (if Parent = No_Element then Child_Count'Result = 0),
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Child_Count (Container : Tree; Parent : Cursor)
      return Count_Type
      with Pre  =&gt Meaningful_For (Container, Parent) 
                      or else raise Program_Error,
           Post =&gt (if Parent = No_Element then Child_Count'Result = 0),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Child_Depth (Parent, Child : Cursor) return Count_Type
      with Pre  =&gt (Parent = No_Element and then Child = No_Element)
                      or else raise Constraint_Error,
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    function Child_Depth (Container : Tree; Parent, Child : Cursor)
      return Count_Type
      with Pre  =&gt ((Parent = No_Element and then Child = No_Element)
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error) and then
                   (Meaningful_For (Container, Child)
                      or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Insert_Child (Container : in out Tree;
                           Parent    : in     Cursor;
                           Before    : in     Cursor;
                           New_Item  : in     Element_Type;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error) and then
                   (Meaningful_For (Container, Before)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Container.Parent (Before) = Parent
                      or else raise Constraint_Error),
           Post =&gt Node_Count (Container) =
                   Node_Count (Container)'Old + Count;

```

```ada
{AI12-0112-1}    procedure Insert_Child (Container : in out Tree;
                           Parent    : in     Cursor;
                           Before    : in     Cursor;
                           New_Item  : in     Element_Type;
                           Position  :    out Cursor;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error) and then
                   (Meaningful_For (Container, Before)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Container.Parent (Before) = Parent
                      or else raise Constraint_Error),
           Post =&gt (Node_Count (Container) =
                    Node_Count (Container)'Old + Count) and then
                    Has_Element (Container, Position);

```

```ada
{AI12-0112-1}    procedure Insert_Child (Container : in out Tree;
                           Parent    : in     Cursor;
                           Before    : in     Cursor;
                           Position  :    out Cursor;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error) and then
                   (Meaningful_For (Container, Before)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Container.Parent (Before) = Parent
                      or else raise Constraint_Error),
           Post =&gt (Node_Count (Container) =
                    Node_Count (Container)'Old + Count) and then
                    Has_Element (Container, Position);

```

```ada
{AI12-0112-1}    procedure Prepend_Child (Container : in out Tree;
                            Parent    : in     Cursor;
                            New_Item  : in     Element_Type;
                            Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error),
           Post =&gt Node_Count (Container) =
                   Node_Count (Container)'Old + Count;

```

```ada
{AI12-0112-1}    procedure Append_Child (Container : in out Tree;
                           Parent    : in     Cursor;
                           New_Item  : in     Element_Type;
                           Count     : in     Count_Type := 1)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error),
           Post =&gt Node_Count (Container) =
                   Node_Count (Container)'Old + Count;

```

```ada
{AI12-0112-1}    procedure Delete_Children (Container : in out Tree;
                              Parent    : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Parent /= No_Element
                       or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error),
           Post =&gt (Node_Count (Container) = Node_Count (Container)'Old -
                      Child_Count (Container, Parent)'Old) and then
                    Child_Count (Container, Parent) = 0;

```

```ada
{AI12-0112-1}    procedure Copy_Subtree (Target   : in out Tree;
                           Parent   : in     Cursor;
                           Before   : in     Cursor;
                           Source   : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                       or else raise Program_Error) and then
                    (Parent /= No_Element
                       or else raise Constraint_Error) and then
                    (Meaningful_For (Target, Parent)
                       or else raise Program_Error) and then
                    (Meaningful_For (Target, Before)
                       or else raise Program_Error) and then
                    (Before = No_Element or else
                       Target.Parent (Before) = Parent
                       or else raise Constraint_Error) and then
                    (not Is_Root (Source)
                       or else raise Constraint_Error),
           Post =&gt Node_Count (Target) =
                   Node_Count (Target)'Old + Subtree_Node_Count (Source),
           Global =&gt in all;

```

```ada
{AI12-0112-1}    procedure Copy_Local_Subtree (Target   : in out Tree;
                                 Parent   : in     Cursor;
                                 Before   : in     Cursor;
                                 Source   : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                       or else raise Program_Error) and then
                    (Parent /= No_Element
                       or else raise Constraint_Error) and then
                    (Meaningful_For (Target, Parent)
                       or else raise Program_Error) and then
                    (Meaningful_For (Target, Before)
                       or else raise Program_Error) and then
                    (Before = No_Element or else
                       Target.Parent (Before) = Parent
                       or else raise Constraint_Error) and then
                    (Meaningful_For (Target, Source)
                       or else raise Program_Error) and then
                    (not Is_Root (Source)
                       or else raise Constraint_Error),
           Post =&gt Node_Count (Target) = Node_Count (Target)'Old +
                      Subtree_Node_Count (Target, Source);

```

```ada
{AI12-0112-1}    procedure Copy_Subtree (Target   : in out Tree;
                           Parent   : in     Cursor;
                           Before   : in     Cursor;
                           Source   : in     Tree;
                           Subtree  : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                       or else raise Program_Error) and then
                    (Parent /= No_Element
                       or else raise Constraint_Error) and then
                    (Meaningful_For (Target, Parent)
                       or else raise Program_Error) and then
                    (Meaningful_For (Target, Before)
                       or else raise Program_Error) and then
                    (Before = No_Element or else
                       Target.Parent (Before) = Parent
                       or else raise Constraint_Error) and then
                    (Meaningful_For (Source, Subtree)
                       or else raise Program_Error) and then
                    (not Is_Root (Source, Subtree)
                       or else raise Constraint_Error),
           Post =&gt Node_Count (Target) = Node_Count (Target)'Old +
                      Subtree_Node_Count (Source, Subtree);

```

```ada
{AI12-0112-1}    procedure Splice_Subtree (Target   : in out Tree;
                             Parent   : in     Cursor;
                             Before   : in     Cursor;
                             Source   : in out Tree;
                             Position : in out Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                       or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                       or else raise Program_Error) and then
                   (Parent /= No_Element
                       or else raise Constraint_Error) and then
                   (Meaningful_For (Target, Parent)
                       or else raise Program_Error) and then
                   (Meaningful_For (Target, Before)
                       or else raise Program_Error) and then
                   (Before = No_Element or else
                    Target.Parent (Before) /= Parent
                       or else raise Constraint_Error) and then
                   (Position /= No_Element
                       or else raise Constraint_Error) and then
                   (Has_Element (Source, Position)
                       or else raise Program_Error) and then
                   (Target'Has_Same_Storage (Source) or else
                    Position = Before or else
                    Is_Ancestor_Of (Target, Position, Parent)
                       or else raise Constraint_Error),
           Post =&gt (declare
                       Org_Sub_Count renames 
                          Subtree_Node_Count (Source, Position)'Old;
                       Org_Target_Count renames Node_Count (Target)'Old;
                    begin
                      (if not Target'Has_Same_Storage (Source) then
                          Node_Count (Target) = Org_Target_Count +
                             Org_Sub_Count and then
                          Node_Count (Source) = Node_Count (Source)'Old -
                             Org_Sub_Count and then
                          Has_Element (Target, Position)
                       else
                          Target.Parent (Position) = Parent and then
                          Node_Count (Target) = Org_Target_Count));

```

```ada
{AI12-0112-1}    procedure Splice_Subtree (Container: in out Tree;
                             Parent   : in     Cursor;
                             Before   : in     Cursor;
                             Position : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                       or else raise Program_Error) and then
                   (Parent /= No_Element
                       or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error) and then
                   (Meaningful_For (Container, Before)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Container.Parent (Before) /= Parent
                      or else raise Constraint_Error) and then
                   (Position /= No_Element
                      or else raise Constraint_Error) and then
                   (Has_Element (Container, Position)
                      or else raise Program_Error) and then
                   (Position = Before or else
                    Is_Ancestor_Of (Container, Position, Parent)
                      or else raise Constraint_Error),
           Post =&gt (Node_Count (Container) =
                      Node_Count (Container)'Old and then
                    Container.Parent (Position) = Parent);

```

```ada
{AI12-0112-1}    procedure Splice_Children (Target          : in out Tree;
                              Target_Parent   : in     Cursor;
                              Before          : in     Cursor;
                              Source          : in out Tree;
                              Source_Parent   : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                       or else raise Program_Error) and then
                   (not Tampering_With_Cursors_Prohibited (Source)
                       or else raise Program_Error) and then
                   (Target_Parent /= No_Element
                       or else raise Constraint_Error) and then
                   (Meaningful_For (Target, Target_Parent)
                       or else raise Program_Error) and then
                   (Meaningful_For (Target, Before)
                       or else raise Program_Error) and then
                   (Source_Parent /= No_Element
                       or else raise Constraint_Error) and then
                   (Meaningful_For (Source, Source_Parent)
                       or else raise Program_Error) and then
                   (Before = No_Element or else
                    Parent (Target, Before) /= Target_Parent
                       or else raise Constraint_Error) and then
                   (Target'Has_Same_Storage (Source) or else
                    Target_Parent = Source_Parent or else
                    Is_Ancestor_Of (Target, Source_Parent, Target_Parent)
                       or else raise Constraint_Error),
           Post =&gt (declare
                       Org_Child_Count renames
                          Child_Count (Source, Source_Parent)'Old;
                       Org_Target_Count renames Node_Count (Target)'Old;
                    begin
                      (if not Target'Has_Same_Storage (Source) then
                          Node_Count (Target) = Org_Target_Count +
                             Org_Child_Count and then
                          Node_Count (Source) = Node_Count (Source)'Old -
                             Org_Child_Count
                       else
                          Node_Count (Target) = Org_Target_Count));

```

```ada
{AI12-0112-1}    procedure Splice_Children (Container       : in out Tree;
                              Target_Parent   : in     Cursor;
                              Before          : in     Cursor;
                              Source_Parent   : in     Cursor)
      with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                      or else raise Program_Error) and then
                   (Target_Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Target_Parent)
                      or else raise Program_Error) and then
                   (Meaningful_For (Container, Before)
                      or else raise Program_Error) and then
                   (Source_Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Source_Parent)
                      or else raise Program_Error) and then
                   (Before = No_Element or else
                    Parent (Container, Before) /= Target_Parent
                      or else raise Constraint_Error) and then
                   (Target_Parent = Source_Parent or else
                    Is_Ancestor_Of (Container, Source_Parent, Target_Parent)
                      or else raise Constraint_Error),
           Post =&gt Node_Count (Container) = Node_Count (Container)'Old;

```

```ada
{AI12-0112-1}    function Parent (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element or else
                       Is_Root (Position) then Parent'Result = No_Element);

```

```ada
{AI12-0112-1}    function Parent (Container : Tree;
                    Position  : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Meaningful_For (Container, Position) 
                      or else raise Program_Error,
           Post =&gt (if Position = No_Element or else
                      Is_Root (Container, Position)
                      then Parent'Result = No_Element
                    else Has_Element (Container, Parent'Result));

```

```ada
{AI12-0112-1}    function First_Child (Parent : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Pre  =&gt Parent /= No_Element or else raise Constraint_Error;

```

```ada
{AI12-0112-1}    function First_Child (Container : Tree;
                         Parent    : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error),
           Post =&gt First_Child'Result = No_Element or else
                   Has_Element (Container, First_Child'Result);

```

```ada
{AI12-0112-1}    function First_Child_Element (Parent : Cursor) return Element_Type
      with Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type,
           Pre  =&gt (Parent /= No_Element and then
                    Last_Child (Parent) /= No_Element)
                        or else raise Constraint_Error;

```

```ada
{AI12-0112-1}    function First_Child_Element (Container : Tree;
                                 Parent    : Cursor) return Element_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type,
           Pre  =&gt (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error) and then
                   (First_Child (Container, Parent) /= No_Element
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Last_Child (Parent : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Pre  =&gt Parent /= No_Element or else raise Constraint_Error;

```

```ada
{AI12-0112-1}    function Last_Child (Container : Tree;
                        Parent    : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error),
           Post =&gt Last_Child'Result = No_Element or else
                   Has_Element (Container, Last_Child'Result);

```

```ada
{AI12-0112-1}    function Last_Child_Element (Parent : Cursor) return Element_Type
      with Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type,
           Pre  =&gt (Parent /= No_Element and then
                    Last_Child (Parent) /= No_Element)
                        or else raise Constraint_Error;

```

```ada
{AI12-0112-1}    function Last_Child_Element (Container : Tree;
                                Parent    : Cursor) return Element_Type
      with Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type,
           Pre  =&gt (Parent /= No_Element
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error) and then
                   (Last_Child (Container, Parent) /= No_Element
                      or else raise Constraint_Error);

```

```ada
{AI12-0112-1}    function Next_Sibling (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element
                       then Next_Sibling'Result = No_Element);

```

```ada
{AI12-0112-1}    function Next_Sibling (Container : Tree;
                          Position  : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Meaningful_For (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Next_Sibling'Result = No_Element then
                      Position = No_Element or else
                      Is_Root (Container, Position) or else
                      Last_Child (Container, Parent (Container, Position))
                         = Position
                    else Has_Element (Container, Next_Sibling'Result));

```

```ada
{AI12-0112-1}    procedure Next_Sibling (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Next_Sibling (Container : in     Tree;
                           Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Meaningful_For (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI12-0112-1}    function Previous_Sibling (Position : Cursor) return Cursor
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
           Post =&gt (if Position = No_Element
                       then Previous_Sibling'Result = No_Element);

```

```ada
{AI12-0112-1}    function Previous_Sibling (Container : Tree;
                              Position  : Cursor) return Cursor
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Meaningful_For (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Previous_Sibling'Result = No_Element then
                      Position = No_Element or else
                      Is_Root (Container, Position) or else
                      First_Child (Container, Parent (Container, Position))
                         = Position
                    else Has_Element (Container, Previous_Sibling'Result));

```

```ada
{AI12-0112-1}    procedure Previous_Sibling (Position : in out Cursor)
      with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Previous_Sibling (Container : in     Tree;
                              Position  : in out Cursor)
      with Nonblocking, Global =&gt null, Use_Formal =&gt null,
           Pre  =&gt Meaningful_For (Container, Position)
                      or else raise Program_Error,
           Post =&gt (if Position /= No_Element
                    then Has_Element (Container, Position));

```

```ada
{AI05-0136-1} {AI05-0248-1} {AI12-0112-1}    procedure Iterate_Children
        (Parent  : in Cursor;
         Process : not null access procedure (Position : in Cursor))
      with Allows_Exit,
           Pre    =&gt Parent /= No_Element or else raise Constraint_Error,
           Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Iterate_Children
        (Container : in Tree;
         Parent    : in Cursor;
         Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit,
           Pre  =&gt (Parent /= No_Element 
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error);

```

```ada
{AI05-0136-1} {AI05-0248-1} {AI12-0112-1}    procedure Reverse_Iterate_Children
        (Parent  : in Cursor;
         Process : not null access procedure (Position : in Cursor))
      with Allows_Exit,
           Pre    =&gt Parent /= No_Element or else raise Constraint_Error,
           Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Reverse_Iterate_Children
        (Container : in Tree;
         Parent    : in Cursor;
         Process   : not null access procedure (Position : in Cursor))
      with Allows_Exit,
           Pre  =&gt (Parent /= No_Element 
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1} {AI12-0266-1}    function Iterate_Children (Container : in Tree; Parent : in Cursor)
      return Tree_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
      with Pre  =&gt (Parent /= No_Element 
                      or else raise Constraint_Error) and then
                   (Meaningful_For (Container, Parent)
                      or else raise Program_Error),
           Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

```ada
{AI12-0111-1}    package Stable is

```

```ada
{AI12-0111-1} {AI12-0399-1}       type Tree (Base : not null access Multiway_Trees.Tree) is
         tagged limited private
         with Constant_Indexing =&gt Constant_Reference,
              Variable_Indexing =&gt Reference,
              Default_Iterator  =&gt Iterate,
              Iterator_Element  =&gt Element_Type,
              Stable_Properties =&gt (Node_Count),
              Global            =&gt null,
              Default_Initial_Condition =&gt Node_Count (Tree) = 1,
              Preelaborable_Initialization;

```

```ada
{AI12-0111-1} {AI12-0399-1}       type Cursor is private
         with Preelaborable_Initialization;

```

```ada
{AI12-0111-1}       Empty_Tree : constant Tree;

```

```ada
{AI12-0111-1}       No_Element : constant Cursor;

```

```ada
{AI12-0111-1}       function Has_Element (Position : Cursor) return Boolean
         with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

```ada
{AI12-0111-1}       package Tree_Iterator_Interfaces is new
         Ada.Iterator_Interfaces (Cursor, Has_Element);

```

```ada
{AI12-0111-1}       procedure Assign (Target : in out Multiway_Trees.Tree;
                        Source : in Tree)
         with Post =&gt Node_Count (Source) = Node_Count (Target);

```

```ada
{AI12-0111-1}       function Copy (Source : Multiway_Trees.Tree) return Tree
         with Post =&gt Node_Count (Copy'Result) = Node_Count (Source);

```

```ada
{AI12-0111-1}       type Constant_Reference_Type
            (Element : not null access constant Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       type Reference_Type
            (Element : not null access Element_Type) is private
         with Implicit_Dereference =&gt Element,
              Nonblocking, Global =&gt null,
              Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0111-1}       -- Additional subprograms as described in the text
      -- are declared here.

```

```ada
{AI12-0111-1}    private

```

```ada
{AI12-0111-1}       ... -- not specified by the language

```

```ada
{AI12-0111-1}    end Stable;

```

```ada
private
   ... -- not specified by the language
end Ada.Containers.Multiway_Trees;

```

{AI05-0136-1} The actual function for the generic formal function "=" on Element_Type values is expected to define a reflexive and symmetric relationship and return the same result value each time it is called with a particular pair of values. If it behaves in some other manner, the functions Find, Reverse_Find, Equal_Subtree, and "=" on tree values return an unspecified value. The exact arguments and number of calls of this generic formal function by the functions Find, Reverse_Find, Equal_Subtree, and "=" on tree values are unspecified.

{AI05-0136-1} The type Tree is used to represent trees. The type Tree needs finalization (see 7.6).

{AI05-0136-1} {AI05-0248-1} Empty_Tree represents the empty Tree object. It contains only the root node (Node_Count (Empty_Tree) returns 1). If an object of type Tree is not otherwise initialized, it is initialized to the same value as Empty_Tree.

{AI05-0136-1} No_Element represents a cursor that designates no element. If an object of type Cursor is not otherwise initialized, it is initialized to the same value as No_Element.

{AI05-0136-1} {AI12-0434-1} The primitive "=" operator for type Cursor returns True if both cursors are No_Element, or designate the same element in the same container.

To be honest: {AI12-0434-1} "The primitive "=" operator" is the one with two parameters of type Cursor which returns Boolean. We're not talking about some other (hidden) primitive function named "=". 

{AI05-0136-1} Execution of the default implementation of the Input, Output, Read, or Write attribute of type Cursor raises Program_Error.

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI12-0437-1} Tree'Write for a Tree object T writes Node_Count(T) - 1 elements of the tree to the stream. It may also write additional information about the tree.

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} Tree'Read reads the representation of a tree from the stream, and assigns to Item a tree with the same elements and structure as was written by Tree'Write.

Ramification: Streaming more elements than the container holds is wrong. For implementation implications of this rule, see the Implementation Note in A.18.2. 

{AI05-0136-1} {AI12-0111-1} {AI12-0112-1} [Some operations  check for "tampering with cursors" of a container because they depend on the set of elements of the container remaining constant, and others check for "tampering with elements" of a container because they depend on elements of the container not being replaced.] When tampering with cursors is prohibited for a particular tree object T, Program_Error is propagated by the finalization of T[, as well as by a call that passes T to certain of the operations of this package, as indicated by the precondition of such an operation]. Similarly, when tampering with elements is prohibited for T, Program_Error is propagated by a call that passes T to certain of the other operations of this package, as indicated by the precondition of such an operation.

Paragraphs 81 through 90 are removed as preconditions now describe these rules. 

Ramification: We don't need to explicitly mention [assignment_statement](./AA-5.2#S0173), because that finalizes the target object as part of the operation, and finalization of an object is already defined as tampering with cursors. 

```ada
{AI12-0112-1} function Has_Element (Position : Cursor) return Boolean
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

Returns True if Position designates an element, and returns False otherwise. [In particular, Has_Element returns False if the cursor designates a root node or equals No_Element.]

To be honest: {AI05-0005-1} {AI05-0136-1} This function might not detect cursors that designate deleted elements; such cursors are invalid (see below) and the result of calling Has_Element with an invalid cursor is unspecified (but not erroneous). 

```ada
function Has_Element (Container : Tree; Position : Cursor)
   return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if Position designates an element in Container, and returns False otherwise. [In particular, Has_Element returns False if the cursor designates a root node or equals No_Element.]

```ada
function Equal_Subtree (Left_Position : Cursor;
                        Right_Position: Cursor) return Boolean;

```

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI05-0264-1} If Left_Position or Right_Position equals No_Element, propagates Constraint_Error. If the number of child nodes of the element designated by Left_Position is different from the number of child nodes of the element designated by Right_Position, the function returns False. If Left_Position designates a root node and Right_Position does not, the function returns False. If Right_Position designates a root node and Left_Position does not, the function returns False. Unless both cursors designate a root node, the elements are compared using the generic formal equality operator. If the result of the element comparison is False, the function returns False. Otherwise, it calls Equal_Subtree on a cursor designating each child element of the element designated by Left_Position and a cursor designating the corresponding child element of the element designated by Right_Position. If any such call returns False, the function returns False; otherwise, it returns True. Any exception raised during the evaluation of element equality is propagated.

Ramification: Left_Position and Right_Position do not need to be from the same tree. 

Implementation Note: This wording describes the canonical semantics. However, the order and number of calls on the formal equality function is unspecified for all of the operations that use it in this package, so an implementation can call it as many or as few times as it needs to get the correct answer. Similarly, a global rule (see the introduction of Annex A) says that language-defined routines are not affected by overriding of other language-defined routines. This means that no reasonable program can tell how many times Equal_Subtree is called, and thus an implementation can call it as many or as few times as it needs to get the correct answer. Specifically, there is no requirement to call the formal equality or Equal_Subtree additional times once the answer has been determined. 

```ada
function "=" (Left, Right : Tree) return Boolean;

```

{AI05-0136-1} {AI05-0262-1} If Left and Right denote the same tree object, then the function returns True. Otherwise, it calls Equal_Subtree with cursors designating the root nodes of Left and Right; the result is returned. Any exception raised during the evaluation of Equal_Subtree is propagated.

Implementation Note: Similar considerations apply here as apply to Equal_Subtree. The actual number of calls performed is unspecified. 

```ada
function Tampering_With_Cursors_Prohibited
   (Container : Tree) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if tampering with cursors or tampering with elements is currently prohibited for Container, and returns False otherwise.

Reason: {AI12-0112-1} Prohibiting tampering with elements also needs to prohibit tampering with cursors, as deleting an element is similar to replacing it. 

Implementation Note: {AI12-0112-1} Various contracts elsewhere in this specification require that this function be implemented with synchronized data. Moreover, it is possible for tampering to be prohibited by multiple operations (sequentially or in parallel). Therefore, tampering needs to be implemented with an atomic or protected counter. The counter is initialized to zero, and is incremented when tampering is prohibited, and decremented when leaving an area that prohibited tampering. Function Tampering_With_Cursors_Prohibited returns True if the counter is nonzero. (Note that any case where the result is not well-defined for one task is incorrect use of shared variables and would be erroneous by the rules of 9.10, so no special protection is needed to read the counter.) 

```ada
function Tampering_With_Elements_Prohibited
   (Container : Tree) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Always returns False[, regardless of whether tampering with elements is prohibited].

Reason: {AI12-0111-1} A definite element cannot change size, so we allow operations that tamper with elements even when tampering with elements is prohibited. That's not true for the indefinite containers, which is why this kind of tampering exists. 

```ada
function Is_Empty (Container : Tree) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt Is_Empty'Result = (Node_Count (Container) = 1);

```

{AI05-0136-1} {AI12-0112-1} Returns True if Container is empty.

Ramification: An empty tree contains just the root node. 

```ada
{AI12-0112-1} function Node_Count (Container : Tree) return Count_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0136-1} Node_Count returns the number of nodes in Container.

Ramification: Since all tree objects have a root node, this can never return a value of 0. Node_Count (Some_Tree) should always equal Subtree_Node_Count (Root (Some_Tree)). 

```ada
{AI12-0112-1} function Subtree_Node_Count (Position : Cursor) return Count_Type
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null);

```

{AI05-0136-1} {AI05-0248-1} If Position is No_Element, Subtree_Node_Count returns 0; otherwise, Subtree_Node_Count returns the number of nodes in the subtree that is rooted by Position.

```ada
function Subtree_Node_Count (Container : Tree; Position : Cursor)
   return Count_Type
   with Pre =&gt Meaningful_For (Container, Position) 
                   or else raise Program_Error,
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0136-1} {AI12-0112-1} If Position is No_Element, Subtree_Node_Count returns 0; otherwise, Subtree_Node_Count returns the number of nodes in the subtree of Container that is rooted by Position.

This paragraph was deleted.{AI12-0112-1} 

Reason: {AI12-0112-1} We raise Program_Error if Position belongs to some other container because we have promised to read only the container passed to this function. Determining the answer requires reading the container that Position belongs to, which we've promised not to do if it is not Container. We don't make this check for functions like Has_Element and Is_Root which do not require reading another container to determine the answer, but we do make it for most functions. 

```ada
{AI12-0112-1} function Depth (Position : Cursor) return Count_Type
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0136-1} {AI05-0248-1} If Position equals No_Element, Depth returns 0; otherwise, Depth returns the number of ancestor nodes of the node designated by Position (including the node itself).

Ramification: Depth (Root (Some_Tree)) = 1. 

```ada
function Depth (Container : Tree; Position : Cursor)
   return Count_Type
   with Pre =&gt Meaningful_For (Container, Position) 
                  or else raise Program_Error,
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} If Position equals No_Element, Depth returns 0; otherwise, Depth returns the number of ancestor nodes of the node of Container designated by Position (including the node itself).

```ada
{AI12-0112-1} function Is_Root (Position : Cursor) return Boolean
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0136-1} {AI05-0248-1} Is_Root returns True if the Position designates the root node of some tree; and returns False otherwise.

```ada
function Is_Root (Container : Tree; Position : Cursor)
   return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Is_Root returns True if the Position designates the root node of Container; and returns False otherwise.

Ramification: The two parameter Is_Root returns False even if Position is the root of some other tree than Container. 

```ada
{AI12-0112-1} function Is_Leaf (Position : Cursor) return Boolean
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0136-1} Is_Leaf returns True if Position designates a node that does not have any child nodes; and returns False otherwise.

Ramification: Is_Leaf returns False if passed No_Element, since No_Element does not designate a node. Is_Leaf can be passed a cursor that designates the root node; Is_Leaf will return True if passed the root node of an empty tree. 

```ada
function Is_Leaf (Container : Tree; Position : Cursor)
   return Boolean
   with Pre =&gt Meaningful_For (Container, Position) 
                  or else raise Program_Error,
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Is_Leaf returns True if Position designates a node in Container that does not have any child nodes; and returns False otherwise.

```ada
function Is_Ancestor_Of (Container : Tree;
                         Parent   : Cursor;
                         Position : Cursor) return Boolean
   with Pre =&gt (Meaningful_For (Container, Position)
                  or else raise Program_Error) and then
               (Meaningful_For (Container, Parent)
                  or else raise Program_Error),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Is_Ancestor_Of returns True if Parent designates an ancestor node of Position (including Position itself), and returns False otherwise.

```ada
{AI12-0112-1} function Root (Container : Tree) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Post =&gt Root'Result /= No_Element and then
                not Has_Element (Container, Root'Result);

```

{AI05-0136-1} Root returns a cursor that designates the root node of Container.

Ramification: There is always a root node, even in an empty container, so this function never returns No_Element. 

```ada
{AI12-0112-1} procedure Clear (Container : in out Tree)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Container)
                    or else raise Program_Error,
        Post =&gt Node_Count (Container) = 1;

```

{AI05-0136-1} Removes all the elements from Container.

Ramification: The root node is not removed; all trees have a root node. 

```ada
function Element (Position : Cursor) return Element_Type
   with Pre =&gt (Position /= No_Element or else
                  raise Constraint_Error) and then
               (Has_Element (Position) or else raise Program_Error),
        Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type;

```

{AI05-0136-1} {AI12-0112-1} Element returns the element designated by Position.

Ramification: The root node does not contain an element, so that value cannot be read or written. 

```ada
function Element (Container : Tree;
                  Position  : Cursor) return Element_Type
   with Pre =&gt (Position /= No_Element
                  or else raise Constraint_Error) and then
               (Has_Element (Container, Position)  
                  or else raise Program_Error),
        Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type;

```

{AI12-0112-1} Element returns the element designated by Position in Container.

```ada
procedure Replace_Element (Container : in out Tree;
                           Position  : in     Cursor;
                           New_item  : in     Element_Type)
   with Pre  =&gt (not Tampering_With_Elements_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI05-0136-1} {AI05-0264-1} {AI12-0112-1} {AI12-0196-1} Replace_Element assigns the value New_Item to the element designated by Position. For the purposes of determining whether the parameters overlap in a call to Replace_Element, the Container parameter is not considered to overlap with any object [(including itself)].

```ada
procedure Query_Element
  (Position : in Cursor;
   Process  : not null access procedure (Element : in Element_Type))
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Position) 
                   or else raise Program_Error),
     Global =&gt in all;

```

{AI05-0136-1} {AI05-0265-1} {AI12-0112-1} Query_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of the tree that contains the element designated by Position is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Query_Element
  (Container : in Tree;
   Position  : in Cursor;
   Process   : not null access procedure (Element : in Element_Type))
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI12-0112-1} Query_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

```ada
procedure Update_Element
  (Container : in out Tree;
   Position  : in     Cursor;
   Process   : not null access procedure
                   (Element : in out Element_Type))
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error);

```

{AI05-0136-1} {AI05-0264-1} {AI05-0265-1} {AI12-0112-1} Update_Element calls Process.all with the element designated by Position as the argument. Tampering with the elements of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

If Element_Type is unconstrained and definite, then the actual Element parameter of Process.all shall be unconstrained.

Ramification: This means that the elements cannot be directly allocated from the heap; it must be possible to change the discriminants of the element in place. 

```ada
{AI12-0112-1} type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI12-0112-1} type Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

{AI05-0212-1} The types Constant_Reference_Type and Reference_Type need finalization.

This paragraph was deleted.{AI12-0112-1} 

Reason: It is expected that Reference_Type (and Constant_Reference_Type) will be a controlled type, for which finalization will have some action to terminate the tampering check for the associated container. If the object is created by default, however, there is no associated container. Since this is useless, and supporting this case would take extra work, we define it to raise an exception. 

```ada
{AI12-0112-1} function Constant_Reference (Container : aliased in Tree;
                             Position  : in Cursor)
   return Constant_Reference_Type
   with Pre  =&gt (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Constant_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read access to an individual element of a tree given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Constant_Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Constant_Reference exists and has not been finalized.

```ada
{AI12-0112-1} function Reference (Container : aliased in out Tree;
                    Position  : in Cursor)
   return Reference_Type
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position) 
                   or else raise Program_Error),
        Post   =&gt Tampering_With_Cursors_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} {AI05-0269-1} This function (combined with the Variable_Indexing and Implicit_Dereference aspects) provides a convenient way to gain read and write access to an individual element of a tree given a cursor.

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} Reference returns an object whose discriminant is an access value that designates the element designated by Position. Tampering with the elements of Container is prohibited while the object returned by Reference exists and has not been finalized.

```ada
{AI12-0112-1} procedure Assign (Target : in out Tree; Source : in Tree)
   with Pre  =&gt not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error,
        Post =&gt Node_Count (Source) = Node_Count (Target);

```

{AI05-0136-1} {AI05-0248-1} If Target denotes the same object as Source, the operation has no effect. Otherwise, the elements of Source are copied to Target as for an [assignment_statement](./AA-5.2#S0173) assigning Source to Target.

Ramification: Each element in Target has a parent element that corresponds to the parent element of the Source element, and has child elements that correspond to the child elements of the Source element. 

Discussion: {AI05-0005-1} This routine exists for compatibility with the bounded tree container. For an unbounded tree, Assign(A, B) and A := B behave identically. For a bounded tree, := will raise an exception if the container capacities are different, while Assign will not raise an exception if there is enough room in the target. 

```ada
{AI12-0112-1} function Copy (Source : Tree) return Tree
   with Post =&gt
           Node_Count (Copy'Result) = Node_Count (Source) and then
           not Tampering_With_Elements_Prohibited (Copy'Result) and then
           not Tampering_With_Cursors_Prohibited (Copy'Result);

```

{AI05-0136-1} Returns a tree with the same structure as Source and whose elements are initialized from the corresponding elements of Source.

```ada
{AI12-0112-1} procedure Move (Target : in out Tree;
                Source : in out Tree)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                   or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                   or else raise Program_Error),
        Post =&gt (if not Target'Has_Same_Storage (Source) then
               Node_Count (Target) = Node_Count (Source'Old) and then
               Node_Count (Source) = 1);

```

{AI05-0136-1} {AI05-0248-1} If Target denotes the same object as Source, then the operation has no effect. Otherwise, Move first calls Clear (Target). Then, the nodes other than the root node in Source are moved to Target (in the same positions). After Move completes, Node_Count (Target) is the number of nodes originally in Source, and Node_Count (Source) is 1.

```ada
procedure Delete_Leaf (Container : in out Tree;
                       Position  : in out Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error) and then
                (Is_Leaf (Container, Position)
                   or else raise Constraint_Error),
        Post =&gt
           Node_Count (Container)'Old = Node_Count (Container) + 1 and then
           Position = No_Element;

```

{AI05-0136-1} {AI05-0248-1} {AI12-0112-1} Delete_Leaf removes (from Container) the element designated by Position, and Position is set to No_Element.

Ramification: The check on Position checks that the cursor does not belong to some other Container. This check implies that a reference to the container is included in the cursor value. This wording is not meant to require detection of dangling cursors; such cursors are defined to be invalid, which means that execution is erroneous, and any result is allowed (including not raising an exception).

The root node cannot be deleted. 

```ada
procedure Delete_Subtree (Container : in out Tree;
                          Position  : in out Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error),
        Post =&gt Node_Count (Container)'Old = Node_Count (Container) +
                   Subtree_Node_Count (Container, Position)'Old and then
                Position = No_Element;

```

{AI05-0136-1} {AI05-0264-1} {AI05-0269-1} {AI12-0112-1} Delete_Subtree removes (from Container) the subtree designated by Position (that is, all descendants of the node designated by Position including the node itself), and Position is set to No_Element.

Ramification: The root node cannot be deleted. To delete the entire contents of the tree, call Clear(Container). 

```ada
procedure Swap (Container : in out Tree;
                I, J      : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (I /= No_Element or else Constraint_Error) and then
                (J /= No_Element or else Constraint_Error) and then
                (Has_Element (Container, I)
                   or else raise Program_Error) and then
                (Has_Element (Container, J)
                   or else raise Program_Error);

```

{AI05-0136-1} {AI12-0112-1} Swap exchanges the values of the elements designated by I and J.

Ramification: After a call to Swap, I designates the element value previously designated by J, and J designates the element value previously designated by I. The position of the elements do not change; for instance, the parent node and the first child node of I are unchanged by the operation.

The root nodes do not contain element values, so they cannot be swapped. 

To be honest: The implementation is not required to actually copy the elements if it can do the swap some other way. But it is allowed to copy the elements if needed. 

```ada
{AI12-0112-1} function Find (Container : Tree;
               Item      : Element_Type)
   return Cursor
   with Post =&gt (if Find'Result /= No_Element
                 then Has_Element (Container, Find'Result));

```

{AI05-0136-1} {AI05-0262-1} Find searches the elements of Container for an element equal to Item (using the generic formal equality operator). The search starts at the root node. The search traverses the tree in a depth-first order. If no equal element is found, then Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

```ada
function Find_In_Subtree (Position : Cursor;
                          Item     : Element_Type)
   return Cursor
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Post =&gt (if Find_In_Subtree'Result = No_Element
                 then Has_Element (Find_In_Subtree'Result)),
        Global =&gt in all;

```

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI12-0112-1} Find_In_Subtree searches the subtree rooted by Position for an element equal to Item (using the generic formal equality operator). The search starts at the element designated by Position. The search traverses the subtree in a depth-first order. If no equal element is found, then Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

Ramification: Find_In_Subtree does not check any siblings of the element designated by Position. The root node does not contain an element, and therefore it can never be returned, but it can be explicitly passed to Position. 

```ada
{AI12-0112-1} function Find_In_Subtree (Container : Tree;
                          Position  : Cursor;
                          Item      : Element_Type)
   return Cursor
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Position)
                   or else raise Program_Error),
        Post =&gt (if Find_In_Subtree'Result = No_Element
                 then Has_Element (Container, Find_In_Subtree'Result));

```

{AI12-0112-1} Find_In_Subtree searches the subtree of Container rooted by Position for an element equal to Item (using the generic formal equality operator). The search starts at the element designated by Position. The search traverses the subtree in a depth-first order. If no equal element is found, then Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

```ada
function Ancestor_Find (Position : Cursor;
                        Item     : Element_Type)
   return Cursor
   with Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Post =&gt (if Ancestor_Find'Result = No_Element
                 then Has_Element (Container, Ancestor_Find'Result)),
        Global =&gt in all;

```

{AI05-0136-1} {AI05-0248-1} {AI12-0112-1} Ancestor_Find searches for an element equal to Item (using the generic formal equality operator). The search starts at the node designated by Position, and checks each ancestor proceeding toward the root of the subtree. If no equal element is found, then Ancestor_Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered. 

Ramification: {AI05-0248-1} No_Element is returned if Position is the root node. 

```ada
{AI12-0112-1} function Ancestor_Find (Container : Tree;
                        Position  : Cursor;
                        Item      : Element_Type)
   return Cursor
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Position)
                   or else raise Program_Error),
        Post =&gt (if Ancestor_Find'Result = No_Element
                 then Has_Element (Container, Ancestor_Find'Result));

```

{AI12-0112-1} Ancestor_Find searches for an element equal to Item (using the generic formal equality operator). The search starts at the node designated by Position in Container, and checks each ancestor proceeding toward the root of the subtree. If no equal element is found, then Ancestor_Find returns No_Element. Otherwise, it returns a cursor designating the first equal element encountered.

```ada
function Contains (Container : Tree;
                   Item      : Element_Type) return Boolean;

```

{AI05-0136-1} Equivalent to Find (Container, Item) /= No_Element.

```ada
{AI12-0112-1} procedure Iterate
  (Container : in Tree;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit;

```

{AI05-0136-1} {AI05-0265-1} {AI12-0069-1} Iterate calls Process.all with a cursor that designates each element in Container, starting from the root node and proceeding in a depth-first order. Tampering with the cursors of Container is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

Ramification: Process is not called with the root node, which does not have an associated element. 

Implementation Note: The purpose of the tamper with cursors check is to prevent erroneous execution from the Position parameter of Process.all becoming invalid. This check takes place when the operations that tamper with the cursors of the container are called. The check cannot be made later (say in the body of Iterate), because that could cause the Position cursor to be invalid and potentially cause execution to become erroneous - defeating the purpose of the check.

See Iterate for vectors (A.18.2) for a suggested implementation of the check. 

```ada
procedure Iterate_Subtree
  (Position  : in Cursor;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit,
        Pre  =&gt Position /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI05-0136-1} {AI05-0265-1} {AI12-0069-1} {AI12-0112-1} Iterate_Subtree calls Process.all with a cursor that designates each element in the subtree rooted by the node designated by Position, starting from the node designated by Position and proceeding in a depth-first order. Tampering with the cursors of the tree that contains the element designated by Position is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

Ramification: Position can be passed a cursor designating the root node; in that case, Process is not called with the root node, which does not have an associated element. 

```ada
procedure Iterate_Subtree
  (Container : in Tree;
   Position  : in Cursor;
   Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit,
        Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Position)
                   or else raise Program_Error);

```

{AI12-0112-1} Iterate_Subtree calls Process.all with a cursor that designates each element in the subtree rooted by the node designated by Position in Container, starting from the node designated by Position and proceeding in a depth-first order. Tampering with the cursors of the tree that contains the element designated by Position is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

```ada
{AI12-0112-1} function Iterate (Container : in Tree)
   return Tree_Iterator_Interfaces.Parallel_Iterator'Class
   with Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0069-1} {AI12-0266-1} Iterate returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each element in Container, starting from the root node and proceeding in a depth-first order when used as a forward iterator, and processing all nodes concurrently when used as a parallel iterator. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

Discussion: Exits are allowed from the loops created using the iterator objects. In particular, to stop the iteration at a particular cursor, just add 

```ada
exit when Cur = Stop;

```

in the body of the loop (assuming that Cur is the loop parameter and Stop is the cursor that you want to stop at). 

```ada
function Iterate_Subtree (Position : in Cursor)
   return Tree_Iterator_Interfaces.Parallel_Iterator'Class
   with Pre    =&gt Position /= No_Element or else raise Constraint_Error,
        Global =&gt in all;

```

{AI05-0212-1} {AI05-0265-1} {AI05-0269-1} {AI12-0069-1} {AI12-0112-1} {AI12-0266-1} Iterate_Subtree returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each element in the subtree rooted by the node designated by Position, starting from the node designated by Position and proceeding in a depth-first order when used as a forward iterator, and processing all nodes in the subtree concurrently when used as a parallel iterator. Tampering with the cursors of the container that contains the node designated by Position is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

```ada
function Iterate_Subtree (Container : in Tree; Position : in Cursor)
   return Tree_Iterator_Interfaces.Parallel_Iterator'Class
   with Pre  =&gt (Position /= No_Element 
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Position)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI12-0112-1} Iterate_Subtree returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each element in the subtree rooted by the node designated by Position in Container, starting from the node designated by Position and proceeding in a depth-first order when used as a forward iterator, and processing all nodes in the subtree concurrently when used as a parallel iterator. Tampering with the cursors of the container that contains the node designated by Position is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

```ada
{AI12-0112-1} function Child_Count (Parent : Cursor) return Count_Type
   with Post =&gt (if Parent = No_Element then Child_Count'Result = 0),
         Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0136-1} Child_Count returns the number of child nodes of the node designated by Parent.

```ada
function Child_Count (Container : Tree; Parent : Cursor)
   return Count_Type
   with Pre  =&gt Meaningful_For (Container, Parent) 
                   or else raise Program_Error,
        Post =&gt (if Parent = No_Element then Child_Count'Result = 0),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Child_Count returns the number of child nodes of the node designated by Parent in Container.

```ada
function Child_Depth (Parent, Child : Cursor) return Count_Type
   with Pre  =&gt (Parent /= No_Element and then Child /= No_Element)
                   or else raise Constraint_Error,
     Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0136-1} {AI05-0262-1} {AI12-0112-1} Child_Depth returns the number of ancestor nodes of Child (including Child itself), up to but not including Parent; Program_Error is propagated if Parent is not an ancestor of Child.

Ramification: Program_Error is propagated if Parent and Child are nodes in different containers.

Child_Depth (Root (Some_Tree), Child) + 1 = Depth (Child) as the root is not counted. 

```ada
function Child_Depth (Container : Tree; Parent, Child : Cursor)
   return Count_Type
   with Pre  =&gt ((Parent /= No_Element and then Child /= No_Element)
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error) and then
                (Meaningful_For (Container, Child)
                   or else raise Program_Error),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Child_Depth returns the number of ancestor nodes of Child within Container (including Child itself), up to but not including Parent; Program_Error is propagated if Parent is not an ancestor of Child.

```ada
procedure Insert_Child (Container : in out Tree;
                        Parent    : in     Cursor;
                        Before    : in     Cursor;
                        New_Item  : in     Element_Type;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error) and then
                (Meaningful_For (Container, Before)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Container.Parent (Before) = Parent
                   or else raise Constraint_Error),
        Post =&gt Node_Count (Container) =
                Node_Count (Container)'Old + Count;

```

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI12-0112-1} Insert_Child allocates Count nodes containing copies of New_Item and inserts them as children of Parent. If Parent already has child nodes, then the new nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the new nodes are inserted after the last existing child node of Parent. Any exception raised during allocation of internal storage is propagated, and Container is not modified.

```ada
procedure Insert_Child (Container : in out Tree;
                        Parent    : in     Cursor;
                        Before    : in     Cursor;
                        New_Item  : in     Element_Type;
                        Position  :    out Cursor;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error) and then
                (Meaningful_For (Container, Before)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Container.Parent (Before) = Parent
                   or else raise Constraint_Error),
        Post =&gt (Node_Count (Container) =
                 Node_Count (Container)'Old + Count) and then
                 Has_Element (Container, Position);

```

{AI05-0136-1} {AI05-0248-1} {AI05-0257-1} {AI05-0262-1} {AI12-0112-1} Insert_Child allocates Count nodes containing copies of New_Item and inserts them as children of Parent. If Parent already has child nodes, then the new nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the new nodes are inserted after the last existing child node of Parent. Position designates the first newly-inserted node, or if Count equals 0, then Position is assigned the value of Before. Any exception raised during allocation of internal storage is propagated, and Container is not modified.

```ada
procedure Insert_Child (Container : in out Tree;
                        Parent    : in     Cursor;
                        Before    : in     Cursor;
                        Position  :    out Cursor;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error) and then
                (Meaningful_For (Container, Before)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Container.Parent (Before) = Parent
                   or else raise Constraint_Error),
        Post =&gt (Node_Count (Container) =
                 Node_Count (Container)'Old + Count) and then
                 Has_Element (Container, Position);

```

{AI05-0136-1} {AI05-0257-1} {AI05-0262-1} {AI05-0264-1} {AI12-0112-1} Insert_Child allocates Count nodes, the elements contained in the new nodes are initialized by default (see 3.3.1), and the new nodes are inserted as children of Parent. If Parent already has child nodes, then the new nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the new nodes are inserted after the last existing child node of Parent. Position designates the first newly-inserted node, or if Count equals 0, then Position is assigned the value of Before. Any exception raised during allocation of internal storage is propagated, and Container is not modified.

```ada
{AI12-0112-1} procedure Prepend_Child (Container : in out Tree;
                         Parent    : in     Cursor;
                         New_Item  : in     Element_Type;
                         Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error),
        Post =&gt Node_Count (Container) =
                Node_Count (Container)'Old + Count;

```

{AI05-0136-1} Equivalent to Insert_Child (Container, Parent, First_Child (Container, Parent), New_Item, Count).

```ada
{AI12-0112-1} procedure Append_Child (Container : in out Tree;
                        Parent    : in     Cursor;
                        New_Item  : in     Element_Type;
                        Count     : in     Count_Type := 1)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error),
        Post =&gt Node_Count (Container) =
                Node_Count (Container)'Old + Count;

```

{AI05-0136-1} {AI05-0269-1} Equivalent to Insert_Child (Container, Parent, No_Element, New_Item, Count).

```ada
procedure Delete_Children (Container : in out Tree;
                           Parent    : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error),
        Post =&gt (Node_Count (Container) = Node_Count (Container)'Old -
                   Child_Count (Container, Parent)'Old) and then
                 Child_Count (Container, Parent) = 0;

```

{AI05-0136-1} {AI12-0112-1} Delete_Children removes (from Container) all of the descendants of Parent other than Parent itself.

Discussion: This routine deletes all of the child subtrees of Parent at once. Use Delete_Subtree to delete an individual subtree. 

```ada
procedure Copy_Subtree (Target   : in out Tree;
                        Parent   : in     Cursor;
                        Before   : in     Cursor;
                        Source   : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                 (Parent /= No_Element
                    or else raise Constraint_Error) and then
                 (Meaningful_For (Target, Parent)
                    or else raise Program_Error) and then
                 (Meaningful_For (Target, Before)
                    or else raise Program_Error) and then
                 (Before = No_Element or else
                    Target.Parent (Before) = Parent
                    or else raise Constraint_Error) and then
                 (not Is_Root (Source)
                    or else raise Constraint_Error),
        Post =&gt Node_Count (Target) =
                Node_Count (Target)'Old + Subtree_Node_Count (Source),
        Global =&gt in all;

```

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI12-0112-1} If Source is equal to No_Element, then the operation has no effect. Otherwise, the subtree rooted by Source (which can be from any tree; it does not have to be a subtree of Target) is copied (new nodes are allocated to create a new subtree with the same structure as the Source subtree, with each element initialized from the corresponding element of the Source subtree) and inserted into Target as a child of Parent. If Parent already has child nodes, then the new nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the new nodes are inserted after the last existing child node of Parent. The parent of the newly created subtree is set to Parent, and the overall count of Target is incremented by Subtree_Node_Count (Source). Any exception raised during allocation of internal storage is propagated, and Container is not modified.

Discussion: {AI12-0112-1} We only need one routine here, as the source object is not modified, so we can use the same routine for both copying within and between containers. However, that requires a contract that allows reading of any container of the correct type, so we provide two other routines wuth more restrictive contracts. 

Ramification: We do not allow copying a subtree that includes a root node, as that would require inserting a node with no value in the middle of the target tree. To copy an entire tree to another tree object, use Copy. 

```ada
procedure Copy_Local_Subtree (Target   : in out Tree;
                              Parent   : in     Cursor;
                              Before   : in     Cursor;
                              Source   : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                 (Parent /= No_Element
                    or else raise Constraint_Error) and then
                 (Meaningful_For (Target, Parent)
                    or else raise Program_Error) and then
                 (Meaningful_For (Target, Before)
                    or else raise Program_Error) and then
                 (Before = No_Element or else
                    Target.Parent (Before) = Parent
                    or else raise Constraint_Error) and then
                 (Meaningful_For (Target, Source)
                    or else raise Program_Error) and then
                 (not Is_Root (Source)
                    or else raise Constraint_Error),
        Post =&gt Node_Count (Target) = Node_Count (Target)'Old +
                   Subtree_Node_Count (Target, Source);

```

{AI12-0112-1} If Source is equal to No_Element, then the operation has no effect. Otherwise, the subtree rooted by Source in Target is copied (new nodes are allocated to create a new subtree with the same structure as the Source subtree, with each element initialized from the corresponding element of the Source subtree) and inserted into Target as a child of Parent. If Parent already has child nodes, then the new nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the new nodes are inserted after the last existing child node of Parent. The parent of the newly created subtree is set to Parent. Any exception raised during allocation of internal storage is propagated, and Container is not modified.

```ada
procedure Copy_Subtree (Target   : in out Tree;
                        Parent   : in     Cursor;
                        Before   : in     Cursor;
                        Source   : in     Tree;
                        Subtree  : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                 (Parent /= No_Element
                    or else raise Constraint_Error) and then
                 (Meaningful_For (Target, Parent)
                    or else raise Program_Error) and then
                 (Meaningful_For (Target, Before)
                    or else raise Program_Error) and then
                 (Before = No_Element or else
                    Target.Parent (Before) = Parent
                    or else raise Constraint_Error) and then
                 (Meaningful_For (Source, Subtree)
                    or else raise Program_Error) and then
                 (not Is_Root (Source, Subtree)
                    or else raise Constraint_Error),
        Post =&gt Node_Count (Target) = Node_Count (Target)'Old +
                   Subtree_Node_Count (Source, Subtree);

```

{AI12-0112-1} If Subtree is equal to No_Element, then the operation has no effect. Otherwise, the subtree rooted by Subtree in Source is copied (new nodes are allocated to create a new subtree with the same structure as the Subtree, with each element initialized from the corresponding element of the Subtree) and inserted into Target as a child of Parent. If Parent already has child nodes, then the new nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the new nodes are inserted after the last existing child node of Parent. The parent of the newly created subtree is set to Parent. Any exception raised during allocation of internal storage is propagated, and Container is not modified.

```ada
procedure Splice_Subtree (Target   : in out Tree;
                          Parent   : in     Cursor;
                          Before   : in     Cursor;
                          Source   : in out Tree;
                          Position : in out Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                    or else raise Program_Error) and then
                (Parent /= No_Element
                    or else raise Constraint_Error) and then
                (Meaningful_For (Target, Parent)
                    or else raise Program_Error) and then
                (Meaningful_For (Target, Before)
                    or else raise Program_Error) and then
                (Before = No_Element or else
                 Target.Parent (Before) /= Parent
                    or else raise Constraint_Error) and then
                (Position /= No_Element
                    or else raise Constraint_Error) and then
                (Has_Element (Source, Position)
                    or else raise Program_Error) and then
                (Target'Has_Same_Storage (Source) or else
                 Position = Before or else
                 Is_Ancestor_Of (Target, Position, Parent)
                    or else raise Constraint_Error),
        Post =&gt (declare
                    Org_Sub_Count renames
                        Subtree_Node_Count (Source, Position)'Old;
                    Org_Target_Count renames Node_Count (Target)'Old;
                 begin
                   (if not Target'Has_Same_Storage (Source) then
                       Node_Count (Target) = Org_Target_Count +
                          Org_Sub_Count and then
                       Node_Count (Source) = Node_Count (Source)'Old -
                          Org_Sub_Count and then
                       Has_Element (Target, Position)
                    else
                       Target.Parent (Position) = Parent and then
                       Node_Count (Target) = Org_Target_Count));

```

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI05-0269-1} {AI12-0112-1} If Source denotes the same object as Target, then: if Position equals Before there is no effect; otherwise, the subtree rooted by the element designated by Position is moved to be a child of Parent. If Parent already has child nodes, then the moved nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the moved nodes are inserted after the last existing child node of Parent. In each of these cases, Position and the count of Target are unchanged, and the parent of the element designated by Position is set to Parent.

Reason: We can't allow moving the subtree of Position to a proper descendant node of the subtree, as the descendant node will be part of the subtree being moved. The result would be a circularly linked tree, or one with inaccessible nodes. Thus we have to check Position against Parent, even though such a check is O(Depth(Source)). 

{AI05-0136-1} {AI05-0248-1} Otherwise (if Source does not denote the same object as Target), the subtree designated by Position is removed from Source and moved to Target. The subtree is inserted as a child of Parent. If Parent already has child nodes, then the moved nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the moved nodes are inserted after the last existing child node of Parent. In each of these cases, the count of Target is incremented by Subtree_Node_Count (Position), and the count of Source is decremented by Subtree_Node_Count (Position), Position is updated to represent an element in Target.

Ramification: If Source is the same as Target, and Position = Before, or Next_Sibling(Position) = Before, Splice_Subtree has no effect, as the subtree does not have to move to meet the postcondition.

We do not allow splicing a subtree that includes a root node, as that would require inserting a node with no value in the middle of the target tree. Splice the children of the root node instead.

For this reason there is no operation to splice an entire tree, as that would necessarily involve splicing a root node. 

```ada
procedure Splice_Subtree (Container: in out Tree;
                          Parent   : in     Cursor;
                          Before   : in     Cursor;
                          Position : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error) and then
                (Meaningful_For (Container, Before)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Container.Parent (Before) /= Parent
                   or else raise Constraint_Error) and then
                (Position /= No_Element
                   or else raise Constraint_Error) and then
                (Has_Element (Container, Position)
                   or else raise Program_Error) and then
                (Position = Before or else
                 Is_Ancestor_Of (Container, Position, Parent)
                   or else raise Constraint_Error),
        Post =&gt (Node_Count (Container) =
                   Node_Count (Container)'Old and then
                 Container.Parent (Position) = Parent);

```

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI05-0269-1} {AI12-0112-1} If Position equals Before, there is no effect. Otherwise, the subtree rooted by the element designated by Position is moved to be a child of Parent. If Parent already has child nodes, then the moved nodes are inserted prior to the node designated by Before, or, if Before equals No_Element, the moved nodes are inserted after the last existing child node of Parent. The parent of the element designated by Position is set to Parent.

Reason: We can't allow moving the subtree of Position to a proper descendant node of the subtree, as the descendant node will be part of the subtree being moved. 

```ada
procedure Splice_Children (Target          : in out Tree;
                           Target_Parent   : in     Cursor;
                           Before          : in     Cursor;
                           Source          : in out Tree;
                           Source_Parent   : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
                    or else raise Program_Error) and then
                (not Tampering_With_Cursors_Prohibited (Source)
                    or else raise Program_Error) and then
                (Target_Parent /= No_Element
                    or else raise Constraint_Error) and then
                (Meaningful_For (Target, Target_Parent)
                    or else raise Program_Error) and then
                (Meaningful_For (Target, Before)
                    or else raise Program_Error) and then
                (Source_Parent /= No_Element
                    or else raise Constraint_Error) and then
                (Meaningful_For (Source, Source_Parent)
                    or else raise Program_Error) and then
                (Before = No_Element or else
                 Parent (Target, Before) /= Target_Parent
                    or else raise Constraint_Error) and then
                (Target'Has_Same_Storage (Source) or else
                 Target_Parent = Source_Parent or else
                 Is_Ancestor_Of (Target, Source_Parent, Target_Parent)
                    or else raise Constraint_Error),
        Post =&gt (declare
                    Org_Child_Count renames
                       Child_Count (Source, Source_Parent)'Old;
                    Org_Target_Count renames Node_Count (Target)'Old;
                 begin
                   (if not Target'Has_Same_Storage (Source) then
                       Node_Count (Target) = Org_Target_Count +
                          Org_Child_Count and then
                       Node_Count (Source) = Node_Count (Source)'Old -
                          Org_Child_Count
                    else
                       Node_Count (Target) = Org_Target_Count));

```

This paragraph was deleted.{AI05-0136-1} {AI05-0262-1} {AI12-0112-1} 

If Source denotes the same object as Target, then:

if Target_Parent equals Source_Parent there is no effect; else

This paragraph was deleted.{AI05-0136-1} {AI05-0269-1} {AI12-0112-1} 

{AI05-0136-1} {AI05-0248-1} {AI05-0269-1} the child elements (and the further descendants) of Source_Parent are moved to be child elements of Target_Parent. If Target_Parent already has child elements, then the moved elements are inserted prior to the node designated by Before, or, if Before equals No_Element, the moved elements are inserted after the last existing child node of Target_Parent. The parent of each moved child element is set to Target_Parent. 

Reason: We can't allow moving the children of Source_Parent to a proper descendant node, as the descendant node will be part of one of the subtrees being moved. 

{AI05-0136-1} {AI05-0248-1} {AI05-0269-1} Otherwise (if Source does not denote the same object as Target), the child elements (and the further descendants) of Source_Parent are removed from Source and moved to Target. The child elements are inserted as children of Target_Parent. If Target_Parent already has child elements, then the moved elements are inserted prior to the node designated by Before, or, if Before equals No_Element, the moved elements are inserted after the last existing child node of Target_Parent. In each of these cases, the overall count of Target is incremented by Subtree_Node_Count (Source_Parent)-1, and the overall count of Source is decremented by Subtree_Node_Count (Source_Parent)-1.

Ramification: The node designated by Source_Parent is not moved, thus we never need to update Source_Parent.

Move (Target, Source) could be written Splice_Children (Target, Target.Root, No_Element, Source, Source.Root); 

```ada
procedure Splice_Children (Container       : in out Tree;
                           Target_Parent   : in     Cursor;
                           Before          : in     Cursor;
                           Source_Parent   : in     Cursor)
   with Pre  =&gt (not Tampering_With_Cursors_Prohibited (Container)
                   or else raise Program_Error) and then
                (Target_Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Target_Parent)
                   or else raise Program_Error) and then
                (Meaningful_For (Container, Before)
                   or else raise Program_Error) and then
                (Source_Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Source_Parent)
                   or else raise Program_Error) and then
                (Before = No_Element or else
                 Parent (Container, Before) /= Target_Parent
                   or else raise Constraint_Error) and then
                (Target_Parent = Source_Parent or else
                 Is_Ancestor_Of (Container, Source_Parent, Target_Parent)
                   or else raise Constraint_Error),
        Post =&gt Node_Count (Container) = Node_Count (Container)'Old;

```

{AI05-0136-1} {AI05-0248-1} {AI05-0262-1} {AI05-0264-1} {AI05-0269-1} {AI12-0112-1} If Target_Parent equals Source_Parent there is no effect. Otherwise, the child elements (and the further descendants) of Source_Parent are moved to be child elements of Target_Parent. If Target_Parent already has child elements, then the moved elements are inserted prior to the node designated by Before, or, if Before equals No_Element, the moved elements are inserted after the last existing child node of Target_Parent. The parent of each moved child element is set to Target_Parent.

```ada
function Parent (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element or else
                    Is_Root (Position) then Parent'Result = No_Element);

```

{AI05-0136-1} {AI12-0112-1} Returns a cursor designating the parent node of the node designated by Position.

```ada
function Parent (Container : Tree;
                 Position  : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Meaningful_For (Container, Position) 
                   or else raise Program_Error,
        Post =&gt (if Position = No_Element or else
                   Is_Root (Container, Position)
                   then Parent'Result = No_Element
                 else Has_Element (Container, Parent'Result));

```

{AI12-0112-1} Returns a cursor designating the parent node of the node designated by Position in Container.

```ada
function First_Child (Parent : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Pre  =&gt Parent /= No_Element or else raise Constraint_Error;

```

{AI05-0136-1} {AI12-0112-1} First_Child returns a cursor designating the first child node of the node designated by Parent; if there is no such node, No_Element is returned.

```ada
function First_Child (Container : Tree;
                      Parent    : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error),
        Post =&gt First_Child'Result = No_Element or else
                Has_Element (Container, First_Child'Result);

```

{AI12-0112-1} First_Child returns a cursor designating the first child node of the node designated by Parent in Container; if there is no such node, No_Element is returned.

```ada
{AI12-0112-1} function First_Child_Element (Parent : Cursor) return Element_Type
   with Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type,
        Pre  =&gt (Parent /= No_Element and then
                 Last_Child (Parent) /= No_Element)
                     or else raise Constraint_Error;

```

{AI05-0136-1} Equivalent to Element (First_Child (Parent)).

```ada
function First_Child_Element (Container : Tree;
                              Parent    : Cursor) return Element_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type,
        Pre  =&gt (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error) and then
                (First_Child (Container, Parent) /= No_Element
                   or else raise Constraint_Error);

```

{AI12-0112-1} Equivalent to Element (Container, First_Child (Container, Parent)).

```ada
function Last_Child (Parent : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Pre  =&gt Parent /= No_Element or else raise Constraint_Error;

```

{AI05-0136-1} {AI12-0112-1} Last_Child returns a cursor designating the last child node of the node designated by Parent; if there is no such node, No_Element is returned.

```ada
function Last_Child (Container : Tree;
                     Parent    : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error),
        Post =&gt Last_Child'Result = No_Element or else
                Has_Element (Container, Last_Child'Result);

```

{AI12-0112-1} Last_Child returns a cursor designating the last child node of the node designated by Parent in Container; if there is no such node, No_Element is returned.

```ada
{AI12-0112-1} function Last_Child_Element (Parent : Cursor) return Element_Type
   with Nonblocking, Global =&gt in all, Use_Formal =&gt Element_Type,
        Pre  =&gt (Parent /= No_Element and then
                 Last_Child (Parent) /= No_Element)
                     or else raise Constraint_Error;

```

{AI05-0136-1} Equivalent to Element (Last_Child (Parent)).

```ada
function Last_Child_Element (Container : Tree;
                             Parent    : Cursor) return Element_Type
   with Nonblocking, Global =&gt null, Use_Formal =&gt Element_Type,
        Pre  =&gt (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error) and then
                (Last_Child (Container, Parent) /= No_Element
                   or else raise Constraint_Error);

```

{AI12-0112-1} Equivalent to Element (Container, Last_Child (Container, Parent)).

```ada
{AI12-0112-1} function Next_Sibling (Position : Cursor) return Cursor
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element
                    then Next_Sibling'Result = No_Element);

```

{AI05-0136-1} If Position equals No_Element or designates the last child node of its parent, then Next_Sibling returns the value No_Element. Otherwise, it returns a cursor that designates the successor (with the same parent) of the node designated by Position.

```ada
function Next_Sibling (Container : Tree;
                       Position  : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Meaningful_For (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Next_Sibling'Result = No_Element then
                   Position = No_Element or else
                   Is_Root (Container, Position) or else
                   Last_Child (Container, Parent (Container, Position))
                      = Position
                 else Has_Element (Container, Next_Sibling'Result));

```

{AI12-0112-1} Next_Sibling returns a cursor that designates the successor (with the same parent) of the node designated by Position in Container.

```ada
{AI12-0112-1} function Previous_Sibling (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null,
        Post =&gt (if Position = No_Element
                    then Previous_Sibling'Result = No_Element);

```

{AI05-0136-1} If Position equals No_Element or designates the first child node of its parent, then Previous_Sibling returns the value No_Element. Otherwise, it returns a cursor that designates the predecessor (with the same parent) of the node designated by Position.

```ada
function Previous_Sibling (Container : Tree;
                           Position  : Cursor) return Cursor
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Meaningful_For (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Previous_Sibling'Result = No_Element then
                   Position = No_Element or else
                   Is_Root (Container, Position) or else
                   First_Child (Container, Parent (Container, Position))
                      = Position
                 else Has_Element (Container, Previous_Sibling'Result));

```

{AI12-0112-1} Previous_Sibling returns a cursor that designates the predecessor (with the same parent) of the node designated by Position in Container.

```ada
{AI12-0112-1} procedure Next_Sibling (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0136-1} Equivalent to Position := Next_Sibling (Position);

```ada
procedure Next_Sibling (Container : in     Tree;
                        Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Meaningful_For (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position));

```

{AI12-0112-1} Equivalent to Position := Next_Sibling (Container, Position);

```ada
{AI12-0112-1} procedure Previous_Sibling (Position : in out Cursor)
   with Nonblocking, Global =&gt in all, Use_Formal =&gt null;

```

{AI05-0136-1} Equivalent to Position := Previous_Sibling (Position);

```ada
procedure Previous_Sibling (Container : in     Tree;
                            Position  : in out Cursor)
   with Nonblocking, Global =&gt null, Use_Formal =&gt null,
        Pre  =&gt Meaningful_For (Container, Position)
                   or else raise Program_Error,
        Post =&gt (if Position /= No_Element
                 then Has_Element (Container, Position);

```

{AI12-0112-1} Equivalent to Position := Previous_Sibling (Container, Position);

```ada
{AI12-0112-1} procedure Iterate_Children
     (Parent  : in Cursor;
      Process : not null access procedure (Position : in Cursor))
   with Allows_Exit,
        Pre    =&gt Parent /= No_Element or else raise Constraint_Error,
        Global =&gt in all, Use_Formal =&gt null;

```

This paragraph was deleted.{AI05-0136-1} {AI05-0248-1} {AI12-0112-1} 

Iterate_Children calls Process.all with a cursor that designates each child node of Parent, starting with the first child node and moving the cursor as per the Next_Sibling function.

{AI05-0265-1} Tampering with the cursors of the tree containing Parent is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

```ada
{AI12-0112-1} procedure Iterate_Children
     (Container : in Tree;
      Parent    : in Cursor;
      Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit,
        Pre  =&gt (Parent /= No_Element 
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error);

```

{AI12-0112-1} Iterate_Children calls Process.all with a cursor that designates each child node of Container and Parent, starting with the first child node and moving the cursor as per the Next_Sibling function.

{AI12-0112-1} Tampering with the cursors of the tree containing Parent is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

```ada
{AI12-0112-1} procedure Reverse_Iterate_Children
     (Parent  : in Cursor;
      Process : not null access procedure (Position : in Cursor))
   with Allows_Exit,
        Pre  =&gt Parent /= No_Element or else raise Constraint_Error,
        Global =&gt in all, Use_Formal =&gt null;

```

This paragraph was deleted.{AI05-0136-1} {AI05-0248-1} {AI12-0112-1} 

Reverse_Iterate_Children calls Process.all with a cursor that designates each child node of Parent, starting with the last child node and moving the cursor as per the Previous_Sibling function.

{AI05-0265-1} Tampering with the cursors of the tree containing Parent is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

```ada
{AI12-0112-1} procedure Reverse_Iterate_Children
     (Container : in Tree;
      Parent    : in Cursor;
      Process   : not null access procedure (Position : in Cursor))
   with Allows_Exit,
        Pre  =&gt (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error);

```

{AI12-0112-1} Reverse_Iterate_Children calls Process.all with a cursor that designates each child node of Container and Parent, starting with the last child node and moving the cursor as per the Previous_Sibling function.

{AI12-0112-1} Tampering with the cursors of the tree containing Parent is prohibited during the execution of a call on Process.all. Any exception raised by Process.all is propagated.

```ada
function Iterate_Children (Container : in Tree; Parent : in Cursor)
   return Tree_Iterator_Interfaces.Parallel_Reversible_Iterator'Class
   with Pre  =&gt (Parent /= No_Element
                   or else raise Constraint_Error) and then
                (Meaningful_For (Container, Parent)
                   or else raise Program_Error),
        Post =&gt Tampering_With_Cursors_Prohibited (Container);

```

{AI05-0212-1} {AI05-0265-1} {AI12-0112-1} {AI12-0266-1} Iterate_Children returns an iterator object (see 5.5.1) that will generate a value for a loop parameter (see 5.5.2) designating each child node of Parent. When used as a forward iterator, the nodes are designated starting with the first child node and moving the cursor as per the function Next_Sibling; when used as a reverse iterator, the nodes are designated starting with the last child node and moving the cursor as per the function Previous_Sibling; when used as a parallel iterator, processing all child nodes concurrently. Tampering with the cursors of Container is prohibited while the iterator object exists (in particular, in the [sequence_of_statements](./AA-5.1#S0166) of the [loop_statement](./AA-5.5#S0178) whose [iterator_specification](./AA-5.5#S0183) denotes this object). The iterator object needs finalization.

{AI12-0111-1} The nested package Multiway_Trees.Stable provides a type Stable.Tree that represents a stable tree, which is one that cannot grow and shrink. Such a tree can be created by calling the Copy function, or by establishing a stabilized view of an ordinary tree.

{AI12-0111-1} The subprograms of package Containers.Multiway_Trees that have a parameter or result of type tree are included in the nested package Stable with the same specification, except that the following are omitted:

Tampering_With_Cursors_Prohibited, Tampering_With_Elements_Prohibited, Assign, Move, Clear, Delete_Leaf, Insert_Child, Delete_Children, Delete_Subtree, Copy_Subtree, Copy_Local_Subtree, Splice_Subtree, and Splice_Children 

Ramification: The names Tree and Cursor mean the types declared in the nested package in these subprogram specifications. 

Reason: The omitted routines are those that tamper with cursors or elements (or test that state). The model is that it is impossible to tamper with cursors or elements of a stable view since no such operations are included. Thus tampering checks are not needed for a stable view, and we omit the operations associated with those checks. 

{AI12-0111-1} The operations of this package are equivalent to those for ordinary trees, except that the calls to Tampering_With_Cursors_Prohibited and Tampering_With_Elements_Prohibited that occur in preconditions are replaced by False, and any that occur in postconditions are replaced by True.

{AI12-0111-1} {AI12-0439-1} If a stable tree is declared with the Base discriminant designating a pre-existing ordinary tree, the stable tree represents a stabilized view of the underlying ordinary tree, and any operation on the stable tree is reflected on the underlying ordinary tree. While a stabilized view exists, any operation that tampers with elements performed on the underlying tree is prohibited. The finalization of a stable tree that provides such a view removes this restriction on the underlying ordinary tree [(though some other restriction can exist due to other concurrent iterations or stabilized views)].

{AI12-0111-1} {AI12-0438-1} If a stable tree is declared without specifying Base, the object is necessarily initialized. The initializing expression of the stable tree, [typically a call on Copy], determines the Node_Count of the tree. The Node_Count of a stable tree never changes after initialization.

Proof: {AI12-0438-1} Initialization is required as the type is indefinite, see 3.3.1. 


#### Bounded (Run-Time) Errors

{AI05-0136-1} {AI05-0248-1} It is a bounded error for the actual function associated with a generic formal subprogram, when called as part of an operation of this package, to tamper with elements of any Tree parameter of the operation. Either Program_Error is raised, or the operation works as defined on the value of the Tree either prior to, or subsequent to, some or all of the modifications to the Tree.

{AI05-0136-1} It is a bounded error to call any subprogram declared in the visible part of Containers.Multiway_Trees when the associated container has been finalized. If the operation takes Container as an in out parameter, then it raises Constraint_Error or Program_Error. Otherwise, the operation either proceeds as it would for an empty container, or it raises Constraint_Error or Program_Error. 


#### Erroneous Execution

{AI05-0136-1} A Cursor value is invalid if any of the following have occurred since it was created: 

The tree that contains the element it designates has been finalized;

The tree that contains the element it designates has been used as the Source or Target of a call to Move;

The tree that contains the element it designates has been used as the Target of a call to Assign or the target of an [assignment_statement](./AA-5.2#S0173);

The element it designates has been removed from the tree that previously contained the element. 

Reason: We talk about which tree the element was removed from in order to handle splicing nodes from one tree to another. The node still exists, but any cursors that designate it in the original tree are now invalid. This bullet covers removals caused by calls to Clear, Delete_Leaf, Delete_Subtree, Delete_Children, Splice_Children, and Splice_Subtree. 

The result of "=" or Has_Element is unspecified if it is called with an invalid cursor parameter. Execution is erroneous if any other subprogram declared in Containers.Multiway_Trees is called with an invalid cursor parameter.

Discussion: The list above is intended to be exhaustive. In other cases, a cursor value continues to designate its original element (or the root node). For instance, cursor values survive the insertion and deletion of other nodes.

While it is possible to check for these cases, in many cases the overhead necessary to make the check is substantial in time or space. Implementations are encouraged to check for as many of these cases as possible and raise Program_Error if detected. 

{AI05-0212-1} Execution is erroneous if the tree associated with the result of a call to Reference or Constant_Reference is finalized before the result object returned by the call to Reference or Constant_Reference is finalized. 

Reason: Each object of Reference_Type and Constant_Reference_Type probably contains some reference to the originating container. If that container is prematurely finalized (which is only possible via Unchecked_Deallocation, as accessibility checks prevent passing a container to Reference that will not live as long as the result), the finalization of the object of Reference_Type will try to access a nonexistent object. This is a normal case of a dangling pointer created by Unchecked_Deallocation; we have to explicitly mention it here as the pointer in question is not visible in the specification of the type. (This is the same reason we have to say this for invalid cursors.) 


#### Implementation Requirements

{AI05-0136-1} No storage associated with a multiway tree object shall be lost upon assignment or scope exit.

{AI05-0136-1} {AI05-0262-1} The execution of an [assignment_statement](./AA-5.2#S0173) for a tree shall have the effect of copying the elements from the source tree object to the target tree object and changing the node count of the target object to that of the source object.

Implementation Note: {AI05-0298-1} An assignment of a Tree is a "deep" copy; that is the elements are copied as well the data structures. We say "effect of" in order to allow the implementation to avoid copying elements immediately if it wishes. For instance, an implementation that avoided copying until one of the containers is modified would be allowed. (Note that this implementation would require care, see A.18.2 for more.) 


#### Implementation Advice

{AI05-0136-1} Containers.Multiway_Trees should be implemented similarly to a multiway tree. In particular, if N is the overall number of nodes for a particular tree, then the worst-case time complexity of Element, Parent, First_Child, Last_Child, Next_Sibling, Previous_Sibling, Insert_Child with Count=1, and Delete should be O(log N). 

Implementation Advice: The worst-case time complexity of the Element, Parent, First_Child, Last_Child, Next_Sibling, Previous_Sibling, Insert_Child with Count=1, and Delete operations of Containers.Multiway_Trees should be O(log N).

Reason: We do not mean to overly constrain implementation strategies here. However, it is important for portability that the performance of large containers has roughly the same factors on different implementations. If a program is moved to an implementation that takes O(N) time to access elements, that program could be unusable when the trees are large. We allow O(log N) access because the proportionality constant and caching effects are likely to be larger than the log factor, and we don't want to discourage innovative implementations. 

{AI05-0136-1} Move should not copy elements, and should minimize copying of internal data structures. 

Implementation Advice: Containers.Multiway_Trees.Move should not copy elements, and should minimize copying of internal data structures.

Implementation Note: Usually that can be accomplished simply by moving the pointer(s) to the internal data structures from the Source container to the Target container. 

{AI05-0136-1} If an exception is propagated from a tree operation, no storage should be lost, nor any elements removed from a tree unless specified by the operation. 

Implementation Advice: If an exception is propagated from a tree operation, no storage should be lost, nor any elements removed from a tree unless specified by the operation.

Reason: This is important so that programs can recover from errors. But we don't want to require heroic efforts, so we just require documentation of cases where this can't be accomplished. 


#### Extensions to Ada 2005

{AI05-0136-1} {AI05-0257-1} {AI05-0265-1} {AI05-0269-1} The generic package Containers.Multiway_Trees is new. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for ordinary containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} A number of new subprograms, types, and even a nested package were added to Containers.Multiway_Trees to better support contracts and stable views. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Extensions to Ada 2012

{AI12-0196-1} Correction: Replace_Element is now defined such that it can be used concurrently so long as it operates on different elements. This allows some container operations to be used in parallel without separate synchronization.

{AI12-0266-1} Most iterators can now return parallel iterators, to be used in parallel constructs. 


#### Wording Changes from Ada 2012

{AI12-0069-1} Corrigendum: Fixed the function Iterate so it is clear that the root node is never visited.

{AI12-0078-1} Corrigendum: The definition of node is clarified so that it it doesn't appear to say all nodes have an element.

{AI12-0110-1} Corrigendum: Clarified that tampering checks precede all other checks made by a subprogram (but come after those associated with the call).

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.11  The Generic Package Containers.Indefinite_Vectors

{AI95-00302-03} The language-defined generic package Containers.Indefinite_Vectors provides a private type Vector and a set of operations. It provides the same operations as the package Containers.Vectors (see A.18.2), with the difference that the generic formal Element_Type is indefinite. 


#### Static Semantics

{AI95-00302-03} {AI05-0092-1} The declaration of the generic library package Containers.Indefinite_Vectors has the same contents and semantics as Containers.Vectors except:

The generic formal Element_Type is indefinite.

The procedures with the profiles: 

```ada
procedure Insert (Container : in out Vector;
                  Before    : in     Extended_Index;
                  Count     : in     Count_Type := 1);

```

```ada
procedure Insert (Container : in out Vector;
                  Before    : in     Cursor;
                  Position  :    out Cursor;
                  Count     : in     Count_Type := 1);

```

are omitted.

Discussion: These procedures are omitted because there is no way to create a default-initialized object of an indefinite type. Note that Insert_Space can be used instead of this routine in most cases. Omitting the routine completely allows any problems to be diagnosed by the compiler when converting from a definite to indefinite vector. 

The actual Element parameter of access subprogram Process of Update_Element may be constrained even if Element_Type is unconstrained.

{AI12-0035-1} The operations "&", Append, Insert, Prepend, Replace_Element, and To_Vector that have a formal parameter of type Element_Type perform indefinite insertion (see A.18).

{AI12-0111-1} {AI12-0112-1} The description of Tampering_With_Elements_Prohibited is replaced by:

Returns True if tampering with elements is prohibited for Container, and False otherwise. 

Reason: Complete replacement of an element can cause its memory to be deallocated while another operation is holding onto a reference to it. That can't be allowed. However, a simple modification of (part of) an element is not a problem, so Update_Element does not cause a problem. 

{AI12-0111-1} {AI12-0112-1} Tampering_With_Cursors_Prohibited is replaced by Tampering_With_Elements_Prohibited in the postcondition for the operations Reference and Constant_Reference.

{AI12-0111-1} The operations Replace_Element, Reverse_Elements, and Swap, and the nested generic unit Generic_Sorting are omitted from the nested package Stable.


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Indefinite_Vectors is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely, as it would require that the package was not implemented in Ada (an Ada [allocator](./AA-4.8#S0164) would raise Program_Error in these circumstances), and that a program inserted a more nested tagged type (or access discriminant) into a container, and then used that object before its type or discriminant went out of scope. All known implementations are implemented in Ada, so we believe there is no practical incompatibility. As such, we mention this only for completeness.

{AI12-0111-1} Defined the Iterator_View aspect, so that the stable view is used for container element iterators. This means that tampering with elements is prohibited during the entire loop, rather than tampering with cursors being prohibited during the loop, and tampering with elements being prohibited only during the lifetimes of references to the loop parameter. Thus, if a container element iterator does an operation that tampers with elements on the iterated container, that operation will fail a tampering check in Ada 2022 (and thus raise Program_Error), while it would have worked in Ada 2012 so long as the loop parameter is not involved. We believe this to be a dubious loop structure that should be rare. Note that this issue only occurs for the indefinite container form, the ordinary and bounded containers allow such operations at any time in Ada 2022.


## A.18.12  The Generic Package Containers.Indefinite_Doubly_Linked_Lists

{AI95-00302-03} The language-defined generic package Containers.Indefinite_Doubly_Linked_Lists provides private types List and Cursor, and a set of operations for each type. It provides the same operations as the package Containers.Doubly_Linked_Lists (see A.18.3), with the difference that the generic formal Element_Type is indefinite. 


#### Static Semantics

{AI95-00302-03} {AI05-0092-1} The declaration of the generic library package Containers.Indefinite_Doubly_Linked_Lists has the same contents and semantics as Containers.Doubly_Linked_Lists except:

The generic formal Element_Type is indefinite.

The procedure with the profile: 

```ada
procedure Insert (Container : in out List;
                  Before    : in     Cursor;
                  Position  :    out Cursor;
                  Count     : in     Count_Type := 1);

```

is omitted.

Discussion: This procedure is omitted because there is no way to create a default-initialized object of an indefinite type. We considered having this routine insert an empty element similar to the empty elements of a vector, but rejected this possibility because the semantics are fairly complex and very different from the existing definite container. That would make it more error-prone to convert a container from a definite type to an indefinite type; by omitting the routine completely, any problems will be diagnosed by the compiler. 

The actual Element parameter of access subprogram Process of Update_Element may be constrained even if Element_Type is unconstrained.

{AI12-0035-1} The operations Append, Insert, Prepend, and Replace_Element that have a formal parameter of type Element_Type perform indefinite insertion (see A.18).

{AI12-0111-1} {AI12-0112-1} The description of Tampering_With_Elements_Prohibited is replaced by:

Returns True if tampering with elements is prohibited for Container, and False otherwise. 

Reason: Complete replacement of an element can cause its memory to be deallocated while another operation is holding onto a reference to it. That can't be allowed. However, a simple modification of (part of) an element is not a problem. 

{AI12-0111-1} {AI12-0112-1} Tampering_With_Cursors_Prohibited is replaced by Tampering_With_Elements_Prohibited in the postcondition for the operations Reference and Constant_Reference.

{AI12-0111-1} The operations Replace_Element and Swap are omitted from the nested package Stable.


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Indefinite_Doubly_Linked_Lists is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely; see Inconsistencies With Ada 2012 in A.18.11 for details.

{AI12-0111-1} Defined the Iterator_View aspect, so that the stable view is used for container element iterators. This leads to a rare situation where Program_Error will be raised in Ada 2022 for code that would have worked in Ada 2012. See Inconsistencies With Ada 2012 in A.18.11 for details. 


## A.18.13  The Generic Package Containers.Indefinite_Hashed_Maps

{AI95-00302-03} The language-defined generic package Containers.Indefinite_Hashed_Maps provides a map with the same operations as the package Containers.Hashed_Maps (see A.18.5), with the difference that the generic formal types Key_Type and Element_Type are indefinite. 


#### Static Semantics

{AI95-00302-03} {AI05-0092-1} The declaration of the generic library package Containers.Indefinite_Hashed_Maps has the same contents and semantics as Containers.Hashed_Maps except:

The generic formal Key_Type is indefinite.

The generic formal Element_Type is indefinite.

The procedure with the profile: 

```ada
procedure Insert (Container : in out Map;
                  Key       : in     Key_Type;
                  Position  :    out Cursor;
                  Inserted  :    out Boolean);

```

is omitted.

Discussion: This procedure is omitted because there is no way to create a default-initialized object of an indefinite type. We considered having this routine insert an empty element similar to the empty elements of a vector, but rejected this possibility because the semantics are fairly complex and very different from the existing case. That would make it more error-prone to convert a container from a definite type to an indefinite type; by omitting the routine completely, any problems will be diagnosed by the compiler. 

The actual Element parameter of access subprogram Process of Update_Element may be constrained even if Element_Type is unconstrained.

{AI12-0035-1} The operations Include, Insert, Replace, and Replace_Element that have a formal parameter of type Element_Type perform indefinite insertion (see A.18). 

Discussion: Some of the named operations also have a formal of the indefinite formal type Key_Type and perform indefinite insertion using that value, but it is sufficient to mention the formal of type Element_Type to cover those. 

{AI12-0111-1} {AI12-0112-1} The description of Tampering_With_Elements_Prohibited is replaced by:

Returns True if tampering with elements is prohibited for Container, and False otherwise. 

Reason: Complete replacement of an element can cause its memory to be deallocated while another operation is holding onto a reference to it. That can't be allowed. However, a simple modification of (part of) an element is not a problem. 

{AI12-0111-1} {AI12-0112-1} Tampering_With_Cursors_Prohibited is replaced by Tampering_With_Elements_Prohibited in the postcondition for the operations Reference and Constant_Reference.

{AI12-0111-1} The operations Replace and Replace_Element are omitted from the nested package Stable.


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Indefinite_Hashed_Maps is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely; see Inconsistencies With Ada 2012 in A.18.11 for details.

{AI12-0111-1} Defined the Iterator_View aspect, so that the stable view is used for container element iterators. This leads to a rare situation where Program_Error will be raised in Ada 2022 for code that would have worked in Ada 2012. See Inconsistencies With Ada 2012 in A.18.11 for details. 


## A.18.14  The Generic Package Containers.Indefinite_Ordered_Maps

{AI95-00302-03} The language-defined generic package Containers.Indefinite_Ordered_Maps provides a map with the same operations as the package Containers.Ordered_Maps (see A.18.6), with the difference that the generic formal types Key_Type and Element_Type are indefinite. 


#### Static Semantics

{AI95-00302-03} {AI05-0092-1} The declaration of the generic library package Containers.Indefinite_Ordered_Maps has the same contents and semantics as Containers.Ordered_Maps except:

The generic formal Key_Type is indefinite.

The generic formal Element_Type is indefinite.

The procedure with the profile: 

```ada
procedure Insert (Container : in out Map;
                  Key       : in     Key_Type;
                  Position  :    out Cursor;
                  Inserted  :    out Boolean);

```

is omitted.

Discussion: This procedure is omitted because there is no way to create a default-initialized object of an indefinite type. We considered having this routine insert an empty element similar to the empty elements of a vector, but rejected this possibility because the semantics are fairly complex and very different from the existing case. That would make it more error-prone to convert a container from a definite type to an indefinite type; by omitting the routine completely, any problems will be diagnosed by the compiler. 

The actual Element parameter of access subprogram Process of Update_Element may be constrained even if Element_Type is unconstrained.

{AI12-0035-1} The operations Include, Insert, Replace, and Replace_Element that have a formal parameter of type Element_Type perform indefinite insertion (see A.18). 

Discussion: Some of the named operations also have a formal of the indefinite formal type Key_Type and perform indefinite insertion using that value, but it is sufficient to mention the formal of type Element_Type to cover those. 

{AI12-0111-1} {AI12-0112-1} The description of Tampering_With_Elements_Prohibited is replaced by:

Returns True if tampering with elements is prohibited for Container, and False otherwise. 

Reason: Complete replacement of an element can cause its memory to be deallocated while another operation is holding onto a reference to it. That can't be allowed. However, a simple modification of (part of) an element is not a problem. 

{AI12-0111-1} {AI12-0112-1} Tampering_With_Cursors_Prohibited is replaced by Tampering_With_Elements_Prohibited in the postcondition for the operations Reference and Constant_Reference.

{AI12-0111-1} The operations Replace and Replace_Element are omitted from the nested package Stable.


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Indefinite_Ordered_Maps is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely; see Inconsistencies With Ada 2012 in A.18.11 for details.

{AI12-0111-1} Defined the Iterator_View aspect, so that the stable view is used for container element iterators. This leads to a rare situation where Program_Error will be raised in Ada 2022 for code that would have worked in Ada 2012. See Inconsistencies With Ada 2012 in A.18.11 for details. 


## A.18.15  The Generic Package Containers.Indefinite_Hashed_Sets

{AI95-00302-03} The language-defined generic package Containers.Indefinite_Hashed_Sets provides a set with the same operations as the package Containers.Hashed_Sets (see A.18.8), with the difference that the generic formal type Element_Type is indefinite. 


#### Static Semantics

{AI95-00302-03} {AI05-0092-1} The declaration of the generic library package Containers.Indefinite_Hashed_Sets has the same contents and semantics as Containers.Hashed_Sets except:

The generic formal Element_Type is indefinite.

The actual Element parameter of access subprogram Process of Update_Element_Preserving_Key may be constrained even if Element_Type is unconstrained.

{AI12-0035-1} The operations Include, Insert, Replace, Replace_Element, and To_Set that have a formal parameter of type Element_Type perform indefinite insertion (see A.18). 

Ramification: This includes the procedure Replace declared in the nested generic package Generic_Keys, as well as the routines declared directly in the Containers.Indefinite_Hashed_Sets package. 

Discussion: {AI12-0111-1} Unlike the other containers, a Hashed_Set has no operations that tamper with elements without tampering with cursors. Modifying the contents of a element of a set can change its position in the set, so those operations tamper with cursors.

{AI12-0111-1} Because of this characteristic, this subclause is missing the rules that other indefinite containers have modifying the definition and use of Tampering_with_Elements, and the definition of the Stable subpackaage is the same as the definite version. 


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Indefinite_Hashed_Sets is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely; see Inconsistencies With Ada 2012 in A.18.11 for details. 


## A.18.16  The Generic Package Containers.Indefinite_Ordered_Sets

{AI95-00302-03} The language-defined generic package Containers.Indefinite_Ordered_Sets provides a set with the same operations as the package Containers.Ordered_Sets (see A.18.9), with the difference that the generic formal type Element_Type is indefinite. 


#### Static Semantics

{AI95-00302-03} {AI05-0092-1} The declaration of the generic library package Containers.Indefinite_Ordered_Sets has the same contents and semantics as Containers.Ordered_Sets except:

The generic formal Element_Type is indefinite.

The actual Element parameter of access subprogram Process of Update_Element_Preserving_Key may be constrained even if Element_Type is unconstrained.

{AI12-0035-1} The operations Include, Insert, Replace, Replace_Element, and To_Set that have a formal parameter of type Element_Type perform indefinite insertion (see A.18). 

Ramification: This includes the procedure Replace declared in the nested generic package Generic_Keys, as well as the routines declared directly in the Containers.Indefinite_Ordered_Sets package. 

Discussion: {AI12-0111-1} Unlike the other containers, a Ordered_Set has no operations that tamper with elements without tampering with cursors. Modifying the contents of a element of a set can change its position in the set, so those operations tamper with cursors.

{AI12-0111-1} Because of this characteristic, this subclause is missing the rules that other indefinite containers have modifying the definition and use of Tampering_with_Elements, and the definition of the Stable subpackaage is the same as the definite version. 


#### Extensions to Ada 95

{AI95-00302-03} The generic package Containers.Indefinite_Ordered_Sets is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely; see Inconsistencies With Ada 2012 in A.18.11 for details. 


## A.18.17  The Generic Package Containers.Indefinite_Multiway_Trees

{AI05-0136-1} The language-defined generic package Containers.Indefinite_Multiway_Trees provides a multiway tree with the same operations as the package Containers.Multiway_Trees (see A.18.10), with the difference that the generic formal Element_Type is indefinite. 


#### Static Semantics

{AI05-0136-1} The declaration of the generic library package Containers.Indefinite_Multiway_Trees has the same contents and semantics as Containers.Multiway_Trees except:

The generic formal Element_Type is indefinite.

The procedure with the profile: 

```ada
procedure Insert_Child (Container : in out Tree;
                        Parent    : in     Cursor;
                        Before    : in     Cursor;
                        Position  :    out Cursor;
                        Count     : in     Count_Type := 1);

```

is omitted.

Discussion: This procedure is omitted because there is no way to create a default-initialized object of an indefinite type. We considered having this routine insert an empty element similar to the empty elements of a vector, but rejected this possibility because the semantics are fairly complex and very different from the existing case. That would make it more error-prone to convert a container from a definite type to an indefinite type; by omitting the routine completely, any problems will be diagnosed by the compiler. 

The actual Element parameter of access subprogram Process of Update_Element may be constrained even if Element_Type is unconstrained.

{AI12-0035-1} The operations Append_Child, Insert_Child, Prepend_Child, and Replace_Element that have a formal parameter of type Element_Type perform indefinite insertion (see A.18).

{AI12-0111-1} {AI12-0112-1} The description of Tampering_With_Elements_Prohibited is replaced by:

Returns True if tampering with elements is prohibited for Container, and False otherwise. 

Reason: Complete replacement of an element can cause its memory to be deallocated while another operation is holding onto a reference to it. That can't be allowed. However, a simple modification of (part of) an element is not a problem. 

{AI12-0111-1} {AI12-0112-1} Tampering_With_Cursors_Prohibited is replaced by Tampering_With_Elements_Prohibited in the postcondition for the operations Reference and Constant_Reference.

{AI12-0111-1} The operations Replace_Element and Swap are omitted from the nested package Stable.


#### Extensions to Ada 2005

{AI05-0136-1} The generic package Containers.Indefinite_Multiway_Trees is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely; see Inconsistencies With Ada 2012 in A.18.11 for details.

{AI12-0111-1} Defined the Iterator_View aspect, so that the stable view is used for container element iterators. This leads to a rare situation where Program_Error will be raised in Ada 2022 for code that would have worked in Ada 2012. See Inconsistencies With Ada 2012 in A.18.11 for details. 


## A.18.18  The Generic Package Containers.Indefinite_Holders

{AI05-0069-1} The language-defined generic package Containers.Indefinite_Holders provides a private type Holder and a set of operations for that type. A holder container holds a single element of an indefinite type.

{AI05-0069-1} A holder container allows the declaration of an object that can be used like an uninitialized variable or component of an indefinite type.

{AI05-0069-1} A holder container may be empty. An empty holder does not contain an element. 


#### Static Semantics

{AI05-0069-1} The generic library package Containers.Indefinite_Holders has the following declaration: 

```ada
{AI05-0069-1} {AI05-0084-1} {AI12-0112-1} generic
   type Element_Type (&lt&gt) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is &lt&gt;
package Ada.Containers.Indefinite_Holders
   with Preelaborate, Remote_Types,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects in this generic package, see the notes on the equivalent operations in the specification of the Containers.Vectors package (see A.18.2). 

```ada
{AI12-0112-1} {AI12-0399-1}    type Holder is tagged private 
      with Stable_Properties =&gt (Is_Empty,
                                 Tampering_With_The_Element_Prohibited),
           Default_Initial_Condition =&gt Is_Empty (Holder),
           Preelaborable_Initialization;

```

```ada
   Empty_Holder : constant Holder;

```

```ada
{AI12-0112-1}    function Equal_Element (Left, Right : Element_Type) return Boolean
      renames "=";

```

```ada
   function "=" (Left, Right : Holder) return Boolean;

```

```ada
{AI12-0112-1}    function Tampering_With_The_Element_Prohibited
     (Container : Holder) return Boolean
      with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0339-1}    function Empty return Holder
      is (Empty_Holder)
      with Post =&gt
            not Tampering_With_The_Element_Prohibited (Empty'Result)
            and then Is_Empty (Empty'Result);

```

```ada
{AI12-0112-1}    function To_Holder (New_Item : Element_Type) return Holder
      with Post =&gt not Is_Empty (To_Holder'Result);

```

```ada
{AI12-0112-1}    function Is_Empty (Container : Holder) return Boolean
      with Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI12-0112-1}    procedure Clear (Container : in out Holder)
      with Pre  =&gt not Tampering_With_The_Element_Prohibited (Container)
                    or else raise Program_Error,
           Post =&gt Is_Empty (Container);

```

```ada
{AI12-0112-1}    function Element (Container : Holder) return Element_Type
      with Pre =&gt not Is_Empty (Container) or else raise Constraint_Error,
           Global =&gt null, Use_Formal =&gt Element_Type;

```

```ada
{AI12-0112-1}    procedure Replace_Element (Container : in out Holder;
                              New_Item  : in     Element_Type)
      with Pre  =&gt not Tampering_With_The_Element_Prohibited (Container)
                    or else raise Program_Error,
           Post =&gt not Is_Empty (Container);

```

```ada
{AI12-0112-1}    procedure Query_Element
     (Container : in Holder;
      Process   : not null access procedure (Element : in Element_Type))
      with Pre =&gt not Is_Empty (Container) or else raise Constraint_Error;

```

```ada
{AI05-0069-1} {AI05-0248-1} {AI12-0112-1}    procedure Update_Element
     (Container : in out Holder;
      Process   : not null access procedure (Element : in out Element_Type))
      with Pre =&gt not Is_Empty (Container) or else raise Constraint_Error;

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Constant_Reference_Type
         (Element : not null access constant Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    type Reference_Type
         (Element : not null access Element_Type) is private
      with Implicit_Dereference =&gt Element,
           Nonblocking, Global =&gt in out synchronized,
           Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Constant_Reference (Container : aliased in Holder)
      return Constant_Reference_Type
      with Pre  =&gt not Is_Empty (Container) 
                      or else raise Constraint_Error,
           Post =&gt Tampering_With_The_Element_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0212-1} {AI12-0112-1}    function Reference (Container : aliased in out Holder)
      return Reference_Type
      with Pre  =&gt not Is_Empty (Container) 
                      or else raise Constraint_Error,
           Post =&gt Tampering_With_The_Element_Prohibited (Container),
           Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
{AI05-0001-1} {AI12-0112-1}    procedure Assign (Target : in out Holder; Source : in Holder)
      with Post =&gt (Is_Empty (Source) = Is_Empty (Target));

```

```ada
{AI05-0001-1} {AI12-0112-1}    function Copy (Source : Holder) return Holder
      with Post =&gt (Is_Empty (Source) = Is_Empty (Copy'Result));

```

```ada
{AI12-0112-1}    procedure Move (Target : in out Holder; Source : in out Holder)
      with Pre  =&gt (not Tampering_With_The_Element_Prohibited (Target)
                      or else raise Program_Error) and then
                   (not Tampering_With_The_Element_Prohibited (Source)
                      or else raise Program_Error),
           Post =&gt (if not Target'Has_Same_Storage (Source) then
                      Is_Empty (Source) and then (not Is_Empty (Target)));

```

```ada
   procedure Swap (Left, Right : in out Holder)
      with Pre  =&gt (not Tampering_With_The_Element_Prohibited (Left)
                      or else raise Program_Error) and then
                   (not Tampering_With_The_Element_Prohibited (Right)
                      or else raise Program_Error),
           Post =&gt Is_Empty (Left) = Is_Empty (Right)'Old and then
                   Is_Empty (Right) = Is_Empty (Left)'Old;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Indefinite_Holders;

```

{AI05-0069-1} The actual function for the generic formal function "=" on Element_Type values is expected to define a reflexive and symmetric relationship and return the same result value each time it is called with a particular pair of values. If it behaves in some other manner, the function "=" on holder values returns an unspecified value. The exact arguments and number of calls of this generic formal function by the function "=" on holder values are unspecified.

Ramification: If the actual function for "=" is not symmetric and consistent, the result returned by any of the functions defined to use "=" cannot be predicted. The implementation is not required to protect against "=" raising an exception, or returning random results, or any other "bad" behavior. And it can call "=" in whatever manner makes sense. But note that only the results of the function "=" is unspecified; other subprograms are not allowed to break if "=" is bad. 

{AI05-0069-1} The type Holder is used to represent holder containers. The type Holder needs finalization (see 7.6).

{AI05-0069-1} Empty_Holder represents an empty holder object. If an object of type Holder is not otherwise initialized, it is initialized to the same value as Empty_Holder.

{AI05-0069-1} {AI05-0262-1} {AI12-0112-1} [Some operations  check for "tampering with the element" of a container because they depend on the element of the container not being replaced.]When tampering with the element is prohibited for a particular holder object H, Program_Error is propagated by the finalization of H[, as well as by a call that passes H to certain of the operations of this package, as indicated by the precondition of such an operation].

Paragraphs 30 through 35 are removed as preconditions now describe these rules. 

Reason: Complete replacement of an element can cause its memory to be deallocated while another operation is holding onto a reference to it. That can't be allowed. However, a simple modification of (part of) an element is not a problem, so Update_Element does not cause a problem. 

```ada
function "=" (Left, Right : Holder) return Boolean;

```

{AI05-0069-1} If Left and Right denote the same holder object, then the function returns True. Otherwise, it compares the element contained in Left to the element contained in Right using the generic formal equality operator, returning the result of that operation. Any exception raised during the evaluation of element equality is propagated.

Implementation Note: This wording describes the canonical semantics. However, the order and number of calls on the formal equality function is unspecified, so an implementation need not call the equality function if the correct answer can be determined without doing so. 

```ada
{AI12-0112-1} function Tampering_With_The_Element_Prohibited
  (Container : Holder) return Boolean
   with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI12-0112-1} Returns True if tampering with the element is currently prohibited for Container, and returns False otherwise.

```ada
{AI12-0112-1} function To_Holder (New_Item : Element_Type) return Holder
   with Post =&gt not Is_Empty (To_Holder'Result);

```

{AI05-0069-1} {AI12-0035-1} Returns a nonempty holder containing an element initialized to New_Item. To_Holder performs indefinite insertion (see A.18).

```ada
{AI12-0112-1} function Is_Empty (Container : Holder) return Boolean
   with Global =&gt null, Use_Formal =&gt null;

```

{AI05-0069-1} Returns True if Container is empty, and False if it contains an element.

```ada
{AI12-0112-1} procedure Clear (Container : in out Holder)
   with Pre  =&gt not Tampering_With_The_Element_Prohibited (Container)
                 or else raise Program_Error,
        Post =&gt Is_Empty (Container);

```

{AI05-0069-1} {AI12-0112-1} Removes the element from Container.

```ada
{AI12-0112-1} function Element (Container : Holder) return Element_Type
   with Pre =&gt not Is_Empty (Container) or else raise Constraint_Error,
        Global =&gt null, Use_Formal =&gt Element_Type;

```

{AI05-0069-1} {AI12-0112-1} Returns the element stored in Container.

```ada
{AI12-0112-1} procedure Replace_Element (Container : in out Holder;
                           New_Item  : in     Element_Type)
   with Pre  =&gt not Tampering_With_The_Element_Prohibited (Container)
                 or else raise Program_Error,
        Post =&gt not Is_Empty (Container);

```

{AI05-0069-1} {AI12-0035-1} {AI12-0112-1} Replace_Element assigns the value New_Item into Container, replacing any preexisting content of Container; Replace_Element performs indefinite insertion (see A.18).

```ada
{AI12-0112-1} procedure Query_Element
  (Container : in Holder;
   Process   : not null access procedure (Element : in Element_Type))
   with Pre =&gt not Is_Empty (Container) or else raise Constraint_Error,
        Global =&gt null, Use_Formal =&gt null;

```

{AI05-0069-1} {AI05-0262-1} {AI05-0265-1} {AI12-0112-1} Query_Element calls Process.all with the contained element as the argument. Tampering with the element of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

Implementation Note: {AI05-0005-1} The "tamper with the element" check is intended to prevent the Element parameter of Process from being replaced or deleted outside of Process. The check prevents data loss (if Element_Type is passed by copy) or erroneous execution (if Element_Type is an unconstrained type). 

```ada
{AI05-0069-1} {AI05-0248-1} {AI12-0112-1} procedure Update_Element
  (Container : in out Holder;
   Process   : not null access procedure (Element : in out Element_Type))
   with Pre =&gt not Is_Empty (Container) or else raise Constraint_Error;

```

{AI05-0069-1} {AI05-0262-1} {AI05-0265-1} {AI12-0112-1} Update_Element calls Process.all with the contained element as the argument. Tampering with the element of Container is prohibited during the execution of the call on Process.all. Any exception raised by Process.all is propagated.

Implementation Note: The Element parameter of Process.all may be constrained even if Element_Type is unconstrained. 

```ada
{AI05-0212-1} {AI12-0112-1} type Constant_Reference_Type
      (Element : not null access constant Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

```ada
{AI05-0212-1} {AI12-0112-1} type Reference_Type (Element : not null access Element_Type) is private
   with Implicit_Dereference =&gt Element,
        Nonblocking, Global =&gt in out synchronized,
        Default_Initial_Condition =&gt (raise Program_Error);

```

{AI05-0212-1} The types Constant_Reference_Type and Reference_Type need finalization.

This paragraph was deleted.{AI05-0212-1} {AI12-0112-1} 

Reason: It is expected that Reference_Type (and Constant_Reference_Type) will be a controlled type, for which finalization will have some action to terminate the tampering check for the associated container. If the object is created by default, however, there is no associated container. Since this is useless, and supporting this case would take extra work, we define it to raise an exception. 

```ada
{AI05-0212-1} {AI12-0112-1} function Constant_Reference (Container : aliased in Holder)
   return Constant_Reference_Type
   with Pre  =&gt not Is_Empty (Container) or else raise Constraint_Error,
        Post =&gt Tampering_With_The_Element_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} This function (combined with the Implicit_Dereference aspect) provides a convenient way to gain read access to the contained element of a holder container.

{AI05-0212-1} {AI05-0262-1} {AI05-0265-1} {AI12-0112-1} Constant_Reference returns an object whose discriminant is an access value that designates the contained element. Tampering with the element of Container is prohibited while the object returned by Constant_Reference exists and has not been finalized.

```ada
{AI05-0212-1} {AI12-0112-1} function Reference (Container : aliased in out Holder)
   return Reference_Type
   with Pre  =&gt not Is_Empty (Container) or else raise Constraint_Error,
        Post =&gt Tampering_With_The_Element_Prohibited (Container),
        Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

{AI05-0212-1} This function (combined with the Implicit_Dereference aspects) provides a convenient way to gain read and write access to the contained element of a holder container.

{AI05-0212-1} {AI05-0262-1} {AI05-0265-1} {AI12-0112-1} Reference returns an object whose discriminant is an access value that designates the contained element. Tampering with the element of Container is prohibited while the object returned by Reference exists and has not been finalized.

```ada
{AI12-0112-1} procedure Assign (Target : in out Holder; Source : in Holder)
   with Post =&gt (Is_Empty (Source) = Is_Empty (Target));

```

{AI05-0001-1} If Target denotes the same object as Source, the operation has no effect. If Source is empty, Clear (Target) is called. Otherwise, Replace_Element (Target, Element (Source)) is called. 

Discussion: {AI05-0005-1} This routine exists for compatibility with the other containers. For a holder, Assign(A, B) and A := B behave effectively the same. (Assign Clears the Target, while := finalizes the Target, but these should have similar effects.) 

```ada
{AI12-0112-1} function Copy (Source : Holder) return Holder
   with Post =&gt (Is_Empty (Source) = Is_Empty (Copy'Result));

```

{AI05-0001-1} If Source is empty, returns an empty holder container; otherwise, returns To_Holder (Element (Source)).

```ada
{AI12-0112-1} procedure Move (Target : in out Holder; Source : in out Holder)
   with Pre  =&gt (not Tampering_With_The_Element_Prohibited (Target)
                   or else raise Program_Error) and then
                (not Tampering_With_The_Element_Prohibited (Source)
                   or else raise Program_Error),
        Post =&gt (if not Target'Has_Same_Storage (Source) then
                   Is_Empty (Source) and then (not Is_Empty (Target)));

```

{AI05-0069-1} {AI05-0248-1} {AI12-0112-1} If Target denotes the same object as Source, then the operation has no effect. Otherwise, the element contained by Source (if any) is removed from Source and inserted into Target, replacing any preexisting content.

```ada
procedure Swap (Left, Right : in out Holder)
   with Pre  =&gt (not Tampering_With_The_Element_Prohibited (Left)
                   or else raise Program_Error) and then
                (not Tampering_With_The_Element_Prohibited (Right)
                   or else raise Program_Error),
        Post =&gt Is_Empty (Left) = Is_Empty (Right)'Old and then
                Is_Empty (Right) = Is_Empty (Left)'Old;

```

{AI12-0350-1} If Left denotes the same object as Right, then the operation has no effect. Otherwise, operation exchanges the elements (if any) contained by Left and Right.


#### Bounded (Run-Time) Errors

{AI05-0022-1} {AI05-0069-1} {AI05-0248-1} {AI05-0262-1} It is a bounded error for the actual function associated with a generic formal subprogram, when called as part of an operation of this package, to tamper with the element of any Holder parameter of the operation. Either Program_Error is raised, or the operation works as defined on the value of the Holder either prior to, or subsequent to, some or all of the modifications to the Holder.

{AI05-0027-1} {AI05-0069-1} It is a bounded error to call any subprogram declared in the visible part of Containers.Indefinite_Holders when the associated container has been finalized. If the operation takes Container as an in out parameter, then it raises Constraint_Error or Program_Error. Otherwise, the operation either proceeds as it would for an empty container, or it raises Constraint_Error or Program_Error. 


#### Erroneous Execution

{AI05-0212-1} {AI05-0269-1} Execution is erroneous if the holder container associated with the result of a call to Reference or Constant_Reference is finalized before the result object returned by the call to Reference or Constant_Reference is finalized.

Reason: {AI05-0212-1} Each object of Reference_Type and Constant_Reference_Type probably contains some reference to the originating container. If that container is prematurely finalized (which is only possible via Unchecked_Deallocation, as accessibility checks prevent passing a container to Reference that will not live as long as the result), the finalization of the object of Reference_Type will try to access a nonexistent object. This is a normal case of a dangling pointer created by Unchecked_Deallocation; we have to explicitly mention it here as the pointer in question is not visible in the specification of the type. (This is the same reason we have to say this for invalid cursors.) 


#### Implementation Requirements

{AI05-0069-1} No storage associated with a holder object shall be lost upon assignment or scope exit.

{AI05-0069-1} {AI05-0269-1} The execution of an [assignment_statement](./AA-5.2#S0173) for a holder container shall have the effect of copying the element (if any) from the source holder object to the target holder object.

Implementation Note: {AI05-0298-1} An assignment of a holder container is a "deep" copy; that is the element is copied as well as any data structures. We say "effect of" in order to allow the implementation to avoid copying the element immediately if it wishes. For instance, an implementation that avoided copying until one of the containers is modified would be allowed. (Note that this implementation would require care, see A.18.2 for more.) 


#### Implementation Advice

{AI05-0069-1} {AI05-0269-1} {AI12-0350-1} Move and Swap should not copy any elements, and should minimize copying of internal data structures. 

Implementation Advice: Move and Swap in Containers.Indefinite_Holders should not copy any elements, and should minimize copying of internal data structures.

Implementation Note: {AI12-0350-1} Usually that can be accomplished simply by moving the pointer(s) to the internal data structures appropriately. 

{AI05-0069-1} {AI05-0269-1} If an exception is propagated from a holder operation, no storage should be lost, nor should the element be removed from a holder container unless specified by the operation. 

Implementation Advice: If an exception is propagated from a holder operation, no storage should be lost, nor should the element be removed from a holder container unless specified by the operation.

Reason: This is important so that programs can recover from errors. But we don't want to require heroic efforts, so we just require documentation of cases where this can't be accomplished. 


#### Extensions to Ada 2005

{AI05-0069-1} {AI05-0084-1} {AI05-0265-1}  The generic package Containers.Indefinite_Holders is new. 


#### Inconsistencies With Ada 2012

{AI12-0035-1} Corrigendum: Defined some routines to "perform indefinite insertion". This could mean that some calls to those routines would now raise Program_Error where they previously worked. However, this is extremely unlikely; see Inconsistencies With Ada 2012 in A.18.11 for details. 


#### Incompatibilities With Ada 2012

{AI12-0111-1} {AI12-0112-1} {AI12-0350-1} A number of new subprograms and types were added to Containers.Indefinite_Holders to better support contracts and provide additional functionality. Therefore, a use clause conflict is possible; see the introduction of Annex A for more on this topic. 


#### Wording Changes from Ada 2012

{AI12-0110-1} Corrigendum: Clarified that tampering checks precede all other checks made by a subprogram (but come after those associated with the call).

{AI12-0112-1} Added contracts to this container. This includes describing some of the semantics with pre- and postconditions, rather than English text. Note that the preconditions can be Suppressed (see 11.5). 


## A.18.19  The Generic Package Containers.Bounded_Vectors

{AI05-0001-1} The language-defined generic package Containers.Bounded_Vectors provides a private type Vector and a set of operations. It provides the same operations as the package Containers.Vectors (see A.18.2), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI05-0001-1} The declaration of the generic library package Containers.Bounded_Vectors has the same contents and semantics as Containers.Vectors except:

{AI12-0112-1} The aspect Preelaborate is replaced with aspect Pure. Aspect Global is deleted.

Ramification: {AI12-0112-1} The Global aspect for a Pure package is null (see 6.1.2), so we don't need to give it explicitly. 

The type Vector is declared with a discriminant that specifies the capacity: 

```ada
{AI12-0112-1}   type Vector (Capacity : Count_Type) is tagged private...

```

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type Vector is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization

```

The type Vector needs finalization if and only if type Element_Type needs finalization.

Implementation Note: {AI05-0212-1} The type Vector cannot depend on package Ada.Finalization unless the element type depends on that package. The objects returned from the Iterator and Reference functions probably do depend on package Ada.Finalization. Restricted environments may need to avoid use of those functions and their associated types. 

{AI12-0112-1} Capacity is omitted from the Stable_Properties of type Vector.

Reason: The capacity is a discriminant here, so it can't be changed by most routines; thus including it in the stable properties adds no information. 

{AI12-0339-1} In function Empty, the postcondition is altered to:

```ada
   Post =&gt
      Empty'Result.Capacity = Capacity and then
      not Tampering_With_Elements_Prohibited (Empty'Result) and then
      not Tampering_With_Cursors_Prohibited (Empty'Result) and then
      Length (Empty'Result) = 0;

```

{AI12-0112-1} In function Copy, the postcondition is altered to: 

```ada
   Post =&gt Length (Copy'Result) = Length (Source) and then
           (if Capacity &gt Length (Source) then
              Copy'Result.Capacity = Capacity
            else Copy'Result.Capacity &gt= Length (Source));

```

The description of Reserve_Capacity is replaced with:

```ada
{AI12-0112-1}    procedure Reserve_Capacity (Container : in out Vector;
                               Capacity  : in     Count_Type)
      with Pre =&gt Capacity &lt= Container.Capacity 
                      or else raise Capacity_Error;

```

{AI12-0112-1} This operation has no effect, [other than checking the precondition]. 

{AI12-0112-1} The portion of the postcondition checking the capacity is omitted from subprograms Set_Length, Assign, Insert, Insert_Space, Prepend, Append, and Delete.

{AI12-0112-1} For procedures Insert, Insert_Space, Prepend, and Append, the part of the precondition reading:

```ada
     (&ltsome length&gt &lt= Maximum_Length - &ltsome other length&gt
        or else raise Constraint_Error)

```

is replaced by:

```ada
     (&ltsome length&gt &lt= Maximum_Length - &ltsome other length&gt
        or else raise Constraint_Error) and then
     (&ltsome length&gt &lt= Container.Capacity - &ltsome other length&gt
        or else raise Capacity_Error)

```


#### Bounded (Run-Time) Errors

{AI05-0160-1} {AI05-0265-1} It is a bounded error to assign from a bounded vector object while tampering with elements [or cursors] of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements [or cursors], or execution proceeds normally. 

Proof: Tampering with elements includes tampering with cursors, so we only really need to talk about tampering with elements here; we mention cursors for clarity. 


#### Erroneous Execution

{AI05-0265-1} When a bounded vector object V is finalized, if tampering with cursors is prohibited for V other than due to an assignment from another vector, then execution is erroneous. 

Reason: This is a tampering event, but since the implementation is not allowed to use Ada.Finalization, it is not possible in a pure Ada implementation to detect this error. (There is no Finalize routine that will be called that could make the check.) Since the check probably cannot be made, the bad effects that could occur (such as an iterator going into an infinite loop or accessing a nonexistent element) cannot be prevented and we have to allow anything. We do allow re-assigning an object that only prohibits tampering because it was copied from another object as that cannot cause any negative effects. 


#### Implementation Requirements

{AI05-0184-1} {AI05-0264-1} For each instance of Containers.Vectors and each instance of Containers.Bounded_Vectors, if the two instances meet the following conditions, then the output generated by the Vector'Output or Vector'Write subprograms of either instance shall be readable by the Vector'Input or Vector'Read of the other instance, respectively:

{AI05-0184-1} {AI05-0248-1} the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

{AI05-0184-1} the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters); and

{AI05-0184-1} the preceding two conditions also hold for the Index_Type parameters of the instances. 


#### Implementation Advice

{AI05-0001-1} Bounded vector objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded vector objects should be implemented without implicit pointers or dynamic allocation.

{AI05-0001-1} The implementation advice for procedure Move to minimize copying does not apply. 

Implementation Advice: The implementation advice for procedure Move to minimize copying does not apply to bounded vectors.


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0160-1} {AI05-0184-1}  The generic package Containers.Bounded_Vectors is new. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for bounded containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Incompatibilities With Ada 2012

{AI12-0409-1} Correction:A bounded vector now only has Preelaborable_Initialization (abbreviated PI in this note) when the actual for the Element_Type has PI. If an program used a vector whose actual Element_Type does not have PI in a context when PI is required (such as a library-level object in a preelaborated unit or as a component of a type with PI), the program would be illegal in Ada 2022 but legal in original Ada 2012. This situation is unlikely, especially as some existing Ada 2012 implementations reject the instance in this case. 


## A.18.20  The Generic Package Containers.Bounded_Doubly_Linked_Lists

{AI05-0001-1} The language-defined generic package Containers.Bounded_Doubly_Linked_Lists provides a private type List and a set of operations. It provides the same operations as the package Containers.Doubly_Linked_Lists (see A.18.3), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI05-0001-1} The declaration of the generic library package Containers.Bounded_Doubly_Linked_Lists has the same contents and semantics as Containers.Doubly_Linked_Lists except:

{AI12-0112-1} The aspect Preelaborate is replaced with aspect Pure. Aspect Global is deleted.

Ramification: {AI12-0112-1} The Global aspect for a Pure package is null (see 6.1.2), so we don't need to give it explicitly. 

The type List is declared with a discriminant that specifies the capacity (maximum number of elements) as follows: 

```ada
{AI12-0112-1}   type List (Capacity : Count_Type) is tagged private...

```

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type List is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization

```

The type List needs finalization if and only if type Element_Type needs finalization.

Implementation Note: {AI05-0212-1} The type List cannot depend on package Ada.Finalization unless the element type depends on that package. The objects returned from the Iterator and Reference functions probably do depend on package Ada.Finalization. Restricted environments may need to avoid use of those functions and their associated types. 

{AI12-0339-1} The function Empty is replaced by:

```ada
   function Empty (Capacity : Count_Type := implementation-defined)
      return List
      with Post =&gt
            Empty'Result.Capacity = Capacity and then
            not Tampering_With_Elements_Prohibited (Empty'Result) and then
            not Tampering_With_Cursors_Prohibited (Empty'Result) and then
            Length (Empty'Result) = 0;

```

{AI12-0112-1} For procedures Insert, Prepend, Append, Merge, and the three-parameter Splice whose parameter Source has type List, the part of the precondition reading:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error)

```

is replaced by:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error) and then
     (&ltsome length&gt &lt= Container.Capacity - &ltsome other length&gt
        or else raise Capacity_Error)

```

{AI12-0112-1} In procedure Assign, the precondition is altered to: 

```ada
   Pre =&gt (not Tampering_With_Cursors_Prohibited (Target)
             or else raise Program_Error) and then
          (Length (Source) &lt= Target.Capacity
             or else raise Capacity_Error),

```

The function Copy is replaced with: 

```ada
{AI12-0112-1}   function Copy (Source : List; Capacity : Count_Type := 0)
     return List
     with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                     or else raise Capacity_Error,
          Post =&gt 
             Length (Copy'Result) = Length (Source) and then
             not Tampering_With_Elements_Prohibited (Copy'Result) and then
             not Tampering_With_Cursors_Prohibited (Copy'Result) and then
             Copy'Result.Capacity = (if Capacity = 0 then
                Length (Source) else Capacity);

```

Returns a list whose elements have the same values as the elements of Source. 

This paragraph was deleted.{AI12-0112-1} 

{AI12-0112-1} In the four-parameter procedure Splice, the precondition is altered to: 

```ada
   Pre  =&gt (not Tampering_With_Cursors_Prohibited (Target)
               or else raise Program_Error) and then
            (not Tampering_With_Cursors_Prohibited (Source)
               or else raise Program_Error) and then
            (Position /= No_Element
               or else raise Constraint_Error) and then
            (Has_Element (Source, Position)
               or else raise Program_Error) and then
            (Before = No_Element or else Has_Element (Target, Before)
               or else raise Program_Error) and then
            (Target'Has_Same_Storage (Source) or else
             Length (Target) /= Count_Type'Last
               or else raise Constraint_Error) and then
            (Target'Has_Same_Storage (Source) or else
             Length (Target) /= Target.Capacity
               or else raise Capacity_Error),

```


#### Bounded (Run-Time) Errors

{AI05-0160-1} {AI05-0265-1} It is a bounded error to assign from a bounded list object while tampering with elements [or cursors] of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements [or cursors], or execution proceeds normally. 

Proof: Tampering with elements includes tampering with cursors, so we only really need to talk about tampering with elements here; we mention cursors for clarity. 


#### Erroneous Execution

{AI05-0265-1} When a bounded list object L is finalized, if tampering with cursors is prohibited for L other than due to an assignment from another list, then execution is erroneous. 

Reason: This is a tampering event, but since the implementation is not allowed to use Ada.Finalization, it is not possible in a pure Ada implementation to detect this error. (There is no Finalize routine that will be called that could make the check.) Since the check probably cannot be made, the bad effects that could occur (such as an iterator going into an infinite loop or accessing a nonexistent element) cannot be prevented and we have to allow anything. We do allow re-assigning an object that only prohibits tampering because it was copied from another object as that cannot cause any negative effects. 


#### Implementation Requirements

{AI05-0184-1} {AI05-0264-1} For each instance of Containers.Doubly_Linked_Lists and each instance of Containers.Bounded_Doubly_Linked_Lists, if the two instances meet the following conditions, then the output generated by the List'Output or List'Write subprograms of either instance shall be readable by the List'Input or List'Read of the other instance, respectively:

{AI05-0184-1} {AI05-0248-1} the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

{AI05-0184-1} the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters). 


#### Implementation Advice

{AI05-0001-1} Bounded list objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded list objects should be implemented without implicit pointers or dynamic allocation.

{AI05-0001-1} The implementation advice for procedure Move to minimize copying does not apply. 

Implementation Advice: The implementation advice for procedure Move to minimize copying does not apply to bounded lists.


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0160-1} {AI05-0184-1}  The generic package Containers.Bounded_Doubly_Linked_Lists is new. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for bounded containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Incompatibilities With Ada 2012

{AI12-0409-1} Correction:A bounded list now only has Preelaborable_Initialization (abbreviated PI in this note) when the actual for the Element_Type has PI. If an program used a list whose actual Element_Type does not have PI in a context when PI is required (such as a library-level object in a preelaborated unit or as a component of a type with PI), the program would be illegal in Ada 2022 but legal in original Ada 2012. This situation is unlikely, especially as some existing Ada 2012 implementations reject the instance in this case. 


## A.18.21  The Generic Package Containers.Bounded_Hashed_Maps

{AI05-0001-1} The language-defined generic package Containers.Bounded_Hashed_Maps provides a private type Map and a set of operations. It provides the same operations as the package Containers.Hashed_Maps (see A.18.5), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI05-0001-1} The declaration of the generic library package Containers.Bounded_Hashed_Maps has the same contents and semantics as Containers.Hashed_Maps except:

{AI12-0112-1} The aspect Preelaborate is replaced with aspect Pure. Aspect Global is deleted.

The type Map is declared with discriminants that specify both the capacity (number of elements) and modulus (number of distinct hash values) of the hash table as follows: 

```ada
{AI12-0112-1}   type Map (Capacity : Count_Type;
            Modulus  : Hash_Type) is tagged private...

```

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type Map is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization
        and
     Key_Type'Preelaborable_Initialization

```

The type Map needs finalization if and only if type Key_Type or type Element_Type needs finalization.

Implementation Note: {AI05-0212-1} The type Map cannot depend on package Ada.Finalization unless the element or key type depends on that package. The objects returned from the Iterator and Reference functions probably do depend on package Ada.Finalization. Restricted environments may need to avoid use of those functions and their associated types. 

{AI12-0339-1} In function Empty, the postcondition is altered to:

```ada
   Post =&gt
      Empty'Result.Capacity = Capacity and then
      Empty'Result.Modulus = Default_Modulus (Capacity) and then
      not Tampering_With_Elements_Prohibited (Empty'Result) and then
      not Tampering_With_Cursors_Prohibited (Empty'Result) and then
      Length (Empty'Result) = 0;

```

The description of Reserve_Capacity is replaced with:

```ada
{AI12-0112-1}    procedure Reserve_Capacity (Container : in out Map;
                               Capacity  : in     Count_Type)
      with Pre =&gt Capacity &lt= Container.Capacity 
                      or else raise Capacity_Error;

```

{AI12-0112-1} This operation has no effect, [other than checking the precondition]. 

An additional operation is added immediately following Reserve_Capacity: 

```ada
  function Default_Modulus (Capacity : Count_Type) return Hash_Type;

```

Default_Modulus returns an implementation-defined value for the number of distinct hash values to be used for the given capacity (maximum number of elements). 

{AI12-0112-1} For procedures Insert and Include, the part of the precondition reading:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error)

```

is replaced by:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error) and then
     (&ltsome length&gt &gt Container.Capacity - &ltsome other length&gt
        or else raise Capacity_Error)

```

{AI12-0112-1} In procedure Assign, the precondition is altered to: 

```ada
   Pre =&gt (not Tampering_With_Cursors_Prohibited (Target)
             or else raise Program_Error) and then
          (Length (Source) &lt= Target.Capacity
             or else raise Capacity_Error),

```

The function Copy is replaced with: 

```ada
{AI12-0112-1}   function Copy (Source   : Map;
                 Capacity : Count_Type := 0;
                 Modulus  : Hash_Type := 0) return Map
     with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                     or else raise Capacity_Error,
          Post =&gt 
             Length (Copy'Result) = Length (Source) and then
             not Tampering_With_Elements_Prohibited (Copy'Result) and then
             not Tampering_With_Cursors_Prohibited (Copy'Result) and then
             Copy'Result.Capacity = (if Capacity = 0 then
                Length (Source) else Capacity) and then
             Copy'Result.Modulus = (if Modulus = 0 then
                Default_Modulus (Capacity) else Modulus);

```

{AI05-0264-1} Returns a map with key/element pairs initialized from the values in Source. 


#### Bounded (Run-Time) Errors

{AI05-0160-1} {AI05-0265-1} It is a bounded error to assign from a bounded map object while tampering with elements [or cursors] of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements [or cursors], or execution proceeds normally. 

Proof: Tampering with elements includes tampering with cursors, so we only really need to talk about tampering with elements here; we mention cursors for clarity. 


#### Erroneous Execution

{AI05-0265-1} When a bounded map object M is finalized, if tampering with cursors is prohibited for M other than due to an assignment from another map, then execution is erroneous. 

Reason: This is a tampering event, but since the implementation is not allowed to use Ada.Finalization, it is not possible in a pure Ada implementation to detect this error. (There is no Finalize routine that will be called that could make the check.) Since the check probably cannot be made, the bad effects that could occur (such as an iterator going into an infinite loop or accessing a nonexistent element) cannot be prevented and we have to allow anything. We do allow re-assigning an object that only prohibits tampering because it was copied from another object as that cannot cause any negative effects. 


#### Implementation Requirements

{AI05-0184-1} {AI05-0264-1} For each instance of Containers.Hashed_Maps and each instance of Containers.Bounded_Hashed_Maps, if the two instances meet the following conditions, then the output generated by the Map'Output or Map'Write subprograms of either instance shall be readable by the Map'Input or Map'Read of the other instance, respectively:

{AI05-0184-1} {AI05-0248-1} the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

{AI05-0184-1} the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters); and

{AI05-0184-1} the preceding two conditions also hold for the Key_Type parameters of the instances. 


#### Implementation Advice

{AI05-0001-1} {AI05-0269-1} Bounded hashed map objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded hashed map objects should be implemented without implicit pointers or dynamic allocation.

{AI05-0001-1} The implementation advice for procedure Move to minimize copying does not apply. 

Implementation Advice: The implementation advice for procedure Move to minimize copying does not apply to bounded hashed maps.


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0160-1} {AI05-0184-1}  The generic package Containers.Bounded_Hashed_Maps is new. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for bounded containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Incompatibilities With Ada 2012

{AI12-0409-1} Correction:A bounded map now only has Preelaborable_Initialization (abbreviated PI in this note) when the actuals for the Element_Type and the Key_Type have PI. If an program used a map whose actual Element_Type or Key_Type does not have PI in a context when PI is required (such as a library-level object in a preelaborated unit or as a component of a type with PI), the program would be illegal in Ada 2022 but legal in original Ada 2012. This situation is unlikely, especially as some existing Ada 2012 implementations reject the instance in this case. 


## A.18.22  The Generic Package Containers.Bounded_Ordered_Maps

{AI05-0001-1} The language-defined generic package Containers.Bounded_Ordered_Maps provides a private type Map and a set of operations. It provides the same operations as the package Containers.Ordered_Maps (see A.18.6), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI05-0001-1} The declaration of the generic library package Containers.Bounded_Ordered_Maps has the same contents and semantics as Containers.Ordered_Maps except:

{AI12-0112-1} The aspect Preelaborate is replaced with aspect Pure. Aspect Global is deleted.

The type Map is declared with a discriminant that specifies the capacity (maximum number of elements) as follows: 

```ada
{AI12-0112-1}   type Map (Capacity : Count_Type) is tagged private...

```

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type Map is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization
        and
     Key_Type'Preelaborable_Initialization

```

The type Map needs finalization if and only if type Key_Type or type Element_Type needs finalization.

Implementation Note: {AI05-0212-1} The type Map cannot depend on package Ada.Finalization unless the element type depends on that package. The objects returned from the Iterator and Reference functions probably do depend on package Ada.Finalization. Restricted environments may need to avoid use of those functions and their associated types. 

{AI12-0339-1} The function Empty is replaced by:

```ada
   function Empty (Capacity : Count_Type := implementation-defined)
      return Map
      with Post =&gt
            Empty'Result.Capacity = Capacity and then
            not Tampering_With_Elements_Prohibited (Empty'Result) and then
            not Tampering_With_Cursors_Prohibited (Empty'Result) and then
            Length (Empty'Result) = 0;

```

{AI12-0112-1} For procedures Insert and Include, the part of the precondition reading:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error)

```

is replaced by:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
         or else raise Constraint_Error) and then
      (&ltsome length&gt &lt= Container.Capacity - &ltsome other length&gt
         or else raise Capacity_Error)

```

{AI12-0112-1} In procedure Assign, the precondition is altered to:

```ada
   Pre =&gt (not Tampering_With_Cursors_Prohibited (Target)
             or else raise Program_Error) and then
          (Length (Source) &lt= Target.Capacity
             or else raise Capacity_Error),

```

The function Copy is replaced with: 

```ada
{AI12-0112-1}   function Copy (Source   : Map;
                 Capacity : Count_Type := 0) return Map
     with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                     or else raise Capacity_Error,
          Post =&gt 
             Length (Copy'Result) = Length (Source) and then
             not Tampering_With_Elements_Prohibited (Copy'Result) and then
             not Tampering_With_Cursors_Prohibited (Copy'Result) and then
             Copy'Result.Capacity = (if Capacity = 0 then
                Length (Source) else Capacity);

```

{AI12-0112-1} Returns a map with key/element pairs initialized from the values in Source. 


#### Bounded (Run-Time) Errors

{AI05-0160-1} {AI05-0265-1} It is a bounded error to assign from a bounded map object while tampering with elements [or cursors] of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements [or cursors], or execution proceeds normally. 

Proof: Tampering with elements includes tampering with cursors, so we only really need to talk about tampering with elements here; we mention cursors for clarity. 


#### Erroneous Execution

{AI05-0265-1} When a bounded map object M is finalized, if tampering with cursors is prohibited for M other than due to an assignment from another map, then execution is erroneous. 

Reason: This is a tampering event, but since the implementation is not allowed to use Ada.Finalization, it is not possible in a pure Ada implementation to detect this error. (There is no Finalize routine that will be called that could make the check.) Since the check probably cannot be made, the bad effects that could occur (such as an iterator going into an infinite loop or accessing a nonexistent element) cannot be prevented and we have to allow anything. We do allow re-assigning an object that only prohibits tampering because it was copied from another object as that cannot cause any negative effects. 


#### Implementation Requirements

{AI05-0184-1} {AI05-0264-1} For each instance of Containers.Ordered_Maps and each instance of Containers.Bounded_Ordered_Maps, if the two instances meet the following conditions, then the output generated by the Map'Output or Map'Write subprograms of either instance shall be readable by the Map'Input or Map'Read of the other instance, respectively:

{AI05-0184-1} {AI05-0248-1} the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

{AI05-0184-1} the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters); and

{AI05-0184-1} the preceding two conditions also hold for the Key_Type parameters of the instances. 


#### Implementation Advice

{AI05-0001-1} {AI05-0269-1} Bounded ordered map objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded ordered map objects should be implemented without implicit pointers or dynamic allocation.

{AI05-0001-1} The implementation advice for procedure Move to minimize copying does not apply. 

Implementation Advice: The implementation advice for procedure Move to minimize copying does not apply to bounded ordered maps.


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0160-1} {AI05-0184-1}  The generic package Containers.Bounded_Ordered_Maps is new. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for bounded containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Incompatibilities With Ada 2012

{AI12-0409-1} Correction:A bounded map now only has Preelaborable_Initialization (abbreviated PI in this note) when the actuals for the Element_Type and the Key_Type have PI. If an program used a map whose actual Element_Type or Key_Type does not have PI in a context when PI is required (such as a library-level object in a preelaborated unit or as a component of a type with PI), the program would be illegal in Ada 2022 but legal in original Ada 2012. This situation is unlikely, especially as some existing Ada 2012 implementations reject the instance in this case. 


## A.18.23  The Generic Package Containers.Bounded_Hashed_Sets

{AI05-0001-1} The language-defined generic package Containers.Bounded_Hashed_Sets provides a private type Set and a set of operations. It provides the same operations as the package Containers.Hashed_Sets (see A.18.8), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI05-0001-1} The declaration of the generic library package Containers.Bounded_Hashed_Sets has the same contents and semantics as Containers.Hashed_Sets except:

{AI12-0112-1} The aspect Preelaborate is replaced with aspect Pure. Aspect Global is deleted.

The type Set is declared with discriminants that specify both the capacity (number of elements) and modulus (number of distinct hash values) of the hash table as follows: 

```ada
{AI12-0112-1}   type Set (Capacity : Count_Type;
            Modulus  : Hash_Type) is tagged private...

```

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type Set is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization

```

The type Set needs finalization if and only if type Element_Type needs finalization.

Implementation Note: {AI05-0212-1} The type Set cannot depend on package Ada.Finalization unless the element or key type depends on that package. The objects returned from the Iterator and Reference functions probably do depend on package Ada.Finalization. Restricted environments may need to avoid use of those functions and their associated types. 

{AI12-0339-1} In function Empty, the postcondition is altered to:

```ada
   Post =&gt
      Empty'Result.Capacity = Capacity and then
      Empty'Result.Modulus = Default_Modulus (Capacity) and then
      not Tampering_With_Cursors_Prohibited (Empty'Result) and then
      Length (Empty'Result) = 0;

```

The description of Reserve_Capacity is replaced with: 

```ada
{AI12-0112-1}    procedure Reserve_Capacity (Container : in out Set;
                               Capacity  : in     Count_Type)
      with Pre =&gt Capacity &lt= Container.Capacity 
         or else raise Capacity_Error;

```

{AI12-0112-1} This operation has no effect, [other than checking the precondition]. 

An additional operation is added immediately following Reserve_Capacity: 

```ada
  function Default_Modulus (Capacity : Count_Type) return Hash_Type;

```

Default_Modulus returns an implementation-defined value for the number of distinct hash values to be used for the given capacity (maximum number of elements). 

{AI12-0112-1} For procedures Insert and Include, the part of the precondition reading:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error)

```

is replaced by:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error) and then
     (&ltsome length&gt &lt= Container.Capacity - &ltsome other length&gt
        or else raise Capacity_Error)

```

{AI12-0112-1} In procedure Assign, the precondition is altered to: 

```ada
   Pre =&gt (not Tampering_With_Cursors_Prohibited (Target)
             or else raise Program_Error) and then
          (Length (Source) &lt= Target.Capacity
             or else raise Capacity_Error),

```

The function Copy is replaced with: 

```ada
{AI12-0112-1}   function Copy (Source   : Set;
                 Capacity : Count_Type := 0;
                 Modulus  : Hash_Type := 0) return Map
     with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                     or else raise Capacity_Error,
          Post =&gt 
             Length (Copy'Result) = Length (Source) and then
             not Tampering_With_Cursors_Prohibited (Copy'Result) and then
             Copy'Result.Capacity = (if Capacity = 0 then
                Length (Source) else Capacity) and then
             Copy'Result.Modulus = (if Modulus = 0 then
                Default_Modulus (Capacity) else Modulus);

```

{AI05-0264-1} Returns a set with key/element pairs initialized from the values in Source. 


#### Bounded (Run-Time) Errors

{AI05-0160-1} {AI05-0265-1} It is a bounded error to assign from a bounded set object while tampering with elements [or cursors] of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements [or cursors], or execution proceeds normally. 

Proof: Tampering with elements includes tampering with cursors, so we only really need to talk about tampering with elements here; we mention cursors for clarity. 


#### Erroneous Execution

{AI05-0265-1} When a bounded set object S is finalized, if tampering with cursors is prohibited for S other than due to an assignment from another set, then execution is erroneous. 

Reason: This is a tampering event, but since the implementation is not allowed to use Ada.Finalization, it is not possible in a pure Ada implementation to detect this error. (There is no Finalize routine that will be called that could make the check.) Since the check probably cannot be made, the bad effects that could occur (such as an iterator going into an infinite loop or accessing a nonexistent element) cannot be prevented and we have to allow anything. We do allow re-assigning an object that only prohibits tampering because it was copied from another object as that cannot cause any negative effects. 


#### Implementation Requirements

{AI05-0184-1} {AI05-0264-1} For each instance of Containers.Hashed_Sets and each instance of Containers.Bounded_Hashed_Sets, if the two instances meet the following conditions, then the output generated by the Set'Output or Set'Write subprograms of either instance shall be readable by the Set'Input or Set'Read of the other instance, respectively:

{AI05-0184-1} {AI05-0248-1} the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

{AI05-0184-1} the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters). 


#### Implementation Advice

{AI05-0001-1} {AI05-0269-1} Bounded hashed set objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded hashed set objects should be implemented without implicit pointers or dynamic allocation.

{AI05-0001-1} The implementation advice for procedure Move to minimize copying does not apply. 

Implementation Advice: The implementation advice for procedure Move to minimize copying does not apply to bounded hashed sets.


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0160-1} {AI05-0184-1}  The generic package Containers.Bounded_Hashed_Sets is new. 


#### Incompatibilities With Ada 2012

{AI12-0409-1} Correction:A bounded set now only has Preelaborable_Initialization (abbreviated PI in this note) when the actual for the Element_Type has PI. If an program used a set whose actual Element_Type does not have PI in a context when PI is required (such as a library-level object in a preelaborated unit or as a component of a type with PI), the program would be illegal in Ada 2022 but legal in original Ada 2012. This situation is unlikely, especially as some existing Ada 2012 implementations reject the instance in this case. 


## A.18.24  The Generic Package Containers.Bounded_Ordered_Sets

{AI05-0001-1} The language-defined generic package Containers.Bounded_Ordered_Sets provides a private type Set and a set of operations. It provides the same operations as the package Containers.Ordered_Sets (see A.18.9), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI05-0001-1} The declaration of the generic library package Containers.Bounded_Ordered_Sets has the same contents and semantics as Containers.Ordered_Sets except:

{AI12-0112-1} The aspect Preelaborate is replaced with aspect Pure. Aspect Global is deleted.

The type Set is declared with a discriminant that specifies the capacity (maximum number of elements) as follows: 

```ada
{AI12-0112-1}   type Set (Capacity : Count_Type) is tagged private...

```

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type Set is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization

```

The type Set needs finalization if and only if type Element_Type needs finalization.

Implementation Note: {AI05-0212-1} The type Set cannot depend on package Ada.Finalization unless the element type depends on that package. The objects returned from the Iterator and Reference functions probably do depend on package Ada.Finalization. Restricted environments may need to avoid use of those functions and their associated types. 

{AI12-0339-1} The function Empty is replaced by:

```ada
   function Empty (Capacity : Count_Type := implementation-defined)
      return Set
      with Post =&gt
            Empty'Result.Capacity = Capacity and then
            not Tampering_With_Cursors_Prohibited (Empty'Result) and then
            Length (Empty'Result) = 0;

```

{AI12-0112-1} For procedures Insert and Include, the part of the precondition reading:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error)

```

is replaced by:

```ada
     (&ltsome length&gt &lt= Count_Type'Last - &ltsome other length&gt
        or else raise Constraint_Error) and then
     (&ltsome length&gt &lt= Container.Capacity - &ltsome other length&gt
        or else raise Capacity_Error)

```

{AI12-0112-1} In procedure Assign, the precondition is altered to:

```ada
   Pre =&gt (not Tampering_With_Cursors_Prohibited (Target)
             or else raise Program_Error) and then
          (Length (Source) &lt= Target.Capacity
             or else raise Capacity_Error),

```

The function Copy is replaced with: 

```ada
{AI12-0112-1}   function Copy (Source   : Set;
                 Capacity : Count_Type := 0) return Map
     with Pre  =&gt Capacity = 0 or else Capacity &gt= Length (Source)
                     or else raise Capacity_Error,
          Post =&gt 
             Length (Copy'Result) = Length (Source) and then
             not Tampering_With_Cursors_Prohibited (Copy'Result) and then
             Copy'Result.Capacity = (if Capacity = 0 then
                Length (Source) else Capacity);

```

{AI12-0112-1} Returns a set with key/element pairs initialized from the values in Source. 


#### Bounded (Run-Time) Errors

{AI05-0160-1} {AI05-0265-1} It is a bounded error to assign from a bounded set object while tampering with elements [or cursors] of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements [or cursors], or execution proceeds normally. 

Proof: Tampering with elements includes tampering with cursors, so we only really need to talk about tampering with elements here; we mention cursors for clarity. 


#### Erroneous Execution

{AI05-0265-1} When a bounded set object S is finalized, if tampering with cursors is prohibited for S other than due to an assignment from another set, then execution is erroneous. 

Reason: This is a tampering event, but since the implementation is not allowed to use Ada.Finalization, it is not possible in a pure Ada implementation to detect this error. (There is no Finalize routine that will be called that could make the check.) Since the check probably cannot be made, the bad effects that could occur (such as an iterator going into an infinite loop or accessing a nonexistent element) cannot be prevented and we have to allow anything. We do allow re-assigning an object that only prohibits tampering because it was copied from another object as that cannot cause any negative effects. 


#### Implementation Requirements

{AI05-0184-1} {AI05-0264-1} For each instance of Containers.Ordered_Sets and each instance of Containers.Bounded_Ordered_Sets, if the two instances meet the following conditions, then the output generated by the Set'Output or Set'Write subprograms of either instance shall be readable by the Set'Input or Set'Read of the other instance, respectively:

{AI05-0184-1} {AI05-0248-1} the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

{AI05-0184-1} the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters). 


#### Implementation Advice

{AI05-0001-1} {AI05-0269-1} Bounded ordered set objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded ordered set objects should be implemented without implicit pointers or dynamic allocation.

{AI05-0001-1} The implementation advice for procedure Move to minimize copying does not apply. 

Implementation Advice: The implementation advice for procedure Move to minimize copying does not apply to bounded ordered sets.


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0160-1} {AI05-0184-1}  The generic package Containers.Bounded_Ordered_Sets is new. 


#### Incompatibilities With Ada 2012

{AI12-0409-1} Correction:A bounded set now only has Preelaborable_Initialization (abbreviated PI in this note) when the actual for the Element_Type has PI. If an program used a set whose actual Element_Type does not have PI in a context when PI is required (such as a library-level object in a preelaborated unit or as a component of a type with PI), the program would be illegal in Ada 2022 but legal in original Ada 2012. This situation is unlikely, especially as some existing Ada 2012 implementations reject the instance in this case. 


## A.18.25  The Generic Package Containers.Bounded_Multiway_Trees

{AI05-0136-1} The language-defined generic package Containers.Bounded_Multiway_Trees provides a private type Tree and a set of operations. It provides the same operations as the package Containers.Multiway_Trees (see A.18.10), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI05-0136-1} The declaration of the generic library package Containers.Bounded_Multiway_Trees has the same contents and semantics as Containers.Multiway_Trees except:

{AI12-0112-1} The aspect Preelaborate is replaced with aspect Pure. Aspect Global is deleted.

The type Tree is declared with a discriminant that specifies the capacity (maximum number of elements) as follows: 

```ada
{AI12-0112-1}   type Tree (Capacity : Count_Type) is tagged private...

```

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type Tree is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization

```

The type Tree needs finalization if and only if type Element_Type needs finalization.

Implementation Note: {AI05-0212-1} The type Tree cannot depend on package Ada.Finalization unless the element type depends on that package. The objects returned from the Iterator and Reference functions probably do depend on package Ada.Finalization. Restricted environments may need to avoid use of those functions and their associated types. 

{AI12-0339-1} The function Empty is replaced by:

```ada
   function Empty (Capacity : Count_Type := implementation-defined)
      return Tree
      with Post =&gt
            Empty'Result.Capacity = Capacity and then
            not Tampering_With_Elements_Prohibited (Empty'Result) and then
            not Tampering_With_Cursors_Prohibited (Empty'Result) and then
            Node_Count (Empty'Result) = 1;

```

{AI12-0112-1} For procedures Insert_Child, Prepend_Child, and Append_Child, the initial subexpression of the precondition is replaced with:

```ada
      with Pre =&gt (not Tampering_With_Cursors_Prohibited (Container)
                     or else raise Program_Error) and then
                  (Node_Count (Container) - 1 &lt= Container.Capacity - Count
                     or else raise Capacity_Error)

```

{AI12-0112-1} In procedure Assign, the precondition is altered to: 

```ada
   Pre =&gt (not Tampering_With_Cursors_Prohibited (Target)
             or else raise Program_Error) and then
          (Node_Count (Source) - 1 &lt= Target.Capacity
             or else raise Capacity_Error),

```

Function Copy is declared as follows: 

```ada
{AI12-0056-1}   function Copy (Source : Tree; Capacity : Count_Type := 0)
     return Tree
     with Pre  =&gt Capacity = 0 or else Capacity &gt= Node_Count (Source) - 1
                     or else raise Capacity_Error,
          Post =&gt 
             Node_Count (Copy'Result) = Node_Count (Source) and then
             not Tampering_With_Elements_Prohibited (Copy'Result) and then
             not Tampering_With_Cursors_Prohibited (Copy'Result) and then
             Copy'Result.Capacity = (if Capacity = 0 then
                Node_Count (Source) - 1 else Capacity);

```

Returns a list whose elements have the same values as the elements of Source. 

{AI12-0112-1} In the four-parameter procedure Copy_Subtree, the last or else of the precondition is replaced by:

```ada
      (not Is_Root (Source)
         or else raise Constraint_Error) and then
      (Node_Count (Target) - 1 + Subtree_Node_Count (Source) &lt= 
         Target.Capacity
         or else raise Capacity_Error),

```

{AI12-0112-1} In the five-parameter procedure Copy_Subtree, the last or else of the precondition is replaced by:

```ada
       (not Is_Root (Source, Subtree)
          or else raise Constraint_Error) and then
       (Node_Count (Target) - 1 +
             Subtree_Node_Count (Source, Subtree) &lt= Target.Capacity
          or else raise Capacity_Error),

```

{AI12-0112-1} In Copy_Local_Subtree, the last or else of the precondition is replaced by:

```ada
       (not Is_Root (Source, Subtree)
          or else raise Constraint_Error) and then
       (Node_Count (Target) - 1 +
             Subtree_Node_Count (Target, Source) &lt= Target.Capacity
          or else raise Capacity_Error),

```

{AI05-0136-1} {AI05-0248-1} {AI12-0112-1} In the five-parameter procedure Splice_Subtree, the penultimate or else of the precondition is replaced by:

```ada
       (Has_Element (Source, Position)
          or else raise Program_Error) and then
       (Target'Has_Same_Storage (Source) or else
        Node_Count (Target) - 1 +
          Subtree_Node_Count (Source, Position) &lt= Target.Capacity
          or else raise Capacity_Error) and then

```

{AI05-0136-1} {AI05-0248-1} {AI12-0112-1} In the five-parameter procedure Splice_Children, the penultimate elsif of the precondition is replaced by:

```ada
      (Before = No_Element or else
       Parent (Target, Before) /= Target_Parent
         or else raise Constraint_Error) and then
      (Target'Has_Same_Storage (Source) or else
       Node_Count (Target) - 1 +
         Child_Count (Source, Source_Parent) &lt= Target.Capacity
         or else raise Capacity_Error) and then

```


#### Bounded (Run-Time) Errors

{AI05-0160-1} {AI05-0265-1} It is a bounded error to assign from a bounded tree object while tampering with elements [or cursors] of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements [or cursors], or execution proceeds normally. 

Proof: Tampering with elements includes tampering with cursors, so we only really need to talk about tampering with elements here; we mention cursors for clarity. 


#### Erroneous Execution

{AI05-0265-1} When a bounded tree object T is finalized, if tampering with cursors is prohibited for T other than due to an assignment from another tree, then execution is erroneous. 

Reason: This is a tampering event, but since the implementation is not allowed to use Ada.Finalization, it is not possible in a pure Ada implementation to detect this error. (There is no Finalize routine that will be called that could make the check.) Since the check probably cannot be made, the bad effects that could occur (such as an iterator going into an infinite loop or accessing a nonexistent element) cannot be prevented and we have to allow anything. We do allow re-assigning an object that only prohibits tampering because it was copied from another object as that cannot cause any negative effects. 


#### Implementation Requirements

{AI05-0184-1} {AI05-0264-1} For each instance of Containers.Multiway_Trees and each instance of Containers.Bounded_Multiway_Trees, if the two instances meet the following conditions, then the output generated by the Tree'Output or Tree'Write subprograms of either instance shall be readable by the Tree'Input or Tree'Read of the other instance, respectively:

{AI05-0184-1} {AI05-0248-1} the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

{AI05-0184-1} the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters). 


#### Implementation Advice

{AI05-0136-1} Bounded tree objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded tree objects should be implemented without implicit pointers or dynamic allocation.

{AI05-0136-1} The implementation advice for procedure Move to minimize copying does not apply. 

Implementation Advice: The implementation advice for procedure Move to minimize copying does not apply to bounded trees.


#### Extensions to Ada 2005

{AI05-0136-1} {AI05-0184-1} The generic package Containers.Bounded_Multiway_Trees is new. 


#### Inconsistencies With Ada 2012

{AI12-0111-1} Correction: Tampering with elements is now defined to be equivalent to tampering with cursors for bounded containers. If a program requires tampering detection to work, it might fail in Ada 2022. Needless to say, this shouldn't happen outside of test programs. See Inconsistencies With Ada 2012 in A.18.2 for more details. 


#### Incompatibilities With Ada 2012

{AI12-0409-1} Correction:A bounded tree now only has Preelaborable_Initialization (abbreviated PI in this note) when the actual for the Element_Type has PI. If an program used a tree whose actual Element_Type does not have PI in a context when PI is required (such as a library-level object in a preelaborated unit or as a component of a type with PI), the program would be illegal in Ada 2022 but legal in original Ada 2012. This situation is unlikely, especially as some existing Ada 2012 implementations reject the instance in this case. 


## A.18.26  Array Sorting

{AI95-00302-03} {AI05-0001-1} The language-defined generic procedures Containers.Generic_Array_Sort, Containers.Generic_Constrained_Array_Sort, and Containers.Generic_Sort provide sorting on arbitrary array types. 


#### Static Semantics

{AI95-00302-03} The generic library procedure Containers.Generic_Array_Sort has the following declaration: 

```ada
{AI12-0112-1} generic
   type Index_Type is (&lt&gt);
   type Element_Type is private;
   type Array_Type is array (Index_Type range &lt&gt) of Element_Type;
   with function "&lt" (Left, Right : Element_Type)
      return Boolean is &lt&gt;
procedure Ada.Containers.Generic_Array_Sort (Container : in out Array_Type)
   with Pure, Nonblocking, Global =&gt null;

```

Discussion: {AI12-0112-1} Global =&gt null means that the only global side-effects allowed are associated with the actual generic parameters. Similarly, when Nonblocking is set to True for a generic unit, the only blocking allowed is that associated with the actual generic parameters. 

Reorders the elements of Container such that the elements are sorted smallest first as determined by the generic formal "&lt" operator provided. Any exception raised during evaluation of "&lt" is propagated.

{AI05-0044-1} {AI05-0262-1} The actual function for the generic formal function "&lt" of Generic_Array_Sort is expected to return the same value each time it is called with a particular pair of element values. It should define a strict weak ordering relationship (see A.18); it should not modify Container. If the actual for "&lt" behaves in some other manner, the behavior of the instance of Generic_Array_Sort is unspecified. The number of times Generic_Array_Sort calls "&lt" is unspecified.

Ramification: This implies swapping the elements, usually including an intermediate copy. This of course means that the elements will be copied. Since the elements are nonlimited, this usually will not be a problem. Note that there is Implementation Advice below that the implementation should use a sort that minimizes copying of elements.

The sort is not required to be stable (and the fast algorithm required will not be stable). If a stable sort is needed, the user can include the original location of the element as an extra "sort key". We considered requiring the implementation to do that, but it is mostly extra overhead -- usually there is something already in the element that provides the needed stability. 

{AI95-00302-03} The generic library procedure Containers.Generic_Constrained_Array_Sort has the following declaration: 

```ada
{AI12-0112-1} generic
   type Index_Type is (&lt&gt);
   type Element_Type is private;
   type Array_Type is array (Index_Type) of Element_Type;
   with function "&lt" (Left, Right : Element_Type)
      return Boolean is &lt&gt;
procedure Ada.Containers.Generic_Constrained_Array_Sort
      (Container : in out Array_Type)
   with Pure, Nonblocking, Global =&gt null;

```

Discussion: {AI12-0112-1} Global =&gt null means that the only global side-effects allowed are associated with the actual generic parameters. Similarly, when Nonblocking is set to True for a generic unit, the only blocking allowed is that associated with the actual generic parameters. 

Reorders the elements of Container such that the elements are sorted smallest first as determined by the generic formal "&lt" operator provided. Any exception raised during evaluation of "&lt" is propagated.

{AI05-0044-1} {AI05-0262-1} The actual function for the generic formal function "&lt" of Generic_Constrained_Array_Sort is expected to return the same value each time it is called with a particular pair of element values. It should define a strict weak ordering relationship (see A.18); it should not modify Container. If the actual for "&lt" behaves in some other manner, the behavior of the instance of Generic_Constrained_Array_Sort is unspecified. The number of times Generic_Constrained_Array_Sort calls "&lt" is unspecified.

{AI05-0001-1} The generic library procedure Containers.Generic_Sort has the following declaration: 

```ada
{AI12-0056-1} {AI12-0112-1} generic
   type Index_Type is (&lt&gt);
   with function Before (Left, Right : Index_Type) return Boolean;
   with procedure Swap (Left, Right : in Index_Type);
procedure Ada.Containers.Generic_Sort
      (First, Last : Index_Type'Base)
   with Pure, Nonblocking, Global =&gt null;

```

Discussion: {AI12-0112-1} Global =&gt null means that the only global side-effects allowed are associated with the actual generic parameters. Similarly, when Nonblocking is set to True for a generic unit, the only blocking allowed is that associated with the actual generic parameters. 

{AI05-0001-1} {AI05-0248-1} Reorders the elements of an indexable structure, over the range First .. Last, such that the elements are sorted in the ordering determined by the generic formal function Before; Before should return True if Left is to be sorted before Right. The generic formal Before compares the elements having the given indices, and the generic formal Swap exchanges the values of the indicated elements. Any exception raised during evaluation of Before or Swap is propagated.

The actual function for the generic formal function Before of Generic_Sort is expected to return the same value each time it is called with index values that identify a particular pair of element values. It should define a strict weak ordering relationship (see A.18); it should not modify the elements. The actual function for the generic formal Swap should exchange the values of the indicated elements. If the actual for either Before or Swap behaves in some other manner, the behavior of Generic_Sort is unspecified. The number of times the Generic_Sort calls Before or Swap is unspecified.


#### Implementation Advice

{AI95-00302-03} The worst-case time complexity of a call on an instance of Containers.Generic_Array_Sort or Containers.Generic_Constrained_Array_Sort should be O(N**2) or better, and the average time complexity should be better than O(N**2), where N is the length of the Container parameter. 

Implementation Advice: Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort should have an average time complexity better than O(N**2) and worst case no worse than O(N**2).

Discussion: In other words, we're requiring the use of a sorting algorithm better than O(N**2), such as Quicksort. No bubble sorts allowed! 

{AI95-00302-03} Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort should minimize copying of elements. 

Implementation Advice: Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort should minimize copying of elements.

To be honest: We do not mean "absolutely minimize" here; we're not intending to require a single copy for each element. Rather, we want to suggest that the sorting algorithm chosen is one that does not copy items unnecessarily. Bubble sort would not meet this advice, for instance. 

{AI05-0248-1} The worst-case time complexity of a call on an instance of Containers.Generic_Sort should be O(N**2) or better, and the average time complexity should be better than O(N**2), where N is the difference between the Last and First parameters plus 1. 

Implementation Advice: Containers.Generic_Sort should have an average time complexity better than O(N**2) and worst case no worse than O(N**2).

{AI05-0248-1} Containers.Generic_Sort should minimize calls to the generic formal Swap. 

Implementation Advice: Containers.Generic_Sort should minimize calls to the generic formal Swap.


#### Extensions to Ada 95

{AI95-00302-03} The generic procedures Containers.Generic_Array_Sort and Containers.Generic_Constrained_Array_Sort are new. 


#### Extensions to Ada 2005

{AI05-0001-1} {AI05-0248-1}  The generic procedure Containers.Generic_Sort is new. 


#### Wording Changes from Ada 2005

{AI05-0044-1} Correction: Redefined "&lt" actuals to require a strict weak ordering; the old definition allowed indeterminant comparisons that would not have worked in a sort. 


#### Wording Changes from Ada 2012

{AI12-0112-1} Added contracts to these procedures. 


## A.18.27  The Generic Package Containers.Synchronized_Queue_Interfaces

{AI05-0159-1} The language-defined generic package Containers.Synchronized_Queue_Interfaces provides interface type Queue, and a set of operations for that type. Interface Queue specifies a first-in, first-out queue. 


#### Static Semantics

{AI05-0159-1} The generic library package Containers.Synchronized_Queue_Interfaces has the following declaration: 

```ada
{AI12-0112-1} generic
   type Element_Type is private;
package Ada.Containers.Synchronized_Queue_Interfaces
   with Pure, Nonblocking, Global =&gt null is

```

Discussion: {AI12-0112-1} Global =&gt null means that the only global side-effects allowed are associated with the actual generic parameters. Similarly, when Nonblocking is set to True for a generic unit, the only blocking allowed is that associated with the actual generic parameters. 

```ada
   type Queue is synchronized interface;

```

```ada
{AI12-0112-1}    procedure Enqueue
     (Container : in out Queue;
      New_Item  : in     Element_Type) is abstract
       with Synchronization =&gt By_Entry,
           Nonblocking =&gt False,
           Global'Class=&gt in out synchronized;

```

```ada
{AI12-0112-1}    procedure Dequeue
     (Container : in out Queue;
      Element   :    out Element_Type) is abstract
       with Synchronization =&gt By_Entry,
           Nonblocking =&gt False,
           Global'Class=&gt in out synchronized;

```

```ada
{AI12-0112-1}    function Current_Use (Container : Queue) return Count_Type is abstract
       with Nonblocking, Global'Class =&gt null, Use_Formal =&gt null;
   function Peak_Use (Container : Queue) return Count_Type is abstract
       with Nonblocking, Global'Class =&gt null, Use_Formal =&gt null,
            Post'Class =&gt Peak_Use'Result &gt= Current_Use (Container);

```

```ada
end Ada.Containers.Synchronized_Queue_Interfaces;

```

{AI12-0112-1} The subprogram behavior descriptions given below are the semantics for the corresponding callable entities found in the language-defined generic packages that have a formal package named Queue_Interfaces.

```ada
{AI12-0112-1} procedure Enqueue
  (Container : in out Queue;
   New_Item  : in     Element_Type) is abstract
   with Synchronization =&gt By_Entry
        Nonblocking =&gt False,
        Global'Class=&gt in out synchronized;

```

{AI05-0159-1} {AI05-0262-1} {AI05-0264-1} A queue type that implements this interface is allowed to have a bounded capacity. If the queue object has a bounded capacity, and the number of existing elements equals the capacity, then Enqueue blocks until storage becomes available; otherwise, Enqueue does not block. In any case, it then copies New_Item onto the queue.

```ada
{AI12-0112-1} procedure Dequeue
  (Container : in out Queue;
   Element   :    out Element_Type) is abstract
   with Synchronization =&gt By_Entry
        Nonblocking =&gt False,
        Global'Class=&gt in out synchronized;

```

{AI05-0159-1} {AI05-0251-1} If the queue is empty, then Dequeue blocks until an item becomes available. In any case, it then assigns the element at the head of the queue to Element, and removes it from the queue.

```ada
{AI12-0112-1} function Current_Use (Container : Queue) return Count_Type is abstract
   with Nonblocking, Global'Class=&gt null, Use_Formal =&gt null;

```

{AI05-0159-1} Returns the number of elements currently in the queue.

```ada
{AI12-0112-1} function Peak_Use (Container : Queue) return Count_Type is abstract
   with Nonblocking, Global'Class=&gt null, Use_Formal =&gt null,
         Post'Class =&gt Peak_Use'Result &gt= Current_Use (Container);

```

{AI05-0159-1} Returns the maximum number of elements that have been in the queue at any one time.

NOTE   {AI05-0251-1} Unlike other language-defined containers, there are no queues whose element types are indefinite. Elements of an indefinite type can be handled by defining the element of the queue to be a holder container (see A.18.18) of the indefinite type, or to be an explicit access type that designates the indefinite type.

Reason: There are no indefinite queues, as a useful definition for Dequeue is not possible. Dequeue cannot be a function, as Ada does not have entries that are functions (thus conditional and timed calls would not be possible). Moreover, protected functions do not allow modifying the queue object (thus it doesn't work even if we decided we didn't care about conditional and timed calls). If Dequeue is an entry, then the dequeued object would have to be an out parameter and that would require the queue client to guess the tag and constraints of the value that will be dequeued (otherwise Constraint_Error would be raised), and that is rarely going to be possible. 


#### Extensions to Ada 2005

{AI05-0159-1} {AI05-0251-1}  The generic package Containers.Synchronized_Queue_Interfaces is new. 


## A.18.28  The Generic Package Containers.Unbounded_Synchronized_Queues


#### Static Semantics

{AI05-0159-1} The language-defined generic package Containers.Unbounded_Synchronized_Queues provides type Queue, which implements the interface type Containers.Synchronized_Queue_Interfaces.Queue.

```ada
{AI12-0112-1} with System;
with Ada.Containers.Synchronized_Queue_Interfaces;
generic
   with package Queue_Interfaces is
      new Ada.Containers.Synchronized_Queue_Interfaces (&lt&gt);
   Default_Ceiling : System.Any_Priority := System.Priority'Last;
package Ada.Containers.Unbounded_Synchronized_Queues
   with Preelaborate,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects of this generic package, see the notes on the specification of the Containers.Vectors package (see A.18.2). 

```ada
   package Implementation is
      ... -- not specified by the language
   end Implementation;

```

```ada
   protected type Queue
        (Ceiling : System.Any_Priority := Default_Ceiling)
           with Priority =&gt Ceiling is
        new Queue_Interfaces.Queue with

```

```ada
      overriding
      entry Enqueue (New_Item : in Queue_Interfaces.Element_Type);
      overriding
      entry Dequeue (Element : out Queue_Interfaces.Element_Type);

```

```ada
{AI12-0112-1}       overriding
      function Current_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;
      overriding
      function Peak_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
   private
      ... -- not specified by the language
   end Queue;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Unbounded_Synchronized_Queues;

```

{AI05-0159-1} The type Queue is used to represent task-safe queues.

{AI05-0159-1} The capacity for instances of type Queue is unbounded.

Ramification: Enqueue never blocks; if more storage is needed for a new element, it is allocated dynamically. We don't need to explicitly specify that Queue needs finalization, because it is visibly protected. 

Discussion: Nested package Implementation can be used to declare the types needed to implement the protected type Queue. This nested package is necessary as types cannot be declared in the private part of a protected type, and the types have to be declared within the generic unit in order to depend on the types imported with package Queue_Interfaces. Clients should never depend on the contents of nested package Implementation. 


#### Extensions to Ada 2005

{AI05-0159-1}  The generic package Containers.Unbounded_Synchronized_Queues is new. 


## A.18.29  The Generic Package Containers.Bounded_Synchronized_Queues


#### Static Semantics

{AI05-0159-1} The language-defined generic package Containers.Bounded_Synchronized_Queues provides type Queue, which implements the interface type Containers.Synchronized_Queue_Interfaces.Queue.

```ada
{AI12-0112-1} with System;
with Ada.Containers.Synchronized_Queue_Interfaces;
generic
   with package Queue_Interfaces is
      new Ada.Containers.Synchronized_Queue_Interfaces (&lt&gt);
   Default_Capacity : Count_Type;
   Default_Ceiling  : System.Any_Priority := System.Priority'Last;
package Ada.Containers.Bounded_Synchronized_Queues
   with Preelaborate,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects of this generic package, see the notes on the specification of the Containers.Vectors package (see A.18.2). 

```ada
   package Implementation is
      ... -- not specified by the language
   end Implementation;

```

```ada
   protected type Queue
        (Capacity : Count_Type := Default_Capacity;
         Ceiling  : System.Any_Priority := Default_Ceiling)
           with Priority =&gt Ceiling is
        new Queue_Interfaces.Queue with

```

```ada
      overriding
      entry Enqueue (New_Item : in Queue_Interfaces.Element_Type);
      overriding
      entry Dequeue (Element : out Queue_Interfaces.Element_Type);

```

```ada
{AI12-0112-1}       overriding
      function Current_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;
      overriding
      function Peak_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
   private
      ... -- not specified by the language
   end Queue;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Bounded_Synchronized_Queues;

```

{AI05-0159-1} The semantics are the same as for Unbounded_Synchronized_Queues, except:

The capacity for instances of type Queue is bounded and specified by the discriminant Capacity. 

Ramification: Since this type has a bounded capacity, Enqueue might block if the queue is full. 


#### Implementation Advice

{AI05-0159-1} Bounded queue objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded queue objects should be implemented without implicit pointers or dynamic allocation.


#### Extensions to Ada 2005

{AI05-0159-1}  The generic package Containers.Bounded_Synchronized_Queues is new. 


## A.18.30  The Generic Package Containers.Unbounded_Priority_Queues


#### Static Semantics

{AI05-0159-1} The language-defined generic package Containers.Unbounded_Priority_Queues provides type Queue, which implements the interface type Containers.Synchronized_Queue_Interfaces.Queue.

```ada
{AI12-0112-1} with System;
with Ada.Containers.Synchronized_Queue_Interfaces;
generic
   with package Queue_Interfaces is
      new Ada.Containers.Synchronized_Queue_Interfaces (&lt&gt);
   type Queue_Priority is private;
   with function Get_Priority
     (Element : Queue_Interfaces.Element_Type) return Queue_Priority is &lt&gt;
   with function Before
     (Left, Right : Queue_Priority) return Boolean is &lt&gt;
   Default_Ceiling : System.Any_Priority := System.Priority'Last;
package Ada.Containers.Unbounded_Priority_Queues
   with Preelaborate,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects of this generic package, see the notes on the specification of the Containers.Vectors package (see A.18.2). 

```ada
   package Implementation is
      ... -- not specified by the language
   end Implementation;

```

```ada
   protected type Queue
        (Ceiling : System.Any_Priority := Default_Ceiling)
           with Priority =&gt Ceiling is
        new Queue_Interfaces.Queue with

```

```ada
      overriding
      entry Enqueue (New_Item : in Queue_Interfaces.Element_Type);
      overriding
      entry Dequeue (Element : out Queue_Interfaces.Element_Type);

```

```ada
{AI05-0159-1} {AI05-0251-1}       not overriding
      procedure Dequeue_Only_High_Priority
        (At_Least : in     Queue_Priority;
         Element  : in out Queue_Interfaces.Element_Type;
         Success  :    out Boolean);

```

```ada
{AI12-0112-1}       overriding
      function Current_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;
      overriding
      function Peak_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
   private
      ... -- not specified by the language
   end Queue;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Unbounded_Priority_Queues;

```

{AI05-0159-1} The type Queue is used to represent task-safe priority queues.

{AI05-0159-1} The capacity for instances of type Queue is unbounded.

{AI05-0159-1} Two elements E1 and E2 are equivalent if Before(Get_Priority(E1), Get_Priority(E2)) and Before(Get_Priority(E2), Get_Priority(E1)) both return False.

{AI05-0159-1} {AI05-0248-1} The actual functions for Get_Priority and Before are expected to return the same value each time they are called with the same actuals, and should not modify their actuals. Before should define a strict weak ordering relationship (see A.18). If the actual functions behave in some other manner, the behavior of Unbounded_Priority_Queues is unspecified.

{AI05-0159-1} Enqueue inserts an item according to the order specified by the Before function on the result of Get_Priority on the elements; Before should return True if Left is to be inserted before Right. If the queue already contains elements equivalent to New_Item, then it is inserted after the existing equivalent elements.

Ramification: Enqueue never blocks; if more storage is needed for a new element, it is allocated dynamically. We don't need to explicitly specify that Queue needs finalization, because it is visibly protected. 

{AI05-0159-1} {AI05-0251-1} {AI05-0262-1} For a call on Dequeue_Only_High_Priority, if the head of the nonempty queue is E, and the function Before(At_Least, Get_Priority(E)) returns False, then E is assigned to Element and then removed from the queue, and Success is set to True; otherwise, Success is set to False and Element is unchanged.

Ramification: {AI05-0251-1} Unlike Dequeue, Dequeue_Only_High_Priority is not blocking; it always returns immediately. 

Reason: {AI05-0251-1} The use of Before is "backwards" so that it acts like "&gt=" (it is defined similarly to "&gt"); thus we dequeue only when it is False. 


#### Extensions to Ada 2005

{AI05-0159-1} {AI05-0251-1}  The generic package Containers.Unbounded_Priority_Queues is new. 


## A.18.31  The Generic Package Containers.Bounded_Priority_Queues


#### Static Semantics

{AI05-0159-1} The language-defined generic package Containers.Bounded_Priority_Queues provides type Queue, which implements the interface type Containers.Synchronized_Queue_Interfaces.Queue.

```ada
{AI12-0112-1} with System;
with Ada.Containers.Synchronized_Queue_Interfaces;
generic
   with package Queue_Interfaces is
      new Ada.Containers.Synchronized_Queue_Interfaces (&lt&gt);
   type Queue_Priority is private;
   with function Get_Priority
     (Element : Queue_Interfaces.Element_Type) return Queue_Priority is &lt&gt;
   with function Before
     (Left, Right : Queue_Priority) return Boolean is &lt&gt;
   Default_Capacity : Count_Type;
   Default_Ceiling  : System.Any_Priority := System.Priority'Last;
package Ada.Containers.Bounded_Priority_Queues
   with Preelaborate,
        Nonblocking, Global =&gt in out synchronized is

```

Discussion: {AI12-0112-1} For discussion on the reasons and meaning of the specifications of the Global and Nonblocking aspects of this generic package, see the notes on the specification of the Containers.Vectors package (see A.18.2). 

```ada
   package Implementation is
      ... -- not specified by the language
   end Implementation;

```

```ada
   protected type Queue
        (Capacity : Count_Type := Default_Capacity;
         Ceiling  : System.Any_Priority := Default_Ceiling)
           with Priority =&gt Ceiling is
      new Queue_Interfaces.Queue with

```

```ada
      overriding
      entry Enqueue (New_Item : in Queue_Interfaces.Element_Type);
      overriding
      entry Dequeue (Element : out Queue_Interfaces.Element_Type);

```

```ada
{AI05-0159-1} {AI05-0251-1}       not overriding
      procedure Dequeue_Only_High_Priority
        (At_Least : in     Queue_Priority;
         Element  : in out Queue_Interfaces.Element_Type;
         Success  :    out Boolean);

```

```ada
{AI12-0112-1}       overriding
      function Current_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;
      overriding
      function Peak_Use return Count_Type
         with Nonblocking, Global =&gt null, Use_Formal =&gt null;

```

```ada
   private
      ... -- not specified by the language
   end Queue;

```

```ada
private

```

```ada
   ... -- not specified by the language

```

```ada
end Ada.Containers.Bounded_Priority_Queues;

```

{AI05-0159-1} The semantics are the same as for Unbounded_Priority_Queues, except:

The capacity for instances of type Queue is bounded and specified by the discriminant Capacity. 

Ramification: Since this type has a bounded capacity, Enqueue might block if the queue is full. 


#### Implementation Advice

{AI05-0159-1} Bounded priority queue objects should be implemented without implicit pointers or dynamic allocation. 

Implementation Advice: Bounded priority queue objects should be implemented without implicit pointers or dynamic allocation.


#### Extensions to Ada 2005

{AI05-0159-1} {AI05-0251-1}  The generic package Containers.Bounded_Priority_Queues is new. 


## A.18.32  The Generic Package Containers.Bounded_Indefinite_Holders

{AI12-0254-1} The language-defined generic package Containers.Bounded_Indefinite_Holders provides a private type Holder and a set of operations for that type. It provides the same operations as the package Containers.Indefinite_Holders (see A.18.18), with the difference that the maximum storage is bounded. 


#### Static Semantics

{AI12-0254-1} The declaration of the generic library package Containers.Bounded_Indefinite_Holders has the same contents and semantics as Containers.Indefinite_Holders except:

The following is added to the context clause:

```ada
   with System.Storage_Elements; use System.Storage_Elements;

```

An additional generic parameter follows Element_Type:

```ada
   Max_Element_Size_in_Storage_Elements : Storage_Count;

```

Reason: This value is in Storage_Elements so that it can be used as a discriminant on a storage pool object in the implementation of the object; Ada doesn't allow discriminant dependent components to use formulas.

This is a generic parameter as it is a property of the Element_Type; the largest possible object of Element_Type is unlikely to be different for different containers, so making it a discriminant (as Capacity is) provides no useful capability. 

{AI12-0409-1} The [aspect_definition](./AA-13.1#S0348) for Preelaborable_Initialization for type Holder is changed to: 

```ada
  Preelaborable_Initialization =&gt
     Element_Type'Preelaborable_Initialization

```

Add to the precondition of To_Holder and Replace_Element:

```ada
  and then (New_Item'Size &lt=
               Max_Element_Size_in_Storage_Elements * System.Storage_Unit
            or else raise Program_Error)

```

Reason: This ensures that an object that won't fit is not inserted into the container. 


#### Bounded (Run-Time) Errors

{AI12-0254-1} It is a bounded error to assign from a bounded holder object while tampering with elements of that object is prohibited. Either Program_Error is raised by the assignment, execution proceeds with the target object prohibiting tampering with elements, or execution proceeds normally. 


#### Implementation Requirements

{AI12-0254-1} For each instance of Containers.Indefinite_Holders and each instance of Containers.Bounded_Indefinite_Holders, if the two instances meet the following conditions, then the output generated by the Holder'Output or Holder'Write subprograms of either instance shall be readable by the Holder'Input or Holder'Read of the other instance, respectively:

the Element_Type parameters of the two instances are statically matching subtypes of the same type; and

the output generated by Element_Type'Output or Element_Type'Write is readable by Element_Type'Input or Element_Type'Read, respectively (where Element_Type denotes the type of the two actual Element_Type parameters). 


#### Implementation Advice

{AI12-0254-1} Bounded holder objects should be implemented without dynamic allocation and any finalization should be trivial unless Element_Type needs finalization. 

Implementation Advice: Bounded holder objects should be implemented without dynamic allocation.

To be honest: Implementation of this container in Ada will probably require the use of a special storage pool. When we say "without dynamic allocation", we mean that this pool does not use heap memory and has a trivial finalization routine (that is, procedures Adjust and Finalize are null procedures). All storage pools are controlled, so we can't reasonably say that a bounded holder will not need finalization. 

Implementation Note: If the implementation supports discontiguous objects that require multiple calls to Allocate in a storage pool, the storage pool will need to support such allocations. The storage pool implementation can assume that all Allocate calls occur together, and similarly for Deallocate calls, thus simplifying the pool implementation so that allocation only occurs at a high-water mark location. 

{AI12-0350-1} {AI12-0445-1} The Implementation Advice about the Move and Swap operations is deleted for bounded holders; these operations can copy elements as necessary.

Reason: The memory of a bounded indefinite holder belongs directly to the container, so it cannot be moved with the element. If the element type contains any internal pointers, moving it without calling Adjust would leave such pointers pointing to the wrong holder object. Thus, a full copy is needed, including any associated finalization and adjustments. 


#### Extensions to Ada 2012

{AI12-0254-1}  The generic package Containers.Bounded_Indefinite_Holders is new. 


## A.18.33  Example of Container Use


#### Examples

{AI05-0212-1} The following example is an implementation of Dijkstra's shortest path algorithm in a directed graph with positive distances. The graph is represented by a map from nodes to sets of edges.

```ada
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
use Ada.Containers;
generic
   type Node is range &lt&gt;
package Shortest_Paths is
   type Distance is new Float range 0.0 .. Float'Last;
   type Edge is record
      To, From : Node;
      Length   : Distance;
   end record;

```

```ada
   package Node_Maps is new Vectors (Node, Node);
   -- The algorithm builds a map to indicate the node used to reach a given
   -- node in the shortest distance.

```

```ada
   package Adjacency_Lists is new Doubly_Linked_Lists (Edge);
   use Adjacency_Lists;

```

```ada
   package Graphs is new Vectors (Node, Adjacency_Lists.List);

```

```ada
   package Paths is new Doubly_Linked_Lists (Node);

```

```ada
   function Shortest_Path
     (G : Graphs.Vector; Source : Node; Target : Node) return Paths.List
      with Pre =&gt G (Source) /= Adjacency_Lists.Empty_List;

```

```ada
end Shortest_Paths;

```

```ada
{AI12-0178-1} package body Shortest_Paths is
   function Shortest_Path
     (G : Graphs.Vector; Source : Node; Target : Node) return Paths.List
   is
      use Node_Maps, Paths, Graphs;
      Reached  : array (Node) of Boolean := (others =&gt False);
      -- The set of nodes whose shortest distance to the source is known.

```

```ada
{AI05-0299-1}       Reached_From : array (Node) of Node;
      So_Far   : array (Node) of Distance := (others =&gt Distance'Last);
      The_Path : Paths.List := Paths.Empty_List;
      Nearest_Distance : Distance;
      Next     : Node;
   begin
      So_Far(Source)  := 0.0;

```

```ada
      while not Reached(Target) loop
         Nearest_Distance := Distance'Last;

```

```ada
         -- Find closest node not reached yet, by iterating over all nodes.
         -- A more efficient algorithm uses a priority queue for this step.

```

```ada
         Next := Source;
         for N in Node'First .. Node'Last loop
            if not Reached(N)
              and then So_Far(N) &lt Nearest_Distance then
                 Next := N;
                 Nearest_Distance := So_Far(N);
            end if;
         end loop;

```

```ada
{AI05-0299-1}          if Nearest_Distance = Distance'Last then
            -- No next node found, graph is not connected
            return Paths.Empty_List;

```

```ada
         else
            Reached(Next) := True;
         end if;

```

```ada
         -- Update minimum distance to newly reachable nodes.

```

```ada
{AI05-0299-1}          for E of G (Next) loop
            if not Reached(E.To) then
               Nearest_Distance := E.Length + So_Far(Next);

```

```ada
               if Nearest_Distance &lt So_Far(E.To) then
                  Reached_From(E.To) := Next;
                  So_Far(E.To) := Nearest_Distance;
               end if;
            end if;
         end loop;
      end loop;

```

```ada
      -- Rebuild path from target to source.

```

```ada
{AI12-0386-1}       declare
         N : Node := Target;
      begin
         Prepend (The_Path, N);
         while N /= Source loop
            N := Reached_From(N);
            Prepend (The_Path, N);
         end loop;
      end;

```

```ada
      return The_Path;
   end;
end Shortest_Paths;

```

{AI05-0212-1} Note that the effect of the Constant_Indexing aspect (on type Vector) and the Implicit_Dereference aspect (on type Reference_Type) is that

```ada
G (Next)

```

{AI05-0212-1} {AI12-0426-1} is a convenient shorthand for

```ada
G.Constant_Reference (Next).Element.all

```

{AI05-0212-1} Similarly, the effect of the loop:

```ada
for E of G (Next) loop
   if not Reached(E.To) then
      ...
   end if;
end loop;

```

{AI05-0212-1} is the same as:

```ada
{AI12-0080-1} for C in G (Next).Iterate loop
   declare
      E : Edge renames G (Next)(C);
   begin
      if not Reached(E.To) then
         ...
      end if;
   end;
end loop;

```

{AI05-0212-1} which is the same as:

```ada
{AI12-0080-1} declare
   L : Adjacency_Lists.List renames G (Next);
   C : Adjacency_Lists.Cursor := L.First;
begin
   while Has_Element (C) loop
      declare
         E : Edge renames L(C);
      begin
         if not Reached(E.To) then
            ...
         end if;
      end;
      C := L.Next (C);
   end loop;
end;

```


#### Wording Changes from Ada 2005

{AI05-0212-1} This example of container use is new. 

