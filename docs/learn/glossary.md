---
sidebar_position: 90
---

# Glossary

Terminology

### `<>`

"Box". Used for defaults or also "not specified."

### `'`

"Tick". Access built-in attributes of types.

### ABE

"Access before elaboration"

### allocator

"new" construct that allocates storage and returns a pointer.
Failure to allocate results in an exception being raised.

### access type

Similar to a pointer, a type which refers to the placement of another object in memory.

### access-to-object type

Pointer type that points to an object in memory. It can be divided further into access-to-constant vs access-to-variable, named access vs anonymous access, pool-specific access (that can only point to the heap) vs general access (that can point to both heap and stack).

### access-to-subprogram type

Pointer type that points to a subprogram (function or procedure).

### aggregate

The literal value for a composite object (array, record or container). A comma-separated list of values enclosed in parentheses or square-brackets (for homogeneous collections only, and only since Ada 2022).

### aliased

Objects (both variables and components) can be declared explicitly as aliased, so that it is valid to use attribute Access to point to the object.

### aspect

Additional specification attached to a declaration, either related to its behavior (like preconditions and postcondition for subprograms) or its representation (like size or alignment for objects).

### attribute

Value or function attached to a type or object, which can be retrieved using the syntax Type'Attribute or Object'Attribute. For example, attributes First and Last denote the first and last indexes of a (constrained) array type, or of any array object.

### ATC

"Asynchronous transfer of control"

### bounded error

The result of a violation of Ada program semantics, when the consequences of the error are precisely bounded by the language. E.g. reading an uninitialized variable may lead to any value of the corresponding base type being read.

### completion

An initial declaration for a type, constant, subprogram or package may be completed by a second declaration, called the completion of the initial declaration.

### component

A record field or array element.

### configuration pragma

A pragma at the very start of a file, or even provided in a separate file depending on the compiler, that applies to the compilation unit as a whole.

### controlled type

Type that supports RAII (Resource Acquisition Is Initialization) through the insertion by the compiler of calls to specific procedures at object creation, assignment and end-of-life.

### definite type

A type for which which requires no explicit constraint or initial value when declared.

### discriminant

Special field in record types, which may be used to control the structure of the type itself, either through a variant-clause (so the presence of other fields depends on the value of the discriminant) or through the constraint on the array subtype for the last field (so the size of this field depends on the value of the discriminant).

### entry

The other kind of callable entities, in addition to subprograms. It is used for queued operations called concurrently, as part of a task or protected object API.

### erroneous behavior

The result of a violation of Ada program semantics, when the consequences of the error are not bounded by the language. E.g. deactivating runtime checks and violating the corresponding conditions may lead to arbitrary code execution.

### indefinite type

A type for which you cannot declare an object without supply bounds a constraint or an initial value.

### imited type

An uncopyable type.

### parent

Non-abstract tagged type being extended.

### pragma

A directive to the compiler. There are many different pragmas defined in Ada, and even more are compiler-specific.

### progenitor

Additional interfaces inherited.

### qualification

Expression used to verify that an object respects the constraint of a subtype, using the syntax Subtype'Object. This is different from type conversion, as the object and its qualification share the same type.

### subprogram

A function (returning a result) or procedure (with no result). This does not include entries of tasks or protected objects, which are used for queued operations called concurrently.

### subtype

A type together with additional constraints, like a range of values for a scalar type. An object can be freely converted to a different subtype of the same type, but the corresponding constraint will be checked at runtime if necessary.

### tagged type

A type with an associated "tag", which specifies its type and allows for dynamic dispatch.

### type conversion

Expression to change the type of its argument, typically between different scalar types. There are no implicit type conversions in Ada.

### unchecked type conversion

Blind conversion of a bit pattern from one type to another, using the predefined generic function Ada.Unchecked_Conversion which must be instantiated with the types of source and target.
